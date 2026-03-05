# ============================================================
# 03_train_models_iter1.R  (Iteración 1)
# Model zoo base: SNaive(12), ETS, ARIMA, ARIMAX (con xregs)
# Genera predicciones de rolling backtest:
#  - ventana inicial: 24
#  - horizon: 12
#  - step: 1
#
# Outputs (local):
#  - data_processed/backtest_predictions_iter1.csv   (NO commitear)
#  - reports/models_iter1_log.md                     (sí puedes subir)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(forecast)
})

# ---------------------------
# Parámetros (ajústalos si quieres)
# ---------------------------
IN_PATH  <- "data_processed/model_ready.csv"
OUT_PRED <- "data_processed/backtest_predictions_iter1.csv"
OUT_LOG  <- "reports/models_iter1_log.md"

INITIAL_WINDOW <- 24
H <- 12
STEP <- 1

# Para no morir en tiempo si hay miles de series:
# (sube a Inf cuando ya quieras correr todo)
MAX_SERIES <- 40
SET_SEED <- 123

# ---------------------------
# Helpers
# ---------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

is_complete_monthly <- function(dates) {
  dates <- sort(unique(as.Date(dates)))
  if (length(dates) <= 1) return(TRUE)
  all(seq(min(dates), max(dates), by = "month") %in% dates) &&
    length(seq(min(dates), max(dates), by = "month")) == length(dates)
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

fit_forecast_safe <- function(model_name, y_train_ts, h, x_train = NULL, x_test = NULL) {
  # devuelve vector yhat (length h) o NA
  tryCatch({
    if (model_name == "SNAIVE") {
      as.numeric(forecast::forecast(forecast::snaive(y_train_ts), h = h)$mean)
    } else if (model_name == "ETS") {
      fit <- forecast::ets(y_train_ts)
      as.numeric(forecast::forecast(fit, h = h)$mean)
    } else if (model_name == "ARIMA") {
      fit <- forecast::auto.arima(y_train_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
      as.numeric(forecast::forecast(fit, h = h)$mean)
    } else if (model_name == "ARIMAX") {
      if (is.null(x_train) || is.null(x_test)) return(rep(NA_real_, h))
      fit <- forecast::auto.arima(y_train_ts, xreg = x_train, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
      as.numeric(forecast::forecast(fit, h = h, xreg = x_test)$mean)
    } else {
      rep(NA_real_, h)
    }
  }, error = function(e) rep(NA_real_, h))
}

# ---------------------------
# Cargar data
# ---------------------------
if (!file.exists(IN_PATH)) stop("No existe data_processed/model_ready.csv. Corre primero 01_merge_validate.R")

ensure_dir("data_processed")
ensure_dir("reports")

df <- readr::read_csv(IN_PATH, show_col_types = FALSE, progress = FALSE)

stopifnot(all(c("date", "serie_id", "y") %in% names(df)))

df <- df %>%
  mutate(
    date = as.Date(date),
    y = safe_num(y)
  ) %>%
  arrange(serie_id, date)

# Detectar xregs disponibles (numéricas y con algo de info)
xreg_candidates <- setdiff(names(df), c("date", "serie_id", "y"))
# quitamos dims si están presentes (no deben ser xregs)
dims_guess <- c("REGION","CANAL","FABRICANTE","CATEGORIA","MARCAS","SUBCATEGORIA","EMPAQUES","TAMANIOS")
xreg_candidates <- setdiff(xreg_candidates, intersect(dims_guess, names(df)))

# nos quedamos con columnas que puedan convertirse a numérico y no sean todo NA
xregs_ok <- xreg_candidates[sapply(xreg_candidates, function(cn) {
  v <- safe_num(df[[cn]])
  any(!is.na(v))
})]

# ---------------------------
# Elegir subset de series para iteración 1 (evitar sesgo alfabético)
# ---------------------------
series_all <- unique(df$serie_id)
set.seed(SET_SEED)
if (is.finite(MAX_SERIES) && length(series_all) > MAX_SERIES) {
  series_keep <- sample(series_all, MAX_SERIES)
} else {
  series_keep <- series_all
}

df <- df %>% filter(serie_id %in% series_keep)

# ---------------------------
# Rolling backtest por serie
# ---------------------------
models <- c("SNAIVE", "ETS", "ARIMA", "ARIMAX")

pred_list <- list()
log_skipped <- list()

series_ids <- unique(df$serie_id)

for (s in series_ids) {
  ds <- df %>% filter(serie_id == s) %>% arrange(date)

  # Requisito mínimo
  if (nrow(ds) < (INITIAL_WINDOW + H)) {
    log_skipped[[length(log_skipped) + 1]] <- paste0("- SKIP (muy corta): ", s, " | n=", nrow(ds))
    next
  }

  # Si hay gaps en fechas, saltamos en iteración 1 (para no meter imputación agresiva)
  if (!is_complete_monthly(ds$date)) {
    log_skipped[[length(log_skipped) + 1]] <- paste0("- SKIP (gaps fechas): ", s)
    next
  }

  # Si hay NA en y, saltamos en iteración 1 (luego podemos decidir imputación)
  if (any(is.na(ds$y))) {
    log_skipped[[length(log_skipped) + 1]] <- paste0("- SKIP (NA en y): ", s)
    next
  }

  # Construir ts
  start_y <- c(year(min(ds$date)), month(min(ds$date)))
  y_ts <- ts(ds$y, start = start_y, frequency = 12)

  # Matriz de xregs completa (si hay)
  xmat_all <- NULL
  if (length(xregs_ok) > 0) {
    xmat_all <- sapply(xregs_ok, function(cn) safe_num(ds[[cn]]))
    xmat_all <- as.matrix(xmat_all)
    colnames(xmat_all) <- xregs_ok
    # si alguna xreg tiene NA, ARIMAX puede fallar; por ahora:
    # requerimos que NO haya NA en filas usadas
  }

  n_total <- length(ds$y)
  origin_ends <- seq(INITIAL_WINDOW, n_total - H, by = STEP)

  for (end_idx in origin_ends) {
    train_dates <- ds$date[1:end_idx]
    test_dates  <- ds$date[(end_idx + 1):(end_idx + H)]

    y_train_ts <- window(y_ts, end = c(year(max(train_dates)), month(max(train_dates))))

    # ARIMAX xregs
    x_train <- NULL; x_test <- NULL
    if (!is.null(xmat_all)) {
      x_train <- xmat_all[1:end_idx, , drop = FALSE]
      x_test  <- xmat_all[(end_idx + 1):(end_idx + H), , drop = FALSE]

      # Si hay NA en xreg train/test, ARIMAX se pone frágil -> devolvemos NA
      if (any(is.na(x_train)) || any(is.na(x_test))) {
        x_train <- NULL; x_test <- NULL
      }
    }

    for (m in models) {
      yhat <- fit_forecast_safe(
        model_name = m,
        y_train_ts = y_train_ts,
        h = H,
        x_train = x_train,
        x_test = x_test
      )

      pred_list[[length(pred_list) + 1]] <- tibble(
        serie_id = s,
        origin_end_date = max(train_dates),
        date = test_dates,
        horizon = 1:H,
        model = m,
        y = ds$y[(end_idx + 1):(end_idx + H)],
        yhat = yhat
      )
    }
  }

  message("OK serie: ", s, " | origins: ", length(origin_ends))
}

pred <- bind_rows(pred_list)

# Guardar preds (local)
readr::write_csv(pred, OUT_PRED)

# Log Markdown (para subir)
lines <- c(
  "# Model Zoo Iteración 1 - Log",
  "",
  paste0("- Archivo input: `", IN_PATH, "`"),
  paste0("- Output preds (local): `", OUT_PRED, "` (NO commitear)"),
  paste0("- Ventana inicial: ", INITIAL_WINDOW, " | Horizon: ", H, " | Step: ", STEP),
  paste0("- Series consideradas: ", length(series_keep)),
  paste0("- Predicciones generadas (filas): ", nrow(pred)),
  "",
  "## Modelos incluidos",
  "- SNAIVE (12)",
  "- ETS",
  "- ARIMA",
  "- ARIMAX (xregs si disponibles y sin NA)",
  "",
  "## Series saltadas (si aplica)"
)

if (length(log_skipped) == 0) {
  lines <- c(lines, "- Ninguna.")
} else {
  lines <- c(lines, log_skipped)
}

source("src/03_models_zoo/03_train_models_iter1.R")
Error en writeLines(lines, OUT_LOG): 
  sólo se pueden escribir objetos de tipo character

message("✅ Iteración 1 lista.")
message(" - Preds: ", OUT_PRED)
message(" - Log:   ", OUT_LOG)
