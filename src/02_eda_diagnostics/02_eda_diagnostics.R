# ============================================================
# 02_eda_diagnostics.R
# - Lee data_processed/model_ready.csv (local)
# - Diagnóstico por serie_id: historia, gaps, NA, estacionalidad, outliers
# - Recomendación de model zoo (alto nivel)
# - Outputs:
#    - reports/eda_summary.md
#    - reports/series_quality.csv
#    - reports/series_outliers_top.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(lubridate)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# Config paths
# -----------------------------
in_path  <- "data_processed/model_ready.csv"
out_md   <- "reports/eda_summary.md"
out_q    <- "reports/series_quality.csv"
out_outl <- "reports/series_outliers_top.csv"

ensure_dir("reports")

if (!file.exists(in_path)) {
  stop(paste0("No existe ", in_path, ". Corre primero 01_merge_validate.R"))
}

df <- readr::read_csv(in_path, show_col_types = FALSE, progress = FALSE)

# checks mínimos
if (!all(c("date","serie_id","y") %in% names(df))) {
  stop("model_ready.csv debe tener columnas: date, serie_id, y")
}

df <- df %>%
  mutate(
    date = as.Date(date),
    y = as.numeric(y)
  )

# -----------------------------
# Helpers
# -----------------------------
expected_months <- function(min_d, max_d) {
  if (is.na(min_d) || is.na(max_d)) return(NA_integer_)
  length(seq(min_d, max_d, by = "month"))
}

safe_cor_lag12 <- function(x) {
  # correlación x[t] vs x[t-12] si hay suficientes puntos y varianza
  x <- x[!is.na(x)]
  if (length(x) < 30) return(NA_real_)     # regla simple: mínimo historia
  if (sd(x) == 0) return(NA_real_)
  # construir lag 12
  if (length(x) <= 12) return(NA_real_)
  x1 <- x[(13):length(x)]
  x0 <- x[1:(length(x)-12)]
  if (sd(x0) == 0 || sd(x1) == 0) return(NA_real_)
  suppressWarnings(cor(x0, x1))
}

robust_z <- function(x) {
  # z robusto con MAD
  med <- median(x, na.rm = TRUE)
  madv <- mad(x, center = med, constant = 1.4826, na.rm = TRUE)
  if (is.na(madv) || madv == 0) return(rep(NA_real_, length(x)))
  (x - med) / madv
}

# -----------------------------
# 1) Quality por serie
# -----------------------------
series_quality <- df %>%
  group_by(serie_id) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    n_rows = n(),
    n_unique_dates = n_distinct(date),
    expected = expected_months(min_date, max_date),
    gaps = ifelse(is.na(expected), NA_integer_, expected - n_unique_dates),
    y_na = sum(is.na(y)),
    y_na_pct = y_na / n_rows,
    y_mean = mean(y, na.rm = TRUE),
    y_sd = sd(y, na.rm = TRUE),
    seasonality_lag12_cor = safe_cor_lag12(y),
    .groups = "drop"
  ) %>%
  arrange(desc(n_rows))

write_csv(series_quality, out_q)

# -----------------------------
# 2) Outliers top (por serie)
# -----------------------------
outliers <- df %>%
  group_by(serie_id) %>%
  mutate(z_rob = robust_z(y)) %>%
  ungroup() %>%
  filter(!is.na(z_rob)) %>%
  arrange(desc(abs(z_rob))) %>%
  slice_head(n = 200) %>%
  select(date, serie_id, y, z_rob)

write_csv(outliers, out_outl)

# -----------------------------
# 3) Recomendación model zoo (texto basado en diagnóstico)
# -----------------------------
n_series <- n_distinct(df$serie_id)
date_min <- min(df$date, na.rm = TRUE)
date_max <- max(df$date, na.rm = TRUE)

# heurísticas simples
share_short <- mean(series_quality$n_unique_dates < 24, na.rm = TRUE)
share_gaps  <- mean(series_quality$gaps > 0, na.rm = TRUE)
share_seas  <- mean(series_quality$seasonality_lag12_cor >= 0.4, na.rm = TRUE)

rec_lines <- c(
  "# EDA Summary (Forecasting Bebidas Lácteas)",
  "",
  "## Panorama general",
  paste0("- Series (serie_id): **", n_series, "**"),
  paste0("- Rango de fechas: **", date_min, "** a **", date_max, "**"),
  paste0("- % series con historia < 24 meses: **", round(100*share_short,1), "%**"),
  paste0("- % series con gaps (meses faltantes): **", round(100*share_gaps,1), "%**"),
  paste0("- % series con señal estacional (corr lag12 >= 0.4): **", round(100*share_seas,1), "%**"),
  "",
  "## Hallazgos clave",
  "- Revisar `reports/series_quality.csv` para ver gaps, NA y largo de historia por serie.",
  "- Revisar `reports/series_outliers_top.csv` para valores atípicos (z-score robusto alto).",
  "",
  "## Recomendación de model zoo (más allá de ARIMAX)",
  "### Baselines (siempre)",
  "- Seasonal Naive (SNaive) como benchmark duro.",
  "- ETS (Holt-Winters) para series estables.",
  "",
  "### Estadísticos",
  "- SARIMA / ARIMA: para series con autocorrelación clara.",
  "- ARIMAX: cuando xregs aportan señal (precio, distribución, macros).",
  "- STL + ETS: robusto cuando hay estacionalidad y tendencia.",
  "- TBATS: si hay estacionalidad compleja o patrones no lineales.",
  "",
  "### ML (si hay suficiente historia y señal)",
  "- XGBoost/LightGBM con features (lags, rolling means, dummies mes) + xregs.",
  "- Útil si hay no-linealidad (promos, distribución, shocks).",
  "",
  "### Ensamble (recomendado)",
  "- Promedio ponderado de top 2–3 modelos por serie/horizonte para robustez.",
  "",
  "## Reglas sugeridas para selección",
  "- Priorizar **MASE** y **R² out-of-sample** en rolling backtest (ventana 24, step 1, horizon 12).",
  "- Elegir modelo por serie y también un 'winner' agregado por segmento si se requiere.",
  ""
)

writeLines(rec_lines, out_md)

message("✅ EDA listo.")
message(" - Reporte: ", out_md)
message(" - Quality: ", out_q)
message(" - Outliers: ", out_outl)
