# ============================================================
# ARIMAX PANEL + RADDAR + (PRECIO + DIST) + EMPAQUES/TAMANIOS
# - Forecast volumen/valor por series (REGION/CANAL/FAB/.../EMPAQUES/TAMANIOS)
# - ARIMAX candidates prueban combinaciones de:
#     precio_promedio, dist_pond, dist_num (y subconjuntos)
# - FIX fechas robustas + FIX lags/ma6 futuro + carry-forward xregs faltantes
# - Incluye baselines: SNaive(12), ETS(auto), Drift_seasonalized
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(stringi)
  library(zoo)
  library(ggplot2)
  library(forecast)
  library(purrr)
  library(readr)
  library(tibble)
})

options(stringsAsFactors = FALSE)

# ============================================================
# 0) Helpers generales
# ============================================================

normalize_names <- function(nms){
  nms %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

pick_first_name <- function(cands, pool){
  x <- intersect(cands, pool)
  if (length(x) == 0) NA_character_ else x[1]
}

as_numeric_flex <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  s <- as.character(x)
  s <- str_replace_all(s, "\\s+", "")
  
  out <- suppressWarnings(readr::parse_number(
    s, locale = locale(decimal_mark=".", grouping_mark=",")
  ))
  if (all(is.na(out))) {
    out <- suppressWarnings(readr::parse_number(
      s, locale = locale(decimal_mark=",", grouping_mark=".")
    ))
  }
  out
}

parse_periodo_to_date <- function(x){
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  # 1) numéricos (Excel serial / yyyymm / yyyymmdd)
  if (is.numeric(x)) {
    out <- rep(as.Date(NA), length(x))
    
    excel_like <- !is.na(x) & x > 20000 & x < 60000
    out[excel_like] <- as.Date(x[excel_like], origin = "1899-12-30")
    
    yyyymmdd <- !excel_like & !is.na(x) & x >= 19000101 & x <= 21001231
    out[yyyymmdd] <- suppressWarnings(lubridate::ymd(as.character(x[yyyymmdd])))
    
    yyyymm <- !excel_like & !yyyymmdd & !is.na(x) & x >= 190001 & x <= 210012
    if (any(yyyymm)) {
      s <- as.character(x[yyyymm])
      out[yyyymm] <- suppressWarnings(lubridate::ymd(
        paste0(substr(s,1,4), "-", substr(s,5,6), "-01")
      ))
    }
    return(out)
  }
  
  # 2) strings
  s <- as.character(x)
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- tolower(s)
  s <- stringr::str_squish(s)
  s[s %in% c("", "na", "nan")] <- NA_character_
  
  out <- rep(as.Date(NA), length(s))
  
  dt_try <- suppressWarnings(lubridate::parse_date_time(
    s,
    orders = c("Ymd", "Y-m-d", "Y/m/d", "d/m/Y", "d-m-Y", "Y-m", "Y/m", "m/Y", "m-Y"),
    locale = "C"
  ))
  out[!is.na(dt_try)] <- as.Date(dt_try[!is.na(dt_try)])
  
  idx <- which(is.na(out) & !is.na(s))
  if (length(idx) > 0) {
    ss <- s[idx]
    mes_txt <- stringr::str_extract(ss, "[a-z]+")
    anio4 <- stringr::str_extract(ss, "\\d{4}")
    anio2 <- stringr::str_extract(ss, "\\d{2}")
    
    mes_num <- dplyr::case_when(
      stringr::str_detect(mes_txt, "^ene|^jan") ~ "01",
      stringr::str_detect(mes_txt, "^feb") ~ "02",
      stringr::str_detect(mes_txt, "^mar") ~ "03",
      stringr::str_detect(mes_txt, "^abr|^apr") ~ "04",
      stringr::str_detect(mes_txt, "^may") ~ "05",
      stringr::str_detect(mes_txt, "^jun") ~ "06",
      stringr::str_detect(mes_txt, "^jul") ~ "07",
      stringr::str_detect(mes_txt, "^ago|^aug") ~ "08",
      stringr::str_detect(mes_txt, "^sep|^set") ~ "09",
      stringr::str_detect(mes_txt, "^oct") ~ "10",
      stringr::str_detect(mes_txt, "^nov") ~ "11",
      stringr::str_detect(mes_txt, "^dic|^dec") ~ "12",
      TRUE ~ NA_character_
    )
    
    anio <- ifelse(!is.na(anio4), anio4, ifelse(!is.na(anio2), paste0("20", anio2), NA_character_))
    ym01 <- ifelse(!is.na(anio) & !is.na(mes_num), paste0(anio, "-", mes_num, "-01"), NA_character_)
    out[idx] <- suppressWarnings(lubridate::ymd(ym01))
  }
  
  out
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ============================================================
# Helpers extra (ensure cols, shift, break dummy, baselines, wmean)
# ============================================================

ensure_cols <- function(df, cols, fill = NA_real_) {
  cols <- unique(cols)
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- fill
  }
  df
}

shift_cols_lag <- function(df, cols, k){
  if (is.null(k) || k == 0) return(df)
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(df)
  df <- df %>% arrange(fecha)
  for (cc in cols) df[[cc]] <- dplyr::lag(df[[cc]], k)
  df
}

detect_break_dummy <- function(train_df, y_col = "ventas_litros", min_seg = 6) {
  df <- train_df %>% arrange(fecha)
  y  <- df[[y_col]]
  n  <- length(y)
  
  if (n < (2 * min_seg + 1)) {
    cut_date <- df$fecha[ceiling(n/2)]
    return(list(cut_date = cut_date, dummy = as.integer(df$fecha > cut_date)))
  }
  
  cand <- (min_seg + 1):(n - min_seg)
  sse <- vapply(cand, function(i){
    y1 <- y[1:i]; y2 <- y[(i+1):n]
    m1 <- mean(y1, na.rm = TRUE); m2 <- mean(y2, na.rm = TRUE)
    sum((y1 - m1)^2, na.rm = TRUE) + sum((y2 - m2)^2, na.rm = TRUE)
  }, numeric(1))
  
  i_best  <- cand[which.min(sse)]
  cut_date <- df$fecha[i_best]
  list(cut_date = cut_date, dummy = as.integer(df$fecha > cut_date))
}

predict_baselines <- function(y_train, y_test, freq = 12, ts_start_year = 2000, ts_start_month = 1) {
  h <- length(y_test)
  y_tr_ts <- ts(as.numeric(y_train), frequency = freq, start = c(ts_start_year, ts_start_month))
  list(
    snaive12 = as.numeric(forecast::snaive(y_tr_ts, h = h)$mean)
  )
}

wmean_safe <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  ww <- w[ok]
  if (!is.finite(sum(ww)) || abs(sum(ww)) < 1e-9) return(mean(x[ok], na.rm = TRUE))
  sum(x[ok] * ww, na.rm = TRUE) / sum(ww, na.rm = TRUE)
}

# ============================================================
# 1) Rutas + carga
# ============================================================

ruta_hist <- "C:/Users/Federico.camacho/Downloads/Datos_nielsen_base_v3.xlsx"
ruta_radd <- "C:/Users/Federico.camacho/Downloads/Proyecciones_raddar_1.0.xlsx"

if (!file.exists(ruta_hist)) message("⚠️ No encuentro ruta_hist. Ajusta ruta_hist: ", ruta_hist)
if (!file.exists(ruta_radd)) message("⚠️ No encuentro ruta_radd. Ajusta ruta_radd: ", ruta_radd)

df_hist <- read_excel(ruta_hist)
df_radd <- read_excel(ruta_radd)

names(df_hist) <- normalize_names(names(df_hist))
names(df_radd) <- normalize_names(names(df_radd))

# ============================================================
# 2) Mapear columnas (histórico) + NUEVO: empaques/tamanios + precio/dist
# ============================================================

nm_periodo    <- pick_first_name(c("periodo","periods"), names(df_hist))
nm_fecha_hist <- pick_first_name(c("fecha"), names(df_hist))
nm_region     <- pick_first_name(c("region"), names(df_hist))
nm_canal      <- pick_first_name(c("canal"), names(df_hist))
nm_fabricante <- pick_first_name(c("fabricante"), names(df_hist))
nm_categoria  <- pick_first_name(c("categoria"), names(df_hist))
nm_marcas     <- pick_first_name(c("marcas","marca"), names(df_hist))
nm_subcat     <- pick_first_name(c("subcategoria","sub_categoria"), names(df_hist))

nm_empaques   <- pick_first_name(c("empaques","empaque","packaging"), names(df_hist))
nm_tamanios   <- pick_first_name(c("tamanios","tamanos","tamano","size"), names(df_hist))

nm_vol        <- pick_first_name(c("venta_litros","ventas_litros","volumen","ventas","venta_volumen"), names(df_hist))
nm_val        <- pick_first_name(c("ventas_valor","venta_valor","valor","sales_value"), names(df_hist))

nm_precio     <- pick_first_name(c("precio_promedio","precio","price_avg","precio_medio"), names(df_hist))
nm_dist_num   <- pick_first_name(c("dist_num_tiendas_handling","dist_num","dist_numerica","distribucion_numerica"), names(df_hist))
nm_dist_pond  <- pick_first_name(c("dist_pond_tiendas_handling","dist_pond","dist_ponderada","distribucion_ponderada"), names(df_hist))

stopifnot(!is.na(nm_vol))
stopifnot(!is.na(nm_region), !is.na(nm_canal), !is.na(nm_fabricante),
          !is.na(nm_categoria), !is.na(nm_marcas), !is.na(nm_subcat))

# ---- parse fecha robusto ----
if (!is.na(nm_fecha_hist)) {
  df_hist <- df_hist %>% mutate(fecha = parse_periodo_to_date(.data[[nm_fecha_hist]]))
} else if (!is.na(nm_periodo)) {
  df_hist <- df_hist %>% mutate(fecha = parse_periodo_to_date(.data[[nm_periodo]]))
} else {
  stop("Histórico: no encuentro ni fecha ni periodo/periods.")
}

if (any(is.na(df_hist$fecha))) {
  message("⚠️ Histórico: hay fechas que NO pude parsear. Ejemplos:")
  col_raw <- if (!is.na(nm_fecha_hist)) nm_fecha_hist else nm_periodo
  print(head(df_hist[[col_raw]][is.na(df_hist$fecha)], 20))
}

df_hist_clean <- df_hist %>%
  transmute(
    fecha = parse_periodo_to_date(fecha),
    REGION = as.character(.data[[nm_region]]),
    CANAL = as.character(.data[[nm_canal]]),
    FABRICANTE = as.character(.data[[nm_fabricante]]),
    CATEGORIA = as.character(.data[[nm_categoria]]),
    MARCAS = as.character(.data[[nm_marcas]]),
    SUBCATEGORIA = as.character(.data[[nm_subcat]]),
    
    # dims extra (si no existen quedan "TOTAL")
    EMPAQUES = if (!is.na(nm_empaques)) as.character(.data[[nm_empaques]]) else "TOTAL",
    TAMANIOS = if (!is.na(nm_tamanios)) as.character(.data[[nm_tamanios]]) else "TOTAL",
    
    ventas_litros = as_numeric_flex(.data[[nm_vol]]),
    valor = if (!is.na(nm_val)) as_numeric_flex(.data[[nm_val]]) else NA_real_,
    
    precio_promedio = if (!is.na(nm_precio)) as_numeric_flex(.data[[nm_precio]]) else NA_real_,
    dist_num = if (!is.na(nm_dist_num)) as_numeric_flex(.data[[nm_dist_num]]) else NA_real_,
    dist_pond = if (!is.na(nm_dist_pond)) as_numeric_flex(.data[[nm_dist_pond]]) else NA_real_
  ) %>%
  filter(!is.na(fecha)) %>%
  mutate(
    serie_id = paste(REGION, CANAL, FABRICANTE, CATEGORIA, MARCAS, SUBCATEGORIA, EMPAQUES, TAMANIOS, sep = " | ")
  ) %>%
  group_by(serie_id, fecha, REGION, CANAL, FABRICANTE, CATEGORIA, MARCAS, SUBCATEGORIA, EMPAQUES, TAMANIOS) %>%
  summarise(
    ventas_litros = sum(ventas_litros, na.rm = TRUE),
    valor         = sum(valor, na.rm = TRUE),
    
    # precio: ponderado por litros; si no existe -> valor/litros
    precio_promedio = {
      p <- wmean_safe(precio_promedio, ventas_litros)
      if (!is.finite(p) || is.na(p)) sum(valor, na.rm = TRUE) / pmax(sum(ventas_litros, na.rm = TRUE), 1e-9) else p
    },
    
    # dists: ponderadas por valor (cámbialo a ventas_litros si lo prefieres)
    dist_num  = wmean_safe(dist_num,  valor),
    dist_pond = wmean_safe(dist_pond, valor),
    
    .groups = "drop"
  ) %>%
  arrange(serie_id, fecha)

# ============================================================
# 3) Mapear columnas (RADDAR) + limpieza
# ============================================================

nm_periods_radd <- pick_first_name(c("periodo","periods"), names(df_radd))
nm_fecha_radd   <- pick_first_name(c("fecha"), names(df_radd))
nm_inf          <- pick_first_name(c("inflacion","ipc"), names(df_radd))
nm_ghog         <- pick_first_name(c("gasto_hogares_raddar","gasto_hogares"), names(df_radd))
nm_gali         <- pick_first_name(c("gasto_alimentos"), names(df_radd))

stopifnot(!is.na(nm_inf), !is.na(nm_ghog), !is.na(nm_gali))

if (!is.na(nm_fecha_radd)) {
  df_radd <- df_radd %>% mutate(fecha = parse_periodo_to_date(.data[[nm_fecha_radd]]))
} else if (!is.na(nm_periods_radd)) {
  df_radd <- df_radd %>% mutate(fecha = parse_periodo_to_date(.data[[nm_periods_radd]]))
} else {
  stop("Raddar: no encuentro ni fecha ni periodo/periods.")
}

if (any(is.na(df_radd$fecha))) {
  message("⚠️ Raddar: hay fechas que NO pude parsear. Ejemplos:")
  col_raw <- if (!is.na(nm_fecha_radd)) nm_fecha_radd else nm_periods_radd
  print(head(df_radd[[col_raw]][is.na(df_radd$fecha)], 20))
}

df_radd_clean <- df_radd %>%
  transmute(
    fecha = parse_periodo_to_date(fecha),
    Inflacion = as_numeric_flex(.data[[nm_inf]]),
    Gasto_hogares_raddar = as_numeric_flex(.data[[nm_ghog]]),
    Gasto_alimentos = as_numeric_flex(.data[[nm_gali]])
  ) %>%
  filter(!is.na(fecha)) %>%
  group_by(fecha) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(fecha) %>%
  mutate(across(where(is.numeric), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
  mutate(across(where(is.numeric), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE)))

# ============================================================
# 4) Merge panel + macro
# ============================================================

Datos_prediccion <- df_hist_clean %>%
  left_join(df_radd_clean, by = "fecha") %>%
  arrange(serie_id, fecha)

# ============================================================
# 5) Features + métricas
# ============================================================

calendar_features <- function(df, fecha_col = "fecha") {
  df <- df %>%
    mutate(fecha = parse_periodo_to_date(.data[[fecha_col]]))
  
  bad <- which(is.na(df$fecha))
  if (length(bad) > 0) {
    message("⚠️ calendar_features: fechas no parseadas (ejemplos):")
    print(head(df[[fecha_col]][bad], 15))
  }
  
  df %>%
    mutate(
      anio = year(fecha),
      mes  = month(fecha),
      fin_anio      = if_else(mes %in% c(10,11,12), 1L, 0L),
      vacaciones    = if_else(mes %in% c(1,7,12), 1L, 0L),
      es_verano     = if_else(mes %in% c(12,1,2), 1L, 0L),
      fin_trimestre = if_else(mes %in% c(3,6,9,12), 1L, 0L),
      sin12 = sin(2*pi*mes/12),
      cos12 = cos(2*pi*mes/12)
    )
}

add_lags_ma <- function(df, y_col = "ventas_litros") {
  y  <- df[[y_col]]
  y1 <- dplyr::lag(y, 1)
  df %>%
    mutate(
      lag_1 = dplyr::lag(.data[[y_col]], 1),
      lag_2 = dplyr::lag(.data[[y_col]], 2),
      lag_3 = dplyr::lag(.data[[y_col]], 3),
      ma3 = zoo::rollmeanr(y1, k = 3, fill = NA_real_),
      ma6 = zoo::rollmeanr(y1, k = 6, fill = NA_real_)
    )
}

calc_basic_metrics <- function(df, y_col="real", yhat_col="pred", mase_den=NULL){
  real <- df[[y_col]]; pred <- df[[yhat_col]]
  mae  <- mean(abs(real - pred), na.rm=TRUE)
  rmse <- sqrt(mean((real - pred)^2, na.rm=TRUE))
  mape <- mean(abs((real - pred)/pmax(1e-9, abs(real))), na.rm=TRUE)*100
  if (is.null(mase_den)) mase_den <- mean(abs(diff(real)), na.rm=TRUE)
  if (!is.finite(mase_den) || mase_den <= 1e-9) mase_den <- 1
  mase <- mae / mase_den
  rss <- sum((real - pred)^2, na.rm=TRUE)
  tss <- sum((real - mean(real, na.rm=TRUE))^2, na.rm=TRUE)
  r2 <- if (is.finite(tss) && tss > 0) 1 - (rss/tss) else NA_real_
  dirhit <- mean(sign(real - dplyr::lag(real)) == sign(pred - dplyr::lag(pred)), na.rm=TRUE)
  tibble(MAE=mae, RMSE=rmse, MAPE=mape, MASE=mase, DirHit=dirhit, R2=r2)
}

seasonal_indices_multip <- function(y_train, start_year, start_month, freq = 12) {
  y_pos <- pmax(as.numeric(y_train), 1e-6)
  if (length(y_pos) < 2*freq) return(rep(1, freq))
  ts_tr <- ts(y_pos, frequency = freq, start = c(start_year, start_month))
  dec <- suppressWarnings(stats::decompose(ts_tr, type = "multiplicative"))
  seas <- as.numeric(dec$seasonal[1:freq])
  if (all(is.na(seas))) return(rep(1, freq))
  seas / mean(seas, na.rm = TRUE)
}

forecast_drift_seasonalized <- function(y_train, h, start_year, start_month, freq=12){
  ts_tr <- ts(as.numeric(y_train), frequency=freq, start=c(start_year, start_month))
  base <- forecast::rwf(ts_tr, drift=TRUE, h=h)$mean
  idx <- seasonal_indices_multip(y_train, start_year, start_month, freq)
  last_month <- ((start_month - 1 + length(y_train)) %% freq) + 1
  fut_idx <- idx[ ((last_month + seq_len(h) - 1) %% freq) + 1 ]
  as.numeric(base) * fut_idx
}

# ============================================================
# 6) Modelo por SERIE
# - ARIMAX candidates prueban variantes de xregs (precio/dist)
# ============================================================

run_model_for_series <- function(
    serie_id, data_panel, macro_future,
    n_holdout = 6, y_col = "ventas_litros",
    out_dir = "arimax_out_panel", verbose = TRUE,
    candidates = c(
      "arimax_nolagY","arimax_full","arimax_break","arimax_min",
      "arimax_log_bc","arimax_xreg_shift",
      "snaive12","ETS(auto)","Theta","Drift_seasonalized"
    ),
    bias_correction = c("multiplicative","additive","none"),
    
    # NUEVO: subconjuntos a testear (puedes recortar la lista si quieres más rápido)
    xreg_variants = list(
      all     = c("precio_promedio","dist_num","dist_pond"),
      p_dp    = c("precio_promedio","dist_pond"),
      p_dn    = c("precio_promedio","dist_num"),
      p_only  = c("precio_promedio"),
      dp_only = c("dist_pond"),
      dn_only = c("dist_num"),
      none    = character(0)
    )
) {
  
  bias_correction <- match.arg(bias_correction)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  safe <- function(expr, default = NULL, step = "") {
    tryCatch(expr, error = function(e) {
      if (isTRUE(verbose)) message("[", serie_id, "] ERROR en ", step, ": ", e$message)
      default
    })
  }
  
  # labels para nombres consistentes
  variant_labels <- c(
    all     = "P+DN+DP",
    p_dp    = "P+DP",
    p_dn    = "P+DN",
    p_only  = "P",
    dp_only = "DP",
    dn_only = "DN",
    none    = "MACRO"
  )
  
  # -----------------------
  # 1) Datos base + features
  # -----------------------
  datos_raw <- data_panel %>%
    filter(.data$serie_id == serie_id) %>%
    mutate(fecha = parse_periodo_to_date(fecha)) %>%
    arrange(fecha)
  
  # asegurar que existan (aunque sean NA) para que las variantes corran
  datos_raw <- ensure_cols(datos_raw, c("precio_promedio","dist_num","dist_pond"), fill = NA_real_)
  
  if (nrow(datos_raw) < (n_holdout + 18)) {
    return(list(
      serie_id = serie_id, status = "insuficiente_hist",
      metrics_table = tibble(), df_backtest = tibble(),
      df_forecast = tibble(), best_model = NA_character_
    ))
  }
  
  meta_cols <- intersect(
    c("REGION","CANAL","FABRICANTE","CATEGORIA","MARCAS","SUBCATEGORIA","EMPAQUES","TAMANIOS","serie_id"),
    names(datos_raw)
  )
  meta <- if (length(meta_cols) > 0) datos_raw %>% slice(1) %>% select(all_of(meta_cols)) else tibble(serie_id = serie_id)
  
  datos_feat <- datos_raw %>%
    calendar_features("fecha") %>%
    arrange(fecha) %>%
    mutate(mes_n = row_number()) %>%
    add_lags_ma(y_col)
  
  datos <- datos_feat %>%
    filter(!is.na(.data[[y_col]])) %>%
    filter(!is.na(lag_3) & !is.na(ma3) & !is.na(ma6)) %>%
    arrange(fecha)
  
  if (nrow(datos) <= (n_holdout + 12)) {
    return(list(
      serie_id = serie_id, status = "insuficiente_postlags",
      metrics_table = tibble(), df_backtest = tibble(),
      df_forecast = tibble(), best_model = NA_character_
    ))
  }
  
  fechas_ord <- sort(unique(datos$fecha))
  cutoff <- fechas_ord[length(fechas_ord) - n_holdout]
  train <- datos %>% filter(fecha <= cutoff)
  test  <- datos %>% filter(fecha >  cutoff)
  
  freq <- 12
  h    <- nrow(test)
  
  ts_start_year  <- year(min(datos$fecha))
  ts_start_month <- month(min(datos$fecha))
  
  mase_den <- mean(abs(diff(train[[y_col]])), na.rm = TRUE)
  if (!is.finite(mase_den) || mase_den <= 1e-9) mase_den <- 1
  
  # -----------------------
  # 2) Sets base (sin incluir extra_x)
  # -----------------------
  macro_cols <- c("Inflacion","Gasto_hogares_raddar","Gasto_alimentos")
  
  base_full <- c(
    macro_cols,
    "lag_1","lag_2","lag_3","ma6",
    "fin_anio","fin_trimestre","sin12","cos12","mes_n"
  )
  base_nolagY <- c(
    macro_cols,
    "fin_anio","fin_trimestre","sin12","cos12","mes_n"
  )
  base_min <- c(
    macro_cols,
    "sin12","cos12","mes_n"
  )
  
  make_scaler_local <- function(M_train) {
    M_train <- as.matrix(M_train)
    mu  <- apply(M_train, 2, function(x) mean(x, na.rm = TRUE))
    sdv <- apply(M_train, 2, function(x) sd(x, na.rm = TRUE))
    sdv[sdv == 0 | is.na(sdv)] <- 1
    list(
      transform = function(M) {
        M <- as.matrix(M)
        M <- sweep(M, 2, mu, "-")
        M <- sweep(M, 2, sdv, "/")
        M
      }
    )
  }
  
  # -----------------------
  # 3) ARIMAX rolling (nivel)
  # -----------------------
  roll_arimax <- function(train_df, test_df, x_cols){
    x_cols <- intersect(x_cols, names(train_df))
    if (length(x_cols) == 0) return(list(pred = rep(NA_real_, nrow(test_df))))
    
    train_x <- train_df[, x_cols, drop = FALSE] %>%
      mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
      mutate(across(everything(), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE)))
    
    y_tr_ts <- ts(train_df[[y_col]], frequency = freq, start = c(ts_start_year, ts_start_month))
    xreg_tr <- as.matrix(train_x)
    sc <- make_scaler_local(xreg_tr)
    
    fit <- safe(
      forecast::auto.arima(
        y_tr_ts,
        xreg = sc$transform(xreg_tr),
        seasonal = TRUE, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE
      ),
      default = NULL, step = "auto.arima bt"
    )
    if (is.null(fit)) return(list(pred = rep(NA_real_, nrow(test_df))))
    
    last6 <- tail(train_df[[y_col]], 6)
    if (length(last6) < 6) last6 <- c(rep(last6[1], 6 - length(last6)), last6)
    
    p <- numeric(nrow(test_df))
    for (i in seq_len(nrow(test_df))) {
      xi <- test_df[i, ]
      
      if ("lag_1" %in% x_cols) xi$lag_1 <- last6[6]
      if ("lag_2" %in% x_cols) xi$lag_2 <- last6[5]
      if ("lag_3" %in% x_cols) xi$lag_3 <- last6[4]
      if ("ma6"   %in% x_cols) xi$ma6   <- mean(last6, na.rm = TRUE)
      
      x_i <- xi[, x_cols, drop = FALSE] %>%
        mutate(across(everything(), ~ as.numeric(.x))) %>%
        mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
        mutate(across(everything(), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE)))
      
      xreg_i <- sc$transform(as.matrix(x_i))
      p[i] <- safe(as.numeric(forecast::forecast(fit, xreg = xreg_i, h = 1)$mean),
                   default = NA_real_, step = "forecast h=1 bt")
      
      last6 <- c(last6[2:6], ifelse(is.na(p[i]), last6[6], p[i]))
    }
    
    list(pred = p)
  }
  
  # -----------------------
  # 4) ARIMAX rolling (log + smearing BC)
  # -----------------------
  roll_arimax_log <- function(train_df, test_df, x_cols){
    x_cols <- intersect(x_cols, names(train_df))
    if (length(x_cols) == 0) return(list(pred = rep(NA_real_, nrow(test_df))))
    
    train_x <- train_df[, x_cols, drop = FALSE] %>%
      mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
      mutate(across(everything(), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE)))
    
    y_log <- log1p(pmax(train_df[[y_col]], 0))
    y_tr_ts <- ts(y_log, frequency = freq, start = c(ts_start_year, ts_start_month))
    
    xreg_tr <- as.matrix(train_x)
    sc <- make_scaler_local(xreg_tr)
    
    fit <- safe(
      forecast::auto.arima(
        y_tr_ts,
        xreg = sc$transform(xreg_tr),
        seasonal = TRUE, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE
      ),
      default = NULL, step = "auto.arima log bt"
    )
    if (is.null(fit)) return(list(pred = rep(NA_real_, nrow(test_df))))
    
    sigma2 <- fit$sigma2 %||% 0
    
    last6 <- tail(train_df[[y_col]], 6)
    if (length(last6) < 6) last6 <- c(rep(last6[1], 6 - length(last6)), last6)
    
    p <- numeric(nrow(test_df))
    for (i in seq_len(nrow(test_df))) {
      xi <- test_df[i, ]
      
      if ("lag_1" %in% x_cols) xi$lag_1 <- last6[6]
      if ("lag_2" %in% x_cols) xi$lag_2 <- last6[5]
      if ("lag_3" %in% x_cols) xi$lag_3 <- last6[4]
      if ("ma6"   %in% x_cols) xi$ma6   <- mean(last6, na.rm = TRUE)
      
      x_i <- xi[, x_cols, drop = FALSE] %>%
        mutate(across(everything(), ~ as.numeric(.x))) %>%
        mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
        mutate(across(everything(), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE)))
      
      xreg_i <- sc$transform(as.matrix(x_i))
      pred_log <- safe(as.numeric(forecast::forecast(fit, xreg = xreg_i, h = 1)$mean),
                       default = NA_real_, step = "forecast log h=1 bt")
      
      p[i] <- ifelse(is.na(pred_log), NA_real_, expm1(pred_log + 0.5 * sigma2))
      if (!is.finite(p[i])) p[i] <- NA_real_
      
      last6 <- c(last6[2:6], ifelse(is.na(p[i]), last6[6], p[i]))
    }
    
    list(pred = p)
  }
  
  # -----------------------
  # 5) Elegir mejor shift k (0..6) solo para macros
  # -----------------------
  roll_arimax_xreg_shift <- function(train_df, test_df, x_cols, shift_range = 0:3) {
    best <- list(k = NA_integer_, comp = NULL, met = NULL)
    
    for (k in shift_range) {
      df_all <- bind_rows(
        train_df %>% mutate(.split = "train"),
        test_df  %>% mutate(.split = "test")
      ) %>%
        arrange(fecha)
      
      df_all <- shift_cols_lag(df_all, intersect(macro_cols, names(df_all)), k)
      
      trk <- df_all %>% filter(.split == "train") %>% select(-.split)
      tek <- df_all %>% filter(.split == "test")  %>% select(-.split)
      
      r <- roll_arimax(trk, tek, x_cols)
      comp <- tibble(fecha = tek$fecha, real = tek[[y_col]], pred = r$pred)
      met  <- calc_basic_metrics(comp, y_col = "real", yhat_col = "pred", mase_den = mase_den)
      
      if (is.null(best$met) ||
          (is.finite(met$MASE) && met$MASE < best$met$MASE) ||
          (isTRUE(all.equal(met$MASE, best$met$MASE)) && met$MAPE < best$met$MAPE)) {
        best <- list(k = k, comp = comp, met = met)
      }
    }
    
    if (is.null(best$comp)) return(NULL)
    best
  }
  
  # ============================================================
  # BACKTEST candidatos
  # ============================================================
  leaderboard <- list()
  preds_bt <- list()
  
  add_candidate <- function(name, comp_df){
    met <- calc_basic_metrics(comp_df, y_col = "real", yhat_col = "pred", mase_den = mase_den) %>%
      mutate(model = name)
    leaderboard[[name]] <<- met
    preds_bt[[name]] <<- comp_df
  }
  
  # helper: construir x_cols por base + variante
  build_xcols <- function(base_vec, extra_vec) unique(c(base_vec, extra_vec))
  
  # Loop por variantes SOLO para ARIMAX (lo que pediste)
  for (vkey in names(xreg_variants)) {
    
    extra_vec <- xreg_variants[[vkey]]
    vlab <- variant_labels[[vkey]] %||% vkey
    
    x_cols_nolagY <- build_xcols(base_nolagY, extra_vec)
    x_cols_full   <- build_xcols(base_full,   extra_vec)
    x_cols_min    <- build_xcols(base_min,    extra_vec)
    
    if ("arimax_nolagY" %in% candidates) {
      r <- roll_arimax(train, test, x_cols_nolagY)
      add_candidate(paste0("ARIMAX_nolagY[", vlab, "]"),
                    tibble(fecha = test$fecha, real = test[[y_col]], pred = r$pred))
    }
    
    if ("arimax_full" %in% candidates) {
      r <- roll_arimax(train, test, x_cols_full)
      add_candidate(paste0("ARIMAX_full[", vlab, "]"),
                    tibble(fecha = test$fecha, real = test[[y_col]], pred = r$pred))
    }
    
    if ("arimax_min" %in% candidates) {
      r <- roll_arimax(train, test, x_cols_min)
      add_candidate(paste0("ARIMAX_min[", vlab, "]"),
                    tibble(fecha = test$fecha, real = test[[y_col]], pred = r$pred))
    }
    
    if ("arimax_break" %in% candidates) {
      br <- detect_break_dummy(train, y_col = y_col, min_seg = 6)
      trb <- train %>% mutate(break_dummy = br$dummy)
      teb <- test  %>% mutate(break_dummy = as.integer(fecha > br$cut_date))
      x_cols_break <- build_xcols(base_nolagY, extra_vec)
      x_cols_break <- build_xcols(x_cols_break, "break_dummy")
      r <- roll_arimax(trb, teb, x_cols_break)
      add_candidate(paste0("ARIMAX_break_dummy[", vlab, "]"),
                    tibble(fecha = teb$fecha, real = teb[[y_col]], pred = r$pred))
    }
    
    if ("arimax_log_bc" %in% candidates) {
      r <- roll_arimax_log(train, test, x_cols_full)
      add_candidate(paste0("ARIMAX_log_BC[", vlab, "]"),
                    tibble(fecha = test$fecha, real = test[[y_col]], pred = r$pred))
    }
    
    if ("arimax_xreg_shift" %in% candidates) {
      rs <- roll_arimax_xreg_shift(train, test, x_cols_full, shift_range = 0:3)
      if (!is.null(rs) && !is.null(rs$comp)) {
        add_candidate(paste0("ARIMAX_xreg_shift(k=", rs$k, ")[", vlab, "]"), rs$comp)
      }
    }
  }
  
  # Baselines (no dependen de variantes)
  bl <- predict_baselines(train[[y_col]], test[[y_col]], freq = 12,
                          ts_start_year = ts_start_year, ts_start_month = ts_start_month)
  if ("snaive12" %in% candidates) {
    add_candidate("SNaive(12)", tibble(fecha = test$fecha, real = test[[y_col]], pred = bl$snaive12))
  }
  
  y_tr_ts <- ts(train[[y_col]], frequency = freq, start = c(ts_start_year, ts_start_month))
  
  if ("ETS(auto)" %in% candidates) {
    pr <- as.numeric(forecast::forecast(forecast::ets(y_tr_ts), h = h)$mean)
    add_candidate("ETS(auto)", tibble(fecha = test$fecha, real = test[[y_col]], pred = pr))
  }
  
  if ("Theta" %in% candidates) {
    pr <- as.numeric(forecast::thetaf(y_tr_ts, h = h)$mean)
    add_candidate("Theta", tibble(fecha = test$fecha, real = test[[y_col]], pred = pr))
  }
  
  if ("Drift_seasonalized" %in% candidates) {
    pr <- forecast_drift_seasonalized(train[[y_col]], h, ts_start_year, ts_start_month, freq)
    add_candidate("Drift_seasonalized", tibble(fecha = test$fecha, real = test[[y_col]], pred = pr))
  }
  
  metrics_tbl <- bind_rows(leaderboard) %>%
    mutate(across(where(is.numeric), ~ round(.x, 6))) %>%
    arrange(MASE, MAPE, desc(DirHit))
  
  if (nrow(metrics_tbl) == 0) {
    return(list(
      serie_id = serie_id, meta = meta, status = "sin_candidatos",
      best_model = NA_character_, metrics_table = tibble(),
      df_backtest = tibble(), df_forecast = tibble()
    ))
  }
  
  # ---- NUEVO: guardarraíl de calidad ----
  metrics_tbl2 <- metrics_tbl %>%
    mutate(ok = is.finite(R2) & R2 > 0) %>%
    arrange(desc(ok), MASE, MAPE, desc(DirHit))
  
  best_model <- metrics_tbl2$model[[1]]
  
  # fallback si TODOS son malos (R2 <= 0)
  if (!any(metrics_tbl2$ok)) best_model <- "SNaive(12)"  # o "Drift_seasonalized" o "ETS(auto)"
  # --------------------------------------
  
  df_pred_bt <- preds_bt[[best_model]]
  df_backtest <- df_pred_bt %>% rename(prediccion = pred)
  
  ME  <- mean(df_pred_bt$pred - df_pred_bt$real, na.rm = TRUE)
  MPE <- mean((df_pred_bt$pred - df_pred_bt$real) / pmax(1e-9, abs(df_pred_bt$real)), na.rm = TRUE) * 100
  
  # ============================================================
  # FORECAST FUTURO
  # ============================================================
  ultima_hist <- max(datos$fecha)
  fut_macro <- macro_future %>%
    mutate(fecha = parse_periodo_to_date(fecha)) %>%
    filter(fecha > ultima_hist) %>%
    arrange(fecha)
  
  df_forecast <- tibble(fecha = as.Date(character()), prediccion = numeric())
  
  if (nrow(fut_macro) > 0) {
    fut_feat_base <- fut_macro %>%
      calendar_features("fecha") %>%
      mutate(mes_n = seq(from = max(datos$mes_n) + 1, by = 1, length.out = n())) %>%
      arrange(fecha)
    
    is_arimax <- startsWith(best_model, "ARIMAX")
    
    if (is_arimax) {
      
      # parse base + variante desde best_model
      # ejemplos: ARIMAX_full[P+DP], ARIMAX_xreg_shift(k=3)[P], ARIMAX_log_BC[MACRO]
      base_name <- sub("\\[.*\\]$", "", best_model)
      vlab      <- stringr::str_match(best_model, "\\[(.*)\\]$")[,2] %||% "MACRO"
      
      # map label -> extra_vec
      inv_map <- setNames(names(variant_labels), variant_labels)
      vkey <- inv_map[[vlab]] %||% "none"
      extra_vec <- xreg_variants[[vkey]] %||% character(0)
      
      # defaults
      x_cols_run <- base_full
      use_break  <- FALSE
      use_log    <- FALSE
      k_shift    <- NA_integer_
      
      if (base_name == "ARIMAX_nolagY") x_cols_run <- base_nolagY
      if (base_name == "ARIMAX_full")   x_cols_run <- base_full
      if (base_name == "ARIMAX_min")    x_cols_run <- base_min
      
      if (base_name == "ARIMAX_break_dummy") {
        x_cols_run <- c(base_nolagY, "break_dummy")
        use_break <- TRUE
      }
      
      if (base_name == "ARIMAX_log_BC") {
        x_cols_run <- base_full
        use_log <- TRUE
      }
      
      if (startsWith(base_name, "ARIMAX_xreg_shift(k=")) {
        k_shift <- suppressWarnings(as.integer(stringr::str_match(base_name, "k=([0-9]+)")[,2]))
        x_cols_run <- base_full
      }
      
      # agregar extra según variante (como pediste)
      x_cols_run <- unique(c(x_cols_run, extra_vec))
      
      datos_run <- datos
      fut_feat  <- fut_feat_base
      
      # FIX CLAVE: si el modelo usa lags/ma6, créalos en fut_feat
      lag_need <- intersect(c("lag_1","lag_2","lag_3","ma6"), x_cols_run)
      if (length(lag_need) > 0) {
        fut_feat  <- ensure_cols(fut_feat,  lag_need, fill = NA_real_)
        datos_run <- ensure_cols(datos_run, lag_need, fill = NA_real_)
      }
      
      # NUEVO: carry-forward de precio/dist si el futuro no tiene proyección
      carry_cols <- intersect(c("precio_promedio","dist_num","dist_pond"), x_cols_run)
      if (length(carry_cols) > 0) {
        fut_feat  <- ensure_cols(fut_feat,  carry_cols, fill = NA_real_)
        datos_run <- ensure_cols(datos_run, carry_cols, fill = NA_real_)
        
        for (cc in carry_cols) {
          if (!all(is.na(fut_feat[[cc]]))) {
            fut_feat[[cc]] <- zoo::na.locf(fut_feat[[cc]], na.rm = FALSE)
            fut_feat[[cc]] <- zoo::na.locf(fut_feat[[cc]], fromLast = TRUE, na.rm = FALSE)
          } else {
            lastv <- tail(stats::na.omit(datos_run[[cc]]), 1)
            if (length(lastv) == 0) lastv <- NA_real_
            fut_feat[[cc]] <- rep(as.numeric(lastv), nrow(fut_feat))
          }
        }
      }
      
      # (A) shift macros (hist + fut)
      if (is.finite(k_shift)) {
        macro_use <- intersect(macro_cols, names(datos_run))
        
        df_allm <- bind_rows(
          datos_run %>% select(fecha, any_of(macro_use)) %>% mutate(.src = "hist"),
          fut_feat  %>% select(fecha, any_of(macro_use)) %>% mutate(.src = "fut")
        ) %>%
          arrange(fecha)
        
        df_allm <- shift_cols_lag(df_allm, macro_use, k_shift)
        
        m_hist <- df_allm %>% filter(.src == "hist") %>% select(-.src)
        m_fut  <- df_allm %>% filter(.src == "fut")  %>% select(-.src)
        
        datos_run <- datos_run %>% select(-any_of(macro_use)) %>% left_join(m_hist, by = "fecha")
        fut_feat  <- fut_feat  %>% select(-any_of(macro_use)) %>% left_join(m_fut,  by = "fecha")
      }
      
      # (B) break dummy (hist + fut)
      if (use_break) {
        br_full <- detect_break_dummy(datos_run, y_col = y_col, min_seg = 6)
        cut_date <- br_full$cut_date
        datos_run <- datos_run %>% mutate(break_dummy = as.integer(fecha > cut_date))
        fut_feat  <- fut_feat  %>% mutate(break_dummy = as.integer(fecha > cut_date))
      }
      
      # columnas definitivas
      x_cols_run <- intersect(x_cols_run, names(datos_run))
      x_cols_run <- intersect(x_cols_run, names(fut_feat))
      
      if (length(x_cols_run) == 0) {
        df_forecast <- tibble(fecha = fut_feat$fecha, prediccion = NA_real_)
      } else {
        
        xreg_full <- as.matrix(datos_run[, x_cols_run, drop = FALSE])
        sc_f <- make_scaler_local(xreg_full)
        
        if (!use_log) {
          y_ts <- ts(datos_run[[y_col]], frequency = freq, start = c(ts_start_year, ts_start_month))
          fit_full <- safe(
            forecast::auto.arima(
              y_ts,
              xreg = sc_f$transform(xreg_full),
              seasonal = TRUE, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE
            ),
            default = NULL, step = "auto.arima full"
          )
        } else {
          y_log <- log1p(pmax(datos_run[[y_col]], 0))
          y_ts  <- ts(y_log, frequency = freq, start = c(ts_start_year, ts_start_month))
          fit_full <- safe(
            forecast::auto.arima(
              y_ts,
              xreg = sc_f$transform(xreg_full),
              seasonal = TRUE, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE
            ),
            default = NULL, step = "auto.arima full log"
          )
          sigma2_full <- fit_full$sigma2 %||% 0
        }
        
        if (is.null(fit_full) || nrow(fut_feat) == 0) {
          df_forecast <- tibble(fecha = fut_feat$fecha, prediccion = NA_real_)
        } else {
          
          last6 <- tail(datos_run[[y_col]], 6)
          if (length(last6) < 6) last6 <- c(rep(last6[1], 6 - length(last6)), last6)
          
          p <- numeric(nrow(fut_feat))
          
          for (i in seq_len(nrow(fut_feat))) {
            xi <- fut_feat[i, ]
            
            if ("lag_1" %in% x_cols_run) xi$lag_1 <- last6[6]
            if ("lag_2" %in% x_cols_run) xi$lag_2 <- last6[5]
            if ("lag_3" %in% x_cols_run) xi$lag_3 <- last6[4]
            if ("ma6"   %in% x_cols_run) xi$ma6   <- mean(last6, na.rm = TRUE)
            
            x_i <- xi[, x_cols_run, drop = FALSE]
            xreg_i <- sc_f$transform(as.matrix(x_i))
            
            pred_i <- safe(
              as.numeric(forecast::forecast(fit_full, xreg = xreg_i, h = 1)$mean),
              default = NA_real_, step = "forecast futuro h=1"
            )
            
            if (!use_log) {
              p[i] <- pred_i
            } else {
              p[i] <- ifelse(is.na(pred_i), NA_real_, expm1(pred_i + 0.5 * sigma2_full))
            }
            
            last6 <- c(last6[2:6], ifelse(is.na(p[i]), last6[6], p[i]))
          }
          
          df_forecast <- tibble(fecha = fut_feat$fecha, prediccion = p)
        }
      }
      
    } else {
      # No-ARIMAX
      y_ts <- ts(datos[[y_col]], frequency = freq, start = c(ts_start_year, ts_start_month))
      hh <- nrow(fut_macro)
      
      pred_uni <- switch(
        best_model,
        "SNaive(12)" = as.numeric(snaive(y_ts, h = hh)$mean),
        "ETS(auto)"  = as.numeric(forecast(ets(y_ts), h = hh)$mean),
        "Theta"      = as.numeric(thetaf(y_ts, h = hh)$mean),
        "Drift_seasonalized" = forecast_drift_seasonalized(datos[[y_col]], hh, ts_start_year, ts_start_month, freq),
        rep(NA_real_, hh)
      )
      
      df_forecast <- tibble(fecha = fut_macro$fecha, prediccion = as.numeric(pred_uni))
    }
  }
  
  # Bias correction general
  if (nrow(df_forecast) > 0) {
    denom <- (1 + (MPE/100))
    if (!is.finite(denom) || abs(denom) < 1e-6) denom <- sign(denom) * 1e-6
    
    df_forecast <- df_forecast %>%
      mutate(
        prediccion_bc = case_when(
          bias_correction == "multiplicative" ~ prediccion / denom,
          bias_correction == "additive" ~ prediccion - ME,
          TRUE ~ prediccion
        )
      )
  }
  
  # Export
  safe({
    file_safe <- str_replace_all(serie_id, "[^A-Za-z0-9]+", "_")
    readr::write_csv(metrics_tbl, file.path(out_dir, paste0("metrics_", file_safe, ".csv")))
    readr::write_csv(df_backtest, file.path(out_dir, paste0("backtest_", file_safe, ".csv")))
    readr::write_csv(df_forecast, file.path(out_dir, paste0("forecast_", file_safe, ".csv")))
  }, default = NULL, step = "write_csv")
  
  list(
    serie_id = serie_id,
    meta = meta,
    status = "ok",
    best_model = best_model,
    metrics_table = metrics_tbl,
    df_backtest = df_backtest,
    df_forecast = df_forecast
  )
}

# ============================================================
# 7) Catálogo (con EMPAQUES/TAMANIOS)
# ============================================================

catalogo_series <- Datos_prediccion %>%
  distinct(REGION,CANAL,FABRICANTE,CATEGORIA,MARCAS,SUBCATEGORIA,EMPAQUES,TAMANIOS,serie_id) %>%
  arrange(REGION,CANAL,CATEGORIA,MARCAS,SUBCATEGORIA,EMPAQUES,TAMANIOS)

print(head(catalogo_series, 10))

# ============================================================
# 7.1) FILTRO (TOTAL por fecha) con empaques/tamanios opcional
# ============================================================

categoria_obj    <- "QUESOS"   # NULL para apagar
subcategoria_obj <- NULL              # ej "ENTERA" o NULL
fabricantes_obj  <- NULL                # ej c("ALPINA","COLANTA")
empaques_obj     <- NULL                # ej c("BOTELLA","BOLSA")
tamanios_obj     <- NULL                # ej c("200 ML","1 L")
canales_obj      <- NULL     # OJO: pon el nombre exacto de tu data

BEB_LACT_ALPINA_TOTAL <- Datos_prediccion %>%
  mutate(
    CATEGORIA_clean    = str_to_upper(str_squish(CATEGORIA)),
    SUBCATEGORIA_clean = str_to_upper(str_squish(SUBCATEGORIA)),
    FABRICANTE_clean   = str_to_upper(str_squish(FABRICANTE)),
    EMPAQUES_clean     = str_to_upper(str_squish(EMPAQUES)),
    TAMANIOS_clean     = str_to_upper(str_squish(TAMANIOS)),
    CANAL_clean        = str_to_upper(str_squish(CANAL))
  ) %>%
  filter(
    if (is.null(categoria_obj)) TRUE else CATEGORIA_clean == str_to_upper(str_squish(categoria_obj)),
    if (is.null(subcategoria_obj)) TRUE else SUBCATEGORIA_clean == str_to_upper(str_squish(subcategoria_obj)),
    if (is.null(fabricantes_obj)) TRUE else FABRICANTE_clean %in% str_to_upper(str_squish(fabricantes_obj)),
    if (is.null(empaques_obj)) TRUE else EMPAQUES_clean %in% str_to_upper(str_squish(empaques_obj)),
    if (is.null(tamanios_obj)) TRUE else TAMANIOS_clean %in% str_to_upper(str_squish(tamanios_obj)),
    if (is.null(canales_obj)) TRUE else CANAL_clean %in% str_to_upper(str_squish(canales_obj))
  ) %>%
  group_by(fecha) %>%
  summarise(
    ventas_litros = sum(ventas_litros, na.rm = TRUE),
    valor         = sum(valor, na.rm = TRUE),
    precio_promedio = {
      p <- wmean_safe(precio_promedio, ventas_litros)
      if (!is.finite(p) || is.na(p)) sum(valor, na.rm = TRUE) / pmax(sum(ventas_litros, na.rm = TRUE), 1e-9) else p
    },
    dist_num  = wmean_safe(dist_num,  valor),
    dist_pond = wmean_safe(dist_pond, valor),
    Inflacion = first(Inflacion),
    Gasto_hogares_raddar = first(Gasto_hogares_raddar),
    Gasto_alimentos = first(Gasto_alimentos),
    .groups = "drop"
  ) %>%
  mutate(
    REGION     = "TOTAL",
    CANAL      = if (is.null(canales_obj)) "TOTAL" else paste(canales_obj, collapse = "+"),
    FABRICANTE = if (is.null(fabricantes_obj)) "TOTAL" else paste(fabricantes_obj, collapse = "+"),
    CATEGORIA  = if (is.null(categoria_obj)) "TOTAL" else categoria_obj,
    MARCAS     = "TOTAL",
    SUBCATEGORIA = if (is.null(subcategoria_obj)) "TOTAL" else subcategoria_obj,
    EMPAQUES   = if (is.null(empaques_obj)) "TOTAL" else paste(empaques_obj, collapse = "+"),
    TAMANIOS   = if (is.null(tamanios_obj)) "TOTAL" else paste(tamanios_obj, collapse = "+"),
    serie_id = paste(
      "TOTAL", 
      if (is.null(canales_obj)) "TOTAL" else paste(canales_obj, collapse = "+"),
      if (is.null(fabricantes_obj)) "TOTAL" else paste(fabricantes_obj, collapse = "+"),
      if (is.null(categoria_obj)) "TOTAL" else categoria_obj,
      "TOTAL",
      if (is.null(subcategoria_obj)) "TOTAL" else subcategoria_obj,
      if (is.null(empaques_obj)) "TOTAL" else paste(empaques_obj, collapse = "+"),
      if (is.null(tamanios_obj)) "TOTAL" else paste(tamanios_obj, collapse = "+"),
      sep = " | "
    )
  ) %>%
  mutate(fecha = parse_periodo_to_date(fecha)) %>%
  arrange(fecha)

stopifnot(nrow(BEB_LACT_ALPINA_TOTAL) > 0)

# ============================================================
# 9) Correr modelo (ejemplo) - puedes correr volumen y valor
# ============================================================

out_dir <- "arimax_out_panel"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

sid <- unique(BEB_LACT_ALPINA_TOTAL$serie_id)[1]

res_vol <- run_model_for_series(
  serie_id = sid,
  data_panel = BEB_LACT_ALPINA_TOTAL,
  macro_future = df_radd_clean,
  n_holdout = 12,
  out_dir = file.path(out_dir, "volumen"),
  verbose = TRUE,
  y_col = "ventas_litros",
  bias_correction = "multiplicative",
  candidates = c(
    "arimax_nolagY","arimax_full","arimax_break","arimax_min",
    "arimax_xreg_shift",
    "snaive12","ETS(auto)","Drift_seasonalized"
  )
)

res_val <- run_model_for_series(
  serie_id = sid,
  data_panel = BEB_LACT_ALPINA_TOTAL,
  macro_future = df_radd_clean,
  n_holdout = 12,
  out_dir = file.path(out_dir, "valor"),
  verbose = TRUE,
  y_col = "valor",
  bias_correction = "multiplicative",
  candidates = c(
    "arimax_nolagY","arimax_full","arimax_break","arimax_min",
    "arimax_log_bc","arimax_xreg_shift",
    "snaive12","ETS(auto)","Drift_seasonalized"
  )
)


print(res_vol$best_model); print(res_vol$metrics_table %>% head(10)); print(head(res_vol$df_forecast))

# ============================================================
# 10) GRÁFICAS
# ============================================================

make_dirhit_plot <- function(df_bt, y_col_real="real", y_col_pred="prediccion",
                             title="Acierto direccional (backtest)") {
  df <- df_bt %>%
    mutate(fecha = parse_periodo_to_date(fecha)) %>%
    arrange(fecha) %>%
    mutate(
      real_delta = .data[[y_col_real]] - lag(.data[[y_col_real]]),
      pred_delta = .data[[y_col_pred]] - lag(.data[[y_col_pred]]),
      acierto = sign(real_delta) == sign(pred_delta)
    )
  acc <- mean(df$acierto, na.rm = TRUE)
  
  ggplot(df, aes(x = fecha, y = as.integer(acierto))) +
    geom_col() +
    scale_y_continuous(breaks=c(0,1), labels=c("No","Sí")) +
    labs(
      title = paste0(title, sprintf(" — DirHit: %.1f%%", 100*acc)),
      x = "Fecha", y = "¿Acertó la dirección?"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_forecast_only <- function(data, serie_id, res, y_col="ventas_litros",
                               out_path=NULL, titulo=NULL) {
  hist <- data %>%
    filter(serie_id == !!serie_id) %>%
    mutate(fecha = parse_periodo_to_date(fecha)) %>%
    arrange(fecha) %>%
    select(fecha, y = all_of(y_col))
  stopifnot(nrow(hist) > 0)
  
  last_real_date <- max(hist$fecha, na.rm = TRUE)
  
  fc <- res$df_forecast %>%
    mutate(fecha = parse_periodo_to_date(fecha),
           pred_final = coalesce(prediccion_bc, prediccion)) %>%
    filter(fecha > last_real_date) %>%
    arrange(fecha)
  
  p <- ggplot() +
    geom_line(data = hist, aes(x = fecha, y = y), linewidth = 1) +
    { if (nrow(fc) > 0) geom_line(data = fc, aes(x = fecha, y = pred_final),
                                  linewidth = 1.2, linetype = "dashed") else NULL } +
    geom_vline(xintercept = as.numeric(last_real_date), linetype = "dotted") +
    labs(
      title = titulo %||% paste0("Histórico vs Forecast — ", serie_id),
      subtitle = "Sólida: histórico | Punteada: forecast (futuro)",
      x = "Fecha", y = y_col
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"))
  
  if (!is.null(out_path)) ggsave(out_path, p, width = 10, height = 5, dpi = 160)
  p
}

plot_test_vs_real <- function(serie_id, res, y_col_real="real", y_col_pred="prediccion",
                              out_path=NULL, titulo=NULL) {
  bt <- res$df_backtest
  if (is.null(bt) || nrow(bt) == 0) stop("No hay backtest en res$df_backtest para ", serie_id)
  
  df <- bt %>%
    mutate(fecha = parse_periodo_to_date(fecha)) %>%
    arrange(fecha) %>%
    mutate(
      real_delta = .data[[y_col_real]] - lag(.data[[y_col_real]]),
      pred_delta = .data[[y_col_pred]] - lag(.data[[y_col_pred]]),
      hit = case_when(
        is.na(real_delta) | is.na(pred_delta) ~ NA,
        sign(real_delta) == sign(pred_delta) ~ TRUE,
        TRUE ~ FALSE
      ),
      hit_lab = if_else(hit, "Acierto dir.", "Fallo dir.")
    ) %>%
    filter(!is.na(real_delta) & !is.na(pred_delta))
  
  acc  <- mean(df$hit, na.rm = TRUE)
  hits <- sum(df$hit, na.rm = TRUE)
  tot  <- nrow(df)
  
  p <- ggplot(df, aes(x = fecha)) +
    geom_line(aes(y = .data[[y_col_real]]), linewidth = 1) +
    geom_line(aes(y = .data[[y_col_pred]]), linewidth = 1.1, linetype = "dashed") +
    geom_point(aes(y = .data[[y_col_real]], color = hit_lab), size = 3) +
    labs(
      title = titulo %||% paste0("TEST vs REAL — DirHit ", sprintf("%.1f%% (%d/%d)", 100*acc, hits, tot)),
      subtitle = paste0("Serie: ", serie_id, " | Sólida: real | Punteada: predicción"),
      x = "Fecha", y = "Ventas (test)", color = "Dirección"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"))
  
  if (!is.null(out_path)) ggsave(out_path, p, width = 10, height = 5, dpi = 160)
  p
}

plot_hist_y_forecast_mes_a_mes <- function(data, serie_id, res, y_col="ventas_litros",
                                           out_path=NULL, titulo=NULL) {
  hist <- data %>%
    filter(serie_id == !!serie_id) %>%
    mutate(fecha = parse_periodo_to_date(fecha)) %>%
    arrange(fecha) %>%
    select(fecha, y = all_of(y_col))
  stopifnot(nrow(hist) > 0)
  
  last_real_date <- max(hist$fecha, na.rm = TRUE)
  
  fc <- res$df_forecast %>%
    mutate(fecha = parse_periodo_to_date(fecha),
           pred_final = coalesce(prediccion_bc, prediccion)) %>%
    filter(fecha > last_real_date) %>%
    arrange(fecha)
  
  xmin_future <- if (nrow(fc) > 0) min(fc$fecha) else NA
  
  p <- ggplot() +
    { if (nrow(fc) > 0) annotate("rect", xmin = xmin_future, xmax = max(fc$fecha),
                                 ymin = -Inf, ymax = Inf, alpha = 0.07) else NULL } +
    geom_line(data = hist, aes(x = fecha, y = y), linewidth = 1) +
    { if (nrow(fc) > 0) geom_line(data = fc, aes(x = fecha, y = pred_final),
                                  linewidth = 1.1, linetype = "dashed") else NULL } +
    geom_vline(xintercept = as.numeric(last_real_date), linetype = "dotted") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0.01, 0.01)) +
    labs(
      title = titulo %||% "Histórico vs Forecast (mes a mes)",
      subtitle = paste0("Serie: ", serie_id, " | Zona sombreada = futuro"),
      x = "Mes", y = y_col
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"))
  
  if (!is.null(out_path)) ggsave(out_path, p, width = 11, height = 5.5, dpi = 170)
  p
}

# ---- Guardar PNG (ejemplo para volumen) ----
out_png <- file.path(out_dir, "volumen_png")
dir.create(out_png, showWarnings = FALSE, recursive = TRUE)

p_fc  <- plot_forecast_only(BEB_LACT_ALPINA_TOTAL, sid, res_vol,
                            y_col = "ventas_litros",
                            out_path = file.path(out_png, "forecast_only.png"),
                            titulo = paste0("Volumen — ", sid))

p_tv  <- plot_test_vs_real(sid, res_vol,
                           out_path = file.path(out_png, "test_vs_real.png"),
                           titulo = paste0("Volumen TEST vs REAL — ", sid))

p_mm  <- plot_hist_y_forecast_mes_a_mes(BEB_LACT_ALPINA_TOTAL, sid, res_vol,
                                        y_col = "ventas_litros",
                                        out_path = file.path(out_png, "hist_forecast_mes_a_mes.png"),
                                        titulo = paste0("Volumen Histórico vs Forecast — ", sid))

p_dir <- make_dirhit_plot(res_vol$df_backtest,
                          title = paste0("Volumen — Acierto direccional (TEST) — ", sid))

ggsave(file.path(out_png, "dirhit.png"), p_dir, width=10, height=3.5, dpi=160)

print(p_fc); print(p_tv); print(p_mm); print(p_dir)
message("Listo ✅ PNG guardados en: ", normalizePath(out_png))
