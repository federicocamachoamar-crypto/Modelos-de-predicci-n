# ============================================================
# 04_backtest_compare_iter1.R
# Lee:
# - data_processed/backtest_predictions_iter1.csv
# - data_processed/model_ready.csv  (para escala MASE por serie)
#
# Escribe (sí se pueden subir a GitHub):
# - reports/model_ranking_iter1.csv
# - reports/model_ranking_iter1_by_horizon.csv
# - reports/backtest_summary_iter1.md
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringr)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

PRED_PATH <- "data_processed/backtest_predictions_iter1.csv"
MR_PATH   <- "data_processed/model_ready.csv"

OUT_RANK  <- "reports/model_ranking_iter1.csv"
OUT_H     <- "reports/model_ranking_iter1_by_horizon.csv"
OUT_MD    <- "reports/backtest_summary_iter1.md"

ensure_dir("reports")

if (!file.exists(PRED_PATH)) stop("No existe backtest_predictions_iter1.csv (corre 03_train_models_iter1.R)")
if (!file.exists(MR_PATH)) stop("No existe model_ready.csv (corre 01_merge_validate.R)")

pred <- read_csv(PRED_PATH, show_col_types = FALSE, progress = FALSE) %>%
  mutate(
    date = as.Date(date),
    origin_end_date = as.Date(origin_end_date),
    y = as.numeric(y),
    yhat = as.numeric(yhat)
  )

mr <- read_csv(MR_PATH, show_col_types = FALSE, progress = FALSE) %>%
  mutate(date = as.Date(date), y = as.numeric(y)) %>%
  arrange(serie_id, date)

# --------------------------
# Escala MASE por serie (m=12):
# scale = mean(|y_t - y_{t-12}|) sobre la historia disponible de la serie
# --------------------------
mase_scale <- mr %>%
  group_by(serie_id) %>%
  arrange(date) %>%
  mutate(lag12 = dplyr::lag(y, 12)) %>%
  summarise(
    mase_scale = mean(abs(y - lag12), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mase_scale = ifelse(is.finite(mase_scale) & mase_scale > 0, mase_scale, NA_real_))

pred2 <- pred %>%
  left_join(mase_scale, by = "serie_id") %>%
  mutate(
    abs_err = abs(y - yhat),
    sq_err  = (y - yhat)^2
  )

# --------------------------
# Ranking por serie y modelo (agregado sobre todos los puntos OOS)
# --------------------------
rank_series_model <- pred2 %>%
  filter(!is.na(y), !is.na(yhat)) %>%
  group_by(serie_id, model) %>%
  summarise(
    n = n(),
    mae = mean(abs_err, na.rm = TRUE),
    rmse = sqrt(mean(sq_err, na.rm = TRUE)),
    mase = ifelse(!is.na(first(mase_scale)),
                  mean(abs_err, na.rm = TRUE) / first(mase_scale),
                  NA_real_),
    # R2 out-of-sample sobre puntos de backtest
    r2_oos = {
      yy <- y
      sse <- sum((yy - yhat)^2, na.rm = TRUE)
      sst <- sum((yy - mean(yy, na.rm = TRUE))^2, na.rm = TRUE)
      ifelse(is.finite(sst) && sst > 0, 1 - sse/sst, NA_real_)
    },
    .groups = "drop"
  )

# Ranking global (promedio sobre series)
rank_global <- rank_series_model %>%
  group_by(model) %>%
  summarise(
    series = n_distinct(serie_id),
    avg_mase = mean(mase, na.rm = TRUE),
    med_mase = median(mase, na.rm = TRUE),
    avg_r2_oos = mean(r2_oos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(avg_mase)

write_csv(rank_global, OUT_RANK)

# --------------------------
# Ranking por horizonte (promedio sobre series)
# --------------------------
rank_by_h <- pred2 %>%
  filter(!is.na(y), !is.na(yhat)) %>%
  group_by(serie_id, model, horizon) %>%
  summarise(
    mase = ifelse(!is.na(first(mase_scale)),
                  mean(abs_err, na.rm = TRUE) / first(mase_scale),
                  NA_real_),
    r2_oos = {
      yy <- y
      sse <- sum((yy - yhat)^2, na.rm = TRUE)
      sst <- sum((yy - mean(yy, na.rm = TRUE))^2, na.rm = TRUE)
      ifelse(is.finite(sst) && sst > 0, 1 - sse/sst, NA_real_)
    },
    .groups = "drop"
  ) %>%
  group_by(model, horizon) %>%
  summarise(
    avg_mase = mean(mase, na.rm = TRUE),
    avg_r2_oos = mean(r2_oos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(horizon, avg_mase)

write_csv(rank_by_h, OUT_H)

# --------------------------
# Reporte markdown ejecutivo
# --------------------------
best <- rank_global %>% slice_head(n = 3)

md <- c(
  "# Backtest Summary — Iteración 1",
  "",
  "Métricas prioridad: **MASE** (principal) y **R² out-of-sample** (secundaria).",
  "",
  paste0("- Predicciones: `", PRED_PATH, "`"),
  paste0("- Ranking global: `", OUT_RANK, "`"),
  paste0("- Ranking por horizonte: `", OUT_H, "`"),
  "",
  "## Top 3 modelos (promedio sobre series)",
  "```",
  capture.output(print(best, n = 3)),
  "```",
  "",
  "## Lectura rápida",
  "- **avg_mase** más bajo = mejor.",
  "- **avg_r2_oos** más alto = mejor (puede ser negativo si el modelo va peor que la media).",
  ""
)

writeLines(md, OUT_MD)

message("✅ Backtest compare iter1 listo.")
message(" - ", OUT_RANK)
message(" - ", OUT_H)
message(" - ", OUT_MD)
