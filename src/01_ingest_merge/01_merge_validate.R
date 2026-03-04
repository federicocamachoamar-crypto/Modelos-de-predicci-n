# ============================================================
# 01_merge_validate.R
# - Lee config/params.yml
# - Lee ventas sample (CSV) + macro sample (XLSX)
# - Parsea fechas (ventas: serial o dd/mm/yyyy; macro: "ene-21")
# - Construye serie_id
# - Merge macro -> ventas por date (mensual, YYYY-MM-01)
# - Valida duplicados/gaps/NA
# - Escribe:
#    - data_processed/model_ready.csv  (NO commitear)
#    - reports/notes_merge.md          (sí se puede commitear)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(readxl)
  library(yaml)
  library(rlang)
})

# -----------------------------
# Helpers
# -----------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

to_na <- function(x) {
  x <- str_trim(as.character(x))
  x[x %in% c("", "-", "$-", "$ -", "NA", "NaN")] <- NA
  x
}

clean_numeric <- function(x) {
  # parse_number tolera "$", comas, texto, etc.
  readr::parse_number(to_na(x))
}

parse_sales_periodo_to_date <- function(x) {
  # x puede ser:
  # - serial Excel (número o string numérico)
  # - string dd/mm/yyyy (ej: 10/1/2024)
  x_chr <- to_na(x)

  # intento: número serial
  x_num <- suppressWarnings(as.numeric(x_chr))
  dt_num <- as.Date(x_num, origin = "1899-12-30")

  # intento: dd/mm/yyyy (Colombia)
  dt_dmy <- suppressWarnings(lubridate::dmy(x_chr))

  # si dt_dmy existe úsalo; si no, usa dt_num
  dt <- dt_dmy
  dt[is.na(dt)] <- dt_num[is.na(dt)]

  # mensual: primer día del mes
  as.Date(format(dt, "%Y-%m-01"))
}

parse_macro_mon_yy_es_to_date <- function(x) {
  # Espera cosas tipo: "ene-21", "feb-2022", "sep-25"
  x_chr <- str_to_lower(str_trim(to_na(x)))
  x_chr <- str_replace_all(x_chr, "\\.", "") # por si vienen "ene."
  parts <- str_split_fixed(x_chr, "-", 2)

  mon <- parts[, 1]
  yr  <- parts[, 2]

  mon_map <- c(
    "ene"=1, "feb"=2, "mar"=3, "abr"=4, "may"=5, "jun"=6,
    "jul"=7, "ago"=8, "sep"=9, "set"=9, "oct"=10, "nov"=11, "dic"=12
  )

  m <- unname(mon_map[mon])
  y <- suppressWarnings(as.integer(yr))
  # si año viene en 2 dígitos, asume 2000+
  y <- ifelse(!is.na(y) & y < 100, 2000 + y, y)

  # si no parseó, devuelve NA
  dt <- as.Date(sprintf("%04d-%02d-01", y, m))
  dt[is.na(m) | is.na(y)] <- as.Date(NA)

  dt
}

build_serie_id <- function(df, dims, sep = " | ") {
  for (col in dims) {
    if (!col %in% names(df)) df[[col]] <- "TOTAL"
  }
  df <- df %>%
    mutate(across(all_of(dims), ~ if_else(is.na(.x) | str_trim(as.character(.x)) == "", "TOTAL", str_trim(as.character(.x)))))

  df$serie_id <- paste(!!!syms(dims), sep = sep)
  df
}

calc_gaps_summary <- function(df) {
  # gaps por serie: compara meses esperados vs presentes
  # df debe tener serie_id y date
  df <- df %>% filter(!is.na(serie_id), !is.na(date))

  gaps <- df %>%
    group_by(serie_id) %>%
    summarise(
      min_date = min(date),
      max_date = max(date),
      n_obs = n(),
      n_unique_dates = n_distinct(date),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      expected_months = ifelse(is.na(min_date) | is.na(max_date), NA_integer_,
                               length(seq(min_date, max_date, by = "month"))),
      gaps = ifelse(is.na(expected_months), NA_integer_, expected_months - n_unique_dates)
    ) %>%
    ungroup()

  gaps
}

# -----------------------------
# 0) Leer config (auto-detect)
# -----------------------------
params_files <- list.files("config", pattern = "\\.ya?ml$", full.names = TRUE)
if (length(params_files) == 0) stop("No encontré archivo YAML en /config. Esperaba config/params.yml")
# si hay varios, prioriza params.yml
params_path <- params_files[which(basename(params_files) %in% c("params.yml", "params.yaml"))][1] %||% params_files[1]

params <- yaml::read_yaml(params_path)

# paths
ventas_path <- params$data$source_1_sample %||% "data_sample/base_ventas_sample.csv"
macro_path  <- params$data$source_2_sample %||% "data_sample/base_xregs_sample.xlsx"
out_path    <- params$data$output_model_ready %||% "data_processed/model_ready.csv"

# schema
date_col_base1 <- params$schema$date_col_base1 %||% "PERIODO"
date_col_base2 <- params$schema$date_col_base2 %||% "fecha"
target_default <- params$schema$target_default %||% "VENTA_LITROS"

dims <- params$schema$dims %||% c("REGION","CANAL","FABRICANTE","CATEGORIA","MARCAS","SUBCATEGORIA","EMPAQUES","TAMANIOS")
sep  <- params$schema$serie_id_sep %||% " | "

xreg_comm <- params$xregs$commercial %||% c("PRECIO_PROMEDIO","DIST_NUM_TIENDAS_HANDLING","DIST_POND_TIENDAS_HANDLING")
xreg_macr <- params$xregs$macro %||% c("Gasto_hogares_raddar","Gasto_alimentos","Inflacion")

# report path
ensure_dir("reports")
notes_path <- file.path("reports", "notes_merge.md")

# output dir
ensure_dir(dirname(out_path))

# -----------------------------
# 1) Leer ventas (CSV)
# -----------------------------
if (!file.exists(ventas_path)) stop(paste0("No existe ventas sample: ", ventas_path))
ventas <- readr::read_csv(ventas_path, show_col_types = FALSE, progress = FALSE)

# fecha ventas -> date
if (!date_col_base1 %in% names(ventas)) {
  stop(paste0("No encontré columna de fecha en ventas: ", date_col_base1))
}

ventas <- ventas %>%
  mutate(
    date = parse_sales_periodo_to_date(.data[[date_col_base1]])
  )

# serie_id
ventas <- build_serie_id(ventas, dims = dims, sep = sep)

# targets / numeric cleanup
if (!target_default %in% names(ventas)) {
  stop(paste0("No encontré el target_default en ventas: ", target_default))
}

# limpia targets y xregs comerciales si existen
num_cols_sales <- intersect(c(target_default, xreg_comm), names(ventas))
ventas <- ventas %>%
  mutate(across(all_of(num_cols_sales), ~ clean_numeric(.x)))

ventas <- ventas %>%
  mutate(y = .data[[target_default]])

# -----------------------------
# 2) Leer macro (XLSX)
# -----------------------------
if (!file.exists(macro_path)) stop(paste0("No existe macro sample: ", macro_path))

macro <- readxl::read_excel(macro_path)

if (!date_col_base2 %in% names(macro)) {
  stop(paste0("No encontré columna de fecha en macro: ", date_col_base2))
}

macro <- macro %>%
  mutate(
    date = parse_macro_mon_yy_es_to_date(.data[[date_col_base2]])
  )

# limpia numéricas macro
num_cols_macro <- intersect(xreg_macr, names(macro))
macro <- macro %>%
  mutate(across(all_of(num_cols_macro), ~ clean_numeric(.x)))

macro_keep <- macro %>%
  select(date, any_of(xreg_macr)) %>%
  distinct(date, .keep_all = TRUE)

# -----------------------------
# 3) Merge macro -> ventas por date
# -----------------------------
model_ready <- ventas %>%
  left_join(macro_keep, by = "date")

# -----------------------------
# 4) Validaciones
# -----------------------------
# duplicados por (serie_id, date)
dups <- model_ready %>%
  count(serie_id, date) %>%
  filter(n > 1)

# gaps
gaps_tbl <- calc_gaps_summary(model_ready)

# NA / nonfinite
na_summary <- tibble(
  col = names(model_ready),
  na = sapply(model_ready, function(x) sum(is.na(x))),
  nonfinite = sapply(model_ready, function(x) sum(!is.na(x) & is.infinite(as.numeric(x))))
)

# -----------------------------
# 5) Exportar outputs
# -----------------------------
# Orden mínimo de columnas
keep_cols <- c("date","serie_id",dims,"y", target_default, xreg_comm, xreg_macr)
keep_cols <- unique(keep_cols)
keep_cols <- intersect(keep_cols, names(model_ready))

model_ready_out <- model_ready %>%
  select(all_of(keep_cols))

readr::write_csv(model_ready_out, out_path)

# -----------------------------
# 6) Reporte Markdown
# -----------------------------
n_rows <- nrow(model_ready_out)
n_series <- n_distinct(model_ready_out$serie_id)
date_min <- min(model_ready_out$date, na.rm = TRUE)
date_max <- max(model_ready_out$date, na.rm = TRUE)

top_gaps <- gaps_tbl %>%
  arrange(desc(gaps)) %>%
  slice_head(n = 10)

top_na <- na_summary %>%
  arrange(desc(na)) %>%
  slice_head(n = 15)

report_lines <- c(
  "# Merge & Quality Report",
  "",
  paste0("- **Config:** `", params_path, "`"),
  paste0("- **Ventas sample:** `", ventas_path, "`"),
  paste0("- **Macro sample:** `", macro_path, "`"),
  paste0("- **Output model_ready:** `", out_path, "` (no commitear)"),
  "",
  "## Resumen",
  paste0("- Filas: **", n_rows, "**"),
  paste0("- Series únicas (serie_id): **", n_series, "**"),
  paste0("- Rango fechas: **", date_min, "** a **", date_max, "**"),
  "",
  "## Duplicados (serie_id, date)",
  if (nrow(dups) == 0) "- No se detectaron duplicados." else paste0("- Se detectaron **", nrow(dups), "** combinaciones duplicadas. (ver abajo)"),
  ""
)

if (nrow(dups) > 0) {
  report_lines <- c(report_lines, "```", capture.output(print(dups, n = 20)), "```", "")
}

report_lines <- c(
  report_lines,
  "## Gaps de fechas por serie (top 10)",
  "```",
  capture.output(print(top_gaps, n = 10)),
  "```",
  "",
  "## NA por columna (top 15)",
  "```",
  capture.output(print(top_na, n = 15)),
  "```",
  "",
  "## Notas",
  "- Si hay muchos gaps o NA, el siguiente paso (02_eda_diagnostics) propondrá el mejor tratamiento (sin imputar agresivamente sin documentar).",
  ""
)

writeLines(report_lines, notes_path)

message("✅ Listo.")
message(" - model_ready escrito en: ", out_path)
message(" - reporte escrito en: ", notes_path)
