# Merge & Quality Report

- **Config:** `config/params.yml`
- **Ventas sample:** `data_sample/base_ventas_sample.csv`
- **Macro sample:** `data_sample/base_xregs_sample.xlsx`
- **Output model_ready:** `data_processed/model_ready.csv` (no commitear)

## Resumen
- Filas: **291**
- Series únicas (serie_id): **45**
- Rango fechas: **2025-08-01** a **2026-01-01**

## Duplicados (serie_id, date)
- Se detectaron **12** combinaciones duplicadas. (ver abajo)

```
# A tibble: 12 × 3
   serie_id                                           date           n
   <chr>                                              <date>     <int>
 1 ANTIOQUIA | SUPER CADENA | COLANTA | QUESOS | COL… 2025-08-01     8
 2 ANTIOQUIA | SUPER CADENA | COLANTA | QUESOS | COL… 2025-09-01     8
 3 ANTIOQUIA | SUPER CADENA | COLANTA | QUESOS | COL… 2025-10-01     8
 4 ANTIOQUIA | SUPER CADENA | COLANTA | QUESOS | COL… 2025-11-01     8
 5 ANTIOQUIA | SUPER CADENA | COLANTA | QUESOS | COL… 2025-12-01     8
 6 ANTIOQUIA | SUPER CADENA | COLANTA | QUESOS | COL… 2026-01-01     8
 7 EJE CAFETERO | SUPER CADENA | OTROS FABRICANTES |… 2025-08-01     2
 8 EJE CAFETERO | SUPER CADENA | OTROS FABRICANTES |… 2025-09-01     2
 9 EJE CAFETERO | SUPER CADENA | OTROS FABRICANTES |… 2025-10-01     2
10 EJE CAFETERO | SUPER CADENA | OTROS FABRICANTES |… 2025-11-01     2
11 EJE CAFETERO | SUPER CADENA | OTROS FABRICANTES |… 2025-12-01     2
12 EJE CAFETERO | SUPER CADENA | OTROS FABRICANTES |… 2026-01-01     2
```

## Gaps de fechas por serie (top 10)
```
# A tibble: 10 × 7
   serie_id min_date   max_date   n_obs n_unique_dates expected_months
   <chr>    <date>     <date>     <int>          <int>           <int>
 1 ANTIOQU… 2025-08-01 2026-01-01     6              6               6
 2 ANTIOQU… 2025-08-01 2026-01-01     6              6               6
 3 ANTIOQU… 2025-08-01 2026-01-01     6              6               6
 4 ANTIOQU… 2026-01-01 2026-01-01     1              1               1
 5 ANTIOQU… 2025-08-01 2026-01-01     6              6               6
 6 ANTIOQU… 2025-08-01 2026-01-01    48              6               6
 7 ANTIOQU… 2025-08-01 2025-09-01     2              2               2
 8 ANTIOQU… 2025-08-01 2026-01-01     6              6               6
 9 ANTIOQU… 2025-08-01 2025-11-01     4              4               4
10 ANTIOQU… 2025-08-01 2026-01-01     6              6               6
# ℹ 1 more variable: gaps <int>
```

## NA por columna (top 15)
```
# A tibble: 15 × 3
   col                           na nonfinite
   <chr>                      <int>     <int>
 1 PRECIO_PROMEDIO               18         0
 2 VENTA_LITROS                  17         0
 3 VENTA_VALOR                   17         0
 4 DIST_POND_TIENDAS_HANDLING    17         0
 5 y                             17         0
 6 PERIODO                        0         0
 7 REGION                         0         0
 8 CANAL                          0         0
 9 FABRICANTE                     0         0
10 CATEGORIA                      0         0
11 MARCAS                         0         0
12 SUBCATEGORIA                   0         0
13 EMPAQUES                       0         0
14 TAMANIOS                       0         0
15 DIST_NUM_TIENDAS_HANDLING      0         0
```

## Notas
- Si hay muchos gaps o NA, el siguiente paso (02_eda_diagnostics) propondrá el mejor tratamiento (sin imputar agresivamente sin documentar).

