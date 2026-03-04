## Parámetros del problema
- Frecuencia: mensual
- Horizonte forecast: 12 meses
- Backtesting rolling-origin:
  - ventana inicial: 24 meses
  - step: 1 mes
  - horizonte evaluación: 12 meses
- Métricas prioridad:
  - **MASE** (principal)
  - **R² out-of-sample** (calculado únicamente sobre predicciones vs reales en el backtest)

## Reglas de merge y calidad (CRÍTICO)
- Unificar ambas fechas a `date` mensual (primer día del mes).
- Merge macro (base 2) hacia ventas (base 1) por `date` (las xregs macro aplican a todas las series del mes).
- Validaciones mínimas:
  - Duplicados por (serie_id, date)
  - Gaps de fechas por serie_id
  - NA / infinitos en `y` y en xregs
- Tratamiento de problemas:
  - El agente debe **proponer** el mejor tratamiento (gaps/missing/outliers),
    pero siempre **documentarlo** en `reports/notes_merge.md`
    y evitar imputaciones agresivas sin dejar rastro.

## Definición de output: data_processed/model_ready.csv
Formato estándar “long” (una fila por serie_id y mes). Debe contener:

1) **Fecha**
- `date` (mensual, YYYY-MM-01)

2) **Target**
- `y` (target estandarizado desde `VENTA_LITROS` o `VENTA_VALOR`)

3) **Identidad de serie**
- `serie_id = paste(REGION, CANAL, FABRICANTE, CATEGORIA, MARCAS, SUBCATEGORIA, EMPAQUES, TAMANIOS, sep = " | ")`
- dimensiones crudas: REGION, CANAL, FABRICANTE, CATEGORIA, MARCAS, SUBCATEGORIA, EMPAQUES, TAMANIOS

4) **Regresores (xregs)**
- Comerciales (base 1): `PRECIO_PROMEDIO`, `DIST_NUM_TIENDAS_HANDLING`, `DIST_POND_TIENDAS_HANDLING`
- Macros (base 2): `Gasto_hogares_raddar`, `Gasto_alimentos`, `Inflacion`
