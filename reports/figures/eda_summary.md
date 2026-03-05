# EDA Summary (Forecasting Bebidas Lácteas)

## Panorama general
- Series (serie_id): **45**
- Rango de fechas: **2025-08-01** a **2026-01-01**
- % series con historia < 24 meses: **100%**
- % series con gaps (meses faltantes): **0%**
- % series con señal estacional (corr lag12 >= 0.4): **0%**

## Hallazgos clave
- Revisar `reports/series_quality.csv` para ver gaps, NA y largo de historia por serie.
- Revisar `reports/series_outliers_top.csv` para valores atípicos (z-score robusto alto).

## Recomendación de model zoo (más allá de ARIMAX)
### Baselines (siempre)
- Seasonal Naive (SNaive) como benchmark duro.
- ETS (Holt-Winters) para series estables.

### Estadísticos
- SARIMA / ARIMA: para series con autocorrelación clara.
- ARIMAX: cuando xregs aportan señal (precio, distribución, macros).
- STL + ETS: robusto cuando hay estacionalidad y tendencia.
- TBATS: si hay estacionalidad compleja o patrones no lineales.

### ML (si hay suficiente historia y señal)
- XGBoost/LightGBM con features (lags, rolling means, dummies mes) + xregs.
- Útil si hay no-linealidad (promos, distribución, shocks).

### Ensamble (recomendado)
- Promedio ponderado de top 2–3 modelos por serie/horizonte para robustez.

## Reglas sugeridas para selección
- Priorizar **MASE** y **R² out-of-sample** en rolling backtest (ventana 24, step 1, horizon 12).
- Elegir modelo por serie y también un 'winner' agregado por segmento si se requiere.

