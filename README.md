# ARIMA Time Series Modeling – Cement Production

## Overview
This project develops a robust time series forecasting pipeline using ARIMA models applied to monthly cement production data (1990–2025).  
It combines statistical testing, model selection, and diagnostic validation to identify a model that accurately captures the dynamics of the series.  
The approach emphasizes methodological rigor and interpretability, ensuring reliable and well-grounded forecasts.

---

## Summary of Key Findings

| Step                     | Method / Tool                | Key Result                              | Interpretation |
|--------------------------|------------------------------|------------------------------------------|----------------|
| Data Analysis            | Visual inspection            | Downward trend                           | Suggests non-stationarity |
| Stationarity Testing     | ADF & KPSS                  | Non-stationary → I(1) after differencing | First differencing required |
| Model Identification     | ACF / PACF                  | q ≈ 1, p up to 7                         | Candidate ARIMA models defined |
| Model Selection          | AIC / BIC                   | AIC → (1,1,1), BIC → (7,1,0)             | Trade-off: fit vs parsimony |
| Model Validation         | Ljung–Box test              | ARIMA(7,1,0) passes (no autocorrelation) | Preferred model |
| Residual Analysis        | QQ-plot / distribution      | Approximately Gaussian                   | Model assumptions acceptable |
| Final Model              | ARIMA(7,1,0)                | Significant AR coefficients              | Captures short-term dynamics |
| Forecasting              | 1–2 step ahead predictions  | Wider intervals over time                | Uncertainty increases with horizon |

---

## Results
- Produced short-term forecasts (T+1, T+2)  
- Constructed confidence intervals showing increasing uncertainty over time  
- Residuals are approximately Gaussian  

---

## Authors
- Houssem Bouabid  
- Willy Lin  
