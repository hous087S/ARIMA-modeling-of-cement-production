# ARIMA Time Series Modeling – Cement Production

## Overview
This project develops a robust time series forecasting pipeline using ARIMA models applied to monthly cement production data (1990–2025).  
It combines statistical testing, model selection, and diagnostic validation to identify a model that accurately captures the dynamics of the series.  
The approach emphasizes methodological rigor and interpretability, ensuring reliable and well-grounded forecasts.

---

## Key Steps
- Tested stationarity using ADF and KPSS → series is I(1)  
- Identified model orders via ACF/PACF  
- Compared models using AIC/BIC  
- Validated with Ljung–Box test and residual diagnostics  

---

## Final Model
**ARIMA(7,1,0)**  
- Selected for strong residual diagnostics (no autocorrelation)  
- Effectively captures short-term dependence  

---

## Results
- Produced short-term forecasts (T+1, T+2)  
- Constructed confidence intervals showing increasing uncertainty over time  
- Residuals are approximately Gaussian  

---

## Authors
- Houssem Bouabid  
- Willy Lin  
