# Multi-Seasonality Demand Forecasting for FHVHV in New York City

This repository contains the final project for the **Business Economic Forecasting and Data (BEFD)** course. The study focuses on predictive modeling and time series analysis applied to urban mobility in New York City.

---

## Project Overview
This research addresses the predictive modeling and forecasting of hourly demand for High-Volume For-Hire Vehicle (FHVHV) services (Uber and Lyft) in New York City. The primary objective is to capture the complex, overlapping temporal dynamics of urban mobility while isolating the impact of exogenous economic variables and systematic calendar shocks.

The econometric workflow is structured into three main stages:
1. **Data Gathering & Preparation:** Ride-level microdata from the *NYC Taxi and Limousine Commission (TLC)* was filtered to remove invalid records (non-positive distances, trips under 60 seconds, and negative fares) and aggregated into an hourly time series. A Variance Inflation Factor (VIF) analysis was conducted to ensure the absence of multicollinearity among predictors.
2. **Exploratory Data Analysis (EDA):** Investigation of strong multi-seasonal patterns driven by daily (24-hour) and weekly (168-hour) cycles, along with calendar shocks.
3. **Modeling & Evaluation:** Five candidate models were estimated and evaluated using a chronological split (80% training, 20% testing) to find the optimal forecasting architecture.

---

## Repository Structure
The repository is kept minimal and clean, containing only the essential operational script and the final theoretical report:

*  **`Report_BEFD.pdf`**: The complete academic paper detailing the theoretical background, methodology, research workflow, VIF diagnostics, residual analysis, and economic implications.
*  **`analysis_script.R`**: A single, fully-commented monolithic R script enclosing the entire pipeline—from data preprocessing and VIF checks to model estimation and out-of-sample validation.

---

## Model Comparison & Performance
Five candidate models were evaluated. The **Fourier-ARIMAX** architecture achieved the highest predictive accuracy on the out-of-sample test set (the final 20% of the time series), capturing dual-seasonality through harmonic series without parameter inflation.

| Model | Key Characteristics | Train RMSE | Test RMSE | Status |
| :--- | :--- | :---: | :---: | :---: |
| **ARIMA(2,0,0)** | Simple autoregressive baseline | 3,083.76 | 12,013.84 | Underfitted |
| **SARIMA(2,0,0)(2,1,0)[24]** | Captures daily seasonality only | 2,461.30 | 8,320.91 | Partial |
| **SARMAX(5,0,1)(2,1,0)[24]** | Includes economic exogenous regressors | 1,933.33 | 5,387.35 | Good |
| **Fourier-ARIMAX(4,1,4)[24, 168]** | **Dual-seasonality + Exogenous regressors** | **1,784.61** | **4,632.40** | **Selected Model** |
| **GAM + SARIMA** | Non-parametric splines + Residual filtering | 2,052.85 | 10,863.25 | Overfitted |

The results demonstrate that integrating economic metrics (such as tips, tolls, and congestion surcharges) as exogenous variables, combined with a precise frequency decomposition of multi-seasonal cycles, significantly enhances forecasting accuracy.
