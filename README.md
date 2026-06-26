# Multi-Seasonality Demand Forecasting for FHVHV in New York City

This repository contains the final project for the **Business Economic Forecasting and Data (BEFD)** course. The study focuses on predictive modeling and time series analysis applied to urban mobility in New York City.

---

## Project Overview
This research addresses the predictive modeling and forecasting of hourly demand for High-Volume For-Hire Vehicle (FHVHV) services (Uber and Lyft) in New York City. The primary objective is to capture the complex, overlapping temporal dynamics of urban mobility while isolating the impact of exogenous economic variables and systematic calendar shocks.

The econometric workflow is structured into three main stages:
1. **Data Gathering & Preparation:** Ride-level microdata from the official [NYC Taxi and Limousine Commission (TLC) Trip Record Data](https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page) (specifically selecting the **High Volume For-Hire Vehicle Trip Records** in **PARQUET** format) was filtered to remove invalid records (non-positive distances, trips under 60 seconds, and negative fares) and aggregated into an hourly time series. A Variance Inflation Factor (VIF) analysis was conducted to ensure the absence of multicollinearity among predictors.
2. **Exploratory Data Analysis (EDA):** Investigation of strong multi-seasonal patterns driven by daily (24-hour) and weekly (168-hour) cycles, along with calendar shocks.
3. **Modeling & Evaluation:** Five candidate models were estimated and evaluated using a chronological split (80% training, 20% testing) to find the optimal forecasting architecture.

---

## Repository Structure
The repository is kept minimal and clean, containing only the essential operational script and the final theoretical report:

*  **`Report_BEFD.pdf`**: The complete academic paper detailing the theoretical background, methodology, research workflow, VIF diagnostics, residual analysis, and economic implications.
*  **`analysis_script.R`**: A single, fully-commented monolithic R script enclosing the entire pipeline—from data preprocessing and VIF checks to model estimation and out-of-sample validation.

---

## Model Comparison & Performance

A total of 10 candidate configurations were tested. The **Fourier-ARIMAX** architecture achieved the highest predictive accuracy on the out-of-sample test set (the final 20% of the time series), capturing dual-seasonality through harmonic series without parameter inflation.

| Model | RMSE train | RMSE test | Gap RMSE | Status |
| :--- | :---: | :---: | :---: | :---: |
| ARIMA(2,0,0) | 3,083.76 | 12,013.84 | 8,930.08 | Underfitted |
| ARIMA(2,0,1) | 3,021.77 | 12,013.03 | 8,991.26 | Underfitted |
| SARIMA(2,0,0)(1,0,0)[24] | 2,400.87 | 11,859.60 | 9,458.73 | Baseline |
| SARIMA(2,0,1)(1,0,0)[24] | 2,400.52 | 11,860.57 | 9,460.05 | Baseline |
| SARIMA(2,0,0)(2,1,0)[24] | 2,461.30 | 8,320.91 | 5,859.61 | Partial |
| SARMAX(2,0,0)(1,0,0)[24] | 1,912.62 | 9,062.13 | 7,149.51 | Insufficient |
| SARMAX(5,0,1)(2,1,0)[24] | 1,933.33 | 5,387.35 | 3,454.02 | Good |
| 🏆 **Fourier-ARIMAX(4,1,4)[24, 168]** | **1,784.61** | **4,632.40** | **2,847.79** | **Selected Model** |
| GAM (Structural) | 9,572.18 | 11,024.10 | 1,451.92 | Underfitted |
| GAM + SARIMA(2,0,0)(1,0,0)[24] | 2,052.85 | 10,863.25 | 8,810.40 | Overfitted |

The results demonstrate that integrating economic metrics (such as tips, tolls, and congestion surcharges) as exogenous variables, combined with a precise frequency decomposition of multi-seasonal cycles, significantly enhances forecasting accuracy.
