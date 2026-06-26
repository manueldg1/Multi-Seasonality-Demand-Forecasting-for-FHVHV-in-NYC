############################################################
# Import libraries
############################################################

library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(tseries)
library(car)
library(stringr)
library(ggfortify)
library(corrplot)
library(zoo)
library(mgcv)

# You can go directly to the "Upload preprocessed dataset" section 
# for the dataset used in the project
# This first stage consists in aggregating and cleaning all the monthly taxi 
# demand parquet files for preparing the final dataset.



############################################################
# Import input files
############################################################

# List of all parquet file (each one represent a month)
cartella_origine <- "C:/Users/Usuario/Desktop/Business_Economic_Financial_Data/Project/Data"

files <- list.files(
  path = cartella_origine,
  pattern = "fhvhv.*\\.parquet$",
  full.names = TRUE
)

# Verify the files are in the path
if (length(files) == 0) {
  stop("No parquet files found in the specified path!")
}

# Create a temporary list to save the hourly aggregation of each month
lista_oraria_mesi <- list()

############################################################
# Aggregation month by month
############################################################

# In this phase, implausible records are removed:
# - trips with zero or negative distance
# - trips too short (<= 60 seconds)
# - negative fares (refunds or errors)
# - records without timestamps (not aggregable)

for (i in 1:length(files)) {
  file_corrente <- files[i]
  message(paste("Elaborating file", i, "of", length(files), ":", basename(file_corrente)))
  
  # Read the single month via open_dataset
  ds_mese <- open_dataset(file_corrente)
  
  # Cleaning on a single month
  orario_mese <- ds_mese %>%
    filter(
      trip_miles > 0,
      trip_time >= 60,
      base_passenger_fare >= 0,
      !is.na(pickup_datetime)
    ) %>%
    mutate(
      hour = floor_date(pickup_datetime, "hour")
    ) %>%
    group_by(hour) %>%
    summarise(
      Demand = n(),
      Avg_Trip_Miles = mean(trip_miles, na.rm = TRUE),
      Avg_Trip_Time = mean(trip_time, na.rm = TRUE),
      Avg_Base_Fare = mean(base_passenger_fare, na.rm = TRUE),
      Avg_Tips = mean(tips, na.rm = TRUE),
      Shared_Trips = sum(shared_request_flag == "Y", na.rm = TRUE),
      Wav_Trips = sum(wav_request_flag == "Y", na.rm = TRUE),
      Avg_Tolls = mean(tolls, na.rm = TRUE),
      Avg_Bcf = mean(bcf, na.rm = TRUE),
      Avg_Sales_Tax = mean(sales_tax, na.rm = TRUE),
      Avg_Congestion_Surcharge = mean(congestion_surcharge, na.rm = TRUE),
      Avg_Airport_Fee = mean(airport_fee, na.rm = TRUE),
      Avg_Cbd_Congestion_Fee = mean(cbd_congestion_fee, na.rm = TRUE)
    ) %>%
    collect()
  
  # Save the compressed result of the month in the list
  lista_oraria_mesi[[i]] <- orario_mese
  
  # Immediate memory cleanup before next month
  rm(ds_mese, orario_mese)
  gc()
}

############################################################
# Join the months
############################################################

# Join the 12 hourly months
df_orario <- do.call(rbind, lista_oraria_mesi)

# Apply the latest changes
df_orario <- df_orario %>%
  # Final chronological ordering of the historical series
  arrange(hour) %>%
  
  # Capitalize all columns
  rename_with(~ str_to_title(.))



############################################################
# Saving the final dataset
############################################################

# The final dataset represents a complete hourly time series
# with target variables (Demand) and aggregate regressors.

file_output_orario <- file.path(cartella_origine, "df_orario_annuale_pulito.parquet")
write_parquet(df_orario, file_output_orario)

############################################################
# Upload the preprocessed dataset
############################################################

cartella_origine <- "C:/Users/Usuario/Desktop/Business_Economic_Financial_Data/Project/Data"
df_orario <- read_parquet(file.path(cartella_origine, "df_orario_annuale_pulito.parquet"))

dim(df_orario)

############################################################
# Control temporal gap
############################################################

expected_hours <- seq(
  min(df_orario$Hour),
  max(df_orario$Hour),
  by = "hour"
)

missing_hours <- setdiff(
  expected_hours,
  df_orario$Hour
)

length(missing_hours)

# If the result is zero, the series is complete.

############################################################
# Desciptive analysis
############################################################

summary(df_orario$Demand)
# Hourly demand varies between 85 and 75,272 trips, with the mean (28,234) slightly 
# lower than the median (30,401), indicating a slight negative skew and significant
# variability over time.

# Preliminary analysis of the distribution and temporal evolution of demand.

df_orario$Demand_smoothed_24h <- rollmean(
  df_orario$Demand,
  k = 24,
  fill = NA,
  align = "right"
)
Sys.setlocale("LC_TIME", "C")
# monthly positions (approximately every 30 days)
month_pos <- seq(1, nrow(df_orario), by = 24 * 30)

# Monthly labels like "March 2026"
month_lab <- format(df_orario$Hour[month_pos], "%b %Y")

# Y-axis limits in thousands
y_vals <- pretty(df_orario$Demand_smoothed_24h, n = 5)

# Basic plot (without axes)
plot(
  df_orario$Hour,
  df_orario$Demand_smoothed_24h,
  type = "l",
  col = "steelblue",
  lwd = 2.5,
  xaxt = "n",
  yaxt = "n",
  main = "Taxi Demand (24-hour Moving Average)",
  xlab = "",
  ylab = ""
)

# Y axis in k
axis(
  2,
  at = y_vals,
  labels = paste0(round(y_vals / 1000), "k"),
  las = 1
)

# X axis with months
axis(
  1,
  at = df_orario$Hour[month_pos],
  labels = month_lab,
  las = 2
)

# Label axes
title(
  #xlab = "Month",
  ylab = "Demand (thousands)"
)


############################################################
# Average time profile
############################################################

df_orario <- df_orario %>%
  mutate(
    hour_of_day = hour(Hour)
  )

hourly_profile <- df_orario %>%
  group_by(hour_of_day) %>%
  summarise(
    avg_demand = mean(Demand)
  )

plot(
  hourly_profile$hour_of_day,
  hourly_profile$avg_demand,
  type = "b",
  xlab = "Hour of the Day",
  ylab = "Average Demand",
  main = "Daily Demand Profile by Hour"
)
#It allows us to identify intraday demand patterns. A decline is observed during
# the nighttime hours, with a low between midnight and 5:00 a.m., followed by a
# gradual recovery throughout the day and an evening peak around 8:00 p.m.

############################################################
# Day of the week and month effect
############################################################

df_orario <- df_orario %>%
  mutate(
    weekday = wday(
      Hour,
      label = TRUE
    )
  )

boxplot(
  Demand ~ weekday,
  data = df_orario,
  main = "Demand by Day of the Week", 
  col= "lightblue",
  names = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
)


monthly_profile <- df_orario %>%
  mutate(month = format(Hour, "%m")) %>%
  group_by(month) %>%
  summarise(avg_demand = mean(Demand, na.rm = TRUE))

plot(
  monthly_profile$month,
  monthly_profile$avg_demand,
  type = "b",
  xlab = "Month",
  ylab = "Average Demand",
  main = "Monthly Demand Profile"
)

# Check for systematic differences between weekdays and weekends.
#There is no substantial difference.

############################################################
# Outlier analysis (after initial screening)
############################################################

df_orario <- df_orario %>%
  mutate(
    z_score =
      (Demand - mean(Demand)) /
      sd(Demand)
  )

outliers <- df_orario %>%
  filter(
    abs(z_score) > 3
  )

outliers

# Outliers are not automatically removed.
# Manual inspection shows that they correspond to real events like New Year's Eve
# and Daylight Saving Time Transition and not to recording errors.

############################################################
# 11. Future regressor analysis (XREG)
############################################################

df_corr <- df_orario %>%
  select(
    Demand,
    Avg_trip_miles,
    Avg_trip_time,
    Avg_base_fare,
    Avg_tips,
    Shared_trips,
    Wav_trips,
    Avg_tolls,
    Avg_bcf,
    Avg_sales_tax,
    Avg_congestion_surcharge,
    Avg_airport_fee,
    Avg_cbd_congestion_fee
  )
cor_mat <- cor(df_corr, use = "pairwise.complete.obs")
colore_testo <- ifelse(cor_mat > 0.6, "white", "black")
corrplot(
  cor_mat,
  method = "color",
  addCoef.col = colore_testo,
  number.digits = 1,
  tl.col = "black",
  tl.srt = 30
)
title("Correlation Matrix of Variables")

modello_lin <- lm(
  
  Demand ~
    
    Avg_trip_time +
    Avg_base_fare +
    Avg_tips +
    Shared_trips +
    Wav_trips +
    Avg_tolls +
    Avg_airport_fee +
    Avg_congestion_surcharge,
  
  data = df_orario
)
summary(modello_lin)
vif(modello_lin)

# It is used exclusively as a diagnostic tool for:
# - assessing multicollinearity;
# - selecting external regressors;
# - preparing future models.

# All VIFs are less than 10.
# Some correlations are relatively high,
# but none exceed the commonly considered critical thresholds (0.8 - 0.9).
# The regressors can therefore all be kept in consideration for subsequent analysis.

############################################################
# Train Test split
############################################################

prop     <- 0.8
n_sample <- floor(nrow(df_orario) * prop)

train <- df_orario[1:n_sample, ]
test  <- df_orario[(n_sample + 1):nrow(df_orario), ]

y_train <- train[["Demand"]]
y_test  <- test[["Demand"]]

cat("Train:", nrow(train), "ore |",
    format(min(train$Hour), "%Y-%m-%d"), "->",
    format(max(train$Hour), "%Y-%m-%d"), "\n")
cat("Test: ", nrow(test), "ore |",
    format(min(test$Hour), "%Y-%m-%d"), "->",
    format(max(test$Hour), "%Y-%m-%d"), "\n")

# Add the Type column to the complete df_orario
Type <- c(rep("Train", nrow(train)), rep("Test", nrow(test)))

df_orario <- df_orario %>%
  mutate(Type = Type)

# Chart with 24h moving average instead of raw data
ggplot(df_orario %>% filter(!is.na(Demand_smoothed_24h)),
       aes(x = Hour, y = Demand_smoothed_24h, color = Type)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Train" = "#6BC3FF", "Test" = "#FF7F7F")) +
  geom_vline(
    xintercept = as.numeric(df_orario$Hour[n_sample]),
    linetype = "dashed", color = "black", linewidth = 0.7
  ) +
  annotate("text",
           x     = df_orario$Hour[n_sample],
           y     = max(df_orario$Demand_smoothed_24h, na.rm = TRUE) * 0.95,
           label = "Split point (80/20)",
           hjust = -0.1, size = 3.5, color = "black") +
  scale_y_continuous(labels = function(x) paste0(round(x / 1000), "k")) +
  labs(
    title = "Train and Test Split – NYC Hourly Taxi Demand (24h MA)",
    x     = "Time",
    y     = "Hourly Demand (thousands)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


############################################################
# Creation of the time series
############################################################

ts_hourly <- ts(
  y_train,
  frequency = 24
)

# The frequency of 24 represents the daily seasonality of the series.

############################################################
# Autocorrelation
############################################################

par(mfrow=c(1,2))

acf(
  as.numeric(ts_hourly), 
  lag.max = 72, 
  main = "ACF (First 72 Hours)",
  xlab = "Lag (in Hours)"
)

pacf(
  as.numeric(ts_hourly), 
  lag.max = 72, 
  main = "PACF (First 72 Hours)",
  xlab = "Lag (in Hours)"
)

par(mfrow=c(1,1))



# The ACF and PACF analysis provides preliminary indications on the AR and MA
# orders of future models.

# General indication: if the ACF is exponentially decaying or sinusoidal and
# there is a significant spike at lag p in PACF and nothing else, it may be an 
# ARMA(p,d,0). If the PACF is exponentially decaying or sinusoidal and there is
# a significant spike at lag p in ACF and nothing else, it may be an ARMA(0,d,q). 


############################################################
# Stationary test
############################################################

adf.test(ts_hourly)

# H0: The series is non-stationary
# H1: The series is stationary

# A p-value less than 0.05 allows
# to reject H0 and conclude that the series is stationary.


############################################################
# Ljung-Box test
############################################################

# H0: The series is white noise
# H1: Autocorrelation exists

Box.test(
  ts_hourly,
  lag = 24,
  type = "Ljung-Box"
)

# If the p-value is < 0.05:
# the null hypothesis of no autocorrelation is rejected.
# The series exhibits temporal dependence and cannot be considered white noise.

#########################################################################
# Models
#########################################################################

#########################################################################
#ARIMA
#########################################################################

#ARIMA(2,0,0)
a1 <- Arima(
  ts_hourly,
  order = c(2,0,0)
)
summary(a1)
plot(
  ts_hourly,
  main = "ARIMA(2,0,0)"
)

# residuals
res_arima1 <- residuals(a1)
tsdisplay(res_arima1)
checkresiduals(res_arima1)


#ARIMA(2,0,1)
a2 <- Arima(
  ts_hourly,
  order = c(2,0,1)
)
summary(a2)

# residuals
res_arima2 <- residuals(a2)
tsdisplay(res_arima2)
checkresiduals(res_arima2)


par(mfrow=c(2,2))
Acf(res_arima1)
Pacf(res_arima1)
Acf(res_arima2)
Pacf(res_arima2)

# The structure of the series is not described by non-seasonal AR and MA components.

AIC(a1)
AIC(a2)


#########################################################################
#SARIMA
#########################################################################

# SARIMA(2,0,0)(1,0,0)[24]

sarima1 <- Arima(
  ts_hourly,
  order = c(2,0,0),
  seasonal = list(
    order = c(1,0,0),
    period = 24
  )
)

summary(sarima1)

fit_s1 <- fitted(sarima1)

plot(
  ts_hourly,
  main = "SARIMA(2,0,0)(1,0,0)[24]"
)

lines(
  fit_s1,
  col = 2
)

r_s1 <- residuals(sarima1)

tsdisplay(r_s1)

checkresiduals(sarima1) #no white noise

par(mfrow=c(1,2))
Acf(residuals(sarima1))
Pacf(residuals(sarima1))

par(mfrow = c(1, 1))

# Horizon setup (2 weeks = 336 hours)
ore_zoom <- 336

# Extract data segments
reali_zoom   <- tail(as.numeric(ts_hourly), ore_zoom)
stimati_zoom <- tail(as.numeric(fitted(sarima1)), ore_zoom)
asse_x       <- 1:ore_zoom

# Ceiling trick: calculate a max limit 15% higher than the highest peak
max_y_value <- max(c(reali_zoom, stimati_zoom))
min_y_value <- min(c(reali_zoom, stimati_zoom))
upper_limit <- max_y_value * 1.15

# Plot Real Data
plot(asse_x, reali_zoom, 
     type = "l", 
     col = "black", 
     lwd = 2,
     ylim = c(min_y_value, upper_limit), 
     yaxt = "n",                        
     xlab = "Time (Hours in the last 2 weeks)", 
     ylab = "Hourly Trip Volume",
     main = "Real Data vs. SARIMA Model (2-Week Focus)",
     col.main = "black", 
     font.main = 2,
     panel.first = grid())

# Overlay SARIMA fitted values
lines(asse_x, stimati_zoom, 
      col = "red", 
      lwd = 2)

# Creating a new Y-axis with "k"
# Automatically generates the perfect points for dashes (e.g., 10000, 20000, 30000...)
y_ticks <- pretty(c(min_y_value, max_y_value))

# it takes the points, divides them by 1000 and adds a "k" (e.g. 40000 becomes "40k")
y_labels <- paste0(y_ticks / 1000, "k")

# Design custom axis
# side = 2 means left axis, las = 1 puts the numbers horizontal and readable
axis(side = 2, at = y_ticks, labels = y_labels, las = 1)

# Clean Legend in the top-left corner
legend("topleft", 
       legend = c("Real Data", "SARIMA(2,0,0)(1,0,0)[24] Model"),
       col = c("black", "red"), 
       lwd = 2, 
       bty = "n",   
       cex = 0.9)

# SARIMA(2,0,1)(1,0,0)[24]

sarima2 <- Arima(
  ts_hourly,
  order = c(2,0,1),
  seasonal = list(
    order = c(1,0,0),
    period = 24
  )
)

summary(sarima2)

r_s2 <- residuals(sarima2)

tsdisplay(r_s2)

checkresiduals(sarima2) #no white noise


#########################################################################
# Model with auto.arima
#########################################################################

auto_fit <- auto.arima(
  ts_hourly,
  seasonal = TRUE
)

summary(auto_fit)

checkresiduals(auto_fit) #no white noise

r_auto <- residuals(auto_fit)

par(mfrow = c(3,2))

Acf(residuals(sarima1))
Pacf(residuals(sarima1))
     
Acf(residuals(sarima2))
Pacf(residuals(sarima2))
Acf(residuals(auto_fit))
Pacf(residuals(auto_fit))

par(mfrow = c(1,1))

AIC(sarima1)
AIC(sarima2)
AIC(auto_fit)

par(mfrow = c(1, 1))
# Horizon setup (2 weeks = 336 hours)
ore_zoom <- 336

# Extract data segments
reali_zoom        <- tail(as.numeric(ts_hourly), ore_zoom)
stimati_sarima1   <- tail(as.numeric(fitted(sarima1)), ore_zoom)  # SARIMA(2,0,0)(1,0,0)[24]
stimati_sarima2   <- tail(as.numeric(fitted(sarima2)), ore_zoom)  # SARIMA(2,0,1)(1,0,0)[24]
stimati_auto      <- tail(as.numeric(fitted(auto_fit)), ore_zoom) # SARIMA(2,0,0)(2,1,0)[24]
asse_x            <- 1:ore_zoom


# Ceiling trick: calculate limits including all three models tutti in the vector
max_y_value <- max(c(reali_zoom, stimati_sarima1, stimati_sarima2, stimati_auto))
min_y_value <- min(c(reali_zoom, stimati_sarima1, stimati_sarima2, stimati_auto))
upper_limit <- max_y_value * 1.15

# Plot Real Data
plot(asse_x, reali_zoom, 
     type = "l", 
     col = "black", 
     lwd = 2,
     ylim = c(min_y_value, upper_limit), 
     yaxt = "n",                         
     xlab = "Time (Hours in the last 2 weeks)", 
     ylab = "Hourly Trip Volume",
     main = "Real Data vs. SARIMA Models (2-Week Focus)",
     col.main = "black", 
     font.main = 2,
     panel.first = grid())

# Overlay of the three SARIMA models
lines(asse_x, stimati_sarima1, col = "red", lwd = 2)
lines(asse_x, stimati_sarima2, col = "blue", lwd = 2)
lines(asse_x, stimati_auto,    col = "yellow", lwd = 2)

# Creating a new Y-axis with "k"
y_ticks <- pretty(c(min_y_value, max_y_value))
y_labels <- paste0(y_ticks / 1000, "k")
axis(side = 2, at = y_ticks, labels = y_labels, las = 1)

# Clean Legend in the top-left corner
legend("topright", 
       legend = c("Real Data", 
                  "SARIMA(2,0,0)(1,0,0)[24]", 
                  "SARIMA(2,0,1)(1,0,0)[24]", 
                  "SARIMA(2,0,0)(2,1,0)[24]"),
       col = c("black", "red", "blue", "yellow"), 
       lwd = 2, 
       bty = "n",   
       cex = 0.80)

#########################################################################
# SARMAX
#########################################################################

xreg_train <- df_orario %>%
  slice(1:length(y_train)) %>%
  select(
    Avg_trip_time,
    Avg_base_fare,
    Avg_tips,
    Shared_trips,
    Wav_trips,
    Avg_tolls,
    Avg_airport_fee,
    Avg_congestion_surcharge
  ) %>%
  as.matrix()

# Regression to understand if the residuals are stationary and choose the parameter value d
modello_ols <- lm(as.numeric(ts_hourly) ~ xreg_train)

# Extrac residuals
residui_ols <- residuals(modello_ols)

# ADF for stationarity of residuals
summary(modello_ols)
adf.test(residui_ols)
# They are stationary, so we can set d = 0

par(mfrow = c(1, 2))

acf(na.omit(residui_ols), lag.max = 72, main = "ACF of OLS residuals")
pacf(na.omit(residui_ols), lag.max = 72, main = "PACF of OLS residuals")

par(mfrow = c(1, 1))

# The correlograms of the OLS residuals clearly demonstrate that classical linear regression
# fails because it ignores the temporal structure of the data. 
# The slow decay in the ACF and the sharp peak at Lag 1 in the PACF indicate a 
# strong uncaptured hourly inertia, while the mirrored and repeated peaks at Lags 24,
# 48, and 72 confirm the presence of systematic daily seasonality. 
# This means that the linear model makes the exact same error at the 
# same time of day every day, making it mathematically mandatory to switch 
# to a SARMAX model to integrate the exogenous regressors with the corrected 
# autoregressive and seasonal components.


# SARMAX(2,0,0)(1,0,0)[24]
sarmax1 <- Arima(
  ts_hourly,
  order = c(2,0,0),
  seasonal = list(order = c(1,0,0), period = 24),
  xreg = xreg_train
)


summary(sarmax1)
checkresiduals(residuals(sarmax1))
Acf(residuals(sarmax1))

#########################################################################
# SARMAX auto.arima
#########################################################################

modello_sarmax_auto <- auto.arima(ts_hourly, 
                                  xreg = xreg_train, 
                                  seasonal = TRUE, 
                                  stepwise = TRUE,       
                                  approximation = TRUE)

summary(modello_sarmax_auto)

Acf(residuals(modello_sarmax_auto))
checkresiduals(modello_sarmax_auto)

par(mfrow = c(2,2))

Acf(residuals(sarmax1))
Pacf(residuals(sarmax1))
Acf(residuals(modello_sarmax_auto))
Pacf(residuals(modello_sarmax_auto))

par(mfrow = c(1,1))


par(mfrow = c(1, 1))
# Horizon setup (2 weeks = 336 hours)
ore_zoom <- 336

# Extract data segments
reali_zoom        <- tail(as.numeric(ts_hourly), ore_zoom)
stimati_zoom      <- tail(as.numeric(fitted(sarmax1)), ore_zoom)
stimati_auto_zoom <- tail(as.numeric(fitted(modello_sarmax_auto)), ore_zoom) # <--- 1. ESTRATTI VALORI AUTO
asse_x            <- 1:ore_zoom

# Ceiling trick: calculate the maximum including the automatic model
max_y_value <- max(c(reali_zoom, stimati_zoom, stimati_auto_zoom)) 
min_y_value <- min(c(reali_zoom, stimati_zoom, stimati_auto_zoom))
upper_limit <- max_y_value * 1.15

# Plot Real Data
plot(asse_x, reali_zoom, 
     type = "l", 
     col = "black", 
     lwd = 2,
     ylim = c(min_y_value, upper_limit), 
     yaxt = "n",                         
     xlab = "Time (Hours in the last 2 weeks)", 
     ylab = "Hourly Trip Volume",
     main = "Real Data vs. SARMAX Models (2-Week Focus)",
     col.main = "black", 
     font.main = 2,
     panel.first = grid())

# Overlay SARMAX manual fitted values (red)
lines(asse_x, stimati_zoom, 
      col = "red", 
      lwd = 2)

# Overlay SARMAX auto fitted values (blue)
lines(asse_x, stimati_auto_zoom, 
      col = "blue",                      
      lwd = 2)

# Creating a new Y-axis with "k"
y_ticks <- pretty(c(min_y_value, max_y_value))
y_labels <- paste0(y_ticks / 1000, "k")
axis(side = 2, at = y_ticks, labels = y_labels, las = 1)

# Clean Legend in the top-left corner
legend("topleft", 
       legend = c("Real Data", "SARMAX(2,0,0)(1,0,0)[24]", "SARMAX(1,0,4)(2,1,0)[24]"), # <--- 5. AGGIORNATO TESTO
       col = c("black", "red", "blue"),                                                  # <--- 6. AGGIUNTO COLORE BLU
       lwd = 2, 
       bty = "n",   
       cex = 0.9)

AIC(sarmax1)
AIC(modello_sarmax_auto)

#########################################################################
# FOURIER
#########################################################################

ts_multi_train <- msts(
  y_train,                      
  seasonal.periods = c(24, 168)
)

# Generate the Fourier terms for the train
fourier_terms_train <- fourier(
  ts_multi_train,
  K = c(6, 8)
)

# Extract the economic regressors for train (first 7008 rows)
xreg_econ_train <- as.matrix(
  df_orario[1:length(y_train), c(
    "Avg_trip_time",
    "Avg_base_fare",
    "Avg_tips",
    "Shared_trips",
    "Wav_trips",
    "Avg_tolls",
    "Avg_airport_fee",
    "Avg_congestion_surcharge"
  )]
)

# Combine the economic regressors and the Fourier terms of train
xreg_full_train <- cbind(
  xreg_econ_train,
  fourier_terms_train
)

# Fit automatic model on train
fourier_model <- auto.arima(
  ts_multi_train,
  xreg = xreg_full_train,
  seasonal = FALSE,          
  stepwise = TRUE,
  approximation = FALSE
)

summary(fourier_model)

checkresiduals(fourier_model)

par(mfrow = c(1,2))
Acf(residuals(fourier_model))
Pacf(residuals(fourier_model))
par(mfrow = c(1,1))

ore_zoom <- 336

reali_zoom <- tail(
  as.numeric(ts_multi_train),
  ore_zoom
)

stimati_zoom <- tail(
  as.numeric(fitted(fourier_model)),
  ore_zoom
)

asse_x <- 1:ore_zoom

max_y <- max(c(reali_zoom, stimati_zoom))
min_y <- min(c(reali_zoom, stimati_zoom))

plot(
  asse_x,
  reali_zoom,
  type = "l",
  lwd = 2,
  col = "black",
  ylim = c(min_y, max_y * 1.15),
  yaxt = "n",
  xlab = "Time (last 2 weeks)",
  ylab = "Hourly Trip Volume",
  main = "Observed vs Fitted Values: Fourier ARIMAX"
)

grid()

lines(
  asse_x,
  stimati_zoom,
  col = "red",
  lwd = 2
)

y_ticks <- pretty(c(min_y, max_y))

axis(
  side = 2,
  at = y_ticks,
  labels = paste0(round(y_ticks/1000), "k"),
  las = 1
)

legend(
  "topright",
  legend = c(
    "Observed",
    "Fourier ARIMAX"
  ),
  col = c(
    "black",
    "red"
  ),
  lwd = 2,
  bty = "n"
)






#########################################################################
# Linearity screening for GAM models
#########################################################################
# This subsection screens for exogenous variables present in the train

# List of all exogenous variables to be tested against Demand
vars_to_test <- c(
  "Avg_trip_time", "Avg_base_fare", "Avg_tips", "Shared_trips", 
  "Wav_trips", "Avg_tolls", "Avg_airport_fee", "Avg_congestion_surcharge", "time_index"
)

# Automatic numerical screening
tabella_linearita <- data.frame(
  Variabile = character(), 
  Pearson_Lin = numeric(), 
  Spearman_NonLin = numeric(), 
  Distanza_Delta = numeric(), 
  stringsAsFactors = FALSE
)

for(v in vars_to_test) { 
  if(v %in% names(train)) {
    p <- cor(train$Demand, train[[v]], method = "pearson", use = "complete.obs")
    s <- cor(train$Demand, train[[v]], method = "spearman", use = "complete.obs")
    
    tabella_linearita <- rbind(tabella_linearita, data.frame(
      Variabile = v, 
      Pearson_Lin = round(p, 3), 
      Spearman_NonLin = round(s, 3), 
      Distanza_Delta = round(abs(abs(s) - abs(p)), 3)
    ))
  } 
}

# Sort the table to show the variables most likely to be nonlinear first.
tabella_linearita <- tabella_linearita %>% arrange(desc(Distanza_Delta)) 

cat("\n--- NON LINEARITY CHECK ---\n")
print(tabella_linearita)




########################################################################
# Estimation of the GAM model
########################################################################


# Enrich the dataset with temporal features and the continuous trend index
df_orario <- df_orario %>% 
  mutate(
    hour_of_day = hour(Hour), 
    weekday = factor(wday(Hour, label = TRUE, week_start = 1), ordered = FALSE), 
    time_index = as.numeric(Hour)
  )

# Update the train and test datasets to include the new temporal regressors
prop     <- 0.8
n_sample <- floor(nrow(df_orario) * prop)

train_gam <- df_orario[1:n_sample, ]
test_gam  <- df_orario[(n_sample + 1):nrow(df_orario), ]

# Fitting the final GAM model to the training data
gam_model <- gam(
  Demand ~ s(hour_of_day, bs = "cc", k = 24) + 
    s(weekday, bs = "re") + 
    time_index +
    s(Avg_trip_time) + 
    s(Avg_base_fare) + 
    s(Avg_tips) + 
    s(Avg_airport_fee) + 
    s(Wav_trips) + 
    Avg_tolls + 
    Avg_congestion_surcharge + 
    Shared_trips, 
  data = train_gam
)

summary(gam_model)


########################################################################
# Diagnostics of GAM residuals
########################################################################

# Residuals extraction from the general additive model
residui_gam <- residuals(gam_model)

par(mfrow = c(1, 2)) 
acf(residui_gam, lag.max = 72, main = "ACF of GAM Model") 
pacf(residui_gam, lag.max = 72, main = "PACF of GAM Model") 
par(mfrow = c(1, 1))

# Ljung-Box test to verify autocorrelation in the residuals
Box.test(residui_gam, lag = 24, type = "Ljung-Box")


########################################################################
# Hybrid model: Fit SARIMA on GAM errors
########################################################################
# This step models the stochastic structure of the residues to clean the peaks at lag 24
sar_err <- Arima(
  residui_gam, 
  order = c(2, 0, 0), 
  seasonal = list(order = c(1, 0, 0), period = 24) 
)

summary(sar_err)


########################################################################
# Diagnostics on GAM + SARIMA
########################################################################

# Residuals extraction of the final hybrid model
residui_finali <- residuals(sar_err)

par(mfrow = c(1, 2)) 
acf(residui_finali, lag.max = 72, main = "ACF GAM+SARIMA Errors") 
pacf(residui_finali, lag.max = 72, main = "PACF GAM+SARIMA Errors") 
par(mfrow = c(1, 1))

# Ljung-Box test
Box.test(residui_finali, lag = 24, type = "Ljung-Box")




#########################################################################
# Linearity screening for GAM models
#########################################################################
# This subsection screens for exogenous variables present in the train

# List of all exogenous variables to be tested against Demand
vars_to_test <- c(
  "Avg_trip_time", "Avg_base_fare", "Avg_tips", "Shared_trips", 
  "Wav_trips", "Avg_tolls", "Avg_airport_fee", "Avg_congestion_surcharge", "time_index"
)

# Automatic numerical screening
tabella_linearita <- data.frame(
  Variabile = character(), 
  Pearson_Lin = numeric(), 
  Spearman_NonLin = numeric(), 
  Distanza_Delta = numeric(), 
  stringsAsFactors = FALSE
)

for(v in vars_to_test) { 
  if(v %in% names(train)) {
    p <- cor(train$Demand, train[[v]], method = "pearson", use = "complete.obs")
    s <- cor(train$Demand, train[[v]], method = "spearman", use = "complete.obs")
    
    tabella_linearita <- rbind(tabella_linearita, data.frame(
      Variabile = v, 
      Pearson_Lin = round(p, 3), 
      Spearman_NonLin = round(s, 3), 
      Distanza_Delta = round(abs(abs(s) - abs(p)), 3)
    ))
  } 
}

# Sort the table to show the variables most likely to be nonlinear first.
tabella_linearita <- tabella_linearita %>% arrange(desc(Distanza_Delta)) 

cat("\n--- NON LINEARITY CHECK ---\n")
print(tabella_linearita)




########################################################################
# Estimation of the GAM model
########################################################################


# Enrich the dataset with temporal features and the continuous trend index
df_orario <- df_orario %>% 
  mutate(
    hour_of_day = hour(Hour), 
    weekday = factor(wday(Hour, label = TRUE, week_start = 1), ordered = FALSE), 
    time_index = as.numeric(Hour)
  )

# Update the train and test datasets to include the new temporal regressors
prop     <- 0.8
n_sample <- floor(nrow(df_orario) * prop)

train_gam <- df_orario[1:n_sample, ]
test_gam  <- df_orario[(n_sample + 1):nrow(df_orario), ]

# Fitting the final GAM model to the training data
gam_model <- gam(
  Demand ~ s(hour_of_day, bs = "cc", k = 24) + 
    s(weekday, bs = "re") + 
    time_index +
    s(Avg_trip_time) + 
    s(Avg_base_fare) + 
    s(Avg_tips) + 
    s(Avg_airport_fee) + 
    s(Wav_trips) + 
    Avg_tolls + 
    Avg_congestion_surcharge + 
    Shared_trips, 
  data = train_gam
)

summary(gam_model)


########################################################################
# Diagnostics of GAM residuals
########################################################################

# Residuals extraction from the general additive model
residui_gam <- residuals(gam_model)

par(mfrow = c(1, 2)) 
acf(residui_gam, lag.max = 72, main = "ACF of GAM Model") 
pacf(residui_gam, lag.max = 72, main = "PACF of GAM Model") 
par(mfrow = c(1, 1))

# Ljung-Box test to verify autocorrelation in the residuals
Box.test(residui_gam, lag = 24, type = "Ljung-Box")


########################################################################
# Hybrid model: Fit SARIMA on GAM errors
########################################################################
# This step models the stochastic structure of the residues to clean the peaks at lag 24
sar_err <- Arima(
  residui_gam, 
  order = c(2, 0, 0), 
  seasonal = list(order = c(1, 0, 0), period = 24) 
)

summary(sar_err)


########################################################################
# Diagnostics on GAM + SARIMA
########################################################################

# Residuals extraction of the final hybrid model
residui_finali <- residuals(sar_err)

par(mfrow = c(1, 2)) 
acf(residui_finali, lag.max = 72, main = "ACF GAM+SARIMA Errors") 
pacf(residui_finali, lag.max = 72, main = "PACF GAM+SARIMA Errors") 
par(mfrow = c(1, 1))

# Ljung-Box test
Box.test(residui_finali, lag = 24, type = "Ljung-Box")



########################################################################
# Prepare regressors for test set
########################################################################

h_test <- length(y_test)


# xreg matrix for SARMAX (Manual and Auto)
xreg_test <- df_orario %>%
  slice((n_sample + 1):nrow(df_orario)) %>%
  select(
    Avg_trip_time, Avg_base_fare, Avg_tips, Shared_trips,
    Wav_trips, Avg_tolls, Avg_airport_fee, Avg_congestion_surcharge
  ) %>%
  as.matrix()


# xreg matrix for Fourier ARIMAX
# Generate future Fourier terms based on the train structure
fourier_terms_test <- fourier(ts_multi_train, K = c(6, 8), h = h_test)
xreg_full_test <- cbind(xreg_test, fourier_terms_test)


########################################################################
# Forecast generation
########################################################################

# Forecast ARIMA / SARIMA / SARMAX models
fc_a1        <- forecast(a1, h = h_test)
fc_a2        <- forecast(a2, h = h_test)
fc_s1        <- forecast(sarima1, h = h_test)
fc_s2        <- forecast(sarima2, h = h_test)
fc_auto      <- forecast(auto_fit, h = h_test)
fc_sarmax_m  <- forecast(sarmax1, xreg = xreg_test, h = h_test)
fc_sarmax_a  <- forecast(modello_sarmax_auto, xreg = xreg_test, h = h_test)
fc_fourier   <- forecast(fourier_model, xreg = xreg_full_test, h = h_test)

# Forecast GAM + SARIMA model
# The total forecast is the sum of the structural trend forecast (GAM)
# and the stochastic residual component forecast (SARIMA)
pred_gam_test    <- predict(gam_model, newdata = test_gam)
fc_sar_err       <- forecast(sar_err, h = h_test)
pred_ibrido_test <- pred_gam_test + as.numeric(fc_sar_err$mean)


########################################################################
# Accuracy extraction
########################################################################

# Use the accuracy() function to extract the metrics (Train/Test)
acc_a1        <- accuracy(fc_a1, y_test)
acc_a2        <- accuracy(fc_a2, y_test)
acc_s1        <- accuracy(fc_s1, y_test)
acc_s2        <- accuracy(fc_s2, y_test)
acc_auto      <- accuracy(fc_auto, y_test)
acc_sarmax_m  <- accuracy(fc_sarmax_m, y_test)
acc_sarmax_a  <- accuracy(fc_sarmax_a, y_test)
acc_fourier   <- accuracy(fc_fourier, y_test)

# Metrics calculation for GAM + SARIMA model

# Train metrics
fit_gam_train     <- fitted(gam_model)
fit_sar_err       <- fitted(sar_err)
pred_ibrido_train <- fit_gam_train + fit_sar_err
rmse_gam_train    <- sqrt(mean((train_gam$Demand - pred_ibrido_train)^2, na.rm = TRUE))

# Test metrics
rmse_gam_test <- sqrt(mean((test_gam$Demand - pred_ibrido_test)^2, na.rm = TRUE))
mae_gam_test  <- mean(abs(test_gam$Demand - pred_ibrido_test), na.rm = TRUE)
mape_gam_test <- mean(abs((test_gam$Demand - pred_ibrido_test) / test_gam$Demand), na.rm = TRUE) * 100


# Metrics calculation for pure GAM (without SARIMA)
# Train metrics
rmse_pure_gam_train <- sqrt(mean((train_gam$Demand - fit_gam_train)^2, na.rm = TRUE))

# Test metrics
rmse_pure_gam_test  <- sqrt(mean((test_gam$Demand - pred_gam_test)^2, na.rm = TRUE))
mae_pure_gam_test   <- mean(abs(test_gam$Demand - pred_gam_test), na.rm = TRUE)
mape_pure_gam_test  <- mean(abs((test_gam$Demand - pred_gam_test) / test_gam$Demand), na.rm = TRUE) * 100

############################################################
# Final table
############################################################

metriche <- data.frame(
  Model = c(
    "ARIMA(2,0,0)",
    "ARIMA(2,0,1)",
    "SARIMA(2,0,0)(1,0,0)[24]",
    "SARIMA(2,0,1)(1,0,0)[24]",
    "SARIMA(2,0,0)(2,1,0)[24]",
    "SARMAX(2,0,0)(1,0,0)[24]",
    "SARMAX(5,0,1)(2,1,0)[24]",
    "Fourier-ARIMAX(4,1,4)[24, 168]",
    "GAM (Structural)",
    "GAM + SARIMA(2,0,0)(1,0,0)[24]"
  ),
  RMSE_train = c(
    acc_a1["Training set",       "RMSE"],
    acc_a2["Training set",       "RMSE"],
    acc_s1["Training set",       "RMSE"],
    acc_s2["Training set",       "RMSE"],
    acc_auto["Training set",     "RMSE"],
    acc_sarmax_m["Training set", "RMSE"],
    acc_sarmax_a["Training set", "RMSE"],
    acc_fourier["Training set",  "RMSE"],
    rmse_pure_gam_train,        
    rmse_gam_train
  ),
  RMSE_test = c(
    acc_a1["Test set",       "RMSE"],
    acc_a2["Test set",       "RMSE"],
    acc_s1["Test set",       "RMSE"],
    acc_s2["Test set",       "RMSE"],
    acc_auto["Test set",     "RMSE"],
    acc_sarmax_m["Test set", "RMSE"],
    acc_sarmax_a["Test set", "RMSE"],
    acc_fourier["Test set",  "RMSE"],
    rmse_pure_gam_test,         
    rmse_gam_test
  ),
  MAE_test = c(
    acc_a1["Test set",       "MAE"],
    acc_a2["Test set",       "MAE"],
    acc_s1["Test set",       "MAE"],
    acc_s2["Test set",       "MAE"],
    acc_auto["Test set",     "MAE"],
    acc_sarmax_m["Test set", "MAE"],
    acc_sarmax_a["Test set", "MAE"],
    acc_fourier["Test set",  "MAE"],
    mae_pure_gam_test,         
    mae_gam_test
  ),
  MAPE_test = c(
    acc_a1["Test set",       "MAPE"],
    acc_a2["Test set",       "MAPE"],
    acc_s1["Test set",       "MAPE"],
    acc_s2["Test set",       "MAPE"],
    acc_auto["Test set",     "MAPE"],
    acc_sarmax_m["Test set", "MAPE"],
    acc_sarmax_a["Test set", "MAPE"],
    acc_fourier["Test set",  "MAPE"],
    mape_pure_gam_test,         
    mape_gam_test
  )
)


# Overfitting check
metriche$Gap_RMSE <- round(metriche$RMSE_test - metriche$RMSE_train, 2)
cat("\n Overfitting Check (Gap between RMSE test and train):\n")

# MAE and MAPE are computed for internal diagnostics only
# Final comparison in the report is based on RMSE
print(metriche[, c("Model", "RMSE_train", "RMSE_test", "Gap_RMSE")])
cat("\n2. Confronto MAE (Test Set):\n")
print(metriche[, c("Model", "MAE_test")])

cat("\n3. Confronto MAPE (Test Set):\n")
print(metriche[, c("Model", "MAPE_test")])


########################################################################
# Prepare regressors for test set
########################################################################

h_test <- length(y_test)


# xreg matrix for SARMAX (Manual and Auto)
xreg_test <- df_orario %>%
  slice((n_sample + 1):nrow(df_orario)) %>%
  select(
    Avg_trip_time, Avg_base_fare, Avg_tips, Shared_trips,
    Wav_trips, Avg_tolls, Avg_airport_fee, Avg_congestion_surcharge
  ) %>%
  as.matrix()


# xreg matrix for Fourier ARIMAX
# Generate future Fourier terms based on the train structure
fourier_terms_test <- fourier(ts_multi_train, K = c(6, 8), h = h_test)
xreg_full_test <- cbind(xreg_test, fourier_terms_test)


########################################################################
# Forecast generation
########################################################################

# Forecast ARIMA / SARIMA / SARMAX models
fc_a1        <- forecast(a1, h = h_test)
fc_a2        <- forecast(a2, h = h_test)
fc_s1        <- forecast(sarima1, h = h_test)
fc_s2        <- forecast(sarima2, h = h_test)
fc_auto      <- forecast(auto_fit, h = h_test)
fc_sarmax_m  <- forecast(sarmax1, xreg = xreg_test, h = h_test)
fc_sarmax_a  <- forecast(modello_sarmax_auto, xreg = xreg_test, h = h_test)
fc_fourier   <- forecast(fourier_model, xreg = xreg_full_test, h = h_test)

# Forecast GAM + SARIMA model
# The total forecast is the sum of the structural trend forecast (GAM)
# and the stochastic residual component forecast (SARIMA)
pred_gam_test    <- predict(gam_model, newdata = test_gam)
fc_sar_err       <- forecast(sar_err, h = h_test)
pred_ibrido_test <- pred_gam_test + as.numeric(fc_sar_err$mean)


########################################################################
# Accuracy extraction
########################################################################

# Use the accuracy() function to extract the metrics (Train/Test)
acc_a1        <- accuracy(fc_a1, y_test)
acc_a2        <- accuracy(fc_a2, y_test)
acc_s1        <- accuracy(fc_s1, y_test)
acc_s2        <- accuracy(fc_s2, y_test)
acc_auto      <- accuracy(fc_auto, y_test)
acc_sarmax_m  <- accuracy(fc_sarmax_m, y_test)
acc_sarmax_a  <- accuracy(fc_sarmax_a, y_test)
acc_fourier   <- accuracy(fc_fourier, y_test)

# Metrics calculation for GAM + SARIMA model

# Train metrics
fit_gam_train     <- fitted(gam_model)
fit_sar_err       <- fitted(sar_err)
pred_ibrido_train <- fit_gam_train + fit_sar_err
rmse_gam_train    <- sqrt(mean((train_gam$Demand - pred_ibrido_train)^2, na.rm = TRUE))

# Test metrics
rmse_gam_test <- sqrt(mean((test_gam$Demand - pred_ibrido_test)^2, na.rm = TRUE))
mae_gam_test  <- mean(abs(test_gam$Demand - pred_ibrido_test), na.rm = TRUE)
mape_gam_test <- mean(abs((test_gam$Demand - pred_ibrido_test) / test_gam$Demand), na.rm = TRUE) * 100


# Metrics calculation for pure GAM (without SARIMA)
# Train metrics
rmse_pure_gam_train <- sqrt(mean((train_gam$Demand - fit_gam_train)^2, na.rm = TRUE))

# Test metrics
rmse_pure_gam_test  <- sqrt(mean((test_gam$Demand - pred_gam_test)^2, na.rm = TRUE))
mae_pure_gam_test   <- mean(abs(test_gam$Demand - pred_gam_test), na.rm = TRUE)
mape_pure_gam_test  <- mean(abs((test_gam$Demand - pred_gam_test) / test_gam$Demand), na.rm = TRUE) * 100

############################################################
# Final table
############################################################

metriche <- data.frame(
  Model = c(
    "ARIMA(2,0,0)",
    "ARIMA(2,0,1)",
    "SARIMA(2,0,0)(1,0,0)[24]",
    "SARIMA(2,0,1)(1,0,0)[24]",
    "SARIMA(2,0,0)(2,1,0)[24]",
    "SARMAX(2,0,0)(1,0,0)[24]",
    "SARMAX(5,0,1)(2,1,0)[24]",
    "Fourier-ARIMAX(4,1,4)[24, 168]",
    "GAM (Structural)",
    "GAM + SARIMA(2,0,0)(1,0,0)[24]"
  ),
  RMSE_train = c(
    acc_a1["Training set",       "RMSE"],
    acc_a2["Training set",       "RMSE"],
    acc_s1["Training set",       "RMSE"],
    acc_s2["Training set",       "RMSE"],
    acc_auto["Training set",     "RMSE"],
    acc_sarmax_m["Training set", "RMSE"],
    acc_sarmax_a["Training set", "RMSE"],
    acc_fourier["Training set",  "RMSE"],
    rmse_pure_gam_train,        
    rmse_gam_train
  ),
  RMSE_test = c(
    acc_a1["Test set",       "RMSE"],
    acc_a2["Test set",       "RMSE"],
    acc_s1["Test set",       "RMSE"],
    acc_s2["Test set",       "RMSE"],
    acc_auto["Test set",     "RMSE"],
    acc_sarmax_m["Test set", "RMSE"],
    acc_sarmax_a["Test set", "RMSE"],
    acc_fourier["Test set",  "RMSE"],
    rmse_pure_gam_test,         
    rmse_gam_test
  ),
  MAE_test = c(
    acc_a1["Test set",       "MAE"],
    acc_a2["Test set",       "MAE"],
    acc_s1["Test set",       "MAE"],
    acc_s2["Test set",       "MAE"],
    acc_auto["Test set",     "MAE"],
    acc_sarmax_m["Test set", "MAE"],
    acc_sarmax_a["Test set", "MAE"],
    acc_fourier["Test set",  "MAE"],
    mae_pure_gam_test,         
    mae_gam_test
  ),
  MAPE_test = c(
    acc_a1["Test set",       "MAPE"],
    acc_a2["Test set",       "MAPE"],
    acc_s1["Test set",       "MAPE"],
    acc_s2["Test set",       "MAPE"],
    acc_auto["Test set",     "MAPE"],
    acc_sarmax_m["Test set", "MAPE"],
    acc_sarmax_a["Test set", "MAPE"],
    acc_fourier["Test set",  "MAPE"],
    mape_pure_gam_test,         
    mape_gam_test
  )
)


# Overfitting check
metriche$Gap_RMSE <- round(metriche$RMSE_test - metriche$RMSE_train, 2)
cat("\n Overfitting Check (Gap between RMSE test and train):\n")

# MAE and MAPE are computed for internal diagnostics only
# Final comparison in the report is based on RMSE
print(metriche[, c("Model", "RMSE_train", "RMSE_test", "Gap_RMSE")])

cat("\n Comparison MAE (Test Set):\n")
print(metriche[, c("Model", "MAE_test")])

cat("\n Comparison MAPE (Test Set):\n")
print(metriche[, c("Model", "MAPE_test")])

