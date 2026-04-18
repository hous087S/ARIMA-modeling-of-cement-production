# ─────────────────────────────────────────────────────────────────────────────
# Project Title     : <ARIMA Modelization of IPI (cement production)>
# Authors           : <Houssem Bouabid & Willy Lin>
# Date              : <31/05/2025>
# Notes             : At the start of the code (step - 0) we left all the packages 
# that we used in comment form if necessary you can uncomment it. All the important 
# outputs (test results, plots ...) are in the appendix of the pdf report. Finally make
# sure the csv path is correct (step - 2) to correctly run the code.
# ─────────────────────────────────────────────────────────────────────────────



# ─────────────────────────────────────────────────────────────────────────────
# 0. necessary packages
# ─────────────────────────────────────────────────────────────────────────────
# Note: Uncomment the following lines if packages are not already installed
#install.packages(c(
#  "tidyverse",  
#  "lubridate",   
#  "tseries",     
#  "forecast",
#  "seastests",
#  "tidyr",
#  "nortest"
#))
# ─────────────────────────────────────────────────────────────────────────────
# 1. Load libraries 
# ─────────────────────────────────────────────────────────────────────────────
library(readr)      
library(dplyr)      
library(ggplot2)    
library(lubridate)  
library(tseries)    
library(forecast)   
library(seastests)  
library(purrr)      
library(tidyr)      
library(nortest)    

# ─────────────────────────────────────────────────────────────────────────────
# 2. Import CSV data from INSEE
# ────────────────────────────────────────────────────────────────────────────

csv_path  <- file.path("C:\\Users\\Houssem Bouabid\\Documents\\Cement_Prod.csv")


ipi_raw <- read_delim(
  "Cement_Prod.csv",        
  delim      = ";",            
  skip       = 4,                  # Skip first 4 lines of metadata
  col_names  = c("Période","Valeur","Codes"),  # Column names
  locale     = locale(decimal_mark = ".")       
)

# ─────────────────────────────────────────────────────────────────────────────
# 3. Prepare the date column and numeric values
# ─────────────────────────────────────────────────────────────────────────────
ipi <- ipi_raw %>% 
  select(Période, Valeur) %>% 
  mutate(
    # Convert “YYYY-MM” in 'Période' to a Date object (first day of month)
    Date       = as.Date(paste0(Période, "-01")),
    # Convert 'Valeur' from character to numeric
    ValeurNum  = as.numeric(Valeur)
  ) %>% 
  arrange(Date)  

# ─────────────────────────────────────────────────────────────────────────────
# 4. Determine the last observation date and its value (for subtitle)
# ─────────────────────────────────────────────────────────────────────────────
last_date  <- max(ipi$Date)                        
last_value <- ipi$ValeurNum[ipi$Date == last_date] 

# ─────────────────────────────────────────────────────────────────────────────
# 5. Plot the full series with ggplot2 (raw series)
# ─────────────────────────────────────────────────────────────────────────────
ggplot(ipi, aes(x = Date, y = ValeurNum)) +
  geom_line(size = 0.8, color = "#348ABD") +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = "Index"
  ) +
  scale_x_date(
    breaks = seq(from = as.Date("1990-01-01"),
                 to   = last_date + years(1),
                 by   = "5 years"),
    date_labels = "%Y",
    expand = expansion(add = c(0, 30))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ─────────────────────────────────────────────────────────────────────────────
# 6. Convert the series to a 'ts' object for stationarity tests
# ─────────────────────────────────────────────────────────────────────────────
ipi_ts <- ts(
  ipi$ValeurNum,
  start     = c(year(min(ipi$Date)), month(min(ipi$Date))), 
  frequency = 12                                            
)

# ─────────────────────────────────────────────────────────────────────────────
# 7. Perform stationarity tests on the raw series
# ─────────────────────────────────────────────────────────────────────────────
# 7.1 Augmented Dickey-Fuller (ADF) test on the raw series
adf_raw <- adf.test(ipi_ts)
print(adf_raw)  
#for this test we obtained a big value => not stationary

# 7.2 Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test on the raw series
#     null = "Trend" tests for trend stationarity
kpss_raw <- kpss.test(ipi_ts, null = "Trend")
print(kpss_raw)  
#same with this test the p_value was below the threshold of 0.05 => presence of a trend

# ─────────────────────────────────────────────────────────────────────────────
# 8. First difference the series
# ─────────────────────────────────────────────────────────────────────────────
ipi_diff1 <- diff(ipi_ts, differences = 1)  # First difference

# Build a data frame for plotting the differenced series
ipi_diff_df <- data.frame(
  Time      = time(ipi_diff1),        
  DiffValue = as.numeric(ipi_diff1)   
)

# Plot the first-differenced series
ggplot(ipi_diff_df, aes(x = Time, y = DiffValue)) +
  geom_line(size = 0.8, color = "#348ABD") +
  labs(
    x = "Year",
    y = "Δ Index"
  ) +
  scale_x_continuous(
    # Breaks every 5 years from min to max of the differenced time
    breaks = seq(
      floor(min(ipi_diff_df$Time)),
      ceiling(max(ipi_diff_df$Time)),
      by = 5
    ),
    expand = expansion(add = c(0, 0.5)) 
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)) 
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ─────────────────────────────────────────────────────────────────────────────
# 9. Perform stationarity tests on the differenced series
# ─────────────────────────────────────────────────────────────────────────────
# 9.1 ADF test on first difference
adf_diff1 <- adf.test(ipi_diff1)
print(adf_diff1)   
#p-value below 0.05 => stationary

# 9.2 KPSS test on first difference (null = "Trend")
kpss_diff1 <- kpss.test(ipi_diff1, null = "Trend")
print(kpss_diff1)  # Note the KPSS statistic and p-value
#p-value above 0.05 => stationary

# ─────────────────────────────────────────────────────────────────────────────
# 10. Plot ACF and PACF of the differenced series
# ─────────────────────────────────────────────────────────────────────────────
# 10.1 Autocorrelation Function (ACF)
ggAcf(ipi_diff1) +
  ggtitle("ACF") +
  theme_bw()

# 10.2 Partial Autocorrelation Function (PACF)
ggPacf(ipi_diff1) +
  ggtitle("PACF") +
  theme_bw()

# ─────────────────────────────────────────────────────────────────────────────
# 11. Grid search over ARMA(p,1,q) for p = 0..7, q = 0..1
# ─────────────────────────────────────────────────────────────────────────────
results <- expand_grid(
  p = 0:7,
  q = 0:1
) %>%
  mutate(
    # Fit ARIMA(p,1,q) on original series (d = 1)
    fit  = map2(p, q, ~ Arima(ipi_ts, order = c(.x, 1, .y))),
    
    # Extract AIC and BIC from fitted models
    AIC  = map_dbl(fit, ~ AIC(.x)),
    BIC  = map_dbl(fit, ~ BIC(.x)),
    
    # Compute sample size (n) and number of estimated parameters (k)
    n    = map_dbl(fit, ~ length(residuals(.x))),
    k    = map_dbl(fit, ~ length(coef(.x)) + 1),
    
    # Compute corrected AIC (AICc) 
    AICc = AIC + 2 * k * (k + 1) / (n - k - 1)
  ) %>%
  arrange(AICc) %>%               # Sort by ascending AICc
  select(p, q, AICc, AIC, BIC)    # Keep relevant columns

print(results)  # Display grid search results

# ─────────────────────────────────────────────────────────────────────────────
# 12. Fit two selected ARIMA models based on grid search
# ─────────────────────────────────────────────────────────────────────────────
# Fit ARIMA(1,1,1)
fit111 <- Arima(ipi_ts, order = c(1, 1, 1))

# Fit ARIMA(7,1,0)
fit710 <- Arima(ipi_ts, order = c(7, 1, 0))

# ─────────────────────────────────────────────────────────────────────────────
# 13. Extract residuals from both models
# ─────────────────────────────────────────────────────────────────────────────
resid_111 <- residuals(fit111)
resid_710 <- residuals(fit710)

# ─────────────────────────────────────────────────────────────────────────────
# 14. Define function Qtests() to compute Ljung–Box p-values up to lag k
# ─────────────────────────────────────────────────────────────────────────────
Qtests <- function(series, k, fitdf = 0) {
  pvals <- apply(matrix(1:k), 1, FUN = function(l) {
    pval <-
      if (l <= fitdf) {
        # If lag <= number of parameters, return NA
        NA
      } else {
        # Compute Ljung–Box test p-value at lag l
        Box.test(
          series,
          lag   = l,
          type  = "Ljung-Box",
          fitdf = fitdf
        )$p.value
      }
    return(c("lag" = l, "pval" = pval))
  })
  return(t(pvals))  # Return transpose: each row is a lag and p-value
}

# ─────────────────────────────────────────────────────────────────────────────
# 15. Compute Ljung–Box p-values for lags 1 to 24 
# ─────────────────────────────────────────────────────────────────────────────
LB_table1 <- Qtests(resid_111, k = 24, fitdf = 2)
LB_table2 <- Qtests(resid_710, k = 24, fitdf = 2)


LB_df1 <- as.data.frame(LB_table1)
LB_df2 <- as.data.frame(LB_table2)


LB_df1$lag  <- as.integer(LB_df1$lag)
LB_df1$pval <- as.numeric(LB_df1$pval)
LB_df2$lag  <- as.integer(LB_df2$lag)
LB_df2$pval <- as.numeric(LB_df2$pval)

# Print Ljung–Box tables
print(LB_df1)
print(LB_df2)

# ─────────────────────────────────────────────────────────────────────────────
# 16. Time‐series plot of residuals from ARIMA(7,1,0)
# ─────────────────────────────────────────────────────────────────────────────
autoplot(resid_710) +
  ggtitle("Residuals from ARIMA(7,1,0)") +
  ylab("Residual") +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────────────────
# 17. Histogram and density overlay of residuals with Normal distribution
# ─────────────────────────────────────────────────────────────────────────────
# Compute sample mean and standard deviation of residuals
mu_r    <- mean(resid_710)
sigma_r <- sd(resid_710)

ggplot(data = data.frame(resid = resid_710), aes(x = resid)) +
  # Histogram with density scaling
  geom_histogram(
    aes(y = ..density..),
    bins  = 30,
    fill  = "#348ABD",
    alpha = 0.7
  ) +
  # Empirical density (dashed black line)
  geom_density(
    color    = "black",
    linetype = "dashed",
    size     = 0.8
  ) +
  # Overlay the “true” normal curve N(mu_r, sigma_r) in red
  stat_function(
    fun   = dnorm,
    args  = list(mean = mu_r, sd = sigma_r),
    color = "red",
    size  = 0.8
  ) +
  ggtitle("Histogram + Density of Residuals\n(with Normal(μ,σ) in red)") +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────────────────
# 18. Extract ARIMA(7,1,0) coefficient estimates, SE, z‐stats, and p‐values
# ─────────────────────────────────────────────────────────────────────────────
# 18.1 Extract point estimates (coefficients)
coefs <- coef(fit710)

# 18.2 Extract standard errors from variance-covariance matrix
ses <- sqrt(diag(fit710$var.coef))

# 18.3 Compute z-statistics and two‐sided p‐values
zvals <- coefs / ses
pvals <- 2 * (1 - pnorm(abs(zvals)))

# 18.4 Assemble results
results <- data.frame(
  Coefficient = names(coefs),
  Estimate    = coefs,
  StdError    = ses,
  Z.value     = round(zvals, 3),
  P.value     = signif(pvals, 3)
)
print(results)

# ─────────────────────────────────────────────────────────────────────────────
# 19. Confidence interval question for points T+1 and T+2
# ─────────────────────────────────────────────────────────────────────────────
# 19.1 Generate forecast object for h = 2 (95% confidence interval)
fc710 <- forecast(fit710, h = 2, level = 95)


print(fc710)


# 19.3 Extract individual components
one_ahead_mean <- fc710$mean[1]               
one_ahead_lo95 <- fc710$lower[1, "95%"]
one_ahead_hi95 <- fc710$upper[1, "95%"]

two_ahead_mean <- fc710$mean[2]              
two_ahead_lo95 <- fc710$lower[2, "95%"]
two_ahead_hi95 <- fc710$upper[2, "95%"]


# ─────────────────────────────────────────────────────────────────────────────
# 20. plot
# ─────────────────────────────────────────────────────────────────────────────
# 20.1 Compute last‐12‐months data frame
last12_start <- time(ipi_ts)[length(ipi_ts) - 11]  # Numeric time for T-11
last12 <- window(ipi_ts, start = last12_start)

last12_df <- data.frame(
  Time  = time(last12),
  Value = as.numeric(last12)
)


fc_df <- data.frame(
  Time           = time(fc710$mean),      
  PointForecast  = as.numeric(fc710$mean), 
  Lo95           = fc710$lower[, "95%"],   
  Hi95           = fc710$upper[, "95%"]    
)

# 20.2 Plot last 12 months and forecast intervals
ggplot() +
  
  # 1) Plot last 12 months as a solid blue line
  geom_line(
    data = last12_df,
    aes(x = Time, y = Value),
    color = "#1f78b4",   # Blue
    size  = 1.2
  ) +
  
  # 2) Add 95% CI ribbon (pink, alpha = 0.3) for the 2‐step forecast
  geom_ribbon(
    data = fc_df,
    aes(x = Time, ymin = Lo95, ymax = Hi95),
    fill  = "#fb9a99",   # Light pink
    alpha = 0.3
  ) +
  
  # 3) Add dashed red line for forecast mean
  geom_line(
    data     = fc_df,
    aes(x = Time, y = PointForecast),
    color    = "#e31a1c",  # Red
    linetype = "dashed",
    size     = 0.9
  ) +
  
  # 4) Add circle markers at the two forecast points
  geom_point(
    data   = fc_df,
    aes(x = Time, y = PointForecast),
    color  = "#e31a1c",   # Red
    fill   = "white",
    shape  = 21,
    size   = 3,
    stroke = 1
  ) +
  
  # 5) Add vertical dotted line at “time T” to separate observed & forecast
  geom_vline(
    xintercept = max(time(ipi_ts)),
    linetype   = "dotted",
    color      = "gray40",
    size       = 0.5
  ) +
  
  # 6) Titles and theme changes
  labs(
    title    = "IPI: Last 12 Months & 2 Step Forecast (95% CI)",
    subtitle = paste0(
      "Blue = last 12 months, ",
      "Red dashed = forecast mean (T+1, T+2), ",
      "Pink ribbon = 95% CI"
    ),
    x = "Time",
    y = "IPI Index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title    = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# ─────────────────────────────────────────────────────────────────────────────
# 21. Compute lengths of forecast intervals
# ─────────────────────────────────────────────────────────────────────────────

# 21.1 Calculate interval length (Hi95 - Lo95) and add as a new column
fc_df$IntervalLength <- fc_df$Hi95 - fc_df$Lo95

# 21.2 (Optional) Reorder columns for clarity: Time, PointForecast, Lo95, Hi95, IntervalLength
fc_df <- fc_df[, c("Time", "PointForecast", "Lo95", "Hi95", "IntervalLength")]

# 21.3 Print the resulting data frame
print(fc_df)
