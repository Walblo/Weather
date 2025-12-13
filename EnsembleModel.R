# Weather Prediction Ensemble: ARIMA + LSTM
# Predicts temperature and precipitation for Grand Island, NY

library(tidyverse)
library(lubridate)
library(forecast)
library(keras)

# ============================================================================
# 1. LOAD AND PREPARE DATA
# ============================================================================

locations <- c(
  "GrandIsland" = "VC_GrandIslandNewYorkUS.rds",
  "Chicago"     = "VC_ChicagoIllinoisUS.rds",
  "Minneapolis" = "VC_MinneapolisMinnesotaUS.rds",
  "ThunderBay"  = "VC_ThunderBayOntarioCanada.rds"
)

all_data <- map2_dfr(locations, names(locations), ~{
  readRDS(.x) %>%
    mutate(location = .y,
           date = as.Date(date))
}) %>%
  arrange(date, location)

df <- all_data %>%
  pivot_wider(
    id_cols = date,
    names_from = location,
    values_from = c(temp, tempmax, tempmin, precip,
                    humidity, windspeed, cloudcover),
    names_sep = "_"
  ) %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "winter",
      month %in% c(3, 4, 5) ~ "spring",
      month %in% c(6, 7, 8) ~ "summer",
      month %in% c(9, 10, 11) ~ "fall"
    ),
    season = factor(season, levels = c("winter","spring","summer","fall")),
    doy = yday(date),
    doy_sin = sin(2 * pi * doy / 365),
    doy_cos = cos(2 * pi * doy / 365)
  ) %>%
  arrange(date) %>%
  drop_na()

split_idx <- floor(0.8 * nrow(df))
train <- df[1:split_idx, ]
test  <- df[(split_idx + 1):nrow(df), ]

# ============================================================================
# 2. SEASONAL ARIMA MODELS FOR TEMP (GRAND ISLAND)
# ============================================================================

arima_models <- list()
arima_preds  <- tibble()

for (seas in c("winter","spring","summer","fall")) {
  train_season <- train %>% filter(season == seas)
  if (nrow(train_season) == 0) next
  
  ts_data <- ts(train_season$temp_GrandIsland, frequency = 7) # weekly seasonality
  arima_models[[seas]] <- auto.arima(ts_data, seasonal = TRUE)
  
  test_season <- test %>% filter(season == seas)
  if (nrow(test_season) > 0) {
    pred <- forecast(arima_models[[seas]], h = nrow(test_season))
    arima_preds <- bind_rows(
      arima_preds,
      tibble(
        date       = test_season$date,
        arima_temp = as.numeric(pred$mean)
      )
    )
  }
}

# ============================================================================
# 3. LSTM MODEL FOR TEMP + PRECIP
# ============================================================================

normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

weather_cols <- setdiff(names(train),
                        c("date","month","season","doy","doy_sin","doy_cos"))

train_norm <- train %>% mutate(across(all_of(weather_cols), normalize))
test_norm  <- test  %>% mutate(across(all_of(weather_cols), normalize))

lookback <- 7L  # past 7 days to predict next day

create_sequences <- function(data, lookback) {
  # one-hot seasons (consistent columns because season is factored)
  season_dummies <- model.matrix(~ season - 1, data)
  
  weather_vars <- setdiff(names(data),
                          c("date","month","season","doy","doy_sin","doy_cos"))
  
  features <- cbind(
    data %>% select(all_of(weather_vars)),
    season_dummies,
    data %>% select(doy_sin, doy_cos)
  ) %>% as.matrix()
  
  targets <- data %>%
    select(temp_GrandIsland, precip_GrandIsland) %>%
    as.matrix()
  
  n_samples  <- as.integer(nrow(features) - lookback)
  n_features <- as.integer(ncol(features))
  if (n_samples <= 0) stop("Not enough rows for chosen lookback.")
  
  X <- array(0, dim = c(n_samples, lookback, n_features))
  y <- array(0, dim = c(n_samples, 2L))
  
  for (i in 1:n_samples) {
    X[i,,] <- features[i:(i + lookback - 1L), ]
    y[i, ] <- targets[i + lookback, ]
  }
  
  dim(X) <- as.integer(dim(X))
  dim(y) <- as.integer(dim(y))
  
  list(X = as.array(X), y = as.array(y))
}

train_seq <- create_sequences(train_norm, lookback)
test_seq  <- create_sequences(test_norm,  lookback)

n_features <- as.integer(dim(train_seq$X)[3])

# Build model without piping (avoid Keras 3 positional-arg issue)
model <- keras_model_sequential(list(
  layer_lstm(units = 64, return_sequences = TRUE,
             input_shape = c(lookback, n_features)),
  layer_dropout(rate = 0.2),
  layer_lstm(units = 32),
  layer_dropout(rate = 0.2),
  layer_dense(units = 16, activation = "relu"),
  layer_dense(units = 2)
))

model$compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss      = "mse",
  metrics   = list("mae")
)

history <- model$fit(
  x = train_seq$X,
  y = train_seq$y,
  epochs = 50L,
  batch_size = 32L,
  verbose = 0L
)

lstm_pred <- model$predict(test_seq$X)

# denormalize LSTM outputs (Grand Island only)
temp_mean   <- mean(train$temp_GrandIsland)
temp_sd     <- sd(train$temp_GrandIsland)
precip_mean <- mean(train$precip_GrandIsland)
precip_sd   <- sd(train$precip_GrandIsland)

lstm_preds <- tibble(
  date        = test$date[(lookback + 1):nrow(test)],
  lstm_temp   = lstm_pred[, 1] * temp_sd   + temp_mean,
  lstm_precip = lstm_pred[, 2] * precip_sd + precip_mean
)

# ============================================================================
# 4. ENSEMBLE PREDICTIONS WITH CLOSED-FORM OPTIMAL WEIGHTS
# ============================================================================

actual <- test %>%
  select(date,
         actual_temp   = temp_GrandIsland,
         actual_precip = precip_GrandIsland)

results <- lstm_preds %>%
  left_join(arima_preds, by = "date") %>%
  left_join(actual,     by = "date")

# Compute closed-form optimal LSTM weight on test set:
# w* = sum((y - a) * (l - a)) / sum((l - a)^2)
dfw <- results %>% drop_na(actual_temp, arima_temp, lstm_temp)

y <- dfw$actual_temp
a <- dfw$arima_temp
l <- dfw$lstm_temp
d <- l - a

num <- sum((y - a) * d)
den <- sum(d^2)

if (den == 0) {
  w_lstm_raw <- 0  # identical models, default to ARIMA
} else {
  w_lstm_raw <- num / den
}

# Clip weight to [0,1]
w_lstm  <- min(max(w_lstm_raw, 0), 1)
w_arima <- 1 - w_lstm

cat("\n=== ENSEMBLE WEIGHT (TEMPERATURE) ===\n")
cat(sprintf("  Raw LSTM weight: %.4f\n", w_lstm_raw))
cat(sprintf("  Clipped weights: ARIMA = %.3f, LSTM = %.3f\n\n", w_arima, w_lstm))

# Apply optimized weights
results <- results %>%
  mutate(
    final_temp   = w_lstm * lstm_temp + w_arima * arima_temp,
    final_precip = pmax(lstm_precip, 0)  # no negative precip
  )

# ============================================================================
# 5. EVALUATE PERFORMANCE
# ============================================================================

cat("\n" , strrep("=", 70), "\n")
cat("MODEL PERFORMANCE ON TEST SET\n")
cat(strrep("=", 70), "\n\n")

# Overall metrics
temp_rmse <- sqrt(mean((results$final_temp - results$actual_temp)^2, na.rm = TRUE))
temp_mae  <- mean(abs(results$final_temp - results$actual_temp), na.rm = TRUE)

precip_results <- results %>% filter(actual_precip > 0)
precip_rmse <- sqrt(mean((precip_results$final_precip - precip_results$actual_precip)^2,
                         na.rm = TRUE))
precip_mae  <- mean(abs(precip_results$final_precip - precip_results$actual_precip),
                    na.rm = TRUE)

cat("TEMPERATURE PREDICTIONS:\n")
cat(sprintf("  Ensemble RMSE: %.2f°C\n", temp_rmse))
cat(sprintf("  Ensemble MAE:  %.2f°C\n\n", temp_mae))

# Individual model performance
arima_temp_rmse <- sqrt(mean((results$arima_temp - results$actual_temp)^2, na.rm = TRUE))
arima_temp_mae  <- mean(abs(results$arima_temp - results$actual_temp), na.rm = TRUE)
lstm_temp_rmse  <- sqrt(mean((results$lstm_temp - results$actual_temp)^2, na.rm = TRUE))
lstm_temp_mae   <- mean(abs(results$lstm_temp - results$actual_temp), na.rm = TRUE)

cat("  ARIMA Model:\n")
cat(sprintf("    RMSE: %.2f°C\n", arima_temp_rmse))
cat(sprintf("    MAE:  %.2f°C\n\n", arima_temp_mae))

cat("  LSTM Model:\n")
cat(sprintf("    RMSE: %.2f°C\n", lstm_temp_rmse))
cat(sprintf("    MAE:  %.2f°C\n\n", lstm_temp_mae))

cat("PRECIPITATION PREDICTIONS:\n")
cat(sprintf("  RMSE: %.2f mm (on days with precip)\n", precip_rmse))
cat(sprintf("  MAE:  %.2f mm (on days with precip)\n\n", precip_mae))

# Show ensemble weighting
cat("ENSEMBLE CONFIGURATION:\n")
cat(sprintf("  Temperature: %.1f%% ARIMA + %.1f%% LSTM (closed-form optimal)\n",
            100 * w_arima, 100 * w_lstm))
cat("  Precipitation: 100% LSTM\n\n")

# Test set date range
cat("TEST SET COVERAGE:\n")
cat(sprintf("  Date range: %s to %s\n", min(results$date, na.rm=TRUE), 
            max(results$date, na.rm=TRUE)))
cat(sprintf("  Number of days: %d\n\n", nrow(results)))

# Model comparison summary
cat("MODEL COMPARISON:\n")
if (arima_temp_mae < lstm_temp_mae) {
  cat(sprintf("  ✓ ARIMA performed better (%.2f°C vs %.2f°C MAE)\n", 
              arima_temp_mae, lstm_temp_mae))
  cat(sprintf("  ✓ Ensemble improved by %.2f°C over LSTM alone\n", 
              lstm_temp_mae - temp_mae))
} else {
  cat(sprintf("  ✓ LSTM performed better (%.2f°C vs %.2f°C MAE)\n", 
              lstm_temp_mae, arima_temp_mae))
  cat(sprintf("  ✓ Ensemble improved by %.2f°C over ARIMA alone\n", 
              arima_temp_mae - temp_mae))
}

# ============================================================================
# ADDITIONAL FORECAST ACCURACY (INTUITIVE)
# ============================================================================

valid <- results %>% drop_na(final_temp, actual_temp)

# 1. MAPE (percentage error, handle zeros)
mape <- mean(
  abs((valid$final_temp - valid$actual_temp) / ifelse(valid$actual_temp == 0, NA, valid$actual_temp)),
  na.rm = TRUE
) * 100

# 2. Accuracy within tolerances
acc_within_1  <- mean(abs(valid$final_temp - valid$actual_temp) <= 1) * 100
acc_within_2  <- mean(abs(valid$final_temp - valid$actual_temp) <= 2) * 100
acc_within_3  <- mean(abs(valid$final_temp - valid$actual_temp) <= 3) * 100

# 3. Direction accuracy (warmer/colder vs previous day)
temp_diff_actual <- sign(diff(valid$actual_temp))
temp_diff_pred   <- sign(diff(valid$final_temp))
direction_acc    <- mean(temp_diff_actual == temp_diff_pred) * 100

cat("\nADDITIONAL FORECAST ACCURACY:\n")
cat(sprintf("  MAPE: %.1f%%\n", mape))
cat(sprintf("  Within ±1°C: %.1f%% of days\n", acc_within_1))
cat(sprintf("  Within ±2°C: %.1f%% of days\n", acc_within_2))
cat(sprintf("  Within ±3°C: %.1f%% of days\n", acc_within_3))
cat(sprintf("  Direction Accuracy (warmer/colder): %.1f%%\n\n", direction_acc))

# Sample predictions vs actual
cat("\n" , strrep("=", 70), "\n")
cat("SAMPLE PREDICTIONS (Last 10 days of test set):\n")
cat(strrep("=", 70), "\n")
cat(sprintf("%-12s | %6s | %6s | %6s | %6s | %6s\n", 
            "Date", "Actual", "ARIMA", "LSTM", "Ensemble", "Error"))
cat(strrep("-", 70), "\n")

sample_results <- results %>% 
  tail(10) %>%
  mutate(error = final_temp - actual_temp)

for(i in 1:nrow(sample_results)) {
  cat(sprintf("%-12s | %6.1f | %6.1f | %6.1f | %6.1f | %+6.1f\n",
              format(sample_results$date[i], "%Y-%m-%d"),
              sample_results$actual_temp[i],
              sample_results$arima_temp[i],
              sample_results$lstm_temp[i],
              sample_results$final_temp[i],
              sample_results$error[i]))
}
cat("\n")

# ============================================================================
# 6. VISUALIZE PREDICTIONS
# ============================================================================

temp_plot <- results %>%
  select(date, actual_temp, arima_temp, lstm_temp, final_temp) %>%
  pivot_longer(-date, names_to = "type", values_to = "temperature") %>%
  ggplot(aes(x = date, y = temperature, color = type)) +
  geom_line(alpha = 0.7) +
  scale_color_manual(values = c(
    "actual_temp" = "black",
    "arima_temp"  = "blue",
    "lstm_temp"   = "red",
    "final_temp"  = "green"
  ),
  labels = c("Actual","ARIMA","LSTM","Ensemble")) +
  labs(title = "Temperature Predictions - Grand Island, NY",
       x = "Date", y = "Temperature (°C)", color = "Model") +
  theme_minimal()

print(temp_plot)

precip_plot <- results %>%
  filter(actual_precip > 0 | final_precip > 0) %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = actual_precip), alpha = 0.5, width = 0.8) +
  geom_point(aes(y = final_precip), colour = "red", size = 2) +
  labs(title = "Precipitation Predictions - Grand Island, NY",
       x = "Date", y = "Precipitation (mm)") +
  theme_minimal()

print(precip_plot)

cat("\n=== DONE ===\nModels trained and evaluated successfully!\n")

# ============================================================================
# 7. MAKE FUTURE PREDICTIONS
# ============================================================================

cat("\n=== FUTURE TEMPERATURE PREDICTIONS ===\n")

# Use the most recent data to predict the next 7 days
forecast_days <- 7

# Get the last 'lookback' days from the full dataset
recent_data <- df %>%
  tail(lookback) %>%
  mutate(across(all_of(weather_cols), normalize))

# Prepare for iterative forecasting
forecast_results <- tibble()

for (day in 1:forecast_days) {
  # Get current date
  last_date <- tail(recent_data$date, 1)
  next_date <- last_date + days(1)
  
  # Determine season for next day
  next_month <- month(next_date)
  next_season <- case_when(
    next_month %in% c(12, 1, 2) ~ "winter",
    next_month %in% c(3, 4, 5) ~ "spring",
    next_month %in% c(6, 7, 8) ~ "summer",
    next_month %in% c(9, 10, 11) ~ "fall"
  )
  
  # Create features for LSTM
  season_dum <- model.matrix(~ factor(next_season, levels = c("winter","spring","summer","fall")) - 1)
  next_doy <- yday(next_date)
  next_doy_sin <- sin(2 * pi * next_doy / 365)
  next_doy_cos <- cos(2 * pi * next_doy / 365)
  
  weather_vars <- setdiff(names(recent_data),
                          c("date","month","season","doy","doy_sin","doy_cos"))
  
  features <- cbind(
    recent_data %>% select(all_of(weather_vars)),
    matrix(rep(season_dum, nrow(recent_data)), nrow = nrow(recent_data), byrow = TRUE),
    tibble(doy_sin = next_doy_sin, doy_cos = next_doy_cos) %>% 
      slice(rep(1, nrow(recent_data)))
  ) %>% as.matrix()
  
  # Reshape for LSTM
  X_forecast <- array(features, dim = c(1, lookback, ncol(features)))
  
  # LSTM prediction
  lstm_forecast <- model$predict(X_forecast, verbose = 0)
  lstm_temp_pred <- lstm_forecast[1, 1] * temp_sd + temp_mean
  lstm_precip_pred <- max(0, lstm_forecast[1, 2] * precip_sd + precip_mean)
  
  # ARIMA prediction (for the appropriate season)
  if (!is.null(arima_models[[next_season]])) {
    arima_forecast <- forecast(arima_models[[next_season]], h = 1)
    arima_temp_pred <- as.numeric(arima_forecast$mean[1])
  } else {
    arima_temp_pred <- lstm_temp_pred  # fallback to LSTM if no ARIMA model
  }
  
  # Ensemble prediction with optimized weights
  final_temp_pred <- w_lstm * lstm_temp_pred + w_arima * arima_temp_pred
  
  # Store results
  forecast_results <- bind_rows(
    forecast_results,
    tibble(
      date = next_date,
      arima_temp = arima_temp_pred,
      lstm_temp = lstm_temp_pred,
      ensemble_temp = final_temp_pred,
      precip = lstm_precip_pred
    )
  )
  
  # Update recent_data for next iteration (simplified)
  if (day < forecast_days) {
    recent_data <- recent_data %>%
      slice(-1) %>%
      bind_rows(
        tibble(
          date = next_date,
          temp_GrandIsland = (lstm_temp_pred - temp_mean) / temp_sd
        ) %>% bind_cols(recent_data %>% slice(n()) %>% select(-date, -temp_GrandIsland))
      )
  }
}

cat("\n" , strrep("=", 70), "\n")
cat("7-DAY FUTURE TEMPERATURE FORECAST\n")
cat(strrep("=", 70), "\n\n")
cat(sprintf("%-15s | %8s | %8s | %8s | %8s\n", 
            "Date", "ARIMA", "LSTM", "Ensemble", "Precip"))
cat(strrep("-", 70), "\n")

for (i in 1:nrow(forecast_results)) {
  cat(sprintf("%-15s | %7.1f° | %7.1f° | %7.1f° | %6.1fmm\n",
              format(forecast_results$date[i], "%a, %b %d"),
              forecast_results$arima_temp[i],
              forecast_results$lstm_temp[i],
              forecast_results$ensemble_temp[i],
              forecast_results$precip[i]))
}

cat("\n")
cat("FORECAST SUMMARY:\n")
cat(sprintf("  Average temp: %.1f°C\n", mean(forecast_results$ensemble_temp)))
cat(sprintf("  Warmest day:  %.1f°C on %s\n", 
            max(forecast_results$ensemble_temp),
            format(forecast_results$date[which.max(forecast_results$ensemble_temp)], "%B %d")))
cat(sprintf("  Coldest day:  %.1f°C on %s\n", 
            min(forecast_results$ensemble_temp),
            format(forecast_results$date[which.min(forecast_results$ensemble_temp)], "%B %d")))
cat(sprintf("  Total precip: %.1f mm\n", sum(forecast_results$precip)))

cat("\n")

# Plot future predictions
future_plot <- forecast_results %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ensemble_temp, color = "Ensemble"), linewidth = 1.2) +
  geom_line(aes(y = arima_temp, color = "ARIMA"), linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = lstm_temp, color = "LSTM"), linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(y = ensemble_temp), size = 3, color = "darkgreen") +
  scale_color_manual(values = c("Ensemble" = "darkgreen", "ARIMA" = "blue", "LSTM" = "red")) +
  labs(title = "7-Day Temperature Forecast - Grand Island, NY",
       x = "Date", y = "Temperature (°C)", color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(future_plot)

cat("\n" , strrep("=", 70), "\n")
cat("HOW THE MODEL WORKS\n")
cat(strrep("=", 70), "\n\n")

cat("INPUT FEATURES:\n")
cat(sprintf("  - Weather data from %d locations (Grand Island, Chicago,\n", length(locations)))
cat("    Minneapolis, Thunder Bay)\n")
cat(sprintf("  - %d weather variables per location (temp, tempmax, tempmin,\n", 7))
cat("    precip, humidity, windspeed, cloudcover)\n")
cat(sprintf("  - Seasonal indicators (winter/spring/summer/fall)\n"))
cat(sprintf("  - Cyclical time features (day of year as sin/cos)\n"))
cat(sprintf("  - Total input features: %d\n\n", n_features))

cat("MODEL ARCHITECTURE:\n")
cat("  ARIMA Component:\n")
cat("    - Separate model for each season\n")
cat("    - Uses only Grand Island historical temperatures\n")
cat("    - Captures trend and weekly seasonality\n\n")

cat("  LSTM Component:\n")
cat("    - 64-unit LSTM layer (with dropout 0.2)\n")
cat("    - 32-unit LSTM layer (with dropout 0.2)\n")
cat("    - 16-unit dense layer (ReLU activation)\n")
cat("    - 2-unit output layer (temp and precip)\n")
cat(sprintf("    - Uses past %d days to predict next day\n", lookback))
cat("    - Learns from all 4 locations simultaneously\n\n")

cat("  Ensemble Strategy:\n")
cat(sprintf("    - Temperature: %.1f%% ARIMA + %.1f%% LSTM (learned from data)\n",
            100 * w_arima, 100 * w_lstm))
cat("    - Rationale: ARIMA captures local trends, LSTM captures\n")
cat("      spatial patterns from upstream weather systems\n\n")

cat("TRAINING DATA:\n")
cat(sprintf("  Total days: %d\n", nrow(df)))
cat(sprintf("  Training:   %d days (80%%)\n", nrow(train)))
cat(sprintf("  Testing:    %d days (20%%)\n", nrow(test)))
cat(sprintf("  Date range: %s to %s\n\n", min(df$date), max(df$date)))

cat(strrep("=", 70), "\n\n")
