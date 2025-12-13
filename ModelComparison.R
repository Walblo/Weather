# Comprehensive Weather Model Comparison
# Compares ARIMA+LSTM Ensemble, Pure LSTM, Pure CNN
# With and without seasonal splitting

library(tidyverse)
library(lubridate)
library(forecast)
library(keras)

# ============================================================================
# 1. LOAD AND PREPARE DATA (SAME AS BEFORE)
# ============================================================================

locations <- c(
  "GrandIsland" = "VC_GrandIslandNewYorkUS.rds",
  "Chicago"     = "VC_ChicagoIllinoisUS.rds",
  "Minneapolis" = "VC_MinneapolisMinnesotaUS.rds",
  "ThunderBay"  = "VC_ThunderBayOntarioCanada.rds"
)

all_data <- map2_dfr(locations, names(locations), ~{
  readRDS(.x) %>%
    mutate(location = .y, date = as.Date(date))
}) %>% arrange(date, location)

df <- all_data %>%
  pivot_wider(
    id_cols = date,
    names_from = location,
    values_from = c(temp, tempmax, tempmin, precip, humidity, windspeed, cloudcover),
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

normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
weather_cols <- setdiff(names(train), c("date","month","season","doy","doy_sin","doy_cos"))

train_norm <- train %>% mutate(across(all_of(weather_cols), normalize))
test_norm  <- test  %>% mutate(across(all_of(weather_cols), normalize))

lookback <- 7L

# ============================================================================
# 2. HELPER FUNCTIONS
# ============================================================================

create_sequences <- function(data, lookback, include_season = TRUE) {
  weather_vars <- setdiff(names(data), c("date","month","season","doy","doy_sin","doy_cos"))
  
  if (include_season) {
    season_dummies <- model.matrix(~ season - 1, data)
    features <- cbind(
      data %>% select(all_of(weather_vars)),
      season_dummies,
      data %>% select(doy_sin, doy_cos)
    ) %>% as.matrix()
  } else {
    features <- cbind(
      data %>% select(all_of(weather_vars)),
      data %>% select(doy_sin, doy_cos)
    ) %>% as.matrix()
  }
  
  targets <- data %>% select(temp_GrandIsland) %>% as.matrix()
  
  n_samples  <- as.integer(nrow(features) - lookback)
  n_features <- as.integer(ncol(features))
  
  X <- array(0, dim = c(n_samples, lookback, n_features))
  y <- array(0, dim = c(n_samples, 1L))
  
  for (i in 1:n_samples) {
    X[i,,] <- features[i:(i + lookback - 1L), ]
    y[i, ] <- targets[i + lookback, ]
  }
  
  list(X = as.array(X), y = as.array(y))
}

evaluate_model <- function(predictions, actual) {
  valid_idx <- !is.na(predictions) & !is.na(actual)
  pred <- predictions[valid_idx]
  act <- actual[valid_idx]
  
  rmse <- sqrt(mean((pred - act)^2))
  mae <- mean(abs(pred - act))
  mape <- mean(abs((pred - act) / ifelse(act == 0, NA, act)), na.rm = TRUE) * 100
  acc_1 <- mean(abs(pred - act) <= 1) * 100
  acc_2 <- mean(abs(pred - act) <= 2) * 100
  acc_3 <- mean(abs(pred - act) <= 3) * 100
  
  list(rmse = rmse, mae = mae, mape = mape, 
       acc_1 = acc_1, acc_2 = acc_2, acc_3 = acc_3)
}

# ============================================================================
# 3. MODEL 1: ARIMA + LSTM ENSEMBLE (WITH SEASONS)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("TRAINING MODEL 1: ARIMA + LSTM ENSEMBLE (Seasonal)\n")
cat(strrep("=", 70), "\n")

# Train ARIMA
arima_models <- list()
arima_preds  <- tibble()

for (seas in c("winter","spring","summer","fall")) {
  train_season <- train %>% filter(season == seas)
  if (nrow(train_season) == 0) next
  
  ts_data <- ts(train_season$temp_GrandIsland, frequency = 7)
  arima_models[[seas]] <- auto.arima(ts_data, seasonal = TRUE, trace = FALSE)
  
  test_season <- test %>% filter(season == seas)
  if (nrow(test_season) > 0) {
    pred <- forecast(arima_models[[seas]], h = nrow(test_season))
    arima_preds <- bind_rows(arima_preds, 
                             tibble(date = test_season$date, 
                                    arima_temp = as.numeric(pred$mean)))
  }
}

# Train LSTM
train_seq <- create_sequences(train_norm, lookback, include_season = TRUE)
test_seq  <- create_sequences(test_norm,  lookback, include_season = TRUE)

model1 <- keras_model_sequential(list(
  layer_lstm(units = 64, return_sequences = TRUE, 
             input_shape = c(lookback, dim(train_seq$X)[3])),
  layer_dropout(rate = 0.2),
  layer_lstm(units = 32),
  layer_dropout(rate = 0.2),
  layer_dense(units = 16, activation = "relu"),
  layer_dense(units = 1)
))

model1$compile(optimizer = optimizer_adam(learning_rate = 0.001),
               loss = "mse", metrics = list("mae"))

model1$fit(x = train_seq$X, y = train_seq$y, 
           epochs = 50L, batch_size = 32L, verbose = 0L)

lstm_pred1 <- model1$predict(test_seq$X, verbose = 0)
temp_mean <- mean(train$temp_GrandIsland)
temp_sd <- sd(train$temp_GrandIsland)
lstm_temp1 <- lstm_pred1[, 1] * temp_sd + temp_mean

results1 <- tibble(
  date = test$date[(lookback + 1):nrow(test)],
  lstm_temp = lstm_temp1
) %>%
  left_join(arima_preds, by = "date") %>%
  left_join(test %>% select(date, actual = temp_GrandIsland), by = "date")

# Optimal weights
dfw <- results1 %>% drop_na()
y <- dfw$actual
a <- dfw$arima_temp
l <- dfw$lstm_temp
d <- l - a
w_lstm1 <- min(max(sum((y - a) * d) / sum(d^2), 0), 1)

results1 <- results1 %>% 
  mutate(pred = w_lstm1 * lstm_temp + (1 - w_lstm1) * arima_temp)

metrics1 <- evaluate_model(results1$pred, results1$actual)

# ============================================================================
# 4. MODEL 2: PURE LSTM (WITH SEASONS)
# ============================================================================

cat("TRAINING MODEL 2: Pure LSTM (Seasonal)\n")

model2 <- keras_model_sequential(list(
  layer_lstm(units = 64, return_sequences = TRUE, 
             input_shape = c(lookback, dim(train_seq$X)[3])),
  layer_dropout(rate = 0.2),
  layer_lstm(units = 32),
  layer_dropout(rate = 0.2),
  layer_dense(units = 16, activation = "relu"),
  layer_dense(units = 1)
))

model2$compile(optimizer = optimizer_adam(learning_rate = 0.001),
               loss = "mse", metrics = list("mae"))

model2$fit(x = train_seq$X, y = train_seq$y, 
           epochs = 50L, batch_size = 32L, verbose = 0L)

lstm_pred2 <- model2$predict(test_seq$X, verbose = 0)
pred2 <- lstm_pred2[, 1] * temp_sd + temp_mean
metrics2 <- evaluate_model(pred2, results1$actual)

# ============================================================================
# 5. MODEL 3: PURE LSTM (NO SEASONS)
# ============================================================================

cat("TRAINING MODEL 3: Pure LSTM (No Seasons)\n")

train_seq_ns <- create_sequences(train_norm, lookback, include_season = FALSE)
test_seq_ns  <- create_sequences(test_norm,  lookback, include_season = FALSE)

model3 <- keras_model_sequential(list(
  layer_lstm(units = 64, return_sequences = TRUE, 
             input_shape = c(lookback, dim(train_seq_ns$X)[3])),
  layer_dropout(rate = 0.2),
  layer_lstm(units = 32),
  layer_dropout(rate = 0.2),
  layer_dense(units = 16, activation = "relu"),
  layer_dense(units = 1)
))

model3$compile(optimizer = optimizer_adam(learning_rate = 0.001),
               loss = "mse", metrics = list("mae"))

model3$fit(x = train_seq_ns$X, y = train_seq_ns$y, 
           epochs = 50L, batch_size = 32L, verbose = 0L)

lstm_pred3 <- model3$predict(test_seq_ns$X, verbose = 0)
pred3 <- lstm_pred3[, 1] * temp_sd + temp_mean
metrics3 <- evaluate_model(pred3, results1$actual)

# ============================================================================
# 6. MODEL 4: PURE CNN (WITH SEASONS)
# ============================================================================

cat("TRAINING MODEL 4: Pure CNN (Seasonal)\n")

model4 <- keras_model_sequential(list(
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", padding = "same",
                input_shape = c(lookback, dim(train_seq$X)[3])),
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", padding = "same"),
  layer_max_pooling_1d(pool_size = 2),
  layer_conv_1d(filters = 32, kernel_size = 2, activation = "relu", padding = "same"),
  layer_global_average_pooling_1d(),
  layer_dropout(rate = 0.2),
  layer_dense(units = 16, activation = "relu"),
  layer_dense(units = 1)
))

model4$compile(optimizer = optimizer_adam(learning_rate = 0.001),
               loss = "mse", metrics = list("mae"))

model4$fit(x = train_seq$X, y = train_seq$y, 
           epochs = 50L, batch_size = 32L, verbose = 0L)

cnn_pred4 <- model4$predict(test_seq$X, verbose = 0)
pred4 <- cnn_pred4[, 1] * temp_sd + temp_mean
metrics4 <- evaluate_model(pred4, results1$actual)

# ============================================================================
# 7. MODEL 5: PURE CNN (NO SEASONS)
# ============================================================================

cat("TRAINING MODEL 5: Pure CNN (No Seasons)\n")

model5 <- keras_model_sequential(list(
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", padding = "same",
                input_shape = c(lookback, dim(train_seq_ns$X)[3])),
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", padding = "same"),
  layer_max_pooling_1d(pool_size = 2),
  layer_conv_1d(filters = 32, kernel_size = 2, activation = "relu", padding = "same"),
  layer_global_average_pooling_1d(),
  layer_dropout(rate = 0.2),
  layer_dense(units = 16, activation = "relu"),
  layer_dense(units = 1)
))

model5$compile(optimizer = optimizer_adam(learning_rate = 0.001),
               loss = "mse", metrics = list("mae"))

model5$fit(x = train_seq_ns$X, y = train_seq_ns$y, 
           epochs = 50L, batch_size = 32L, verbose = 0L)

cnn_pred5 <- model5$predict(test_seq_ns$X, verbose = 0)
pred5 <- cnn_pred5[, 1] * temp_sd + temp_mean
metrics5 <- evaluate_model(pred5, results1$actual)

# ============================================================================
# 8. DISPLAY COMPARISON TABLE
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("COMPREHENSIVE MODEL COMPARISON - TEMPERATURE PREDICTION\n")
cat(strrep("=", 80), "\n\n")

comparison <- tibble(
  Model = c(
    "ARIMA+LSTM Ensemble (Seasonal)",
    "Pure LSTM (Seasonal)",
    "Pure LSTM (No Seasons)",
    "Pure CNN (Seasonal)",
    "Pure CNN (No Seasons)"
  ),
  RMSE = c(metrics1$rmse, metrics2$rmse, metrics3$rmse, metrics4$rmse, metrics5$rmse),
  MAE = c(metrics1$mae, metrics2$mae, metrics3$mae, metrics4$mae, metrics5$mae),
  MAPE = c(metrics1$mape, metrics2$mape, metrics3$mape, metrics4$mape, metrics5$mape),
  `±1°C` = c(metrics1$acc_1, metrics2$acc_1, metrics3$acc_1, metrics4$acc_1, metrics5$acc_1),
  `±2°C` = c(metrics1$acc_2, metrics2$acc_2, metrics3$acc_2, metrics4$acc_2, metrics5$acc_2),
  `±3°C` = c(metrics1$acc_3, metrics2$acc_3, metrics3$acc_3, metrics4$acc_3, metrics5$acc_3)
)

# Print formatted table
cat(sprintf("%-35s | %6s | %6s | %7s | %6s | %6s | %6s\n",
            "Model", "RMSE", "MAE", "MAPE", "±1°C", "±2°C", "±3°C"))
cat(strrep("-", 80), "\n")

for(i in 1:nrow(comparison)) {
  cat(sprintf("%-35s | %6.2f | %6.2f | %6.1f%% | %5.1f%% | %5.1f%% | %5.1f%%\n",
              comparison$Model[i],
              comparison$RMSE[i],
              comparison$MAE[i],
              comparison$MAPE[i],
              comparison$`±1°C`[i],
              comparison$`±2°C`[i],
              comparison$`±3°C`[i]))
}

cat("\n")

# Find best model
best_mae_idx <- which.min(comparison$MAE)
cat("WINNER (Lowest MAE):", comparison$Model[best_mae_idx], "\n")
cat(sprintf("  MAE: %.2f°C\n", comparison$MAE[best_mae_idx]))
cat(sprintf("  Accuracy within ±2°C: %.1f%%\n\n", comparison$`±2°C`[best_mae_idx]))

# Key insights
cat("KEY INSIGHTS:\n")
cat(sprintf("  • Seasonal splitting impact: %.2f°C MAE improvement (LSTM)\n",
            metrics3$mae - metrics2$mae))
cat(sprintf("  • Ensemble advantage: %.2f°C MAE improvement over pure LSTM\n",
            metrics2$mae - metrics1$mae))
cat(sprintf("  • CNN vs LSTM (seasonal): %.2f°C difference\n",
            abs(metrics4$mae - metrics2$mae)))

cat("\n", strrep("=", 80), "\n\n")

# Visual comparison
comparison_plot <- comparison %>%
  select(Model, MAE, `±2°C`) %>%
  pivot_longer(-Model, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = reorder(Model, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Model Performance Comparison",
       x = "Model", y = "Value",
       subtitle = "Lower MAE is better, Higher ±2°C accuracy is better") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(comparison_plot)