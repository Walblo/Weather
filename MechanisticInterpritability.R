# ============================================================================
# LSTM MECHANISTIC INTERPRETABILITY: FEATURE IMPORTANCE (iml)
# ============================================================================

library(iml)

# --- 1. Choose which LSTM to analyze ----------------------------------------
# If your LSTM object has a different name (e.g. model1), change this:
lstm_model <- model

# --- 2. Rebuild feature names in the same order used in create_sequences ----
#   features <- cbind(weather_vars, season_dummies, doy_sin, doy_cos)
feature_names <- c(
  weather_cols,                               # all weather vars, all locations
  paste0("season_", levels(train$season)),    # season_winter, season_spring, ...
  "doy_sin", "doy_cos"
)

# Last timestep of each training sequence as tabular input for iml
X <- as.data.frame(train_seq$X[, lookback, ])
colnames(X) <- feature_names
Y <- train_seq$y[, 1]   # Grand Island temp target

# --- 3. Prediction wrapper (ONE value per row: temperature) -----------------
predict_lstm <- function(m, newdata) {
  arr <- array(
    as.matrix(newdata),
    dim = c(nrow(newdata), lookback, n_features)
  )
  preds <- m$predict(arr)   # N x 2 matrix: [,1]=temp, [,2]=precip
  as.numeric(preds[, 1])    # return temperature only
}

# --- 4. Build iml Predictor --------------------------------------------------
predictor <- Predictor$new(
  model = lstm_model,
  data  = X,
  y     = Y,
  predict.function = predict_lstm
)

# --- 5. Permutation feature importance (RMSE-based) -------------------------
imp <- FeatureImp$new(
  predictor,
  loss = "rmse"
)

print(imp)
plot(imp)


