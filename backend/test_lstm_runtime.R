library(reticulate)
use_python("C:/Users/Lenovo/Documents/.virtualenvs/r-keras/Scripts/python.exe", required = TRUE)

library(keras)
library(tensorflow)

source("analytics/ml/models.R")
source("analytics/ml/lstm.R")

set.seed(42)
X <- matrix(rnorm(400 * 5), nrow = 400, ncol = 5)
y <- rep(c("BUY", "HOLD", "SELL"), length.out = 400)

cat("\n--- Starting real AlphaForge LSTM training test ---\n")

model <- train_lstm_model(
  X_train_scaled = X,
  y_train = y,
  hyperparams = list(
    sequence_length = 20,
    units = 8,
    learning_rate = 0.001,
    epochs = 1,
    batch_size = 32,
    dropout = 0.2
  )
)

cat("\n--- Training completed ---\n")

pred <- model$predict(X)

cat("Prediction dimensions:", paste(dim(pred), collapse = " x "), "\n")
cat("Predictions finite:", all(is.finite(pred)), "\n")
cat("Probability rows sum to 1:", all(abs(rowSums(pred) - 1) < 1e-5), "\n")

stopifnot(
  identical(dim(pred), c(400L, 3L)),
  all(is.finite(pred)),
  all(abs(rowSums(pred) - 1) < 1e-5)
)

cat("\nLSTM INTEGRATION TEST PASSED\n")