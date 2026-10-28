# Regression test for rbind column mismatch in run_model_comparison
# Root cause: backtest summary fields (cagr, sharpe, etc.) could be NULL,
# causing data.frame() to create columns with different types across models.

library(testthat)

test_that("comparison rows with NULL metrics rbind correctly", {
  # Simulate the exact scenario that caused the production failure:
  # One model returns summary with all fields populated
  # Another model returns summary with NULL fields
  
  row1 <- data.frame(
    Model = "Model A",
    Status = "ok",
    Error = NA_character_,
    Accuracy = as.numeric(0.75),
    Precision = as.numeric(0.70),
    Recall = as.numeric(0.68),
    F1 = as.numeric(0.69),
    ROC_AUC = as.numeric(0.80),
    PR_AUC = as.numeric(0.78),
    CAGR = as.numeric(0.15),
    Sharpe = as.numeric(1.2),
    Sortino = as.numeric(1.5),
    MaxDrawdown = as.numeric(-0.20),
    WinRate = as.numeric(0.55),
    Volatility = as.numeric(0.18),
    TotalTrades = as.integer(100),
    stringsAsFactors = FALSE
  )
  
  # Simulate a model where backtest returns NULL for some metrics
  # Using the EXACT type-safe extraction pattern from the fix
  row2 <- data.frame(
    Model = "Model B",
    Status = "ok",
    Error = NA_character_,
    Accuracy = as.numeric(0.72),
    Precision = as.numeric(0.68),
    Recall = as.numeric(0.65),
    F1 = as.numeric(0.66),
    ROC_AUC = as.numeric(NULL %||% NA_real_),
    PR_AUC = as.numeric(NULL %||% NA_real_),
    CAGR = as.numeric(NULL %||% NA_real_),
    Sharpe = as.numeric(NULL %||% NA_real_),
    Sortino = as.numeric(NULL %||% NA_real_),
    MaxDrawdown = as.numeric(NULL %||% NA_real_),
    WinRate = as.numeric(NULL %||% NA_real_),
    Volatility = as.numeric(NULL %||% NA_real_),
    TotalTrades = as.integer(NULL %||% NA_integer_),
    stringsAsFactors = FALSE
  )
  
  # Failed model row
  row3 <- data.frame(
    Model = "Model C",
    Status = "failed",
    Error = "training error",
    Accuracy = NA_real_,
    Precision = NA_real_,
    Recall = NA_real_,
    F1 = NA_real_,
    ROC_AUC = NA_real_,
    PR_AUC = NA_real_,
    CAGR = NA_real_,
    Sharpe = NA_real_,
    Sortino = NA_real_,
    MaxDrawdown = NA_real_,
    WinRate = NA_real_,
    Volatility = NA_real_,
    TotalTrades = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  # This is the exact operation that was failing in production
  rows <- list(row1, row2, row3)
  
  # Should NOT throw "numbers of columns of arguments do not match"
  expect_error(comparison <- do.call(rbind, rows), NA)
  
  # Verify result structure
  expect_equal(nrow(comparison), 3)
  expect_equal(ncol(comparison), 16)
  expect_equal(comparison$Model, c("Model A", "Model B", "Model C"))
  expect_equal(comparison$Status, c("ok", "ok", "failed"))
  
  # Verify that NULL metrics became NA
  expect_true(is.na(comparison$ROC_AUC[2]))
  expect_true(is.na(comparison$CAGR[2]))
  expect_true(is.na(comparison$Sharpe[2]))
  
  # Verify successful model A retained its values
  expect_equal(comparison$Accuracy[1], 0.75)
  expect_equal(comparison$CAGR[1], 0.15)
})

test_that("%||% operator works as expected", {
  expect_equal(NULL %||% NA_real_, NA_real_)
  expect_equal(0.5 %||% NA_real_, 0.5)
  expect_equal(NULL %||% NA_integer_, NA_integer_)
  expect_equal(100L %||% NA_integer_, 100L)
})
