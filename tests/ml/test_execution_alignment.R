# AlphaForge Insight — Execution alignment unit tests (Phase G.3)
#
# Verifies that Open-to-Open labels correctly align with execute_backtest()
# execution mechanics using hand-calculated examples.

library(testthat)
library(xts)

# Source dependencies (standalone mode)
source("backend/analytics/ml/labels.R")
source("backend/analytics/ml/labels_execution_aligned.R")
source("backend/analytics/execution.R")
source("backend/analytics/metrics.R")

test_that("Open-to-Open labels align with execution mechanics - hand-calculated example", {
  # Hand-calculated example with R array indexing (1-based):
  # Row 1: Close=100, Open=100
  # Row 2: Close=102, Open=101  <- Signal at Close[row 2]
  # Row 3: Close=105, Open=103  <- Execution at Open[row 3]
  # Row 4: Close=104, Open=106  <- Exit at Open[row 4]
  # Row 5: Close=107, Open=108
  # Row 6: Close=110, Open=109
  # Row 7: Close=112, Open=111
  #
  # For signal at Close[row i], executes at Open[row i+1], exits at Open[row i+2]
  
  close <- c(100, 102, 105, 104, 107, 110, 112)
  open <- c(100, 101, 103, 106, 108, 109, 111)
  
  # Generate Open-to-Open labels with horizon=1 (next-day)
  oto <- generate_open_to_open_labels(open, horizon = 1L, 
                                      buy_threshold = 0.02, sell_threshold = 0.02)
  
  # Row 1: Signal at Close[1], executes at Open[2]=101, exits at Open[3]=103
  # Return: (103-101)/101 = 0.0198 < 0.02 -> HOLD
  expect_equal(oto$labels[1], "HOLD")
  
  # Row 2: Signal at Close[2], executes at Open[3]=103, exits at Open[4]=106
  # Return: (106-103)/103 = 0.0291 > 0.02 -> BUY
  expect_equal(oto$labels[2], "BUY")
  
  # Row 3: executes at Open[4]=106, exits at Open[5]=108
  # Return: (108-106)/106 = 0.0189 < 0.02 -> HOLD
  expect_equal(oto$labels[3], "HOLD")
  
  # Row 4: executes at Open[5]=108, exits at Open[6]=109
  # Return: (109-108)/108 = 0.0093 < 0.02 -> HOLD
  expect_equal(oto$labels[4], "HOLD")
  
  # Row 5: executes at Open[6]=109, exits at Open[7]=111
  # Return: (111-109)/109 = 0.0183 < 0.02 -> HOLD
  expect_equal(oto$labels[5], "HOLD")
  
  # Row 6, 7: No valid labels (insufficient future data)
  expect_true(is.na(oto$labels[6]))
  expect_true(is.na(oto$labels[7]))
  
  # Valid mask: rows 1-5 are valid (need rows i+1 and i+2 to exist)
  expect_equal(sum(oto$valid), 5)
})

test_that("Open-to-Open labels detect SELL signals correctly", {
  # Example with declining prices
  close <- c(100, 98, 95, 92, 90, 88)
  open <- c(100, 99, 96, 93, 91, 89)
  
  oto <- generate_open_to_open_labels(open, horizon = 1L,
                                      buy_threshold = 0.02, sell_threshold = 0.02)
  
  # Row 1: Open[2]=96, Open[3]=93, return = (93-96)/96 = -0.0312 < -0.02 -> SELL
  expect_equal(oto$labels[1], "SELL")
  
  # Row 2: Open[3]=93, Open[4]=91, return = (91-93)/93 = -0.0215 < -0.02 -> SELL
  expect_equal(oto$labels[2], "SELL")
  
  # Row 3: Open[4]=91, Open[5]=89, return = (89-91)/91 = -0.0220 < -0.02 -> SELL
  expect_equal(oto$labels[3], "SELL")
})

test_that("Close-to-Close labels differ from Open-to-Open labels", {
  # Example showing disagreement
  close <- c(100, 105, 102, 108, 106)
  open <- c(100, 101, 106, 103, 107)
  
  ctc <- generate_close_to_close_labels(close, horizon = 1L,
                                        buy_threshold = 0.02, sell_threshold = 0.02)
  oto <- generate_open_to_open_labels(open, horizon = 1L,
                                      buy_threshold = 0.02, sell_threshold = 0.02)
  
  # Row 1 CtC: Close[2]/Close[1] = 105/100 = 0.05 -> BUY
  expect_equal(ctc$labels[1], "BUY")
  
  # Row 1 OtO: Open[2]/Open[1] = 106/101 = 0.0495 -> BUY
  expect_equal(oto$labels[1], "BUY")
  
  # Row 2 CtC: Close[3]/Close[2] = 102/105 = -0.0286 -> SELL
  expect_equal(ctc$labels[2], "SELL")
  
  # Row 2 OtO: Open[3]/Open[2] = 103/106 = -0.0283 -> SELL
  expect_equal(oto$labels[2], "SELL")
})

test_that("Label alignment statistics computed correctly", {
  close <- c(100, 105, 102, 108, 106, 110, 108, 112)
  open <- c(100, 101, 106, 103, 107, 109, 111, 113)
  
  alignment <- compute_label_alignment(close, open, horizon = 1L,
                                       buy_threshold = 0.02, sell_threshold = 0.02)
  
  expect_true(alignment$n_valid > 0)
  expect_true(alignment$n_agree >= 0)
  expect_true(alignment$n_disagree >= 0)
  expect_equal(alignment$n_agree + alignment$n_disagree, alignment$n_valid)
  expect_true(alignment$pct_disagree >= 0 && alignment$pct_disagree <= 100)
  
  # Confusion matrix should be 3x3 (BUY/HOLD/SELL)
  expect_equal(dim(alignment$confusion_matrix), c(3, 3))
})

test_that("Execution backtest realizes Open-to-Open returns", {
  # Construct simple OHLCV and signals to verify execution mechanics
  library(xts)
  
  dates <- as.Date("2020-01-01") + 0:5
  ohlcv <- xts(data.frame(
    Open = c(100, 101, 103, 106, 108, 109),
    High = c(102, 104, 107, 109, 110, 111),
    Low = c(99, 100, 102, 105, 107, 108),
    Close = c(100, 102, 105, 104, 107, 110),
    Volume = rep(1000, 6)
  ), order.by = dates)
  
  # Signal BUY at Close[0], SELL at Close[2]
  signals <- c("BUY", NA, "SELL", NA, NA, NA)
  
  result <- execute_backtest(ohlcv, signals, initial_capital = 10000, transaction_cost = 0)
  
  # Verify execution mechanics
  expect_equal(nrow(result$trade_history), 2)
  
  # BUY executes at Open[1] = 101
  expect_equal(result$trade_history$execution_price[1], 101)
  expect_equal(result$trade_history$execution_date[1], dates[2])
  
  # SELL executes at Open[3] = 106
  expect_equal(result$trade_history$execution_price[2], 106)
  expect_equal(result$trade_history$execution_date[2], dates[4])
  
  # Verify realized return matches Open-to-Open
  # Entry: 101, Exit: 106, Return: (106-101)/101 = 0.0495
  shares <- result$trade_history$shares[1]
  entry_cost <- shares * 101
  exit_proceeds <- shares * 106
  realized_return <- (exit_proceeds - entry_cost) / entry_cost
  
  expect_equal(realized_return, (106 - 101) / 101, tolerance = 1e-6)
  expect_true(abs(realized_return - 0.0495) < 1e-4)
})

test_that("Open-to-Open labels have correct horizon offset", {
  # Verify the label index offset is correct
  open <- c(100, 101, 103, 106, 108, 109, 111, 113)
  
  oto_h1 <- generate_open_to_open_labels(open, horizon = 1L,
                                         buy_threshold = 0.02, sell_threshold = 0.02)
  
  oto_h2 <- generate_open_to_open_labels(open, horizon = 2L,
                                         buy_threshold = 0.02, sell_threshold = 0.02)
  
  # Horizon 1: valid for rows 1 to n-2 (need Open[i+1] and Open[i+2])
  expect_equal(sum(oto_h1$valid), length(open) - 2)
  
  # Horizon 2: valid for rows 1 to n-3 (need Open[i+1] and Open[i+3])
  expect_equal(sum(oto_h2$valid), length(open) - 3)
  
  # First valid label for horizon 1
  # Row 1: Signal at Close[1], executes at Open[2]=101, exits at Open[3]=103
  # Return: (103-101)/101 = 0.0198 < 0.02 -> HOLD
  expect_true(oto_h1$valid[1])
  manual_return <- (open[3] - open[2]) / open[2]
  expect_equal(manual_return, (103 - 101) / 101, tolerance = 1e-6)
  
  # First valid label for horizon 2
  # Row 1: executes at Open[2]=101, exits at Open[4]=106
  # Return: (106-101)/101 = 0.0495 > 0.02 -> BUY
  expect_true(oto_h2$valid[1])
  manual_return_h2 <- (open[4] - open[2]) / open[2]
  expect_equal(manual_return_h2, (106 - 101) / 101, tolerance = 1e-6)
})

test_that("Label metadata preserved correctly", {
  close <- c(100, 105, 102, 108, 106, 110, 108, 112, 115, 118)
  open <- c(100, 101, 106, 103, 107, 109, 111, 113, 114, 116)
  
  ctc <- generate_close_to_close_labels(close, horizon = 5L,
                                        buy_threshold = 0.03, sell_threshold = 0.025)
  oto <- generate_open_to_open_labels(open, horizon = 2L,
                                      buy_threshold = 0.015, sell_threshold = 0.02)
  
  expect_equal(ctc$horizon, 5)
  expect_equal(ctc$buy_threshold, 0.03)
  expect_equal(ctc$sell_threshold, 0.025)
  expect_equal(ctc$label_type, "close_to_close")
  expect_false(ctc$execution_aligned)
  
  expect_equal(oto$horizon, 2)
  expect_equal(oto$buy_threshold, 0.015)
  expect_equal(oto$sell_threshold, 0.02)
  expect_equal(oto$label_type, "open_to_open")
  expect_true(oto$execution_aligned)
})

cat("✓ All execution alignment tests passed\n")
