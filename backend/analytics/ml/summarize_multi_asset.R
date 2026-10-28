# Phase G.3: Extract and summarize multi-asset results
library(jsonlite)

results <- readRDS("scripts/experiments/output/multi_asset/multi_asset_results.rds")

cat("===================================================================\n")
cat("Phase G.3: Multi-Asset Results Summary\n")
cat("===================================================================\n\n")

for (key in names(results)) {
  r <- results[[key]]
  if (!r$success) {
    cat(sprintf("%s: FAILED (%s)\n", key, r$error))
    next
  }
  
  cat(sprintf("\n--- %s (%s) ---\n", r$ticker, r$label_type))
  cat(sprintf("Folds: %d | Observations: %d\n", r$n_folds, r$n_observations))
  
  # Fold-level metrics
  strategies <- c("buy_hold", "ma_cross", "fixed_exposure", "random_forest")
  strat_labels <- c("Buy&Hold", "MA Cross", "Fixed 50%", "RF")
  
  cat("\n  Per-fold Total Return (%):\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    vals <- sapply(r$fold_results, function(f) {
      if (!is.null(f[[s]])) f[[s]]$total_return * 100 else NA_real_
    })
    cat(sprintf("    %s: %s  | mean=%.1f%%\n", 
                strat_labels[si],
                paste(sprintf("%.1f", vals), collapse=", "),
                mean(vals, na.rm=TRUE)))
  }
  
  cat("\n  Per-fold Sharpe:\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    vals <- sapply(r$fold_results, function(f) {
      if (!is.null(f[[s]])) f[[s]]$sharpe else NA_real_
    })
    cat(sprintf("    %s: %s  | mean=%.3f\n",
                strat_labels[si],
                paste(sprintf("%.3f", vals), collapse=", "),
                mean(vals, na.rm=TRUE)))
  }
  
  cat("\n  Per-fold Max Drawdown (%):\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    vals <- sapply(r$fold_results, function(f) {
      if (!is.null(f[[s]])) f[[s]]$maximum_drawdown * 100 else NA_real_
    })
    cat(sprintf("    %s: %s  | mean=%.1f%%\n",
                strat_labels[si],
                paste(sprintf("%.1f", vals), collapse=", "),
                mean(vals, na.rm=TRUE)))
  }
  
  # RF classification metrics
  cat("\n  RF Classification:\n")
  for (fi in seq_along(r$fold_results)) {
    f <- r$fold_results[[fi]]
    if (!is.null(f$rf_classification)) {
      cat(sprintf("    Fold %d: acc=%.3f, train=%d, test=%d\n",
                  fi, f$rf_classification$accuracy,
                  f$rf_classification$n_train, f$rf_classification$n_test))
    }
  }
  
  # Bootstrap CIs
  cat("\n  Bootstrap 95% CI (Total Return):\n")
  agg <- r$aggregated
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    if (!is.null(agg[[s]]$total_return)) {
      ci <- agg[[s]]$total_return
      cat(sprintf("    %s: mean=%.1f%%, CI=[%.1f%%, %.1f%%], SE=%.3f\n",
                  strat_labels[si],
                  ci$mean * 100, ci$ci_lower * 100, ci$ci_upper * 100, ci$se))
    }
  }
  
  cat("\n  Bootstrap 95% CI (Sharpe):\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    if (!is.null(agg[[s]]$sharpe)) {
      ci <- agg[[s]]$sharpe
      cat(sprintf("    %s: mean=%.3f, CI=[%.3f, %.3f], SE=%.3f\n",
                  strat_labels[si],
                  ci$mean, ci$ci_lower, ci$ci_upper, ci$se))
    }
  }
}

# Cross-asset aggregate
cat("\n\n===================================================================\n")
cat("Cross-Asset Aggregate Summary\n")
cat("===================================================================\n")

for (label_type in c("close_to_close", "open_to_open")) {
  cat(sprintf("\n--- Label type: %s ---\n", label_type))
  
  tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
  strategies <- c("buy_hold", "ma_cross", "fixed_exposure", "random_forest")
  strat_labels <- c("Buy&Hold", "MA Cross", "Fixed 50%", "RF")
  
  cat("\n  Mean Return across assets (%):\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    asset_means <- sapply(tickers, function(t) {
      key <- paste(t, label_type, sep = "_")
      r <- results[[key]]
      if (!is.null(r) && r$success) {
        r$aggregated[[s]]$total_return$mean * 100
      } else NA_real_
    })
    cat(sprintf("    %s: %s  | GRAND MEAN=%.1f%%\n",
                strat_labels[si],
                paste(sprintf("%.1f", asset_means), collapse=", "),
                mean(asset_means, na.rm=TRUE)))
  }
  
  cat("\n  Mean Sharpe across assets:\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    asset_means <- sapply(tickers, function(t) {
      key <- paste(t, label_type, sep = "_")
      r <- results[[key]]
      if (!is.null(r) && r$success) {
        r$aggregated[[s]]$sharpe$mean
      } else NA_real_
    })
    cat(sprintf("    %s: %s  | GRAND MEAN=%.3f\n",
                strat_labels[si],
                paste(sprintf("%.3f", asset_means), collapse=", "),
                mean(asset_means, na.rm=TRUE)))
  }
  
  cat("\n  Mean MaxDD across assets (%):\n")
  for (si in seq_along(strategies)) {
    s <- strategies[si]
    asset_means <- sapply(tickers, function(t) {
      key <- paste(t, label_type, sep = "_")
      r <- results[[key]]
      if (!is.null(r) && r$success) {
        r$aggregated[[s]]$maximum_drawdown$mean * 100
      } else NA_real_
    })
    cat(sprintf("    %s: %s  | GRAND MEAN=%.1f%%\n",
                strat_labels[si],
                paste(sprintf("%.1f", asset_means), collapse=", "),
                mean(asset_means, na.rm=TRUE)))
  }
}
