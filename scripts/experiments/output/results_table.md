# AlphaForge Insight — Research Experiment Results

- **Data source:** Yahoo Finance via quantmod (src='yahoo')
- **Date range:** 2015-01-01 to 2025-12-31 (n observations per ticker recorded)
- **Initial capital:** $10000.00
- **Transaction cost:** 0%
- **Risk-free rate:** 2.00%
- **Execution timestamp (UTC):** 2026-09-12T16:44:06.317Z
- **Implementation commit:** ad61da1e3c05b626d02261f7f6c3f59aea6a9d15
- **Engine versions:** R 4.6.1; quantmod 0.4.29; TTR 0.24.4; xts 0.14.3
- **Execution convention:** signal generated at the close of t is executed at the OPEN of t+1 (no look-ahead bias); signals on the final candle are never executed; long-only single asset; completed BUY->SELL round trips count as trades; open final position is marked to market but not counted as completed

## 10.1 Per-Asset Metrics by Strategy

| Asset | Strategy | Total Return (%) | CAGR (%) | Sharpe | Sortino | MaxDD (%) | Win Rate (%) | Volatility (%) | Trades |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| AAPL | MA Cross (50/200) | 337.19 | 14.35 | 0.61 | 0.87 | 41.93 | 60.00 | 23.31 | 5 |
| AAPL | RSI (14, 30/70) | 198.05 | 10.44 | 0.49 | 0.73 | 30.06 | 91.67 | 20.68 | 12 |
| AAPL | Buy & Hold | 907.77 | 23.38 | 0.81 | 1.18 | 38.71 | — | 28.84 | 1 |
| MSFT | MA Cross (50/200) | 513.30 | 17.93 | 0.78 | 1.15 | 28.13 | 60.00 | 21.58 | 5 |
| MSFT | RSI (14, 30/70) | 172.63 | 9.55 | 0.55 | 0.88 | 31.93 | 85.71 | 15.08 | 7 |
| MSFT | Buy & Hold | 948.39 | 23.82 | 0.86 | 1.27 | 37.54 | — | 26.88 | 1 |
| GOOGL | MA Cross (50/200) | 440.42 | 16.58 | 0.71 | 1.02 | 31.05 | 80.00 | 22.22 | 5 |
| GOOGL | RSI (14, 30/70) | 123.45 | 7.58 | 0.37 | 0.54 | 43.42 | 87.50 | 19.43 | 8 |
| GOOGL | Buy & Hold | 1089.60 | 25.25 | 0.86 | 1.27 | 44.31 | — | 28.84 | 1 |
| AMZN | MA Cross (50/200) | 419.94 | 16.17 | 0.66 | 0.97 | 38.83 | 60.00 | 24.45 | 5 |
| AMZN | RSI (14, 30/70) | 198.67 | 10.46 | 0.47 | 0.70 | 44.49 | 90.00 | 22.31 | 10 |
| AMZN | Buy & Hold | 1413.84 | 28.03 | 0.86 | 1.29 | 56.14 | — | 32.93 | 1 |
| TSLA | MA Cross (50/200) | 592.48 | 19.24 | 0.57 | 0.87 | 64.05 | 30.00 | 45.30 | 10 |
| TSLA | RSI (14, 30/70) | 357.33 | 14.82 | 0.50 | 0.77 | 52.57 | 81.82 | 38.25 | 11 |
| TSLA | Buy & Hold | 3076.49 | 36.95 | 0.80 | 1.21 | 73.63 | — | 57.75 | 1 |

## 10.2 Aggregate Statistics Across Assets (per Strategy)

| Strategy | Mean Total Return (%) | Median CAGR (%) | Mean Sharpe | Mean MaxDD (%) | Mean Win Rate (%) |
| :--- | ---: | ---: | ---: | ---: | ---: |
| MA Cross (50/200) | 460.67 | 16.58 | 0.67 | 40.80 | 58.00 |
| RSI (14, 30/70) | 210.03 | 10.44 | 0.48 | 40.49 | 87.34 |
| Buy & Hold | 1487.21 | 25.25 | 0.84 | 50.07 | — |

