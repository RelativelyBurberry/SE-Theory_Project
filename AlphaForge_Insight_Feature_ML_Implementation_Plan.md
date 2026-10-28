# AlphaForge Insight — Feature Engineering & ML Implementation Plan

## 0. Scope

This is an **extension of the existing working AlphaForge Insight demo**, not a rewrite.

### Existing system — DO NOT REBUILD

- React.js (Vite) frontend
- Tailwind CSS
- R backend
- Plumber REST API
- Yahoo Finance market-data retrieval
- PostgreSQL via Neon
- JWT authentication
- Strategy Builder
- Existing SMA / RSI strategies
- Existing backtesting engine
- Existing trade simulation
- Existing performance metrics
- Existing results dashboard / persistence

The new implementation adds:

1. Feature Engineering
2. Feature Selection
3. Supervised ML/DL signal generation
4. 12 ML/DL algorithms
5. Hyperparameter tuning
6. Model comparison
7. ML-generated trading signals
8. Top-N stock ranking
9. Integration with the existing backtesting engine
10. ML-specific result visualizations

---

# 1. Target ML Problem

## Objective

Predict a **3-class trading signal**:

```text
BUY
HOLD
SELL
```

The models learn from information available at time `t` and predict the subsequent return over a fixed horizon.

### Example label definition

Using a 5-trading-day horizon:

```text
Future 5-day return > +2%  → BUY
Future 5-day return < -2%  → SELL
Otherwise                  → HOLD
```

The exact threshold and prediction horizon should be configurable.

### Critical rule

Future prices/returns are used **only to construct the training label**.

They must never appear as model input features.

This prevents look-ahead/data leakage.

---

# 2. Feature Engineering

The existing Yahoo Finance OHLCV pipeline remains unchanged.

```text
Yahoo Finance
      ↓
Existing OHLCV data
      ↓
Feature Engineering
      ↓
ML-ready dataset
```

## 2.1 Price / Return Features

Create:

- Daily Return
- Log Return
- 5-Day Return
- 10-Day Return
- 20-Day Return
- Price Change
- Gap %

## 2.2 Trend Features

Using `TTR`:

- SMA 10
- SMA 20
- SMA 50
- SMA 200
- EMA 10
- EMA 20
- EMA 50

Derived features:

- Close / SMA20
- Close / SMA50
- EMA20 / EMA50
- SMA20 / SMA50

## 2.3 Momentum Features

- RSI 14
- MACD
- MACD Signal
- MACD Histogram
- ROC
- Momentum
- Stochastic %K
- Stochastic %D

## 2.4 Volatility Features

- ATR
- Rolling Volatility
- Bollinger Upper Band
- Bollinger Lower Band
- Bollinger Band Width

## 2.5 Volume Features

- Raw Volume
- Volume Change
- Volume / Rolling Average Volume
- OBV

---

# 3. Feature Selection

Do not blindly send every feature into every model.

The feature-selection pipeline should include:

1. Remove features with excessive missing values.
2. Remove constant / near-zero-variance features.
3. Check correlations between numerical features.
4. Remove highly redundant features where appropriate.
5. Use model-based importance where appropriate.
6. Compare feature subsets during model tuning.

Possible R tools:

```r
nearZeroVar()
cor()
findCorrelation()
```

For tree models:

- permutation importance
- Gini/impurity importance

For final analysis:

- feature importance plots
- optional SHAP analysis for selected models

---

# 4. ML/DL Algorithms

The project will implement **12 algorithms**.

## Classical / Statistical ML

### 1. Logistic Regression
Baseline classifier; outputs BUY/HOLD/SELL probabilities.

### 2. Linear Discriminant Analysis (LDA)
Linear statistical classifier.

### 3. Quadratic Discriminant Analysis (QDA)
Non-linear decision boundaries through class-specific covariance.

### 4. K-Nearest Neighbors (KNN)
Distance-based classification.

### 5. Naive Bayes
Probabilistic classification baseline.

## Tree / Ensemble Models

### 6. Decision Tree
Non-linear classification with interpretable splits.

### 7. Random Forest
Tree ensemble with feature importance.

### 8. Support Vector Machine (SVM)
Kernel-based classification.

### 9. Gradient Boosting
Sequential ensemble of weak learners.

### 10. XGBoost
High-performance gradient boosting for tabular financial features.

## Neural / Deep Learning Models

### 11. MLP / Artificial Neural Network
Feed-forward neural network using engineered financial features.

### 12. LSTM
Sequence model using windows of historical observations.

Unlike the other models, LSTM receives sequences:

```text
Previous 20 trading days
          ↓
      LSTM network
          ↓
   BUY / HOLD / SELL
```

---

# 5. Model Pipeline

All models should follow the same high-level workflow.

```text
Historical OHLCV
       ↓
Feature Engineering
       ↓
Label Generation
       ↓
Feature Selection
       ↓
Chronological Split
       ↓
Training Data
       ↓
┌─────────────────────────────┐
│ 12 ML/DL Algorithms         │
└─────────────────────────────┘
       ↓
Predictions + Probabilities
       ↓
BUY / HOLD / SELL
       ↓
Existing Backtesting Engine
       ↓
Financial Performance
```

---

# 6. Time-Series Data Split

Do **not** use a random train/test split for the primary experiment.

Use chronological data:

```text
2015 ───────── 2021 | 2022 ─ 2023 | 2024 ─ 2025
       TRAIN              VALID         TEST
```

The exact dates should be determined by the available Yahoo Finance data.

For stronger validation, implement walk-forward / rolling validation after the initial pipeline works.

---

# 7. Model Training Architecture

Do not build 12 completely independent pipelines.

Build a common model interface.

```text
                Feature Dataset
                      │
          ┌───────────┴───────────┐
          ↓                       ↓
     Classical Models        Sequence Models
          │                       │
          ├─ Logistic             └─ LSTM
          ├─ LDA
          ├─ QDA
          ├─ KNN
          ├─ Naive Bayes
          ├─ Decision Tree
          ├─ Random Forest
          ├─ SVM
          ├─ Gradient Boosting
          └─ XGBoost
                  ↓
          Common Prediction Format
                  ↓
          Existing Backtester
```

Every model should return:

```text
model_name
ticker
date
predicted_class
prob_buy
prob_hold
prob_sell
```

This keeps the backtesting engine model-agnostic.

---

# 8. Probability-Based Stock Ranking

Do not only use the class label.

Use model probabilities.

Example:

| Stock | P(BUY) | Prediction |
|---|---:|---|
| INFY | 0.82 | BUY |
| RELIANCE | 0.74 | BUY |
| HDFC | 0.71 | BUY |
| TCS | 0.69 | BUY |
| ICICI | 0.65 | BUY |

The system can rank stocks by `P(BUY)`.

```text
NIFTY 50
   ↓
ML predictions
   ↓
Rank by P(BUY)
   ↓
Select Top N
   ↓
Portfolio
```

Initial implementation:

- Top 5 stocks
- Equal weighting
- Periodic rebalancing

Make `N` configurable later.

---

# 9. ML → Existing Backtester Integration

The existing backtesting engine should **not** be rewritten.

Add an ML signal adapter:

```text
ML Model
   ↓
Prediction
   ↓
Signal Adapter
   ↓
Existing Signal/Execution Interface
   ↓
Existing Backtesting Engine
```

The existing execution rule remains:

```text
Signal generated at t
        ↓
Execute at open of t+1
```

This preserves the existing look-ahead-bias protection.

---

# 10. Evaluation

## Classification Metrics

- Accuracy
- Precision
- Recall
- F1 Score
- Confusion Matrix
- ROC-AUC
- Precision-Recall / PR-AUC

## Trading Metrics

Use the existing AlphaForge performance layer:

- Total Return
- CAGR
- Sharpe Ratio
- Sortino Ratio
- Maximum Drawdown
- Win Rate
- Volatility
- Total Trades

The project therefore evaluates both:

```text
Does the model classify signals correctly?
                  AND
Does following its signals produce useful
backtest performance?
```

---

# 11. Hyperparameter Tuning

Use controlled parameter searches rather than huge grids.

Example:

```text
Random Forest
    ↓
mtry = {sqrt(p), p/3}
trees = {200, 500}
```

```text
XGBoost
    ↓
depth = {3, 5, 7}
learning_rate = {0.03, 0.1}
n_estimators = {100, 300}
```

Validation data is used for model selection.

The test period remains untouched until final evaluation.

---

# 12. R Backend Implementation

Suggested additions to the existing backend:

```text
backend/
├── R/
│   ├── features/
│   │   ├── returns.R
│   │   ├── trend_indicators.R
│   │   ├── momentum_indicators.R
│   │   ├── volatility_features.R
│   │   └── volume_features.R
│   │
│   ├── ml/
│   │   ├── preprocessing.R
│   │   ├── labels.R
│   │   ├── feature_selection.R
│   │   ├── train_logistic.R
│   │   ├── train_lda.R
│   │   ├── train_qda.R
│   │   ├── train_knn.R
│   │   ├── train_naive_bayes.R
│   │   ├── train_tree.R
│   │   ├── train_random_forest.R
│   │   ├── train_svm.R
│   │   ├── train_gbm.R
│   │   ├── train_xgboost.R
│   │   ├── train_mlp.R
│   │   └── train_lstm.R
│   │
│   ├── ml_pipeline.R
│   ├── model_evaluation.R
│   └── ml_backtest_adapter.R
│
└── plumber.R
```

Adapt the structure to the existing repository rather than forcing a new architecture.

---

# 13. New Plumber Endpoints

## Feature generation

```http
POST /ml/features
```

Input:

```text
ticker
start
end
feature_config
```

## Train model

```http
POST /ml/train
```

Example:

```json
{
  "model": "random_forest",
  "ticker": "INFY.NS",
  "start": "...",
  "end": "...",
  "horizon": 5,
  "threshold": 0.02
}
```

Output:

```text
model_id
model metrics
feature importance
training metadata
```

## Compare models

```http
POST /ml/compare
```

Output:

```text
model comparison table
```

## Generate predictions

```http
POST /ml/predict
```

Output:

```text
ticker
date
prediction
prob_buy
prob_hold
prob_sell
```

## ML Backtest

```http
POST /ml/backtest
```

Input:

```text
model
tickers
date range
initial capital
top_n
rebalance frequency
```

Output:

```text
equity curve
trade history
portfolio holdings
financial metrics
```

---

# 14. React Frontend Changes

Do not redesign the existing application.

Add an **ML Strategy** section to the existing Strategy Builder.

```text
Strategy Type

○ Technical Strategy
○ ML Strategy
```

When ML Strategy is selected:

```text
Model
[ Random Forest ▼ ]

Prediction Horizon
[ 5 days ]

BUY Threshold
[ 2% ]

SELL Threshold
[ 2% ]

Features
☑ RSI
☑ EMA
☑ MACD
☑ ATR
☑ Bollinger Bands
☑ Momentum
☑ Volatility

Portfolio Selection
[ Top 5 ]

Rebalancing
[ Weekly ]

[ Train / Run Backtest ]
```

---

# 15. ML Results Page

Add:

## Model Performance

- Accuracy
- Precision
- Recall
- F1
- ROC-AUC
- PR-AUC

## Trading Performance

- Total Return
- CAGR
- Sharpe
- Sortino
- Maximum Drawdown
- Win Rate
- Volatility
- Total Trades

## Visualizations

- Confusion Matrix
- ROC Curve
- Precision-Recall Curve
- Feature Importance
- Equity Curve
- Drawdown Curve
- Model Comparison
- Portfolio Allocation
- Predicted Buy Probability by Stock

---

# 16. Model Comparison

Main comparison table:

| Model | Accuracy | F1 | ROC-AUC | CAGR | Sharpe | Max Drawdown |
|---|---:|---:|---:|---:|---:|---:|
| Logistic Regression | | | | | | |
| LDA | | | | | | |
| QDA | | | | | | |
| KNN | | | | | | |
| Naive Bayes | | | | | | |
| Decision Tree | | | | | | |
| Random Forest | | | | | | |
| SVM | | | | | | |
| Gradient Boosting | | | | | | |
| XGBoost | | | | | | |
| MLP | | | | | | |
| LSTM | | | | | | |

---

# 17. Rule-Based Alpha Library — Separate From ML

These are **not** counted as the 12 ML algorithms.

They can be implemented as preset strategies:

1. SMA Crossover
2. EMA Crossover
3. RSI Mean Reversion
4. MACD Crossover
5. Bollinger Mean Reversion
6. Momentum
7. Breakout
8. Volatility Breakout
9. Trend Following
10. Mean Reversion

They plug directly into the existing backtesting engine.

Final structure:

```text
Strategy Builder
       │
       ├── Rule-Based
       │      ├── SMA
       │      ├── RSI
       │      ├── MACD
       │      └── ...
       │
       └── ML-Based
              ├── Logistic
              ├── Random Forest
              ├── XGBoost
              ├── MLP
              └── LSTM
```

---

# 18. Implementation Order

## Phase A — Foundation

- [ ] Feature engineering pipeline
- [ ] Return calculations
- [ ] Technical indicators
- [ ] Label generation
- [ ] Missing-value handling
- [ ] Chronological splitting
- [ ] Feature selection

## Phase B — Classical ML

- [ ] Logistic Regression
- [ ] LDA
- [ ] QDA
- [ ] KNN
- [ ] Naive Bayes

## Phase C — Tree / Ensemble

- [ ] Decision Tree
- [ ] Random Forest
- [ ] SVM
- [ ] Gradient Boosting
- [ ] XGBoost

## Phase D — Deep Learning

- [ ] MLP
- [ ] LSTM

## Phase E — Evaluation

- [ ] Classification metrics
- [ ] Confusion matrices
- [ ] ROC curves
- [ ] PR curves
- [ ] Feature importance
- [ ] Model comparison

## Phase F — Backtesting

- [ ] ML prediction adapter
- [ ] Top-N ranking
- [ ] Portfolio construction
- [ ] Rebalancing
- [ ] Existing backtester integration
- [ ] Financial metrics

## Phase G — Frontend

- [ ] ML strategy selector
- [ ] Model selector
- [ ] Feature selector
- [ ] Training configuration
- [ ] Model comparison page
- [ ] ML backtest page
- [ ] ML visualizations

## Phase H — Polish

- [ ] Hyperparameter tuning
- [ ] Model persistence
- [ ] Database persistence
- [ ] Error handling
- [ ] Loading/progress states
- [ ] Tests
- [ ] Documentation

---

# 19. Definition of Done

A user should be able to:

```text
Login
  ↓
Select NIFTY stocks
  ↓
Select ML strategy
  ↓
Select model
  ↓
Generate features
  ↓
Train model
  ↓
View validation metrics
  ↓
Generate predictions
  ↓
Rank stocks
  ↓
Select Top N
  ↓
Run backtest
  ↓
View equity curve
  ↓
View financial metrics
```

The system should also support:

```text
Train all 12 models
       ↓
Compare them
       ↓
Compare classification performance
       ↓
Compare backtest performance
       ↓
Persist the results
```

---

# 20. Core Design Principle

**The existing AlphaForge backtesting engine remains the source of truth for trade execution and financial metrics.**

The new ML module is responsible only for:

```text
FEATURES
   ↓
MODEL
   ↓
PREDICTION
   ↓
SIGNAL
```

The existing engine remains responsible for:

```text
SIGNAL
   ↓
EXECUTION
   ↓
POSITION
   ↓
TRADE
   ↓
EQUITY CURVE
   ↓
FINANCIAL METRICS
```

This keeps the extension modular and avoids duplicating the already-working backtesting logic.
