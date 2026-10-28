import apiClient from './apiClient'

/**
 * ML/DL API service (Feature-Engineering + ML module).
 *
 * POST /ml/features   Feature matrix for one ticker (JWT required)
 * POST /ml/train      Train one model + classification/backtest metrics (JWT)
 * POST /ml/compare    Train/evaluate several models + comparison table (JWT)
 * POST /ml/predict    Standardized BUY/HOLD/SELL prediction record (JWT)
 * POST /ml/backtest   Top-N portfolio backtest through the existing engine (JWT)
 *
 * All payloads use the snake_case contract documented in the backend handlers
 * (backend/api/ml.R). Auth is attached by the shared apiClient interceptor.
 */

/** The 12 R classification models, in display order. */
export const ML_MODEL_OPTIONS = [
  { value: 'logistic_regression', label: 'Logistic Regression' },
  { value: 'lda', label: 'Linear Discriminant Analysis (LDA)' },
  { value: 'qda', label: 'Quadratic Discriminant Analysis (QDA)' },
  { value: 'knn', label: 'K-Nearest Neighbors (KNN)' },
  { value: 'naive_bayes', label: 'Naive Bayes' },
  { value: 'decision_tree', label: 'Decision Tree' },
  { value: 'random_forest', label: 'Random Forest' },
  { value: 'svm', label: 'Support Vector Machine (SVM)' },
  { value: 'gradient_boosting', label: 'Gradient Boosting' },
  { value: 'xgboost', label: 'XGBoost' },
  { value: 'mlp', label: 'MLP / Neural Network' },
  { value: 'lstm', label: 'LSTM' },
]

/** The 12 Python time-series regression models, in display order. */
export const TS_MODEL_OPTIONS = [
  { value: 'ts_zero_return', label: 'Zero-Return Baseline' },
  { value: 'ts_historical_mean', label: 'Historical-Mean Baseline' },
  { value: 'ts_linear_regression', label: 'Linear Regression (TS)' },
  { value: 'ts_elastic_net', label: 'Elastic Net' },
  { value: 'ts_arima', label: 'ARIMA' },
  { value: 'ts_ets', label: 'ETS (Exponential Smoothing)' },
  { value: 'ts_garch', label: 'GARCH (Volatility)' },
  { value: 'ts_random_forest', label: 'Random Forest Regression' },
  { value: 'ts_xgboost', label: 'XGBoost Regression' },
  { value: 'ts_svr', label: 'Support Vector Regression' },
  { value: 'ts_lstm', label: 'LSTM (TensorFlow)' },
  { value: 'ts_gru', label: 'GRU' },
]

/** All models combined (R classification + Python time-series). */
export const ALL_MODEL_OPTIONS = [
  { 
    group: 'Classification Models (R)',
    options: ML_MODEL_OPTIONS
  },
  { 
    group: 'Time-Series Models (Python)',
    options: TS_MODEL_OPTIONS
  },
]

/** Feature-group checkboxes (map to FEATURE_GROUP_ALIASES in the backend). */
export const ML_FEATURE_GROUP_OPTIONS = [
  { value: 'returns', label: 'Returns' },
  { value: 'trend', label: 'Trend (SMA / EMA)' },
  { value: 'rsi', label: 'RSI' },
  { value: 'macd', label: 'MACD' },
  { value: 'atr', label: 'ATR' },
  { value: 'bollinger', label: 'Bollinger Bands' },
  { value: 'volatility', label: 'Volatility' },
  { value: 'momentum', label: 'Momentum / Stochastic' },
  { value: 'volume', label: 'Volume / OBV' },
]

export const REBALANCE_OPTIONS = [
  { value: 'daily', label: 'Daily' },
  { value: 'weekly', label: 'Weekly' },
  { value: 'monthly', label: 'Monthly' },
]

/**
 * POST /ml/features
 * Response: { success, ticker, groups, feature_names, n_rows, n_features,
 *             features: [{ date, values: { <feature>: number } }] }
 */
export async function getMlFeatures(payload) {
  const { data } = await apiClient.post('/ml/features', payload)
  return data
}

/**
 * POST /ml/train
 * Response: { success, model_id, model, model_name, ticker, label_config,
 *             hyperparameters, train_info, feature_importance,
 *             validation_metrics: { accuracy, precision, recall, f1,
 *               roc_auc_macro, pr_auc_macro, confusion_matrix, curves, ... },
 *             backtest: { engine, metrics }, predictions: [...] }
 */
export async function trainMlModel(payload) {
  const { data } = await apiClient.post('/ml/train', payload)
  return data
}

/**
 * POST /ml/compare
 * Response: { success, ticker, models_run, models_failed, best_by_f1,
 *             comparison: [{ Model, Status, Error, Accuracy, Precision, Recall,
 *               F1, ROC_AUC, PR_AUC, CAGR, Sharpe, Sortino, MaxDrawdown,
 *               WinRate, Volatility, TotalTrades }] }
 */
export async function compareMlModels(payload) {
  // Training up to 12 models takes minutes, far beyond apiClient's default 10s
  // timeout (which silently aborts the request before the server responds).
  const { data } = await apiClient.post('/ml/compare', payload, { timeout: 600000 })
  return data
}

/**
 * POST /ml/predict
 * Response: { success, model, model_name, ticker, as_of,
 *             prediction: { ticker, date, prediction, prob_buy, prob_hold, prob_sell },
 *             ranking: [{ ticker, prob_buy, rank }] }
 */
export async function predictMlModel(payload) {
  const { data } = await apiClient.post('/ml/predict', payload)
  return data
}

/**
 * POST /ml/backtest
 * Response: { success, backtestId, model, model_name, tickers, start, end,
 *             config: { top_n, rebalance_days }, classification_metrics,
 *             rebalances: [{ date, holdings: [{ ticker, prob_buy, rank }] }],
 *             portfolio: { metrics, equityCurve },
 *             perTicker: [{ ticker, metrics, tradeHistory }] }
 */
export async function runMlBacktest(payload) {
  // The portfolio backtest trains the model before trading the test window and
  // typically takes well over apiClient's default 10s timeout (which silently
  // aborts the request before the server responds).
  const { data } = await apiClient.post('/ml/backtest', payload, { timeout: 600000 })
  return data
}

export default {
  getMlFeatures,
  trainMlModel,
  compareMlModels,
  predictMlModel,
  runMlBacktest,
}
