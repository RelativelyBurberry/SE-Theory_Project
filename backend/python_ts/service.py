"""
AlphaForge Insight — Python Time-Series ML Service

FastAPI service for time-series forecasting and quantitative strategy models.
Implements 12 models: baselines, statistical, ML, and deep learning.

This service integrates with the existing R/Plumber backend for the ML Compare
feature, providing genuine forecasting implementations (not placeholders).

Models:
  Baselines: zero_return, historical_mean
  Statistical: linear_regression, elastic_net, arima, ets, garch
  ML: random_forest, xgboost, svr
  Deep Learning: lstm, gru
"""

import os
import re
import warnings
from datetime import datetime, timezone
import sqlite3
from typing import Any, Dict, List, Optional, Union

import numpy as np
import pandas as pd
import httpx
from fastapi import FastAPI, HTTPException, status
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field, field_validator

# Suppress TensorFlow warnings for cleaner logs
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
warnings.filterwarnings("ignore", category=UserWarning)

# Import ML libraries with error handling
try:
    from sklearn.linear_model import LinearRegression, ElasticNet
    from sklearn.ensemble import RandomForestRegressor
    from sklearn.svm import SVR
    from sklearn.preprocessing import StandardScaler
    from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False

try:
    import xgboost as xgb
    XGBOOST_AVAILABLE = True
except ImportError:
    XGBOOST_AVAILABLE = False

try:
    import tensorflow as tf
    tf.get_logger().setLevel("ERROR")
    from tensorflow import keras
    TENSORFLOW_AVAILABLE = True
except ImportError:
    TENSORFLOW_AVAILABLE = False

try:
    from arch import arch_model
    ARCH_AVAILABLE = True
except ImportError:
    ARCH_AVAILABLE = False

try:
    from statsmodels.tsa.arima.model import ARIMA
    from statsmodels.tsa.holtwinters import ExponentialSmoothing
    STATSMODELS_AVAILABLE = True
except ImportError:
    STATSMODELS_AVAILABLE = False

try:
    import yfinance as yf
    YFINANCE_AVAILABLE = True
except ImportError:
    yf = None
    YFINANCE_AVAILABLE = False


# =============================================================================
# Pydantic Models for Request/Response Validation
# =============================================================================

class ModelConfig(BaseModel):
    """Configuration for a single model run."""
    model_key: str = Field(..., description="Canonical model key")
    hyperparameters: Optional[Dict[str, Any]] = Field(default_factory=dict)


class ExecuteRequest(BaseModel):
    """Request to execute one or more models on prepared data."""
    features: List[Dict[str, Any]] = Field(..., description="Feature records with 'date' and feature columns")
    target: Optional[List[float]] = Field(default=None, description="Target values (future returns) for training")
    dates: Optional[List[str]] = Field(default=None, description="Date index for the features")
    horizon: int = Field(default=5, ge=1, le=60, description="Forecast horizon in trading days")
    train_end_idx: Optional[int] = Field(default=None, description="Index where training data ends (exclusive)")
    val_end_idx: Optional[int] = Field(default=None, description="Index where validation data ends (exclusive)")
    models: List[str] = Field(default_factory=lambda: ["zero_return", "historical_mean"], description="Models to run")
    hyperparameters: Optional[Dict[str, Dict[str, Any]]] = Field(default_factory=dict, description="Per-model hyperparameters")
    initial_capital: float = Field(default=10000.0, gt=0, description="Initial capital for backtest metrics")
    
    @field_validator("features")
    @classmethod
    def validate_features(cls, v):
        if not v or len(v) < 10:
            raise ValueError("At least 10 feature records are required")
        return v


class ModelResult(BaseModel):
    """Result from a single model execution."""
    model_key: str
    model_name: str
    model_family: str
    status: str = "ok"
    error: Optional[str] = None
    predictions: Optional[List[float]] = None
    prediction_dates: Optional[List[str]] = None
    metrics: Optional[Dict[str, Any]] = None
    train_info: Optional[Dict[str, Any]] = None
    hyperparameters_used: Optional[Dict[str, Any]] = None
    feature_importance: Optional[Dict[str, float]] = None


class ExecuteResponse(BaseModel):
    """Response from model execution."""
    success: bool
    results: List[ModelResult]
    execution_time_ms: float
    config: Dict[str, Any]


class HealthResponse(BaseModel):
    """Health check response."""
    status: str
    version: str
    python_version: str
    dependencies: Dict[str, Union[str, bool]]
    models_available: List[str]


class ModelMetadata(BaseModel):
    """Metadata for a single model."""
    key: str
    name: str
    family: str
    description: str
    needs_scaling: bool
    is_regression: bool
    is_volatility: bool
    default_hyperparameters: Dict[str, Any]
    available: bool


class ModelsResponse(BaseModel):
    """Response listing all available models."""
    models: List[ModelMetadata]


# =============================================================================
# Model Registry
# =============================================================================

MODEL_REGISTRY = {
    # Baselines
    "zero_return": {
        "name": "Zero-Return Baseline",
        "family": "baseline",
        "description": "Predicts zero future return for all observations.",
        "needs_scaling": False,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {},
    },
    "historical_mean": {
        "name": "Historical-Mean Baseline",
        "family": "baseline",
        "description": "Predicts the mean return estimated from training data only.",
        "needs_scaling": False,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {},
    },
    # Statistical
    "linear_regression": {
        "name": "Linear Regression",
        "family": "statistical",
        "description": "Ordinary least squares regression predicting future returns.",
        "needs_scaling": True,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {},
    },
    "elastic_net": {
        "name": "Elastic Net",
        "family": "statistical",
        "description": "Regularized regression with L1/L2 penalty mixing.",
        "needs_scaling": True,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"alpha": 1.0, "l1_ratio": 0.5},
    },
    "arima": {
        "name": "ARIMA",
        "family": "statistical",
        "description": "Autoregressive Integrated Moving Average time-series model.",
        "needs_scaling": False,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"order": [1, 0, 0]},
    },
    "ets": {
        "name": "ETS (Exponential Smoothing)",
        "family": "statistical",
        "description": "Exponential smoothing state space model.",
        "needs_scaling": False,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"trend": None, "seasonal": None},
    },
    "garch": {
        "name": "GARCH",
        "family": "statistical",
        "description": "Generalized Autoregressive Conditional Heteroskedasticity for volatility forecasting.",
        "needs_scaling": False,
        "is_regression": False,
        "is_volatility": True,
        "defaults": {"p": 1, "q": 1},
    },
    # Machine Learning
    "random_forest": {
        "name": "Random Forest Regression",
        "family": "ml",
        "description": "Ensemble of decision trees for regression.",
        "needs_scaling": False,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"n_estimators": 200, "max_depth": 10, "min_samples_leaf": 5},
    },
    "xgboost": {
        "name": "XGBoost Regression",
        "family": "ml",
        "description": "Gradient boosting regression with XGBoost.",
        "needs_scaling": False,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"n_estimators": 200, "max_depth": 3, "learning_rate": 0.05},
    },
    "svr": {
        "name": "Support Vector Regression",
        "family": "ml",
        "description": "Support vector machine for regression.",
        "needs_scaling": True,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"kernel": "rbf", "C": 1.0, "epsilon": 0.1},
    },
    # Deep Learning
    "lstm": {
        "name": "LSTM",
        "family": "deep_learning",
        "description": "Long Short-Term Memory neural network for sequence prediction.",
        "needs_scaling": True,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"sequence_length": 20, "units": 32, "epochs": 20, "batch_size": 32, "dropout": 0.2},
    },
    "gru": {
        "name": "GRU",
        "family": "deep_learning",
        "description": "Gated Recurrent Unit neural network for sequence prediction.",
        "needs_scaling": True,
        "is_regression": True,
        "is_volatility": False,
        "defaults": {"sequence_length": 20, "units": 32, "epochs": 20, "batch_size": 32, "dropout": 0.2},
    },
}


# =============================================================================
# Metric Calculations
# =============================================================================

def compute_regression_metrics(
    y_true: np.ndarray,
    y_pred: np.ndarray,
    train_mean: Optional[float] = None
) -> Dict[str, Any]:
    """Compute regression metrics for return prediction."""
    y_true = np.asarray(y_true).flatten()
    y_pred = np.asarray(y_pred).flatten()
    
    # Remove NaN/Inf
    valid = np.isfinite(y_true) & np.isfinite(y_pred)
    if valid.sum() < 2:
        return {
            "rmse": None,
            "mae": None,
            "r2": None,
            "ic": None,
            "directional_accuracy": None,
            "n_eval": int(valid.sum()),
        }
    
    y_true_v = y_true[valid]
    y_pred_v = y_pred[valid]
    
    # RMSE and MAE
    rmse = float(np.sqrt(mean_squared_error(y_true_v, y_pred_v)))
    mae = float(mean_absolute_error(y_true_v, y_pred_v))
    
    # R-squared (can be negative for bad models)
    ss_res = np.sum((y_true_v - y_pred_v) ** 2)
    ss_tot = np.sum((y_true_v - np.mean(y_true_v)) ** 2)
    r2 = float(1 - ss_res / ss_tot) if ss_tot > 0 else None
    
    # Information Coefficient (rank correlation)
    if len(y_true_v) >= 5:
        try:
            from scipy.stats import spearmanr
            ic, _ = spearmanr(y_true_v, y_pred_v)
            ic = float(ic) if np.isfinite(ic) else None
        except Exception:
            ic = None
    else:
        ic = None
    
    # Directional Accuracy
    if len(y_true_v) >= 2:
        true_direction = np.sign(y_true_v)
        pred_direction = np.sign(y_pred_v)
        # Only count non-zero predictions
        non_zero = (pred_direction != 0)
        if non_zero.sum() > 0:
            directional_accuracy = float(np.mean(true_direction[non_zero] == pred_direction[non_zero]))
        else:
            directional_accuracy = None
    else:
        directional_accuracy = None
    
    return {
        "rmse": round(rmse, 6),
        "mae": round(mae, 6),
        "r2": round(r2, 6) if r2 is not None else None,
        "ic": round(ic, 6) if ic is not None else None,
        "directional_accuracy": round(directional_accuracy, 4) if directional_accuracy is not None else None,
        "n_eval": int(len(y_true_v)),
    }


def compute_volatility_metrics(
    realized_vol: np.ndarray,
    predicted_vol: np.ndarray
) -> Dict[str, Any]:
    """Compute metrics for volatility forecasting (GARCH)."""
    realized_vol = np.asarray(realized_vol).flatten()
    predicted_vol = np.asarray(predicted_vol).flatten()
    
    valid = np.isfinite(realized_vol) & np.isfinite(predicted_vol) & (realized_vol > 0)
    if valid.sum() < 2:
        return {
            "vol_rmse": None,
            "vol_mae": None,
            "vol_mape": None,
            "n_eval": int(valid.sum()),
        }
    
    rv = realized_vol[valid]
    pv = predicted_vol[valid]
    
    vol_rmse = float(np.sqrt(np.mean((rv - pv) ** 2)))
    vol_mae = float(np.mean(np.abs(rv - pv)))
    
    # MAPE (avoid division by zero)
    vol_mape = float(np.mean(np.abs((rv - pv) / rv)) * 100) if np.all(rv > 0) else None
    
    return {
        "vol_rmse": round(vol_rmse, 6),
        "vol_mae": round(vol_mae, 6),
        "vol_mape": round(vol_mape, 4) if vol_mape is not None else None,
        "n_eval": int(len(rv)),
    }


# =============================================================================
# Model Implementations
# =============================================================================

class BaseModelClass:
    """Base class for all models."""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.model = None
        self.scaler = None
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        raise NotImplementedError
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        raise NotImplementedError
    
    def get_feature_importance(self) -> Optional[Dict[str, float]]:
        return None


class ZeroReturnBaseline(BaseModelClass):
    """Predicts zero for all observations."""
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        pass
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        return np.zeros(len(X))


class HistoricalMeanBaseline(BaseModelClass):
    """Predicts the mean of training data."""
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.train_mean = 0.0
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        if valid.sum() > 0:
            self.train_mean = float(np.mean(y[valid]))
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        return np.full(len(X), self.train_mean)


class LinearRegressionModel(BaseModelClass):
    """Linear regression model."""
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        self.scaler = StandardScaler()
        X_scaled = self.scaler.fit_transform(X_train)
        self.model = LinearRegression()
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        self.model.fit(X_scaled[valid], y[valid])
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        X_scaled = self.scaler.transform(X)
        return self.model.predict(X_scaled)
    
    def get_feature_importance(self) -> Optional[Dict[str, float]]:
        if self.model is None:
            return None
        return {"coef_norm": float(np.linalg.norm(self.model.coef_))}


class ElasticNetModel(BaseModelClass):
    """Elastic Net regularized regression."""
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        alpha = self.config.get("alpha", 1.0)
        l1_ratio = self.config.get("l1_ratio", 0.5)
        
        self.scaler = StandardScaler()
        X_scaled = self.scaler.fit_transform(X_train)
        self.model = ElasticNet(alpha=alpha, l1_ratio=l1_ratio, random_state=42, max_iter=5000)
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        self.model.fit(X_scaled[valid], y[valid])
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        X_scaled = self.scaler.transform(X)
        return self.model.predict(X_scaled)


class ARIMAModel(BaseModelClass):
    """ARIMA time-series model."""
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.order = tuple(config.get("order", [1, 0, 0]))
        self.last_values = None
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        # ARIMA uses only the target series
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        y_clean = y[valid]
        
        if len(y_clean) < 10:
            raise ValueError("ARIMA requires at least 10 observations")
        
        try:
            self.model = ARIMA(y_clean, order=self.order).fit()
            self.last_values = y_clean
        except Exception as e:
            raise ValueError(f"ARIMA fitting failed: {e}")
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        n = len(X)
        if self.model is None:
            return np.zeros(n)
        
        # One-step-ahead forecasts
        try:
            forecast = self.model.forecast(steps=n)
            return np.asarray(forecast).flatten()[:n]
        except Exception:
            return np.zeros(n)


class ETSModel(BaseModelClass):
    """Exponential Smoothing model."""
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.trend = config.get("trend", None)
        self.seasonal = config.get("seasonal", None)
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        y_clean = y[valid]
        
        if len(y_clean) < 10:
            raise ValueError("ETS requires at least 10 observations")
        
        try:
            self.model = ExponentialSmoothing(
                y_clean,
                trend=self.trend,
                seasonal=self.seasonal,
                initialization_method="estimated"
            ).fit()
        except Exception as e:
            raise ValueError(f"ETS fitting failed: {e}")
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        n = len(X)
        if self.model is None:
            return np.zeros(n)
        
        try:
            forecast = self.model.forecast(n)
            return np.asarray(forecast).flatten()[:n]
        except Exception:
            return np.zeros(n)


class GARCHModel(BaseModelClass):
    """GARCH volatility model."""
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.p = config.get("p", 1)
        self.q = config.get("q", 1)
        self.conditional_vol = None
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        # GARCH models volatility, not returns directly
        # Use returns (y_train) as input
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        y_clean = y[valid]
        
        if len(y_clean) < 50:
            raise ValueError("GARCH requires at least 50 observations")
        
        # Scale returns to percentage for numerical stability
        y_scaled = y_clean * 100
        
        try:
            am = arch_model(y_scaled, vol="Garch", p=self.p, q=self.q, mean="Zero")
            self.model = am.fit(disp="off", show_warning=False)
            self.conditional_vol = self.model.conditional_volatility / 100  # Scale back
        except Exception as e:
            raise ValueError(f"GARCH fitting failed: {e}")
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        # Return predicted volatility (not returns)
        n = len(X)
        if self.model is None:
            return np.ones(n) * 0.2  # Default 20% annualized vol
        
        try:
            forecast = self.model.forecast(horizon=n)
            vol_forecast = np.sqrt(forecast.variance.values[-1, :]) / 100
            return np.asarray(vol_forecast).flatten()[:n]
        except Exception:
            return np.ones(n) * 0.2


class RandomForestModel(BaseModelClass):
    """Random Forest regression model."""
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, feature_names: List[str] = None, **kwargs) -> None:
        n_estimators = self.config.get("n_estimators", 200)
        max_depth = self.config.get("max_depth", 10)
        min_samples_leaf = self.config.get("min_samples_leaf", 5)
        
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        
        self.model = RandomForestRegressor(
            n_estimators=n_estimators,
            max_depth=max_depth,
            min_samples_leaf=min_samples_leaf,
            random_state=42,
            n_jobs=-1
        )
        self.model.fit(X_train[valid], y[valid])
        self.feature_names = feature_names
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        return self.model.predict(X)
    
    def get_feature_importance(self) -> Optional[Dict[str, float]]:
        if self.model is None or self.feature_names is None:
            return None
        importance = self.model.feature_importances_
        return {name: float(imp) for name, imp in zip(self.feature_names, importance)}


class XGBoostModel(BaseModelClass):
    """XGBoost regression model."""
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, feature_names: List[str] = None, **kwargs) -> None:
        n_estimators = self.config.get("n_estimators", 200)
        max_depth = self.config.get("max_depth", 3)
        learning_rate = self.config.get("learning_rate", 0.05)
        
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        
        self.model = xgb.XGBRegressor(
            n_estimators=n_estimators,
            max_depth=max_depth,
            learning_rate=learning_rate,
            random_state=42,
            n_jobs=-1,
            verbosity=0
        )
        self.model.fit(X_train[valid], y[valid])
        self.feature_names = feature_names
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        return self.model.predict(X)
    
    def get_feature_importance(self) -> Optional[Dict[str, float]]:
        if self.model is None or self.feature_names is None:
            return None
        try:
            importance = self.model.feature_importances_
            return {name: float(imp) for name, imp in zip(self.feature_names, importance)}
        except Exception:
            return None


class SVRModel(BaseModelClass):
    """Support Vector Regression model."""
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, **kwargs) -> None:
        kernel = self.config.get("kernel", "rbf")
        C = self.config.get("C", 1.0)
        epsilon = self.config.get("epsilon", 0.1)
        
        self.scaler = StandardScaler()
        X_scaled = self.scaler.fit_transform(X_train)
        
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        
        self.model = SVR(kernel=kernel, C=C, epsilon=epsilon)
        self.model.fit(X_scaled[valid], y[valid])
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        X_scaled = self.scaler.transform(X)
        return self.model.predict(X_scaled)


class LSTMModel(BaseModelClass):
    """LSTM neural network model."""
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.sequence_length = config.get("sequence_length", 20)
        self.units = config.get("units", 32)
        self.epochs = config.get("epochs", 20)
        self.batch_size = config.get("batch_size", 32)
        self.dropout = config.get("dropout", 0.2)
    
    def _build_sequences(self, X: np.ndarray, y: Optional[np.ndarray] = None):
        """Build sequences for LSTM input."""
        n_samples = len(X) - self.sequence_length + 1
        if n_samples <= 0:
            return np.zeros((0, self.sequence_length, X.shape[1])), np.zeros(0)
        
        X_seq = np.zeros((n_samples, self.sequence_length, X.shape[1]))
        for i in range(n_samples):
            X_seq[i] = X[i:i + self.sequence_length]
        
        if y is not None:
            y_seq = y[self.sequence_length - 1:]
            return X_seq, y_seq
        return X_seq
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, X_val: np.ndarray = None, y_val: np.ndarray = None, **kwargs) -> None:
        self.scaler = StandardScaler()
        X_scaled = self.scaler.fit_transform(X_train)
        
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        
        X_seq, y_seq = self._build_sequences(X_scaled[valid], y[valid])
        
        if len(X_seq) < 10:
            raise ValueError(f"LSTM needs at least 10 sequences, got {len(X_seq)}")
        
        # Build model
        self.model = keras.Sequential([
            keras.layers.Input(shape=(self.sequence_length, X_train.shape[1])),
            keras.layers.LSTM(self.units, return_sequences=False),
            keras.layers.Dropout(self.dropout),
            keras.layers.Dense(16, activation="relu"),
            keras.layers.Dense(1)
        ])
        
        self.model.compile(
            optimizer=keras.optimizers.Adam(learning_rate=0.001),
            loss="mse"
        )
        
        # Validation data
        validation_data = None
        if X_val is not None and y_val is not None:
            X_val_scaled = self.scaler.transform(X_val)
            y_val_arr = np.asarray(y_val).flatten()
            valid_val = np.isfinite(y_val_arr)
            X_val_seq, y_val_seq = self._build_sequences(X_val_scaled[valid_val], y_val_arr[valid_val])
            if len(X_val_seq) > 0:
                validation_data = (X_val_seq, y_val_seq)
        
        self.model.fit(
            X_seq, y_seq,
            epochs=self.epochs,
            batch_size=self.batch_size,
            validation_data=validation_data,
            verbose=0,
            shuffle=False
        )
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        X_scaled = self.scaler.transform(X)
        X_seq = self._build_sequences(X_scaled)
        
        if len(X_seq) == 0:
            return np.zeros(len(X))
        
        predictions = self.model.predict(X_seq, verbose=0).flatten()
        
        # Pad with last known value for the first sequence_length - 1 positions
        full_predictions = np.zeros(len(X))
        full_predictions[self.sequence_length - 1:] = predictions
        full_predictions[:self.sequence_length - 1] = predictions[0] if len(predictions) > 0 else 0
        
        return full_predictions


class GRUModel(BaseModelClass):
    """GRU neural network model."""
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.sequence_length = config.get("sequence_length", 20)
        self.units = config.get("units", 32)
        self.epochs = config.get("epochs", 20)
        self.batch_size = config.get("batch_size", 32)
        self.dropout = config.get("dropout", 0.2)
    
    def _build_sequences(self, X: np.ndarray, y: Optional[np.ndarray] = None):
        """Build sequences for GRU input."""
        n_samples = len(X) - self.sequence_length + 1
        if n_samples <= 0:
            return np.zeros((0, self.sequence_length, X.shape[1])), np.zeros(0)
        
        X_seq = np.zeros((n_samples, self.sequence_length, X.shape[1]))
        for i in range(n_samples):
            X_seq[i] = X[i:i + self.sequence_length]
        
        if y is not None:
            y_seq = y[self.sequence_length - 1:]
            return X_seq, y_seq
        return X_seq
    
    def fit(self, X_train: np.ndarray, y_train: np.ndarray, X_val: np.ndarray = None, y_val: np.ndarray = None, **kwargs) -> None:
        self.scaler = StandardScaler()
        X_scaled = self.scaler.fit_transform(X_train)
        
        y = np.asarray(y_train).flatten()
        valid = np.isfinite(y)
        
        X_seq, y_seq = self._build_sequences(X_scaled[valid], y[valid])
        
        if len(X_seq) < 10:
            raise ValueError(f"GRU needs at least 10 sequences, got {len(X_seq)}")
        
        # Build model
        self.model = keras.Sequential([
            keras.layers.Input(shape=(self.sequence_length, X_train.shape[1])),
            keras.layers.GRU(self.units, return_sequences=False),
            keras.layers.Dropout(self.dropout),
            keras.layers.Dense(16, activation="relu"),
            keras.layers.Dense(1)
        ])
        
        self.model.compile(
            optimizer=keras.optimizers.Adam(learning_rate=0.001),
            loss="mse"
        )
        
        # Validation data
        validation_data = None
        if X_val is not None and y_val is not None:
            X_val_scaled = self.scaler.transform(X_val)
            y_val_arr = np.asarray(y_val).flatten()
            valid_val = np.isfinite(y_val_arr)
            X_val_seq, y_val_seq = self._build_sequences(X_val_scaled[valid_val], y_val_arr[valid_val])
            if len(X_val_seq) > 0:
                validation_data = (X_val_seq, y_val_seq)
        
        self.model.fit(
            X_seq, y_seq,
            epochs=self.epochs,
            batch_size=self.batch_size,
            validation_data=validation_data,
            verbose=0,
            shuffle=False
        )
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        X_scaled = self.scaler.transform(X)
        X_seq = self._build_sequences(X_scaled)
        
        if len(X_seq) == 0:
            return np.zeros(len(X))
        
        predictions = self.model.predict(X_seq, verbose=0).flatten()
        
        # Pad with last known value for the first sequence_length - 1 positions
        full_predictions = np.zeros(len(X))
        full_predictions[self.sequence_length - 1:] = predictions
        full_predictions[:self.sequence_length - 1] = predictions[0] if len(predictions) > 0 else 0
        
        return full_predictions


# =============================================================================
# Model Factory
# =============================================================================

def create_model(model_key: str, hyperparameters: Dict[str, Any] = None) -> BaseModelClass:
    """Factory function to create model instances."""
    hp = hyperparameters or {}
    
    model_classes = {
        "zero_return": ZeroReturnBaseline,
        "historical_mean": HistoricalMeanBaseline,
        "linear_regression": LinearRegressionModel,
        "elastic_net": ElasticNetModel,
        "arima": ARIMAModel,
        "ets": ETSModel,
        "garch": GARCHModel,
        "random_forest": RandomForestModel,
        "xgboost": XGBoostModel,
        "svr": SVRModel,
        "lstm": LSTMModel,
        "gru": GRUModel,
    }
    
    if model_key not in model_classes:
        raise ValueError(f"Unknown model: {model_key}")
    
    return model_classes[model_key](hp)


def check_model_availability(model_key: str) -> bool:
    """Check if a model can be instantiated with available dependencies."""
    if model_key in ["linear_regression", "elastic_net", "random_forest", "svr"]:
        return SKLEARN_AVAILABLE
    if model_key == "xgboost":
        return SKLEARN_AVAILABLE and XGBOOST_AVAILABLE
    if model_key in ["lstm", "gru"]:
        return SKLEARN_AVAILABLE and TENSORFLOW_AVAILABLE
    if model_key == "garch":
        return ARCH_AVAILABLE
    if model_key in ["arima", "ets"]:
        return STATSMODELS_AVAILABLE
    # Baselines always available
    return True


# =============================================================================
# FastAPI Application
# =============================================================================

app = FastAPI(
    title="AlphaForge Time-Series ML Service",
    description="Python service for time-series forecasting and quantitative strategy models",
    version="1.0.0",
)

# CORS middleware for integration with R backend
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health", response_model=HealthResponse)
async def health_check():
    """Health check endpoint."""
    import sys
    
    models_available = [
        key for key in MODEL_REGISTRY.keys()
        if check_model_availability(key)
    ]
    
    return HealthResponse(
        status="healthy",
        version="1.0.0",
        python_version=f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}",
        dependencies={
            "sklearn": SKLEARN_AVAILABLE,
            "xgboost": XGBOOST_AVAILABLE,
            "tensorflow": TENSORFLOW_AVAILABLE,
            "arch": ARCH_AVAILABLE,
            "statsmodels": STATSMODELS_AVAILABLE,
        },
        models_available=models_available,
    )


@app.get("/models", response_model=ModelsResponse)
async def list_models():
    """List all available models with their metadata."""
    models = []
    for key, meta in MODEL_REGISTRY.items():
        models.append(ModelMetadata(
            key=key,
            name=meta["name"],
            family=meta["family"],
            description=meta["description"],
            needs_scaling=meta["needs_scaling"],
            is_regression=meta["is_regression"],
            is_volatility=meta["is_volatility"],
            default_hyperparameters=meta["defaults"],
            available=check_model_availability(key),
        ))
    return ModelsResponse(models=models)


class LiveForecastRequest(BaseModel):
    ticker: str = Field(default="AAPL", min_length=1, max_length=20)
    horizons: List[int] = Field(default_factory=lambda: [1, 5, 10])
    history_period: str = Field(default="5y")

    @field_validator("ticker")
    @classmethod
    def normalize_ticker(cls, value):
        value = value.strip().upper()
        if not value or any(ch not in "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.^-=" for ch in value):
            raise ValueError("Invalid ticker symbol")
        return value

    @field_validator("horizons")
    @classmethod
    def validate_horizons(cls, values):
        if not values or any(int(v) not in (1, 5, 10, 20) for v in values):
            raise ValueError("Horizons must be selected from 1, 5, 10, 20 trading days")
        return sorted(set(int(v) for v in values))


def _history_period_start(period: str) -> str:
    """Convert a yfinance-style history period into an ISO start date."""
    today = pd.Timestamp.now(tz="UTC").normalize().tz_localize(None)
    value = (period or "5y").strip().lower()
    if value == "max":
        return "1990-01-01"
    match = re.fullmatch(r"([1-9][0-9]*)(d|mo|y)", value)
    if not match:
        raise HTTPException(status_code=422, detail="history_period must be like 180d, 12mo, 5y, or max.")
    amount, unit = int(match.group(1)), match.group(2)
    if unit == "d":
        start = today - pd.Timedelta(days=amount)
    elif unit == "mo":
        start = today - pd.DateOffset(months=amount)
    else:
        start = today - pd.DateOffset(years=amount)
    return start.date().isoformat()


async def _fetch_shared_ohlcv(ticker: str, history_period: str) -> pd.DataFrame:
    """Fetch prices through R/Plumber, the canonical provider chain and cache."""
    base_url = os.getenv("ALPHAFORGE_R_MARKET_DATA_URL", "").strip().rstrip("/")
    token = os.getenv("ALPHAFORGE_INTERNAL_MARKET_DATA_TOKEN", "").strip()
    if not base_url or not token:
        raise HTTPException(
            status_code=503,
            detail="Shared market data is not configured. Set ALPHAFORGE_R_MARKET_DATA_URL and ALPHAFORGE_INTERNAL_MARKET_DATA_TOKEN on the Python service.",
        )

    params = {
        "ticker": ticker,
        "start": _history_period_start(history_period),
        "end": pd.Timestamp.now(tz="UTC").date().isoformat(),
    }
    headers = {"X-AlphaForge-Internal-Token": token}
    try:
        async with httpx.AsyncClient(timeout=httpx.Timeout(45.0, connect=10.0)) as client:
            response = await client.get(
                f"{base_url}/internal/market-data",
                params=params,
                headers=headers,
            )
    except httpx.HTTPError as exc:
        raise HTTPException(
            status_code=502,
            detail=f"Could not reach the shared R market-data service: {type(exc).__name__}.",
        ) from exc

    if response.status_code != 200:
        try:
            detail = response.json().get("error", "Shared market-data service returned an error.")
        except (ValueError, AttributeError):
            detail = "Shared market-data service returned an error."
        raise HTTPException(status_code=502, detail=f"Shared market-data retrieval failed: {detail}")

    payload = response.json()
    rows = payload.get("data") or []
    if not rows:
        raise HTTPException(status_code=404, detail=f"No historical OHLCV rows returned for {ticker}.")

    frame = pd.DataFrame(rows)
    required = {"date", "close", "volume"}
    if not required.issubset(frame.columns):
        raise HTTPException(status_code=502, detail="Shared market-data response is missing date, close, or volume.")
    frame["date"] = pd.to_datetime(frame["date"], errors="coerce")
    for col in ("open", "high", "low", "close", "adjusted_close", "volume"):
        if col in frame.columns:
            frame[col] = pd.to_numeric(frame[col], errors="coerce")
    frame = frame.dropna(subset=["date", "close"]).sort_values("date").drop_duplicates("date")
    frame = frame.set_index("date")
    frame = frame.rename(columns={
        "open": "Open", "high": "High", "low": "Low", "close": "Close",
        "adjusted_close": "Adj Close", "volume": "Volume",
    })
    if "Volume" not in frame:
        frame["Volume"] = 0.0
    return frame


def _live_feature_frame(ohlcv):
    close = ohlcv["Close"].astype(float)
    volume = ohlcv["Volume"].astype(float)
    ret = close.pct_change()
    features = pd.DataFrame(index=ohlcv.index)
    for lag in (1, 2, 3, 5, 10, 20):
        features[f"return_lag_{lag}"] = ret.shift(lag - 1)
    for window in (5, 10, 20):
        features[f"momentum_{window}"] = close.pct_change(window)
        features[f"volatility_{window}"] = ret.rolling(window).std()
        features[f"sma_ratio_{window}"] = close / close.rolling(window).mean() - 1
    features["volume_change"] = volume.pct_change().replace([np.inf, -np.inf], np.nan)
    features["volume_z20"] = (volume - volume.rolling(20).mean()) / volume.rolling(20).std()
    return features.replace([np.inf, -np.inf], np.nan)


def _forecast_db_path():
    return os.path.join(os.path.dirname(__file__), "forecast_history.sqlite3")


def _init_forecast_db():
    with sqlite3.connect(_forecast_db_path()) as con:
        con.execute("""CREATE TABLE IF NOT EXISTS forecast_history (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            ticker TEXT NOT NULL, model TEXT NOT NULL, horizon_days INTEGER NOT NULL,
            as_of TEXT NOT NULL, target_date TEXT NOT NULL,
            predicted_return REAL, predicted_price REAL, origin_close REAL, created_at TEXT NOT NULL,
            actual_return REAL, actual_price REAL, evaluated_at TEXT
        )""")
        columns = {row[1] for row in con.execute("PRAGMA table_info(forecast_history)").fetchall()}
        if "origin_close" not in columns:
            con.execute("ALTER TABLE forecast_history ADD COLUMN origin_close REAL")


def _reconcile_forecasts(ticker, raw):
    """Fill actual outcomes only after the requested trading horizon has elapsed."""
    _init_forecast_db()
    dates = [pd.Timestamp(x).date().isoformat() for x in raw.index]
    closes = raw["Close"].astype(float).to_numpy()
    with sqlite3.connect(_forecast_db_path()) as con:
        pending = con.execute("SELECT id, as_of, horizon_days, origin_close FROM forecast_history WHERE ticker=? AND actual_return IS NULL", (ticker,)).fetchall()
        for record_id, as_of, horizon, origin_close in pending:
            origin_date = pd.Timestamp(as_of).date().isoformat()
            try:
                origin_idx = dates.index(origin_date)
            except ValueError:
                continue
            target_idx = origin_idx + int(horizon)
            if target_idx >= len(closes):
                continue
            base = float(origin_close) if origin_close is not None else float(closes[origin_idx])
            if base <= 0:
                continue
            con.execute("UPDATE forecast_history SET actual_return=?, actual_price=?, evaluated_at=? WHERE id=? AND actual_return IS NULL", (float(closes[target_idx] / base - 1), float(closes[target_idx]), pd.Timestamp(raw.index[target_idx]).isoformat(), record_id))


@app.post("/live-forecast")
async def live_forecast(request: LiveForecastRequest):
    """Train leakage-aware daily-return models, report held-out metrics, and save forecasts.

    Historical OHLCV is retrieved from the R/Plumber internal endpoint, which
    uses the same configured provider chain and cache as backtesting. Direct
    yfinance access is an opt-in emergency fallback only.
    """
    if not YFINANCE_AVAILABLE:
        raise HTTPException(status_code=503, detail="Python dependency yfinance is missing. Install backend/python_ts/requirements.txt.")
    if not SKLEARN_AVAILABLE:
        raise HTTPException(status_code=503, detail="scikit-learn is unavailable in this Python environment.")

    ticker = request.ticker
    try:
        try:
            raw = await _fetch_shared_ohlcv(ticker, request.history_period)
        except HTTPException:
            # Yahoo is not used by default; enable only as an explicit emergency fallback.
            if os.getenv("PYTHON_MARKET_DATA_ALLOW_YFINANCE_FALLBACK", "false").strip().lower() not in {"1", "true", "yes"}:
                raise
            if not YFINANCE_AVAILABLE:
                raise
            raw = yf.download(ticker, period=request.history_period, interval="1d", auto_adjust=True, progress=False, threads=False)
            if raw is None or raw.empty:
                raise HTTPException(status_code=404, detail=f"No daily OHLCV history returned for {ticker}.")
            if isinstance(raw.columns, pd.MultiIndex):
                raw.columns = raw.columns.get_level_values(0)
        if raw is None or raw.empty:
            raise HTTPException(status_code=404, detail=f"No daily OHLCV history returned for {ticker}.")
        required = {"Close", "Volume"}
        if not required.issubset(set(raw.columns)):
            raise HTTPException(status_code=502, detail="Shared market-data response is missing required Close/Volume fields.")
        raw = raw.dropna(subset=["Close"]).sort_index()
        feature_frame = _live_feature_frame(raw)
        results = []
        now = datetime.now(timezone.utc).isoformat()
        _init_forecast_db()
        _reconcile_forecasts(ticker, raw)

        for horizon in request.horizons:
            target = raw["Close"].shift(-horizon) / raw["Close"] - 1.0
            dataset = feature_frame.join(target.rename("target")).dropna()
            if len(dataset) < 180:
                results.append({"horizon_days": horizon, "status": "insufficient_data", "error": f"Need at least 180 usable rows; found {len(dataset)}."})
                continue
            X_all = dataset.drop(columns=["target"]).astype(float)
            y_all = dataset["target"].astype(float)
            # Chronological holdout with a horizon-sized purge before the test region.
            split = int(len(dataset) * 0.8)
            train_end = max(80, split - horizon)
            test_start = split
            if len(dataset) - test_start < 30 or train_end <= 50:
                results.append({"horizon_days": horizon, "status": "insufficient_data", "error": "Insufficient train/test observations after chronological purge."})
                continue
            X_train, y_train = X_all.iloc[:train_end], y_all.iloc[:train_end]
            X_test, y_test = X_all.iloc[test_start:], y_all.iloc[test_start:]
            last_x = feature_frame.dropna().iloc[[-1]].reindex(columns=X_all.columns)
            if last_x.isna().any().any():
                results.append({"horizon_days": horizon, "status": "insufficient_data", "error": "Latest candle does not have a complete feature row."})
                continue

            candidates = {
                "Historical mean": None,
                "Elastic Net": ElasticNet(alpha=0.0005, l1_ratio=0.2, max_iter=10000) if SKLEARN_AVAILABLE else None,
                "Random Forest": RandomForestRegressor(n_estimators=150, max_depth=5, min_samples_leaf=8, random_state=42, n_jobs=-1),
            }
            if XGBOOST_AVAILABLE:
                candidates["XGBoost"] = xgb.XGBRegressor(n_estimators=150, max_depth=3, learning_rate=0.03, subsample=0.8, colsample_bytree=0.8, objective="reg:squarederror", random_state=42, n_jobs=2)

            model_rows = []
            for model_name, model in candidates.items():
                try:
                    if model_name == "Historical mean":
                        test_pred = np.full(len(y_test), float(y_train.mean()))
                        # For the current forecast, use all returns whose labels are known.
                        live_pred = float(y_all.mean())
                    else:
                        model.fit(X_train, y_train)
                        test_pred = np.asarray(model.predict(X_test), dtype=float).reshape(-1)
                        model.fit(X_all, y_all)
                        live_pred = float(np.asarray(model.predict(last_x)).reshape(-1)[0])
                    actual = y_test.to_numpy(dtype=float)
                    direction = float(np.mean((test_pred > 0) == (actual > 0)))
                    rmse = float(np.sqrt(np.mean((test_pred - actual) ** 2)))
                    mae = float(np.mean(np.abs(test_pred - actual)))
                    price_now = float(raw["Close"].iloc[-1])
                    as_of = pd.Timestamp(raw.index[-1]).isoformat()
                    target_date = (pd.Timestamp(raw.index[-1]) + pd.offsets.BDay(horizon)).date().isoformat()
                    row = {"model": model_name, "status": "ok", "predicted_return": live_pred, "predicted_price": price_now * (1 + live_pred), "metrics": {"rmse": rmse, "mae": mae, "directional_accuracy": direction, "test_observations": int(len(actual)), "test_start": pd.Timestamp(dataset.index[test_start]).date().isoformat(), "test_end": pd.Timestamp(dataset.index[-1]).date().isoformat()}}
                    model_rows.append(row)
                    with sqlite3.connect(_forecast_db_path()) as con:
                        con.execute("""INSERT INTO forecast_history
                            (ticker, model, horizon_days, as_of, target_date, predicted_return, predicted_price, origin_close, created_at)
                            SELECT ?, ?, ?, ?, ?, ?, ?, ?, ?
                            WHERE NOT EXISTS (SELECT 1 FROM forecast_history WHERE ticker=? AND model=? AND horizon_days=? AND as_of=?)""",
                            (ticker, model_name, horizon, as_of, target_date, live_pred, price_now * (1 + live_pred), price_now, now, ticker, model_name, horizon, as_of))
                except Exception as model_error:
                    model_rows.append({"model": model_name, "status": "failed", "error": str(model_error)[:300]})
            results.append({"horizon_days": horizon, "status": "ok" if any(x.get("status") == "ok" for x in model_rows) else "failed", "as_of": pd.Timestamp(raw.index[-1]).isoformat(), "latest_close": float(raw["Close"].iloc[-1]), "models": model_rows})

        with sqlite3.connect(_forecast_db_path()) as con:
            history = con.execute("SELECT id, ticker, model, horizon_days, as_of, target_date, predicted_return, predicted_price, created_at, actual_return, actual_price, evaluated_at FROM forecast_history WHERE ticker=? ORDER BY id DESC LIMIT 100", (ticker,)).fetchall()
        history_rows = [dict(zip(["id", "ticker", "model", "horizon_days", "as_of", "target_date", "predicted_return", "predicted_price", "created_at", "actual_return", "actual_price", "evaluated_at"], row)) for row in history]
        return {"success": True, "ticker": ticker, "source": "Yahoo Finance via yfinance", "as_of": pd.Timestamp(raw.index[-1]).isoformat(), "latest_close": float(raw["Close"].iloc[-1]), "history_count": len(raw), "horizons": results, "forecast_history": history_rows, "disclaimer": "Research estimates, not investment advice. Metrics use a chronological held-out segment; results do not establish profitability."}
    except HTTPException:
        raise
    except Exception as exc:
        raise HTTPException(status_code=502, detail=f"Forecast request failed for {ticker}: {str(exc)[:300]}") from exc


@app.get("/forecast-history/{ticker}")
async def get_forecast_history(ticker: str):
    """Return locally persisted forecasts for one ticker."""
    _init_forecast_db()
    with sqlite3.connect(_forecast_db_path()) as con:
        rows = con.execute("SELECT id, ticker, model, horizon_days, as_of, target_date, predicted_return, predicted_price, created_at, actual_return, actual_price, evaluated_at FROM forecast_history WHERE ticker=? ORDER BY id DESC LIMIT 100", (ticker.strip().upper(),)).fetchall()
    keys = ["id", "ticker", "model", "horizon_days", "as_of", "target_date", "predicted_return", "predicted_price", "created_at", "actual_return", "actual_price", "evaluated_at"]
    return {"ticker": ticker.strip().upper(), "records": [dict(zip(keys, row)) for row in rows]}


@app.post("/execute", response_model=ExecuteResponse)
async def execute_models(request: ExecuteRequest):
    """Execute one or more models on prepared data."""
    import time
    start_time = time.time()
    
    results = []
    
    try:
        # Convert features to DataFrame
        df = pd.DataFrame(request.features)
        
        # Extract dates if provided
        if request.dates:
            df["_date"] = request.dates
        elif "date" in df.columns:
            df["_date"] = df["date"]
            df = df.drop(columns=["date"])
        
        # Extract target if provided
        if request.target:
            y = np.array(request.target)
        else:
            y = None
        
        # Extract feature columns (exclude date)
        feature_cols = [c for c in df.columns if c != "_date"]
        X = df[feature_cols].values
        
        # Handle missing values
        X = np.nan_to_num(X, nan=0.0, posinf=0.0, neginf=0.0)
        
        # Determine splits
        n = len(X)
        train_end = request.train_end_idx or int(n * 0.6)
        val_end = request.val_end_idx or int(n * 0.8)
        
        X_train = X[:train_end]
        X_val = X[train_end:val_end]
        X_test = X[val_end:]
        
        y_train = y[:train_end] if y is not None else None
        y_val = y[train_end:val_end] if y is not None else None
        y_test = y[val_end:] if y is not None else None
        
        # Get dates for predictions
        pred_dates = df["_date"].iloc[val_end:].tolist() if "_date" in df.columns else None
        
        # Track train mean for metrics
        train_mean = float(np.mean(y_train[np.isfinite(y_train)])) if y_train is not None else 0.0
        
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Data preparation failed: {str(e)}"
        )
    
    # Execute each model
    for model_key in request.models:
        if model_key not in MODEL_REGISTRY:
            results.append(ModelResult(
                model_key=model_key,
                model_name=model_key,
                model_family="unknown",
                status="failed",
                error=f"Unknown model: {model_key}",
            ))
            continue
        
        meta = MODEL_REGISTRY[model_key]
        hp = request.hyperparameters.get(model_key, {})
        
        # Check availability
        if not check_model_availability(model_key):
            results.append(ModelResult(
                model_key=model_key,
                model_name=meta["name"],
                model_family=meta["family"],
                status="unavailable",
                error=f"Required dependencies not available for {meta['name']}",
            ))
            continue
        
        try:
            # Create and train model
            model = create_model(model_key, {**meta["defaults"], **hp})
            
            fit_kwargs = {
                "X_train": X_train,
                "y_train": y_train if y_train is not None else np.zeros(len(X_train)),
                "feature_names": feature_cols,
            }
            
            # Add validation data for deep learning models
            if model_key in ["lstm", "gru"]:
                fit_kwargs["X_val"] = X_val
                fit_kwargs["y_val"] = y_val if y_val is not None else np.zeros(len(X_val))
            
            model.fit(**fit_kwargs)
            
            # Predict on test set
            predictions = model.predict(X_test)
            
            # Compute metrics - only when a target was supplied
            if y_test is not None and len(y_test) > 0:
                if meta["is_volatility"]:
                    # For GARCH, use absolute returns as realized-vol proxy
                    realized_vol = np.abs(y_test)
                    metrics = compute_volatility_metrics(realized_vol, predictions)
                else:
                    metrics = compute_regression_metrics(y_test, predictions, train_mean)
            else:
                metrics = None
            
            # Get feature importance
            importance = model.get_feature_importance()
            
            results.append(ModelResult(
                model_key=model_key,
                model_name=meta["name"],
                model_family=meta["family"],
                status="ok",
                predictions=predictions.tolist(),
                prediction_dates=pred_dates,
                metrics=metrics,
                train_info={
                    "n_train": len(X_train),
                    "n_val": len(X_val),
                    "n_test": len(X_test),
                },
                hyperparameters_used={**meta["defaults"], **hp},
                feature_importance=importance,
            ))
            
        except Exception as e:
            results.append(ModelResult(
                model_key=model_key,
                model_name=meta["name"],
                model_family=meta["family"],
                status="failed",
                error=str(e),
            ))
    
    execution_time = (time.time() - start_time) * 1000
    
    return ExecuteResponse(
        success=True,
        results=results,
        execution_time_ms=round(execution_time, 2),
        config={
            "horizon": request.horizon,
            "train_end_idx": train_end,
            "val_end_idx": val_end,
            "n_features": len(feature_cols),
        }
    )


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8001)
