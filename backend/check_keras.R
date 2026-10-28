library(reticulate)
use_python("C:/Users/Lenovo/Documents/.virtualenvs/r-keras/Scripts/python.exe", required = TRUE)
library(keras)
library(tensorflow)

cat("Python executable:\n")
print(reticulate::py_config()$python)

cat("Legacy Keras available:\n")
print(keras::is_keras_available())