
FROM rocker/r-ver:4.4.3

ENV DEBIAN_FRONTEND=noninteractive


RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libomp-dev \
    zlib1g-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*




# Install and verify Plumber
RUN R -e "options(repos=c(CRAN='https://cloud.r-project.org')); install.packages('plumber'); stopifnot(requireNamespace('plumber', quietly=TRUE)); print(packageVersion('plumber'))"

# Install remaining backend dependencies
RUN R -e "options(repos=c(CRAN='https://cloud.r-project.org')); install.packages(c('jsonlite', 'DBI', 'RPostgres', 'bcrypt', 'jose', 'quantmod', 'TTR', 'xts', 'zoo', 'MASS', 'FNN', 'naivebayes', 'e1071', 'rpart', 'randomForest', 'gbm', 'xgboost', 'nnet', 'httr'))"


WORKDIR /app

COPY backend/ ./backend/

ENV PLUMBER_HOST=0.0.0.0

EXPOSE 10000

CMD ["sh", "-c", "PORT=${PORT:-10000} Rscript backend/plumber.R"]
