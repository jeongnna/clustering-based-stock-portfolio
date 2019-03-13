library(clValid)  # dunn()


time_slice <- function(data, time_idx) {
  data %>%
    group_by(code) %>%
    slice(time_idx) %>%
    ungroup()
}


time_expand <- function(data, skip = 1:2) {
  cols <- setdiff(1:ncol(data), skip)
  
  while (length(unique(data[["time"]])) > 1) {
    lagged <-
      apply(data[cols], 2, lag) %>%
      as_tibble() %>%
      setNames(str_c("x", cols))  # Dummy names
    data <- bind_cols(data, lagged)
    
    data <-
      data %>%
      group_by(code) %>%
      slice(-1) %>%
      ungroup()
    
    cols <- cols + length(cols)
  }
  data
}


scale_tbl <- function(data, skip = 1:2) {
  # 데이터의 각 변수들을 표준화한다.
  #
  # Args:
  #   data: data frame
  #   skip: 표준화를 건너뛸 열 번호 (integer vector)
  #
  # Returns:
  #   표준화된 data frame
  
  vals <- data[-skip]
  for (i in 1:ncol(vals)) {
    vals[[i]] <- scale(vals[[i]])
  }
  bind_cols(data[skip], vals)
}


PCA <- function(data, skip = 1:2, threshold = 0.8) {
  # 데이터에 PCA(주성분분석)을 수행한다.
  #
  # Args:
  #   data: data frame
  #   skip: PCA 대상에서 제외할 열 번호 (integer vector)
  #   threshold: 주성분 개수 선택의 기준이 되는 변동의 비율 (0 ~ 1)
  #
  # Returns:
  #   변수들이 주성분으로 대체된 data frame
  
  prfit <-
    data %>%
    select(-skip) %>%
    na.omit() %>%
    prcomp()
  
  pve <- prfit$sdev^2 %>% (function(x) {x / sum(x)})  # 각 주성분이 차지하는 변동 비율
  n_pc <- which(cumsum(pve) > threshold)[1]  # 사용할 주성분 개수
  
  x_origin <- as.matrix(data[-skip])  # 기존 변수 행렬
  loading <- prfit$rotation[, 1:n_pc]  # 주성분 로딩 벡터
  x_pc <- x_origin %*% loading  # 주성분 행렬
  
  bind_cols(data[1:2], as_tibble(x_pc))
}


add_factors_residual <- function(data, risk_free) {
  risk_free <-
    risk_free %>%
    filter(time %in% unique(data[["time"]]))
  
  data <-
    data %>%
    group_by(code) %>%
    mutate(
      logret = lead(logret),
      rf = lead(risk_free[["r"]])
    ) %>%
    ungroup()
  
  y <- data[["logret"]] - data[["rf"]]
  x <- data %>% select(-logret, -rf) %>% PCA()
  
  lmfit <- lm(y ~ ., data = x[-(1:2)])
  yhat <- predict(lmfit, newdata = x[-(1:2)])
  res <- y - yhat
  data[["factors_res"]] <- scale(res)
  
  n_time <- length(unique(data[["time"]]))
  data %>%
    group_by(code) %>%
    slice(-n_time) %>%
    ungroup()
}


add_market_residual <- function(data, market, risk_free) {
  market <-
    market %>%
    filter(time %in% unique(data[["time"]]))
  
  risk_free <-
    risk_free %>%
    filter(time %in% unique(data[["time"]]))
  
  data <-
    data %>%
    group_by(code) %>%
    mutate(
      mk = market[["logret"]],
      rf = risk_free[["r"]]
    ) %>%
    mutate(
      y = logret - rf,
      x = mk - rf
    ) %>%
    ungroup()
  
  lmfit <- lm(y ~ x, data = data)
  yhat <- predict(lmfit, newdata = data["x"])
  res <- data[["y"]] - yhat
  data[["market_res"]] <- scale(res)
  
  data
}


kmeanspp <- function(x, k, iter_max = 500, nstart = 20,
                     algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                   "MacQueen"), trace = FALSE) {
  n <- nrow(x)
  centers <- integer(k)
  centers[1] <- sample(1:n, 1)
  L2_mat <- as.matrix(dist(x))^2
  
  for (i in 2:k) {
    weight <- apply(as.matrix(L2_mat[, centers]), 1, min)
    centers[i] <- sample(1:n, 1, prob = weight)
  }
  
  kmeans(x, x[centers, ], iter_max, nstart, algorithm, trace)
}


get_kmeans_tbl <- function(data, ncmin = 2, ncmax = 5) {
  data <-
    data %>%
    na.omit()
  
  ncs <- ncmin:ncmax
  models <- lapply(ncs, function(nc) {kmeanspp(data[-(1:2)], nc)})
  dunns <- sapply(models, function(object) {dunn(Data = data, clusters = object$cluster)})
  best_model <- models[[which.max(dunns)]]
  
  data %>%
    select(code) %>%
    distinct() %>%
    mutate(cluster = best_model$cluster)
}


kmeans_with <- function(data, with, market, risk_free) {
  if (with == "return") {
    data %>%
      select(1:2, logret)
    
  } else if (with == "market_residual") {
    data %>%
      add_market_residual(market, risk_free) %>%
      select(1:2, market_res)
    
  } else if (with == "factors") {
    data %>%
      select(-logret) %>%
      PCA()
    
  } else if (with == "factors_residual") {
    data %>%
      add_factors_residual(risk_free) %>%
      select(1:2, factors_res)
    
  } else {
    stop("ERROR: argument `with` must be one of ('return', 'market_residual', 'factors', 'factors_residual')")
  }
}


integrate_return <- function(return, weight) {
  weight <- weight / sum(weight)
  log(sum(weight * exp(return)))
}


get_cluster_return <- function(data, time_idx, with, market, risk_free) {
  cluster_df <-
    data %>%
    time_slice(time_idx) %>%
    scale_tbl() %>%
    kmeans_with(with, market, risk_free) %>%
    time_expand() %>%
    get_kmeans_tbl()
  
  data <-
    data %>%
    left_join(cluster_df, by = "code") %>%
    select(code, time, logret, size, cluster) %>%
    mutate(size = lag(size)) %>%
    na.omit() %>%
    group_by(cluster, time) %>%
    summarize(logret = integrate_return(r = logret, w = size)) %>%
    spread(key = cluster, value = logret, sep = "")
  
  x <- data %>% slice(time_idx)
  y <- data %>% slice(last(time_idx) + 1)
  
  if (nrow(x) == length(time_idx) &
      y[["time"]] == unique(data[["time"]])[last(time_idx) + 1]) {
    list(x = x, y = y)
  } else {
    stop("ERROR")
  }
}
