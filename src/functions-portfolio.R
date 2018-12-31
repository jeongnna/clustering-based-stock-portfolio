library(quadprog)  # solve.QP()
library(parallel)  # mcmapply()


get_weight <-function(x, method = c("GMV", "Tangency"), risk_free = NULL) {
  n_cluster <- ncol(x)
  zeros <- matrix(0, nrow = n_cluster)
  if (method == "GMV") {
    A <- cbind(matrix(1, nrow = n_cluster), diag(n_cluster))
    b <- c(1, rep(0, n_cluster))
  } else if (method == "Tangency") {
    if (is.null(risk_free)) {
      stop("ERROR: if 'method' is 'Tangency', 'risk_free' must not be NULL.")
    }
    rf <- mean(risk_free[["r"]])
    A <- cbind(matrix(apply(x, 2, mean) - rf), diag(rep(1, n_cluster)))
    b <- c(sum(matrix(apply(x, 2, mean) - rf)), rep(0, n_cluster))
  } else {
    stop("ERROR: invalid 'method'")
  }
  qp <- solve.QP(cov(x), zeros, A, b, meq = 1)
  qp$solution
}

get_portfolio_return <- function(data, timeset, n_time, with, method,
                                 market, risk_free, seed = 123) {
  # set.seed(seed)

  p_return <- numeric(length(timeset))

  for (i in 1:length(timeset)) {
    current_time <- timeset[i]
    time_idx <- (current_time - n_time + 1):current_time
    c_return <- get_cluster_return(data, time_idx, with, market, risk_free)
    x <- as.matrix(c_return[["x"]][-1])
    y <- as.matrix(c_return[["y"]][-1])

    weight <- get_weight(x, method, risk_free %>% slice(time_idx))
    p_return[i] <- integrate_return(r = y, w = weight)
  }

  p_return
}

expand_grid <- function(...) {
  grd <- expand.grid(...) %>%
    as_tibble()
  nc <- ncol(grd)
  text <- str_c("grd %>% arrange(",
                str_c(str_c(".[[", 1:nc, "]]"), collapse = ", "),
                ")")
  ex <- parse(text = text)
  eval(ex)
}

evaluate_portfolio <- function(data, market, risk_free, start, end,
                               with_list, n_time_list, method_list, seed) {
  timeset <- unique(data[["time"]])
  timeset <- str_which(timeset, start):str_which(timeset, end)

  # Model grid
  grd <- expand_grid(with_list, n_time_list, method_list) %>%
    setNames(c("with", "n_time", "method"))
  model_names <- grd %>% unite("name", 1:3) %>% .[[1]]

  # Portfolio returns table
  pr_tbl <- kospi %>% slice(timeset + 1) %>% setNames(c("time", "kospi"))
  cat("from ", start, " to ", end, "\n", sep = "")
  for (with in with_list) {
    for (n_time in n_time_list) {
      for (method in method_list) {
        cat("with: ", with,
            ", n_time: ", n_time,
            ", method: ", method,
            "\n", sep = "")
        pr <- get_portfolio_return(data, timeset, n_time, with,
                                   method, market, risk_free)
        pr_tbl <- bind_cols(pr_tbl, pr = pr)
      }
    }
  }
  colnames(pr_tbl) <- c("time", "kospi", model_names)

  # Model performance summary
  pr_cumsum <- apply(pr_tbl[-(1:2)], 2, function(x) {last(cumsum(x))})
  pr_sd <- diag(sqrt(cov(pr_tbl[-(1:2)])))
  pr_info_rate <-
    (as.matrix(pr_tbl[-(1:2)]) - as.vector(as.matrix(pr_tbl[2]))) %>%
    apply(2, function(x) {mean(x) / sd(x)})
  summ <- bind_cols(grd, cumsum = pr_cumsum, sd = pr_sd, info_r = pr_info_rate)

  list("return" = pr_tbl, "summary" = summ)
}

# evaluate_portfolio <- function(data, market, risk_free, start, end,
#                                with_list, n_time_list, method_list, seed) {
#   timeset <- unique(data[["time"]])
#   timeset <- str_which(timeset, start):str_which(timeset, end)
#
#   # Model grid
#   grd <- expand_grid(with_list, n_time_list, method_list) %>%
#     setNames(c("with", "n_time", "method"))
#   model_names <- grd %>% unite(1:3) %>% .[[1]]
#
#   # Portfolio returns table
#   n1 <- length(with_list)
#   n2 <- length(n_time_list)
#   n3 <- length(method_list)
#   args_list <- list(data      = data,
#                     timeset   = timeset,
#                     market    = market,
#                     risk_free = risk_free)
#   pr_tbl <-
#     mcmapply(FUN      = get_portfolio_return,
#              # with     = rep(with_list, each = n2 * n3),
#              # n_time   = rep(rep(n_time_list, each = n3), n1),
#              # method   = rep(method_list, n1 * n2),
#              with     = grd[[1]],
#              n_time   = grd[[2]],
#              method   = grd[[3]],
#              MoreArgs = args_list,
#              mc.cores = detectCores()) %>%
#     as_tibble() %>%
#     bind_cols(kospi %>% slice(timeset + 1) %>% setNames(c("time", "kospi"))) %>%
#     setNames(c(model_names, "time", "kospi")) %>%
#     select(c("time", "kospi", model_names))
#
#   # Model performance summary
#   pr_cumsum <-
#     pr_tbl %>%
#     select(-time, -kospi) %>%
#     apply(2, function(x) {last(cumsum(x))})
#   pr_sd <-
#     pr_tbl %>%
#     select(-time, -kospi) %>%
#     cov() %>% diag() %>% sqrt()
#   pr_info_rate <-
#     (as.matrix(pr_tbl %>% select(-time, -kospi)) -
#        as.vector(as.matrix(pr_tbl %>% select(kospi)))) %>%
#     apply(2, function(x) {mean(x) / sd(x)})
#   summ <- bind_cols(grd, cumsum = pr_cumsum, sd = pr_sd, info_r = pr_info_rate)
#
#   list("return" = pr_tbl, "summary" = summ)
# }
