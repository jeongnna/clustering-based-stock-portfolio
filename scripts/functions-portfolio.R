library(quadprog)  # solve.QP()
library(parallel)  # mcmapply()


subset_timeset <- function(timeset, start, end) {
    start_idx <- which(timeset == start)
    end_idx <- which(timeset == end)
    timeset[start_idx:end_idx]
}

get_weight_gmv <- function(x) {
    n_cluster <- ncol(x)
        A <- cbind(matrix(1, nrow = n_cluster), diag(n_cluster))
        b <- c(1, rep(0, n_cluster))
        zeros <- matrix(0, nrow = n_cluster)
        qp <- solve.QP(cov(x), zeros, A, b, meq = 1)
        return (qp$solution)
}

get_weight_tan <- function(x, risk_free) {
    n_cluster <- ncol(x)
    rf <- mean(risk_free[["r"]])
        A <- cbind(matrix(apply(x, 2, mean) - rf), diag(rep(1, n_cluster)))
        b <- c(sum(matrix(apply(x, 2, mean) - rf)), rep(0, n_cluster))
        zeros <- matrix(0, nrow = n_cluster)
        qp <- solve.QP(cov(x), zeros, A, b, meq = 1)
        qp$solution
}

get_portfolio_return <- function(data, timeset, n_time, with, method,
                                 market, risk_free) {
    cat("with: ", with,
        ", n_time: ", n_time,
        ", method: ", method,
        "\n", sep = "")

    p_return <- numeric(length(timeset))

    for (i in 1:length(timeset)) {
        current_time <- timeset[i]
        time_idx <- (current_time - n_time + 1):current_time

        cat("i = ", i, "... ", sep = "")
        c_return <- get_cluster_return(data, time_idx, with, market, risk_free)
        cat("Good!\n")
        x <- as.matrix(c_return[["x"]][-1])
        y <- as.matrix(c_return[["y"]][-1])

        if (method == "GMV") {
            weight <- get_weight_gmv(x)
        } else if (method == "Tangency") {
            weight <- get_weight_tan(x, risk_free %>% slice(time_idx))
        }
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

evaluate_portfolio <- function(data, market, risk_free,
                               start, end,
                               with_setlist, n_time_setlist, method_setlist) {
    # timeset <- subset_timeset(unique(data[["time"]]), start, end)
    # timeset <- which(unique(data[["time"]]) %in% timeset)
    timeset <- unique(data[["time"]])
    timeset <- str_which(timeset, start):str_which(timeset, end)

    grd <- expand_grid(with_setlist, n_time_setlist, method_setlist) %>%
        setNames(c("with", "n_time", "method"))

    pr_mat <- kospi %>% slice(timeset + 1) %>% setNames(c("time", "kospi"))

    for (with in with_setlist) {
        for (n_time in n_time_setlist) {
            for (method in method_setlist) {
                cat("with: ", with,
                    ", n_time: ", n_time,
                    ", method: ", method,
                    "\n", sep = "")
                pr <- get_portfolio_return(data, timeset, n_time, with,
                                           method, market, risk_free)
                pr_mat <- bind_cols(pr_mat, pr = pr)
            }
        }
    }

    colnames(pr_mat) <- c("time", "kospi", unite(grd, 1:3)[[1]])

    pr_cumsum <- apply(pr_mat[-(1:2)], 2, function(x) {last(cumsum(x))})
    pr_sd <- diag(sqrt(cov(pr_mat[-(1:2)])))
    pr_info_rate <-
        (as.matrix(pr_mat[-(1:2)]) - as.vector(as.matrix(pr_mat[2]))) %>%
        apply(2, function(x) {mean(x) / sd(x)})
    summ <- bind_cols(grd, cumsum = pr_cumsum, sd = pr_sd, info_r = pr_info_rate)

    list("return" = pr_mat, "summary" = summ)
}

evaluate_portfolio2 <- function(data, market, risk_free,
                                start, end,
                                with_setlist, n_time_setlist, method_setlist) {
    # timeset <- subset_timeset(unique(data[["time"]]), start, end)
    # timeset <- which(unique(data[["time"]]) %in% timeset)
    timeset <- unique(data[["time"]])
    timeset <- str_which(timeset, start):str_which(timeset, end)

    grd <- expand_grid(with_setlist, n_time_setlist, method_setlist) %>%
        setNames(c("with", "n_time", "method"))

    n1 <- length(with_setlist)
    n2 <- length(n_time_setlist)
    n3 <- length(method_setlist)
    args_list <- list(data      = data,
                      timeset   = timeset,
                      market    = market,
                      risk_free = risk_free)
    pr_mat <- mcmapply(FUN      = get_portfolio_return,
                       with     = rep(with_setlist, each = n2 * n3),
                       n_time   = rep(n_time_setlist, each = n1 * n3),
                       method   = rep(method_setlist, each = n1 * n2),
                       MoreArgs = args_list,
                       mc.cores = 4)
    pr_mat <- bind_cols(kospi %>%
                            slice(timeset + 1) %>%
                            setNames(c("time", "kospi")),
                        pr_mat %>% as_tibble())
    colnames(pr_mat) <- c("time", "kospi", unite(grd, 1:3)[[1]])

    pr_cumsum <- apply(pr_mat[-(1:2)], 2, function(x) {last(cumsum(x))})
    pr_sd <- diag(sqrt(cov(pr_mat[-(1:2)])))
    pr_info_rate <-
        (as.matrix(pr_mat[-(1:2)]) - as.vector(as.matrix(pr_mat[2]))) %>%
        apply(2, function(x) {mean(x) / sd(x)})
    summ <- bind_cols(grd, cumsum = pr_cumsum, sd = pr_sd, info_r = pr_info_rate)

    list("return" = pr_mat, "summary" = summ)
}
