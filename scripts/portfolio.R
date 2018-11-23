library(quadprog)


subset_timeset <- function(timeset, start, end) {
    start <- which(timeset == start)
    end <- which(timeset == end)
    timeset[start:end]
}

get_weight <- function(x, nonnegative) {
    n_cluster <- ncol(x)
    
    if (nonnegative == TRUE) {
        A <- cbind(matrix(1, nrow=n_cluster), diag(n_cluster))
        b <- c(1, rep(0, n_cluster))
        zeros <- matrix(0, nrow=n_cluster)
        qp <- solve.QP(cov(x), zeros, A, b, meq = 1)
        return (qp$solution)
        
    } else {
        A <- matrix(1, nrow=n_cluster)
        b <- 1
        zeros <- matrix(0, nrow=n_cluster)
        qp <- solve.QP(cov(x), zeros, A, b, meq = 1)
        return (qp$solution)
    }
}

get_portfolio_return <- function(data, start, end, n_time, with, market, nnw) {
    p_return <- NULL
    timeset <- subset_timeset(unique(data[["time"]]), start, end)
    for (current_time in timeset) {
        c_return <- get_cluster_return(data, current_time, n_time, with, market)
        x <- as.matrix(c_return[["x"]][-1])
        y <- as.matrix(c_return[["y"]][-1])
        weight <- get_weight(x, nonnegative=nnw)
        p_return <- c(p_return, integrate_return(r=y, w=weight))
    }
    p_return
}
