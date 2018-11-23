library(clValid)


time_slice <- function(data, time_idx) {
    data %>% 
        group_by(code) %>% 
        slice(time_idx) %>% 
        ungroup()
}

time_expand <- function(data) {
    cols <- setdiff(1:ncol(data), 1:2)
    
    while (length(unique(data[["time"]])) > 1) {
        lagged <- 
            apply(data[cols], 2, lag) %>% 
            as_tibble() %>% 
            setNames(str_c("x", cols)) # Dummy names
        data <- bind_cols(data, lagged)
        
        data <- 
            data %>% 
            group_by(code) %>% 
            slice(-1) %>% 
            ungroup()
        
        cols <- cols + length(cols)
    }
    return (data)
}

scale_tbl <- function(data, skip) {
    vals <- data[-skip]
    for (i in 1:ncol(vals)) {
        vals[[i]] <- scale(vals[[i]])
    }
    bind_cols(data[skip], vals)
}

PCA <- function(data, threshold=0.8) {
    prfit <- 
        data %>% 
        select(-(1:2)) %>% 
        na.omit() %>% 
        prcomp()
    
    pve <- prfit$sdev^2 %>% (function(x) {x / sum(x)})
    n_pc <- which(cumsum(pve) > threshold)[1]
    
    x_origin <- as.matrix(data[-(1:2)])
    loading <- prfit$rotation[, 1:n_pc]
    x_pc <- x_origin %*% loading
    
    bind_cols(data[1:2], as_tibble(x_pc))
}

add_factors_residual <- function(data) {
    data <-
        data %>% 
        group_by(code) %>% 
        mutate(logret = lead(logret)) %>% 
        ungroup()
    
    y <- data[["logret"]]
    x <- data %>% select(-logret) %>% PCA()
    
    lmfit <- lm(y ~ ., data=x[-(1:2)])
    yhat <- predict(lmfit, newdata=x[-(1:2)])
    res <- y - yhat
    data[["factors_res"]] <- scale(res)
    
    n_time <- length(unique(data[["time"]]))
    data %>% 
        group_by(code) %>% 
        slice(-n_time) %>% 
        ungroup()
}

add_market_residual <- function(data, market) {
    market <- 
        market %>% 
        filter(time %in% unique(data[["time"]]))
    
    data <- 
        data %>% 
        group_by(code) %>% 
        mutate(market = market[["logret"]]) %>% 
        ungroup()
    
    lmfit <- lm(logret ~ market, data=data)
    yhat <- predict(lmfit, newdata=data["market"])
    res <- data[["logret"]] - yhat
    data[["market_res"]] <- scale(res)
    
    return (data)
}

dunn_index <- function(data, ncmin=2, ncmax=6, iter_max=100) {
    data <- 
        data %>% 
        na.omit()
    
    dunns <- NULL
    for (nc in ncmin:ncmax) {
        kmfit <- 
            data %>% 
            select(-(1:2)) %>% 
            kmeans(centers=nc, iter.max=iter_max)
        dunns <- c(dunns, dunn(Data=data, clusters=kmfit$cluster))
    }
    
    dunn_table <- dunns %>% as.matrix %>% signif(3)
    colnames(dunn_table) <- "Dunn index"
    rownames(dunn_table) <- str_c("nc=", ncmin:ncmax)
    
    best_nc <- (ncmin - 1) + which.max(dunns)
    
    return(list("dunns"=dunn, "dunn_table"=dunn_table, "best_nc"=best_nc))
}

get_kmeans_tbl <- function(data, n_cluster) {
    data <- 
        data %>% 
        na.omit()
    
    kmfit <- 
        data %>% 
        select(-(1:2)) %>% 
        kmeans(centers=n_cluster)
    
    cluster_tbl <- 
        data %>% 
        select(code) %>% 
        distinct() %>% 
        mutate(cluster=kmfit$cluster)
    
    return (cluster_tbl)
}

kmeans_with <- function(data, with, market, n_cluster=NULL) {
    if (with == "return") {
        data <- 
            data %>% 
            select(1:2, logret) %>% 
            time_expand()
        
    } else if (with == "market_residual") {
        data <- 
            data %>% 
            add_market_residual(market) %>% 
            select(1:2, market_res) %>% 
            time_expand()
        
    } else if (with == "factors") {
        data <- 
            data %>% 
            select(-logret) %>% 
            PCA() %>% 
            time_expand()
        
    } else if (with == "factors_residual") {
        data <- 
            data %>% 
            add_factors_residual() %>% 
            select(1:2, factors_res) %>% 
            time_expand()
        
    } else {
        stop("ERROR: argument 'with' must be one of 
             ('return', 'market_residual', 'factors', 'factors_residual')")
    }
    
    if (is.null(n_cluster)) {
        best_nc <- dunn_index(data)[["best_nc"]] # Find best number of clusters
        return (get_kmeans_tbl(data, best_nc))
        
    } else {
        return (get_kmeans_tbl(data, n_cluster))
    }
}

integrate_return <- function(return, weight) {
    weight <- weight / sum(weight)
    log(sum(weight * exp(return)))
}

get_cluster_return <- function(data, current_time, n_time, 
                              with, market, n_cluster=NULL) {
    
    # If 'current_time' is a character value such as "2015-1", 
    # convert to the integer index
    if (is.character(current_time)) {
        times <- unique(data[["time"]])
        if (!current_time %in% times) {
            stop("ERROR: invalid 'current_time'")
        } else {
            current_time <- which(times == current_time)
        }
    }
    time_idx <- (current_time - n_time + 1):current_time

    cluster_df <-
        data %>% 
        time_slice(time_idx) %>% 
        scale_tbl(skip=1:2) %>% 
        kmeans_with(with, market, n_cluster)
    
    # x <- 
    #     data %>% 
    #     time_slice(time_idx) %>% 
    #     left_join(cluster_df, by="code") %>% 
    #     select(code, time, logret, size, cluster) %>% 
    #     na.omit() %>% 
    #     group_by(cluster, time) %>% 
    #     summarize(logret = weighted.mean(logret, w=size)) %>% 
    #     spread(key=cluster, value=logret, sep="")
    # 
    # y <-
    #     data %>% 
    #     time_slice(current_time + 1) %>% 
    #     left_join(cluster_df, by="code") %>% 
    #     select(code, time, logret, size, cluster) %>% 
    #     na.omit() %>% 
    #     group_by(cluster, time) %>% 
    #     summarize(logret = weighted.mean(logret, w=size)) %>% 
    #     spread(key=cluster, value=logret, sep="")
    
    data <-
        data %>% 
        left_join(cluster_df, by="code") %>% 
        select(code, time, logret, size, cluster) %>% 
        mutate(size = lag(size)) %>% 
        select(code, time, logret, size, cluster) %>% 
        na.omit() %>% 
        group_by(cluster, time) %>% 
        # summarize(logret = weighted.mean(logret, w=size)) %>% 
        summarize(logret = integrate_return(r=logret, w=size)) %>% 
        spread(key=cluster, value=logret, sep="")
    
    x <- data %>% slice(time_idx)
    y <- data %>% slice(current_time + 1)
    
    
    if (nrow(x) == length(time_idx) &
        y[["time"]] == times[current_time + 1]) {
        return (list(x=x, y=y))
    } else {
        stop("ERROR")
    }
}
