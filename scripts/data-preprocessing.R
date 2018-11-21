library(tidyverse)
library(readxl)
library(lubridate)


# ---- Functions ----

as_quarter <- function(x) {
    # This function converts 'month' to 'quarter'.
    # For example, (1, 2, 3, 4, 5, 6) -> (1, 1, 1, 2, 2, 2)
    
    x <- as.numeric(x)
    
    if (!all(x %in% 1:12)) {
        stop("range of months exceeds [1, 12].")
    }
    
    return ((x - 1) %/% 3 + 1)
}

is_quarter_interval <- function(x) {
    return (length(grep("/", x)) > 0)
}

reshape_long <- function(data) {
    colnames(data)[1:2] <- c("code", "name")
    data <- data %>% gather("time", "val", -(1:2))
    data[["val"]] <- as.numeric(data[["val"]])

    if (is_quarter_interval(data$time[1])) {
        data <- data %>% separate(time, into=c("year", "quarter"))
        
        data[["quarter"]] <-
            data[["quarter"]] %>%
            plyr::revalue(c("Semi" = 2,
                            "Annual" = 4)) %>% 
            as.numeric()

        data <- data %>% arrange(code, year, quarter)
        
    } else {
        data[["time"]] <- data[["time"]] %>% as.Date(format="%Y%m%d")

        data <-
            data %>%
            group_by(code, name,
                     year=year(time),
                     quarter=as_quarter(month(time))) %>%
            summarize(val=mean(val, na.rm=TRUE)) %>% 
            ungroup()
    }
    
    data <- data %>% unite("time", "year", "quarter", sep="-")
    col_order <- c("code", "name", "time", "val")
    data <- data[col_order]

    return (data)
}

nan2na <- function(data) {
    for (i in 1:ncol(data)) {
        data[[i]][is.nan(data[[i]])] <- NA
    }
    
    return (data)
}


# ---- Data preprocessing ----

# Load data
file_names <- c("Leverage", "NetProfit", "Equity",
                "Asset", "AssetGrowth", "Trade_Amount",
                "PCR", "PER", "StockPrice", "Stock_Number",
                "MarketCap", "EquityTurnover", "Volatility")

var_names <- c("leverage", "net_profit", "equity",
               "asset", "asset_growth", "trade_amount",
               "pcr", "per", "price", "stock_num",
               "market_cap", "equity_turnover", "volatility")

vals <- NULL
for(name in file_names) {
    file_path <- paste0("../data/", name, ".xls")
    data <- read_excel(file_path, skip=5)[-1, -1] %>% reshape_long()
    vals <- bind_cols(vals, data["val"])
}
colnames(vals) <- var_names
vals <- vals %>% nan2na()


kospi <- 
    read_excel("../data/KOSPI_index.xlsx", col_names=FALSE) %>% 
    setNames(c("time", "price")) %>% 
    filter(year(time) %in% 1997:2017) %>% 
    group_by(year(time), as_quarter(month(time))) %>% 
    summarize(price = mean(price, na.rm=TRUE)) %>% 
    ungroup() %>% 
    transmute(time = str_c(.[[1]], .[[2]], sep="-"), 
              logret = c(NA, diff(log(price))))


# Create stock attributes table
stock_attr <-
    data %>%
    group_by(code, name) %>%
    count() %>% 
    ungroup()


# Create stock data frame
features <- 
    vals %>% 
    transmute(leverage = leverage,
              asset_growth = asset_growth,
              sharesturnover = trade_amount / stock_num,
              roa = net_profit / asset,
              roe = net_profit / equity,
              size = market_cap,
              pcr = pcr,
              per = per,
              equity_turnover = equity_turnover,
              volatility = volatility,
              logret = c(NA, diff(log(price))))


# stock_df <-
#     bind_cols(data[c("code", "time")], features)
stock_df <- 
    data %>% 
    select(code, time) %>% 
    bind_cols(features)


# Log-transformation & Scaling
# stock_df[["assets_rate"]] <- (stock_df[["assets_rate"]] + 100) / 100
# log_vars <- c("leverage", "assets_rate", "turnover", "pcr", "per")
# stock_df[log_vars] <- lapply(stock_df[log_vars], log)
# stock_df[-(1:2)] <- scale(stock_df[-(1:2)])


# # Create 3-dimensional stock matrix
# #     dim[1]: each stock item
# #     dim[2]: time
# #     dim[3]: features
# nstock <- nrow(stock_attr)
# ntime <- stock_attr$n[1]
# nvar <- ncol(stock_df) - 2
# stock_mat <- array(dim=c(nstock, ntime, nvar))
# 
# for (i in 1:nstock) {
#     wb <- 1 + (i-1) * ntime
#     we <- wb + ntime - 1
#     for (k in 1:nvar) {
#         stock_mat[i, , k] <- stock_df[[k+2]][wb:we]
#     }
# }
# stock_mat <- stock_mat[, -1, ]


# Save workspace
save.image(file="../RData/data_preprocessing.RData")
