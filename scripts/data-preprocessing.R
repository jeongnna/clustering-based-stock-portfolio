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

preprocess <- function(file_names, var_names) {
    
    # Load data
    vals <- NULL
    for(name in file_names) {
        file_path <- paste0("../data/", name, ".xls")
        data <- read_excel(file_path, skip=5)[-1, -1] %>% reshape_long()
        vals <- bind_cols(vals, data["val"])
    }
    colnames(vals) <- var_names
    vals <- vals %>% nan2na()
    
    
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
    
    stock_df <- 
        data %>% 
        select(code, time) %>% 
        bind_cols(features)
    
    
    # Create stock attributes table
    stock_attr <-
        data %>%
        select(code, name) %>%
        distinct()
    
    
    return (list(data=stock_df, attr=stock_attr))
}

merge_time <- function(ppc1, ppc2) {
    
    # Merge data
    data1 <- ppc1[["data"]]
    data2 <- ppc2[["data"]]
    
    data1 <- semi_join(data1, data2, by="code")
    data2 <- semi_join(data2, data1, by="code")
    
    data1 <- 
        data1 %>% 
        bind_rows(data2) %>% 
        arrange(code, time)
    
    # Merge attr
    attr1 <- ppc1[["attr"]]
    attr1 <- semi_join(attr1, data2, by="code")
    
    return (list(data=data1, attr=attr1))
}


# ---- Data preprocessing ----

# Stock data
file_names <- c("Leverage", "NetProfit", "Equity",
                "Asset", "AssetGrowth", "Trade_Amount",
                "PCR", "PER", "StockPrice", "Stock_Number",
                "MarketCap", "EquityTurnover", "Volatility")

var_names <- c("leverage", "net_profit", "equity",
               "asset", "asset_growth", "trade_amount",
               "pcr", "per", "price", "stock_num",
               "market_cap", "equity_turnover", "volatility")

ppc <- preprocess(file_names, var_names)
ppc_2018 <- preprocess(str_c("data2018/", file_names, "_2018"), var_names)
ppc <- merge_time(ppc, ppc_2018)

stock_df <- ppc[["data"]]
stock_attr <- ppc[["attr"]]

rm(list=c("ppc", "ppc_2018"))


# KOSPI index
kospi <- 
    read_excel("../data/KOSPI_index.xlsx", col_names=FALSE) %>% 
    setNames(c("time", "price")) %>% 
    group_by(year(time), as_quarter(month(time))) %>% 
    summarize(price = mean(price, na.rm=TRUE)) %>% 
    ungroup() %>% 
    transmute(time = str_c(.[[1]], .[[2]], sep="-"), 
              logret = c(NA, diff(log(price))))


# Save workspace
save.image(file="../RData/data_preprocessing.RData")
