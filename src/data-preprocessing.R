library(tidyverse)
library(readxl)
library(lubridate)


# Functions ---------------------------------------------------------------

as_quarter <- function(x) {
    # 월(month) 값을 분기(quarter) 값으로 전환한다.
    # Example: (1, 2, 3, 4, 5, 6) -> (1, 1, 1, 2, 2, 2)
    #
    # Args:
    #   x: 1 ~ 12 사이의 integer vector
    #
    # Returns:
    #   월 -> 분기로 전환된 integer vector

    x <- as.numeric(x)

    # 1 ~ 12 사이의 integer vector가 아닐 경우 stop
    if (!all(x %in% 1:12)) {
        stop("range of months exceeds [1, 12].")
    }

    (x - 1) %/% 3 + 1
}

is_quarter_interval <- function(x) {
    # 시간 변수가
    # 날짜 형식(ex: "20010131")인지 혹은
    # 분기 형식(ex: "2001/1 Quarter)인지 검사한다.
    #
    # Args:
    #   x: charactor vector
    #
    # Returns:
    #   날짜 형식이면 FALSE, 분기 형식이면 TRUE

    length(grep("/", x)) > 0
}

reshape_long <- function(data) {
    # Short form 데이터를 long form 데이터로 전환한다.
    # 시간 변수가 날짜 형식인 경우 분기 형식으로 전환한다.
    #
    # Args:
    #   data: raw data를 불러들인 data frame
    #
    # Returns:
    #   long form으로 전환된 data frame

    colnames(data)[1:2] <- c("code", "name")
    data <- data %>% gather("time", "val", -(1:2))
    data[["val"]] <- as.numeric(data[["val"]])

    if (is_quarter_interval(data$time[1])) {
        data <- data %>% separate(time, into = c("year", "quarter"))

        data[["quarter"]] <-
            data[["quarter"]] %>%
            plyr::revalue(c("Semi" = 2,
                            "Annual" = 4)) %>%
            as.numeric()

        data <- data %>% arrange(code, year, quarter)

    } else {
        data[["time"]] <- data[["time"]] %>% as.Date(format = "%Y%m%d")

        data <-
            data %>%
            group_by(code, name,
                     year = year(time),
                     quarter = as_quarter(month(time))) %>%
            summarize(val = mean(val, na.rm = TRUE)) %>%
            ungroup()
    }

    data <- data %>% unite("time", "year", "quarter", sep = "-")
    col_order <- c("code", "name", "time", "val")
    data[col_order]
}

nan2na <- function(data) {
    # 데이터의 NaN인 부분을 NA로 전환한다.
    #
    # Args:
    #   data frame
    #
    # Returns:
    #   NaN이 NA로 전환된 data frame

    for (i in 1:ncol(data)) {
        data[[i]][is.nan(data[[i]])] <- NA
    }
    data
}

preprocess <- function(path, file_names, var_names, extention = ".xls") {
    # Args:
    #   path: 데이터 경로
    #   file_names: 불러올 데이터 파일 이름
    #   var_names: 각 데이터에 지정할 변수명
    #   extention: 불러올 데이터 파일 형식
    #
    # Returns:
    #   전처리 완료된 데이터

    # 데이터를 불러오고 long form으로 전환 후 변수 값들만 모은다.
    vals <- NULL
    for(name in file_names) {
        file_path <- paste0(path, name, extention)
        data <- read_excel(file_path, skip = 5)[-1, -1] %>% reshape_long()
        vals <- bind_cols(vals, data["val"])
    }
    colnames(vals) <- var_names
    vals <- vals %>% nan2na()  # NaN을 NA로 전환

    # 특징값을 만들어내고 분석에 사용할 데이터를 생성한다.
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
    stock_tbl <-
        data %>%
        select(code, time) %>%
        bind_cols(features)

    # Create stock attributes table
    stock_attr <-
        data %>%
        select(code, name) %>%
        distinct()

    list(data = stock_tbl, attr = stock_attr)
}


# Data preprocessing ------------------------------------------------------

# Stock data
file_names <- c("Leverage", "NetProfit", "Equity",
                "Asset", "AssetGrowth", "Trade_Amount",
                "PCR", "PER", "StockPrice", "Stock_Number",
                "MarketCap", "EquityTurnover", "Volatility")

var_names <- c("leverage", "net_profit", "equity",
               "asset", "asset_growth", "trade_amount",
               "pcr", "per", "price", "stock_num",
               "market_cap", "equity_turnover", "volatility")

ppc <- preprocess("../data/raw/", file_names, var_names)

stock_tbl <- ppc[["data"]]
stock_attr <- ppc[["attr"]]

rm(ppc)


# KOSPI index
kospi <-
    read_excel("../data/raw/KOSPI_index.xlsx", col_names = FALSE) %>%
    setNames(c("time", "price")) %>%
    group_by(year(time), as_quarter(month(time))) %>%
    summarize(price = mean(price, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(time = str_c(.[[1]], .[[2]], sep = "-"),
              logret = c(NA, diff(log(price))))
kospi <- 
    kospi %>% 
    filter(!time %in% str_c("2018", 1:4, sep = "-"))


# Risk-free rate
risk_free <-
    read_excel("../data/raw/CD_RiskFree.xlsx") %>%
    setNames(c("time", "r")) %>%
    mutate(r = log(1 + r/100),
           time = time %>%
               str_extract("[:digit:]+/[:digit:]") %>%
               str_replace("/", "-"))
risk_free <- 
    risk_free %>% 
    filter(!time %in% str_c("2018", 1:4, sep = "-"))


# Save processed data
write.csv(stock_tbl, "../data/processed/stock.csv", row.names=FALSE)
write.csv(kospi, "../data/processed/kospi.csv", row.names=FALSE)
write.csv(risk_free, "../data/processed/risk_free.csv", row.names=FALSE)
