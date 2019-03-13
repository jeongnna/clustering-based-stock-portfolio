library(tidyverse)
library(readxl)
library(lubridate)
source("src/preprocessing-utils")


# stock data
path <- "data/raw/"
file_names <- c("asset", "asset-growth", "equity", "equity-turnover",
                "leverage", "market-cap", "net-profit", "pcr", "per",
                "stock-number", "stock-price", "trade-amount", "volatility")
var_names <- c("asset", "asset_growth", "equity", "equity_turnover",
               "leverage", "market_cap", "net_profit", "pcr", "per",
               "stock_num", "price", "trade_amount", "volatility")
stock_tbl <- preprocess(path, file_names, var_names)

# KOSPI index
kospi <-
  read_excel("data/raw/kospi-index.xlsx", col_names = FALSE) %>%
  setNames(c("time", "price")) %>%
  group_by(year(time), as_quarter(month(time))) %>%
  summarize(price = mean(price, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(
    time = str_c(.[[1]], .[[2]], sep = "-"),
    logret = c(NA, diff(log(price)))
  )

# risk-free rate
risk_free <-
  read_excel("data/raw/cd-risk-free.xlsx") %>%
  setNames(c("time", "r")) %>%
  mutate(
    r = log(1 + r / 100),
    time = 
      time %>%
      str_extract("[:digit:]+/[:digit:]") %>%
      str_replace("/", "-")
  )

# save processed data
dir.create("data/processed", showWarnings = FALSE)
write_csv(stock_tbl, "data/processed/stock.csv")
write_csv(kospi, "data/processed/kospi.csv")
write_csv(risk_free, "data/processed/risk_free.csv")
