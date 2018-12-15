library(tidyverse)
library(readxl)
source("../src/data-preprocessing.R")
source("../src/functions-clustering.R")
source("../src/functions-portfolio.R")


# Set random seed
set.seed(123)


# Read data
stock_df <- read_csv("../data/processed/stock_df.csv")
kospi <- read_csv("../data/processed/kospi.csv")
risk_free <- read_csv("../data/processed/risk_free.csv")


# Training
start_list <- str_c(c("2002", "2005", "2008", "2011"), "-4")
end_list <- str_c(c("2005", "2008", "2011", "2014"), "-3")

with_setlist <- c("return", "market_residual", "factors", "factors_residual")
n_time_setlist <- c(6, 8, 10, 12)
method_setlist <- c("GMV", "Tangency")

train_result_list <- list()
for (i in 1:4) {
    start <- start_list[i]
    end <- end_list[i]
    train_result_list[[i]] <-
        evaluate_portfolio(stock_df, kospi, risk_free,
                           start, end,
                           with_setlist, n_time_setlist, method_setlist)
}


# Test
start <- "2014-4"
end <- "2017-3"

with_setlist <- c("return", "market_residual", "factors", "factors_residual")
n_time_setlist <- c(6, 8, 10, 12)
method_setlist <- c("GMV", "Tangency")

test_result <- evaluate_portfolio(stock_df, kospi, risk_free,
                           start, end,
                           with_setlist, n_time_setlist, method_setlist)


# Save workspace
save.image(file = "../RData/main.RData")
