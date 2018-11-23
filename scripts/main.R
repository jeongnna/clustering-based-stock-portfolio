rm(list=ls())
source("data-preprocessing.R")
source("clustering.R")
source("portfolio.R")


set.seed(123)

data <- stock_df
start <- "2001-1"
end <- "2014-4"
n_time <- 8
with <- "return" # "return", "market_residual", "factors" or "factors_residual"
market <- kospi # Market return data
nnw <- TRUE

get_portfolio_return(data, start, end, n_time, with, market, nnw)




# dir.create("../RData")
# save.image(file="../RData/main.RData")
