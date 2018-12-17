library(tidyverse)
library(readxl)
source("../src/functions-clustering.R")
source("../src/functions-portfolio.R")


# Random seed
set.seed(123)

# Read data
stock_df <- read_csv("../data/processed/stock_df.csv")
kospi <- read_csv("../data/processed/kospi.csv")
risk_free <- read_csv("../data/processed/risk_free.csv")

# Models
with_list <- c("return", "market_residual", "factors", "factors_residual")
n_time_list <- c(6, 8, 10, 12)
method_list <- c("GMV", "Tangency")

# In-sample
start_list <- str_c(c("2002", "2005", "2008", "2011"), "-4")
end_list <- str_c(c("2005", "2008", "2011", "2014"), "-3")
train_res_list <- list()
for (i in 1:4) {
  start <- start_list[i]
  end <- end_list[i]
  insamp_res_list[[i]] <-
    evaluate_portfolio(stock_df, kospi, risk_free, start, end,
                       with_list, n_time_list, method_list)
}

# Out-sample
start <- "2014-4"
end <- "2017-3"
outsamp_res <- evaluate_portfolio(stock_df, kospi, risk_free, start, end,
                                  with_list, n_time_list, method_list)

# Save results
dir.create("../model")
save(insamp_res_list, file = "../model/insamp_res_list.RData")
save(ousamp_res, file = "../model/outsamp_res.RData")
