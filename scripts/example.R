rm(list=ls()) # Remove all objects in the workspace (not necessary)


library(tidyverse)
load("../RData/data_preprocessing.RData")
source("clustering.R")


# Example code
current_time <- "2015-1"
n_time <- 4
with <- "return" # "return", "market_residual", "factors" or "factors_residual"
market <- kospi # Market return data

# Number of clusters will be automatically selected by Dunn index
get_cluster_index(stock_df, current_time, n_time, with, market)
# or
get_cluster_index(stock_df, "2015-1", 4, "return", kospi)
get_cluster_index(stock_df, "2015-2", 4, "return", kospi)
get_cluster_index(stock_df, "2015-3", 8, "factors", kospi)



# Use like this:
cluster_index <- get_cluster_index(data = stock_df, 
                                   current_time = "2015-1", 
                                   n_time = 4, 
                                   with = "return", 
                                   market = kospi)
cluster_index$x
cluster_index$y

# Convert to matrix:
as.matrix(cluster_index$x[-1])
as.matrix(cluster_index$y[-1])
