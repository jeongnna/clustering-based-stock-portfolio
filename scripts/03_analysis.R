library(tidyverse)


load("outputs/valid_res_list.RData")
load("outputs/test_res.RData")

kospi <- read_csv("data/processed/kospi.csv")


# Summary -----------------------------------------------------------------

select_model <- function(x) {
  x %>%
    select(time, kospi, factors_8_GMV, factors_8_Tangency) %>%
    setNames(c("time", "kospi", "GMV", "Tangency"))
}

summarize_sd <- function(x) {
  x %>%
    summarize(
      kospi = sd(kospi),
      GMV = sd(GMV),
      Tangency = sd(Tangency)
    )
}

sd_summ <- NULL
for (i in 1:4) {
  newline <-
    valid_res_list[[i]]$return %>%
    select_model() %>%
    summarize_sd() %>%
    as.matrix()
  sd_summ <- rbind(sd_summ, newline)
}
sd_summ <- t(sd_summ)[3:1, ]
sd_summ <- cbind(sd_summ, apply(sd_summ, 1, mean))
colnames(sd_summ) <- c(str_c("Partition ", 1:4), "Average")

write.csv(sd_summ, "outputs/sd_summ.csv", row.names = TRUE)


##

test_res$return %>%
  select_model() %>%
  summarize_sd() %>%
  as.matrix()

####

method_avg <- function(x) {
  n <- nrow(x)
  y <-
    x %>%
    select(cumsum, sd, info_r) %>%
    as.matrix() %>%
    matrix(nrow = 2) %>%
    apply(2, mean) %>%
    matrix(nrow = 0.5 * n) %>%
    round(3) %>%
    as_tibble() %>%
    setNames(c("cumsum", "sd", "info_r"))
  x %>%
    select(with, n_time) %>%
    distinct() %>%
    bind_cols(y)
}


n <- nrow(valid_res_list[[1]]$summary)
summ_mat <- matrix(0, nrow = n, ncol = 3)

for (i in seq_along(valid_res_list)) {
  summ_mat <-
    summ_mat +
    valid_res_list[[i]]$summary %>%
    select(cumsum, sd, info_r) %>%
    as.matrix()
}

summ_mat <-
  (summ_mat / length(valid_res_list)) %>%
  as_tibble() %>%
  setNames(c("cumsum", "sd", "info_r"))

valid_summ <-
  valid_res_list[[1]]$summary %>%
  select(with, n_time, method) %>%
  bind_cols(summ_mat) %>%
  method_avg() %>%
  mutate(rank = min_rank(-info_r))

test_summ <-
  test_res$summary %>%
  method_avg() %>%
  mutate(rank = min_rank(-info_r))


# Validation plot ----------------------------------------------------------

cumret_plot <- function(x) {
  x %>%
    gather(key = asset, value = logret, 2:4) %>%
    mutate(asset = factor(asset, levels = c("Tangency", "GMV", "kospi"))) %>%
    group_by(asset) %>%
    mutate(cumret = cumsum(logret)) %>%
    ggplot(aes(x = time, y = cumret, group = asset, col = asset)) +
    geom_point() +
    geom_line() +
    scale_color_discrete(name = NULL, labels = c("Tangency", "GMV", "KOSPI")) +
    geom_hline(yintercept = 0, col = "grey") +
    xlab("Time") + ylab("Cumulative log return") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))
}

valid_cumret_total_plot <-
  bind_rows(
    valid_res_list[[1]]$return,
    valid_res_list[[2]]$return,
    valid_res_list[[3]]$return,
    valid_res_list[[4]]$return
  ) %>%
  select_model() %>%
  cumret_plot()

valid_cumret_part_plot <- list()
for (i in 1:4) {
  valid_cumret_part_plot[[i]] <-
    valid_res_list[[i]]$return %>%
    select_model() %>%
    cumret_plot()
}

ggsave(
  "outputs/valid-plot_00.png",
  valid_cumret_total_plot,
  width = 10, height = 5
)
ggsave(
  "outputs/valid-plot_01.png",
  valid_cumret_part_plot[[1]] + ylim(-0.2, 1),
  width = 8, height = 5
)
ggsave(
  "outputs/valid-plot_02.png",
  valid_cumret_part_plot[[2]] + ylim(-0.2, 1),
  width = 8, height = 5
)
ggsave(
  "outputs/valid-plot_03.png",
  valid_cumret_part_plot[[3]] + ylim(-0.2, 1),
  width = 8, height = 5
)
ggsave(
  "outputs/valid-plot_04.png",
  valid_cumret_part_plot[[4]] + ylim(-0.2, 1),
  width = 8, height = 5
)


# Test plot ---------------------------------------------------------

test_ret <-
  test_res$return %>%
  select(time, kospi, factors_8_GMV, factors_8_Tangency) %>%
  setNames(c("time", "kospi", "GMV", "Tangency"))

test_ret[2:4] <- lapply(test_ret[2:4], cumsum)

test_cumret_plot <-
  test_ret %>%
  gather(key = asset, value = logret, 2:4) %>%
  mutate(asset = factor(asset, levels = c("Tangency", "GMV", "kospi"))) %>%
  ggplot(aes(x = time, y = logret, group = asset, col = asset)) +
  geom_point() +
  geom_line() +
  scale_color_discrete(name = NULL, labels = c("Tangency", "GMV", "KOSPI")) +
  geom_hline(yintercept = 0, col = "grey") +
  xlab("Time") + ylab("Cumulative log return") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

ggsave("outputs/test-plot.png", test_cumret_plot, width = 8, height = 5)
