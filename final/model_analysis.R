# Final Model Tuning ----
# Analysis of tuned and trained models (comparisons)
# Select final model
# Fit & analyze final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

#Load Tuning Results
load(here("results/lm_results.rda"))
load(here("results/en_tune_r1.rda"))
load(here("results/en_tune_r2.rda"))
load(here("results/knn_tune_r1.rda"))
load(here("results/knn_tune_r2.rda"))
load(here("results/bt_res1.rda")) 
load(here("results/bt_res2.rda"))

#Autoplots
auto_en1 <- autoplot(en_tune_r1,  metric = "rmse")
auto_en2 <- autoplot(en_tune_r2,  metric = "rmse")
auto_knn1 <- autoplot(knn_tune_r1,  metric = "rmse")
auto_knn2 <- autoplot(knn_tune_r2,  metric = "rmse")
auto_bt1 <- autoplot(bt_res1, metric = "rmse")
auto_bt2 <- autoplot(bt_res2, metric = "rmse")

# Collect Best Metrics: LM
lm_metrics <- collect_metrics(lm_results) |>
  filter(.metric == "rmse")

save(lm_metrics, file = ("results/lm_metrics"))

#Collect Best Metrics: EN 
en_best1<- select_best(en_tune_r1, metric = "rmse")
en_best2 <- select_best(en_tune_r2, metric = "rmse")

save(en_best1, file = ("results/en_best1.rda"))
save(en_best2, file = ("results/en_best2.rda"))

#Collect Best Metrics: EN
en_best1_metrics <- collect_metrics(en_tune_r1) %>%
  filter(.metric == "rmse",
         penalty  == en_best1$penalty,
         mixture  == en_best1$mixture)

en_best2_metrics <- collect_metrics(en_tune_r2) %>%
  filter(.metric == "rmse",
         penalty  == en_best2$penalty,
         mixture  == en_best2$mixture)

#Collect Best Metrics : KNN

knn_best1<- select_best(knn_tune_r1, metric = "rmse")
knn_best2 <- select_best(knn_tune_r2, metric = "rmse")

save(knn_best1, file = ("results/knn_best1.rda"))
save(knn_best2, file = ("results/knn_best2.rda"))

knn_best1_metrics <- collect_metrics(knn_tune_r1) %>%
  filter(.metric == "rmse",
         neighbors == knn_best1$neighbors)
knn_best2_metrics <- collect_metrics(knn_tune_r2) %>%
  filter(.metric == "rmse",
         neighbors == knn_best2$neighbors)

#Boosted Tree Model
bt_best1 <- select_best(bt_res1_final, metric = "rmse")
bt_best2 <- select_best(bt_res2_final, metric = "rmse")
save(bt_best1, file = here("results/bt_best1.rda"))
save(bt_best2, file = here("results/bt_best2.rda"))

bt_best1_metrics <- collect_metrics(bt_res1) %>%
  filter(.metric    == "rmse",
         tree_depth == bt_best1$tree_depth,
         min_n      == bt_best1$min_n)

bt_best2_metrics <- collect_metrics(bt_res2) %>%
  filter(.metric    == "rmse",
         tree_depth == bt_best2$tree_depth,
         min_n      == bt_best2$min_n)

# Saving Plots
ggsave(here("plots/autoplot_en_r1.png"),  plot = autoplot(en_tune_r1),  width = 8, height = 5, dpi = 300)
ggsave(here("plots/autoplot_en_r2.png"),  plot = autoplot(en_tune_r2),  width = 8, height = 5, dpi = 300)
ggsave(here("plots/autoplot_knn_r1.png"), plot = autoplot(knn_tune_r1), width = 8, height = 5, dpi = 300)
ggsave(here("plots/autoplot_knn_r2.png"), plot = autoplot(knn_tune_r2), width = 8, height = 5, dpi = 300)
ggsave(here("results/autoplot_bt_r1.png"), plot = autoplot(bt_res1), width = 10, height = 7, dpi = 300)
ggsave(here("results/autoplot_bt_r2.png"), plot = autoplot(bt_res2), width = 10, height = 7, dpi = 300)

