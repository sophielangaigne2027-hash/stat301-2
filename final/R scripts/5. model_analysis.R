#Model Comparison

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

#Load Tuning Results
load(here("final/results/lm_results.rda"))
load(here("final/results/en_tune_r1.rda"))
load(here("final/results/en_tune_r2.rda"))
load(here("final/results/knn_tune_r1.rda"))
load(here("final/results/knn_tune_r2.rda"))
load(here("final/results/bt_res1.rda")) 
load(here("final/results/bt_res2.rda"))
load(here("final/results/rf_tune_r1.rda")) 
load(here("final/results/rf_tune_r2.rda"))
load(here("final/results/null_metrics.rda"))
load(here("final/results/baseline_metrics.rda"))

#Autoplots
auto_en1 <- autoplot(en_tune_r1,  metric = "rmse")
auto_en2 <- autoplot(en_tune_r2,  metric = "rmse")
auto_knn1 <- autoplot(knn_tune_r1,  metric = "rmse")
auto_knn2 <- autoplot(knn_tune_r2,  metric = "rmse")
auto_bt1 <- autoplot(bt_res1, metric = "rmse")
auto_bt2 <- autoplot(bt_res2, metric = "rmse")
auto_rf1<- autoplot(rf_tune_r1, metric = "rmse")
auto_rf2 <- autoplot(rf_tune_r2, metric = "rmse")

# Collect Best Metrics: LM
lm_metrics <- collect_metrics(lm_results) |>
  filter(.metric == "rmse")

save(lm_metrics, file = ("final/results/lm_metrics"))

#Collect Best Metrics: EN 
en_best1<- select_best(en_tune_r1, metric = "rmse")
en_best2 <- select_best(en_tune_r2, metric = "rmse")

save(en_best1, file = ("final/results/en_best1.rda"))
save(en_best2, file = ("final/results/en_best2.rda"))

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

save(knn_best1, file = ("final/results/knn_best1.rda"))
save(knn_best2, file = ("final/results/knn_best2.rda"))

knn_best1_metrics <- collect_metrics(knn_tune_r1) %>%
  filter(.metric == "rmse",
         neighbors == knn_best1$neighbors)
knn_best2_metrics <- collect_metrics(knn_tune_r2) %>%
  filter(.metric == "rmse",
         neighbors == knn_best2$neighbors)

#Boosted Tree Model
bt_best1 <- select_best(bt_res1, metric = "rmse")
bt_best2 <- select_best(bt_res2, metric = "rmse")
save(bt_best1, file = here("final/results/bt_best1.rda"))
save(bt_best2, file = here("final/results/bt_best2.rda"))

bt_best1_metrics <- collect_metrics(bt_res1) %>%
  filter(.metric    == "rmse",
         tree_depth == bt_best1$tree_depth,
         min_n      == bt_best1$min_n)

bt_best2_metrics <- collect_metrics(bt_res2) %>%
  filter(.metric    == "rmse",
         tree_depth == bt_best2$tree_depth,
         min_n      == bt_best2$min_n)
# Random Forest Model
#Random Forest Model
rf_best1 <- select_best(rf_tune_r1, metric = "rmse")
rf_best2 <- select_best(rf_tune_r2, metric = "rmse")

save(rf_best1, file = here("final/results/rf_best1.rda"))
save(rf_best2, file = here("final/results/rf_best2.rda"))

rf_best1_metrics <- collect_metrics(rf_tune_r1)%>%
  filter(.metric == "rmse",
         mtry    == rf_best1$mtry,
         min_n   == rf_best1$min_n)

rf_best2_metrics <- collect_metrics(rf_tune_r2) %>%
  filter(.metric == "rmse",
         mtry    == rf_best2$mtry,
         min_n   == rf_best2$min_n)


# Saving Plots
# Saving Plots
ggsave(here("final/plots/autoplot_en_r1.png"),  plot = autoplot(en_tune_r1),  width = 8, height = 5, dpi = 300)
ggsave(here("final/plots/autoplot_en_r2.png"),  plot = autoplot(en_tune_r2),  width = 8, height = 5, dpi = 300)
ggsave(here("final/plots/autoplot_knn_r1.png"), plot = autoplot(knn_tune_r1), width = 8, height = 5, dpi = 300)
ggsave(here("final/plots/autoplot_knn_r2.png"), plot = autoplot(knn_tune_r2), width = 8, height = 5, dpi = 300)
ggsave(here("final/plots/autoplot_bt_r1.png"),  plot = autoplot(bt_res1),     width = 10, height = 7, dpi = 300)
ggsave(here("final/plots/autoplot_bt_r2.png"),  plot = autoplot(bt_res2),     width = 10, height = 7, dpi = 300)
ggsave(here("final/plots/autoplot_rf_r1.png"),  plot = autoplot(rf_tune_r1),  width = 10, height = 7, dpi = 300)  # fixed
ggsave(here("final/plots/autoplot_rf_r2.png"),  plot = autoplot(rf_tune_r2),  width = 10, height = 7, dpi = 300)  # fixed

# Compare All Models ----
model_comparison <- bind_rows(
  null_metrics     %>% mutate(model = "Null",             recipe = "baseline"),
  baseline_metrics %>% mutate(model = "Baseline (LM)",    recipe = "baseline"),
  
  lm_metrics       %>% mutate(model = "Linear Regression", recipe = "r1"),
  
  en_best1_metrics %>% mutate(model = "Elastic Net",       recipe = "r1"),
  en_best2_metrics %>% mutate(model = "Elastic Net",       recipe = "r2"),
  
  knn_best1_metrics %>% mutate(model = "KNN",              recipe = "r1"),
  knn_best2_metrics %>% mutate(model = "KNN",              recipe = "r2"),
  
  bt_best1_metrics %>% mutate(model = "Boosted Tree",      recipe = "r1"),
  bt_best2_metrics %>% mutate(model = "Boosted Tree",      recipe = "r2"),
  
  rf_best1_metrics %>% mutate(model = "Random Forest",     recipe = "r1"),
  rf_best2_metrics %>% mutate(model = "Random Forest",     recipe = "r2")
) %>%
  select(model, recipe, mean, std_err) %>%
  rename(rmse = mean) %>%
  arrange(rmse)

save(model_comparison, file = "final/data/model_comparison.rda")

model_comparison %>%
  mutate(
    rmse    = round(rmse, 4),
    std_err = round(std_err, 4)
  ) %>%
  rename(
    Model        = model,
    Recipe       = recipe,
    RMSE         = rmse,
    `Std Error`  = std_err
  ) %>%
  write_csv(here("final/tables/model_comparison.csv"))

# Therefore the best model is the Boosted Trees model with the Engineered Recipe

