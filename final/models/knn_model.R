# Load packages
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
tidymodels_prefer()

# Load data and recipes
load(here("data/energy_train.rda"))
load(here("data/energy_folds.rda"))
load(here("recipes/recipe1_main.rda"))
load(here("recipes/recipe2_main.rda"))

# KNN MODEL SPEC
knn_model <- nearest_neighbor(
  neighbors = tune()
) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# WORKFLOWS
knn_workflow_r1 <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(recipe1_main)

knn_workflow_r2 <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(recipe2_main)

# SAME TUNING GRID FOR BOTH (fair comparison)

knn_grid_r1 <- tibble(neighbors = seq(8, 35, by = 1))

# Recipe 2 — still dropping fast, need to go much higher  
knn_grid_r2 <- tibble(neighbors = seq(8, 60, by = 1))

# PARALLEL PROCESSING
cores <- parallel::detectCores(logical = FALSE)
cl <- makeCluster(cores - 1)
registerDoParallel(cl)

# TUNING - Recipe 1 (Kitchen Sink)
knn_tune_r1 <- tune_grid(
  knn_workflow_r1,
  resamples = energy_folds,
  grid      = knn_grid_r1,
  control   = control_grid(save_pred = TRUE, verbose = TRUE),
  metrics   = metric_set(rmse)
)

# TUNING - Recipe 2 (Engineered)
knn_tune_r2 <- tune_grid(
  knn_workflow_r2,
  resamples = energy_folds,
  grid      = knn_grid_r2,
  control   = control_grid(save_pred = TRUE, verbose = TRUE),
  metrics   = metric_set(rmse)
)

stopCluster(cl)
registerDoSEQ()

# RESULTS
save(knn_tune_r1, file = here("results/knn_tune_r1.rda"))
save(knn_tune_r2, file = here("results/knn_tune_r2.rda"))



