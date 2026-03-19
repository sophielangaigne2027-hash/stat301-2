# Load packages
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
tidymodels_prefer()

# Load data and recipes
load(here("data/energy_train.rda"))
load(here("data/energy_folds.rda"))
load(here("recipes/recipe1_tree.rda"))
load(here("recipes/recipe2_tree.rda"))

# RF MODEL SPEC — trees back to tune()
rf_model <- rand_forest(
  mtry  = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# WORKFLOWS
rf_workflow_r1 <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(recipe1_tree)

rf_workflow_r2 <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(recipe2_tree)

rf_params1 <- rf_workflow_r1 |>
  extract_parameter_set_dials() |>
  update(
    mtry  = mtry(range = c(2, 15)),
    trees = trees(range = c(100, 1500)),
    min_n = min_n(range = c(2, 50))
  )
rf_grid1 <- grid_latin_hypercube(rf_params1, size = 20)

rf_params2 <- rf_workflow_r2 |>
  extract_parameter_set_dials() |>
  update(
    mtry  = mtry(range = c(2, 15)),
    trees = trees(range = c(100, 1500)),
    min_n = min_n(range = c(2, 50))
  )
rf_grid2 <- grid_latin_hypercube(rf_params2, size = 40)

# PARALLEL PROCESSING
cores <- parallel::detectCores(logical = FALSE)
cl <- makeCluster(cores - 1)
registerDoParallel(cl)

# TUNING - Recipe 1
rf_tune_r1 <- tune_grid(
  rf_workflow_r1,
  resamples = energy_folds,
  grid      = rf_grid1,
  control   = control_grid(save_pred = TRUE, verbose = TRUE)
)

# TUNING - Recipe 2
rf_tune_r2 <- tune_grid(
  rf_workflow_r2,
  resamples = energy_folds,
  grid      = rf_grid2,
  control   = control_grid(save_pred = TRUE, verbose = TRUE)
)

stopCluster(cl)
registerDoSEQ()

# SAVE RESULTS
save(rf_tune_r1, file = here("results/rf_tune_r1.rda"))
save(rf_tune_r2, file = here("results/rf_tune_r2.rda"))

