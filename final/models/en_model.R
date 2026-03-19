# Load packages
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
tidymodels_prefer()

# Load data and recipes
load(here("final/data/energy_train.rda"))
load(here("final/data/energy_folds.rda"))
load(here("final/recipes/recipe1_main.rda"))
load(here("final/recipes/recipe2_main.rda"))

# Elastic Net Model
en_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# WORKFLOWS
en_workflow_r1 <- workflow() %>%
  add_model(en_model) %>%
  add_recipe(recipe1_main)

en_workflow_r2 <- workflow() %>%
  add_model(en_model) %>%
  add_recipe(recipe2_main)

# TUNING GRID — wider penalty range + bigger size for better coverage
en_grid_r1 <- grid_latin_hypercube(
  penalty(range = c(1, 3)),   # focus on where RMSE plateaus/minimizes
  mixture(range = c(0.4, 1)), # lasso-leaning region performs best
  size = 50
)

# Recipe 2 — cut off the high penalty region that hurts performance
en_grid_r2 <- grid_latin_hypercube(
  penalty(range = c(0, 2)),     # shift left boundary up, optimum is ~1–1.5
  mixture(range = c(0.1, 0.6)), # this is well-behaved, keep it
  size = 50
)

# PARALLEL PROCESSING
cores <- parallel::detectCores(logical = FALSE)
cl <- makeCluster(cores - 1)
registerDoParallel(cl)

# TUNING - Recipe 1
en_tune_r1 <- tune_grid(
  en_workflow_r1,
  resamples = energy_folds,
  grid      = en_grid_r1,
  control   = control_grid(save_pred = TRUE, verbose = TRUE),
  metrics   = metric_set(rmse, rsq, mae)  # broader metrics for diagnosis
)

# TUNING - Recipe 2
en_tune_r2 <- tune_grid(
  en_workflow_r2,
  resamples = energy_folds,
  grid      = en_grid_r2,
  control   = control_grid(save_pred = TRUE, verbose = TRUE),
  metrics   = metric_set(rmse, rsq, mae)
)

stopCluster(cl)
registerDoSEQ()

# BEST HYPERPARAMETERS
en_best_params_r1 <- select_best(en_tune_r1, metric = "rmse")
en_best_params_r2 <- select_best(en_tune_r2, metric = "rmse")

# SAVE
save(en_tune_r1,     file = here("final/results/en_tune_r1.rda"))
save(en_tune_r2,     file = here("final/results/en_tune_r2.rda"))
save(en_best_params_r1, file = here("final/results/best_params_r1.rda"))
save(en_best_params_r2, file = here("final/results/best_params_r2.rda"))


