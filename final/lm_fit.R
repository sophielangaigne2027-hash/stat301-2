# Final Baseline Models ----
# Define and fit linear model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(xgboost)
library(doParallel)

# handle common conflicts
tidymodels_prefer()


# parallel processing ----
load(here("data/energy_train.rda"))
load(here("data/energy_test.rda"))


# load resamples/folds & controls ----
load(here("data/energy_folds.rda"))
load(here("data/keep_pred.rda"))
load(here("data/keep_grid.rda"))

# load pre-processing/feature engineering/recipe ----
load(here("recipes/energy_recipe_lm.rda"))
load(here("recipes/energy_recipe_tree.rda"))
load(here("recipes/energy_recipe_baseline.rda"))

# model specifications ----
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# define workflows ----
lm_wf <- workflow() |> 
  add_recipe(energy_recipe_lm) |> 
  add_model(lm_spec)

# fit workflow/model ----
lm_results <- fit_resamples(
  lm_wf,
  resamples = energy_folds,
  metrics = metric_set(rmse),
  control = control_resamples(save_pred = TRUE)
)


lm_metrics <- collect_metrics(lm_results) %>%
  mutate(model = "Linear Regression")

# write out results (fitted/trained workflows) ----
save(lm_results, file = here("results/lm_results.rda"))


