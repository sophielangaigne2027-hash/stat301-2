# Load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)

# Handle common conflicts
tidymodels_prefer()

# Load training data ----
load(here("final/data/energy_train.rda"))
load(here("final/data/energy_test.rda"))
load(here("final/data/energy_folds.rda"))

# Load recipes ----
load(here("final/recipes/energy_recipe_lm.rda"))
load(here("final/recipes/energy_recipe_tree.rda"))
load(here("final/recipes/energy_recipe_baseline.rda"))

# ============================================================
# NULL MODEL
# ============================================================

null_spec <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

null_workflow <- workflow() %>%
  add_recipe(energy_recipe_baseline) %>%
  add_model(null_spec)

set.seed(347)
null_results <- fit_resamples(
  null_workflow,
  resamples = energy_folds,
  metrics = metric_set(rmse, rsq, mae),
  control = control_resamples(save_workflow = TRUE)
)

null_metrics <- collect_metrics(null_results) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "Null", recipe = "baseline")

save(null_metrics, file = here("final/results/null_metrics.rda"))

# ============================================================
# BASELINE MODEL — Linear Regression
# ============================================================

lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lm_baseline_wflow <- workflow() %>%
  add_recipe(energy_recipe_baseline) %>%
  add_model(lm_spec)

set.seed(347)
lm_baseline_results <- fit_resamples(
  lm_baseline_wflow,
  resamples = energy_folds,
  metrics = metric_set(rmse, rsq, mae),
  control = control_resamples(save_workflow = TRUE)
)

baseline_metrics <- collect_metrics(lm_baseline_results) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "Baseline (LM)", recipe = "baseline")

save(baseline_metrics, file = here("final/results/baseline_metrics.rda"))

