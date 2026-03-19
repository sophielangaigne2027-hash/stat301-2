library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
tidymodels_prefer()

# load data ----
load(here("final/data/energy_train.rda"))
load(here("final/data/energy_test.rda"))
load(here("final/data/energy_folds.rda"))

# load recipe ----
load(here("final/recipes/energy_recipe_lm.rda"))

# model specification ----
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# define workflow ----
lm_wf <- workflow() |>
  add_recipe(energy_recipe_lm) |>
  add_model(lm_spec)

# fit with resamples ----
set.seed(100)
lm_results <- fit_resamples(
  lm_wf,
  resamples = energy_folds,
  metrics   = metric_set(rmse),
  control   = control_resamples(save_pred = TRUE)
)


# write out results (fitted/trained workflows) ----
save(lm_results, file = here("final/results/lm_results.rda"))

