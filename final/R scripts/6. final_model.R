# Finalize & Train Final Model ----
library(tidyverse)
library(tidymodels)
library(here)
library(xgboost)

tidymodels_prefer()

# load necessary objects ----
load(here("final/data/energy_train.rda"))
load(here("final/data/energy_test.rda"))
load(here("final/recipes/recipe2_tree.rda"))
load(here("final/results/bt_res2.rda"))

bt_best2 <- select_best(bt_res2, metric = "rmse")

# rebuild spec with same fixed params ----
bt_spec_final <- boost_tree(
  trees      = 500,
  tree_depth = bt_best2$tree_depth,
  learn_rate = 0.05,
  min_n      = bt_best2$min_n
) %>%
  set_engine("xgboost", nthread = parallel::detectCores(logical = FALSE)) %>%
  set_mode("regression")

# finalize workflow ----
bt_final_wf <- workflow() %>%
  add_recipe(recipe2_tree) %>%
  add_model(bt_spec_final)

bt_final_fit <- fit(bt_final_wf, data = energy_train)

# evaluate on test set ----
bt_final_results <- augment(bt_final_fit, new_data = energy_test) %>%
  metrics(truth = site_eui, estimate = .pred)

bt_final_results

# save ----
save(bt_best2,        file = here("final/results/bt_best2.rda"))
save(bt_final_fit,    file = here("final/results/bt_final_fit.rda"))
save(bt_final_results, file = here("final/results/bt_final_results.rda"))

