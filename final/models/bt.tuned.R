library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
library(xgboost)
tidymodels_prefer()

# Load data and objects ----
load(here("final/data/energy_train.rda"))
load(here("final/data/energy_folds.rda"))
load(here("final/recipes/recipe1_tree.rda"))
load(here("final/recipes/recipe2_tree.rda"))

# Define model — trees fixed, now tuning depth and min_n ----
bt_spec <- boost_tree(
  trees      = 500,   # fixed — plateau starts here for both recipes
  tree_depth = tune(),
  learn_rate = 0.05,  # fixed
  min_n      = tune()
) %>%
  set_engine("xgboost", nthread = parallel::detectCores(logical = FALSE)) %>%
  set_mode("regression")

# Create workflows ----
bt_workflow1 <- workflow() %>%
  add_model(bt_spec) %>%
  add_recipe(recipe1_tree)

bt_workflow2 <- workflow() %>%
  add_model(bt_spec) %>%
  add_recipe(recipe2_tree)

# Grid for depth and min_n only ----
bt_grid <- grid_latin_hypercube(
  min_n(range      = c(20, 30)),  
  tree_depth(range = c(7, 10)),
  size = 10
)

# Parallel processing ----
num_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(num_cores - 1)
registerDoParallel(cl)

# Tune ----
set.seed(100)
bt_res1 <- tune_grid(
  bt_workflow1,
  resamples = energy_folds,
  grid      = bt_grid,
  metrics   = metric_set(rmse),
  control   = control_grid(save_pred = TRUE, verbose = TRUE, parallel_over = "everything")
)

bt_res2 <- tune_grid(
  bt_workflow2,
  resamples = energy_folds,
  grid      = bt_grid,
  metrics   = metric_set(rmse),
  control   = control_grid(save_pred = TRUE, verbose = TRUE, parallel_over = "everything")
)

stopCluster(cl)
registerDoSEQ()

# Examine results ----

# Save autoplots ----
ggsave(here("final/results/autoplot_bt_r1.png"), plot = autoplot(bt_res1), width = 10, height = 7, dpi = 300)
ggsave(here("final/results/autoplot_bt_r2.png"), plot = autoplot(bt_res2), width = 10, height = 7, dpi = 300)

# Save results ----
save(bt_res1, file = here("final/results/bt_res1.rda"))
save(bt_res2, file = here("final/results/bt_res2.rda"))

