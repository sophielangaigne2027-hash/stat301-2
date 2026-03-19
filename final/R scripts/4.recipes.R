# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)

tidymodels_prefer()

load(here("data/energy_train.rda"))

# ============================================================
# RECIPE 1: KITCHEN SINK
# ============================================================

recipe1_main <- recipe(site_eui ~ ., data = energy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_rm(`Largest Property Use Type`) %>%
  step_nzv(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.85) %>%
  step_lincomb(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

recipe1_tree <- recipe(site_eui ~ ., data = energy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_rm(`Largest Property Use Type`) %>%
  step_nzv(all_predictors()) %>%
  step_integer(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# ============================================================
# RECIPE 2: ENGINEERED
# EDA results used to consider
# ============================================================

recipe2_main <- recipe(site_eui ~ ., data = energy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_rm(`Largest Property Use Type`) %>%
  step_log(
    `Natural Gas Use (kBtu)`,
    `Electricity Use - Grid Purchase (kBtu)`,
    `Hospital (General Medical & Surgical) - Gross Floor Area (ft²)`,
    `Laboratory - Gross Floor Area (ft²)`,
    offset = 1
  ) %>%
  step_cut(
    `Year Built`,
    breaks = c(-Inf, 1950, 1980, 2000, Inf),
    include_outside_range = TRUE
  ) %>%
  step_interact(
    ~ is_office:`Office - Percent That Can Be Heated` +
      is_office:`Office - Percent That Can Be Cooled`
  ) %>%
  
  step_nzv(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.85) %>%
  step_lincomb(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())
recipe2_tree <- recipe(site_eui ~ ., data = energy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_rm(`Largest Property Use Type`) %>%
  step_log(
    `Natural Gas Use (kBtu)`,
    `Electricity Use - Grid Purchase (kBtu)`,
    `Hospital (General Medical & Surgical) - Gross Floor Area (ft²)`,
    `Laboratory - Gross Floor Area (ft²)`,
    offset = 1
  ) %>%
  step_cut(
    `Year Built`,
    breaks = c(-Inf, 1950, 1980, 2000, Inf),
    include_outside_range = TRUE
  ) %>%
  step_nzv(all_predictors()) %>%
  step_integer(all_nominal_predictors()) %>%
  step_zv(all_predictors())

recipe1_main %>% prep() %>% bake(new_data = NULL) %>% glimpse()
recipe1_tree %>% prep() %>% bake(new_data = NULL) %>% glimpse()
recipe2_main %>% prep() %>% bake(new_data = NULL) %>% glimpse()
recipe2_tree %>% prep() %>% bake(new_data = NULL) %>% glimpse()

save(recipe1_main, file = here("recipes/recipe1_main.rda"))
save(recipe1_tree, file = here("recipes/recipe1_tree.rda"))
save(recipe2_main, file = here("recipes/recipe2_main.rda"))
save(recipe2_tree, file = here("recipes/recipe2_tree.rda"))


