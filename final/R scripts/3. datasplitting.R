#Load Packages
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

#Splitting Data Into Training and Test Data
set.seed(100)
energy_split <- initial_split(energy, prop = 0.8, strata = site_eui)
energy_train <- training(energy_split)
energy_test  <- testing(energy_split)

save(energy_train, file = "data/energy_train.rda")
save(energy_test,  file = "data/energy_test.rda")

#Folds 5 x 5
set.seed(100)
energy_folds <- vfold_cv(energy_train, v = 5, repeats = 4, strata = site_eui)

save(energy_folds,  file = "data/energy_folds.rda")

nrow(energy_train)

# Predictor count (excludes outcome)
ncol(energy_train) - 1
