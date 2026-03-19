#Load Packages
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
tidymodels_prefer()


#Loading and Cleaning Data
set.seed(100)
energy <- read_csv("data/energy.csv",
                   na = c("", "NA", "Not Available", "N/A", "not available")) %>%
  rename(site_eui = `Site EUI (kBtu/ft²)`) %>%
  select(
    site_eui,
    `Property GFA - Calculated (Buildings) (ft²)`,
    `Year Built`,
    `Number of Buildings`,
    `Construction Status`,
    `Occupancy`,
    `Property GFA - Calculated (Parking) (ft²)`,
    Borough,
    `Latitude`,
    `Longitude`,
    `Primary Property Type - Portfolio Manager-Calculated`,
    `Largest Property Use Type`,
    `Largest Property Use Type - Gross Floor Area (ft²)`,
    `Electricity Use - Grid Purchase (kBtu)`,
    `Natural Gas Use (kBtu)`,
    `District Steam Use (kBtu)`,
    `District Chilled Water Use (kBtu)`,
    `District Hot Water Use (kBtu)`,
    `Fuel Oil #2 Use (kBtu)`,
    `Fuel Oil #4 Use (kBtu)`,
    `Fuel Oil #5 & 6 Use (kBtu)`,
    `Office - Gross Floor Area (ft²)`,
    `Office - Weekly Operating Hours`,
    `Office - Number of Workers on Main Shift`,
    `Office - Percent That Can Be Heated`,
    `Office - Percent That Can Be Cooled`,
    `Multifamily Housing - Gross Floor Area (ft²)`,
    `Multifamily Housing - Total Number of Residential Living Units`,
    `Multifamily Housing - Percent That Can Be Heated`,
    `Multifamily Housing - Percent That Can Be Cooled`,
    `Hotel - Gross Floor Area (ft²)`,
    `K-12 School - Gross Floor Area (ft²)`,
    `Hospital (General Medical & Surgical) - Gross Floor Area (ft²)`,
    `Retail Store - Gross Floor Area (ft²)`,
    `Parking - Gross Floor Area (ft²)`,
    `Supermarket/Grocery - Gross Floor Area (ft²)`,
    `Laboratory - Gross Floor Area (ft²)`,
    `College/University - Gross Floor Area (ft²)`,
    `Senior Living Community - Gross Floor Area (ft²)`,
    `Non-Refrigerated Warehouse - Gross Floor Area (ft²)`,
    `Food Service - Gross Floor Area (ft²)`,
    `Medical Office - Gross Floor Area (ft²)`,
    `Indoor Water Use (All Water Sources) (kgal)`
  ) %>%
  
  filter(!is.na(site_eui)) %>%
  
  drop_na(
    `Property GFA - Calculated (Buildings) (ft²)`,
    `Largest Property Use Type - Gross Floor Area (ft²)`,
    `Year Built`,
    `Occupancy`,
    `Electricity Use - Grid Purchase (kBtu)`,
    `Indoor Water Use (All Water Sources) (kgal)`
  ) %>%
  mutate(
    is_office      = if_else(is.na(`Office - Weekly Operating Hours`), 0, 1),
    is_multifamily = if_else(is.na(`Multifamily Housing - Total Number of Residential Living Units`), 0, 1)
  ) %>%
  mutate(across(
    c(
      `Office - Gross Floor Area (ft²)`,
      `Office - Weekly Operating Hours`,
      `Office - Number of Workers on Main Shift`,
      `Office - Percent That Can Be Heated`,
      `Office - Percent That Can Be Cooled`,
      `Multifamily Housing - Gross Floor Area (ft²)`,
      `Multifamily Housing - Total Number of Residential Living Units`,
      `Multifamily Housing - Percent That Can Be Heated`,
      `Multifamily Housing - Percent That Can Be Cooled`,
      `Hotel - Gross Floor Area (ft²)`,
      `K-12 School - Gross Floor Area (ft²)`,
      `Hospital (General Medical & Surgical) - Gross Floor Area (ft²)`,
      `Retail Store - Gross Floor Area (ft²)`,
      `Parking - Gross Floor Area (ft²)`,
      `Supermarket/Grocery - Gross Floor Area (ft²)`,
      `Laboratory - Gross Floor Area (ft²)`,
      `College/University - Gross Floor Area (ft²)`,
      `Senior Living Community - Gross Floor Area (ft²)`,
      `Non-Refrigerated Warehouse - Gross Floor Area (ft²)`,
      `Food Service - Gross Floor Area (ft²)`,
      `Medical Office - Gross Floor Area (ft²)`,
      `District Steam Use (kBtu)`,
      `District Chilled Water Use (kBtu)`,
      `District Hot Water Use (kBtu)`,
      `Fuel Oil #2 Use (kBtu)`,
      `Fuel Oil #4 Use (kBtu)`,
      `Fuel Oil #5 & 6 Use (kBtu)`,
      `Natural Gas Use (kBtu)`,
      `Property GFA - Calculated (Parking) (ft²)`
    ),
    ~ replace_na(.x, 0)
  )) %>%
  mutate(across(
    c(Borough,
      `Primary Property Type - Portfolio Manager-Calculated`,
      `Largest Property Use Type`,
      `Construction Status`),
    ~ replace_na(as.character(.x), "Unknown")
  )) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, median(.x, na.rm = TRUE))))


save(energy, file = here("data/energy.rda"))




