# EDA on Site EUI

library(tidyverse)
library(tidymodels)
library(here)
library(corrplot)
library(ggplot2)

load(here("final/data/energy_train.rda"))

# Initial EDA before data cleaning
library(tidyverse)
library(tidymodels)
library(here)
tidymodels_prefer()
set.seed(100)

energy <- read_csv("data/energy.csv")

# Build summary table
data_summary <- tibble(
  Metric = c(
    "Total Observations",
    "Total Variables",
    "Numeric Variables",
    "Categorical Variables",
    "Factor Variables"
  ),
  Value = c(
    nrow(energy),
    ncol(energy),
    sum(sapply(energy, is.numeric)),
    sum(sapply(energy, is.character)),
    sum(sapply(energy, is.factor))
  )
)
# Save as CSV
dir.create(here("tables"), showWarnings = FALSE)
write_csv(data_summary, here("tables/data_summary.csv"))

#Missingness
energy |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") |>
  filter(pct_missing > 0) |>
  arrange(desc(pct_missing)) |>
  ggplot(aes(x = reorder(variable, pct_missing), y = pct_missing)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Missingness by Variable",
       x = NULL,
       y = "% Missing") +
  theme_minimal()

# # From here on I used the energy_train set because the energy dataset had been cleaned and missingness was addressed. 


# DISTRIBUTION OF TARGET VARIABLE 

p1 <- ggplot(energy_train, aes(x = site_eui)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  labs(title = "Distribution of Site EUI", x = "Site EUI (kBtu/ft²)", y = "Count")

ggsave(here("final/plots/site_eui_histogram.png"), plot = p1, width = 8, height = 5)

#Log Scale
p2 <- ggplot(energy_train, aes(x = site_eui)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  scale_x_log10() +
  labs(title = "Distribution of Site EUI (log scale)", x = "Site EUI (kBtu/ft²)", y = "Count")

ggsave(here("final/plots/site_eui_histogram_log.png"), plot = p2, width = 8, height = 5)


# SITE EUI BY BOROUGH 
p5 <- ggplot(energy_train, aes(x = Borough, y = site_eui)) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.2) +
  labs(title = "Site EUI by Borough", y = "Site EUI (kBtu/ft²)")

ggsave(here("final/plots/site_eui_by_borough.png"), plot = p5, width = 8, height = 5)

# TOP CORRELATED VARIABLES PLOTTED AGAINST SITE EUI 
numeric_cors <- energy_train %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  select(variable, site_eui) %>%
  filter(variable != "site_eui") %>%
  arrange(desc(abs(site_eui)))

# grab top 4 numeric correlates
#Predictors of Site EUI
top6_vars <- numeric_cors %>% slice_head(n = 6) %>% pull(variable)

p4 <- energy_train %>%
  select(site_eui, all_of(top6_vars)) %>%
  pivot_longer(-site_eui) %>%
  ggplot(aes(x = value, y = site_eui)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # loess shows non-linearity
  facet_wrap(~name, scales = "free_x", ncol = 3) +
  labs(title = "Top 6 Predictors vs Site EUI (with loess curve)",
       x = NULL, y = "Site EUI")

ggsave(here("final/plots/site_eui_predictors.png"), plot = p4, 
       width = 14, height = 10, dpi = 300)

