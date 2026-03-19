# Final Model Assessment ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load ----
load(here("final/data/energy_test.rda"))
load(here("final/results/bt_final_fit.rda"))

# generate predictions ----
bt_preds <- augment(bt_final_fit, new_data = energy_test)

# metrics ----
bt_final_results <- bt_preds %>%
  metrics(truth = site_eui, estimate = .pred)

bt_final_results

save(bt_preds,         file = here("final/results/bt_preds.rda"))
save(bt_final_results, file = here("final/results/bt_final_results.rda"))

# ============================================================
# PLOTS
# ============================================================

# predicted vs actual ----
pred_vs_actual <- bt_preds %>%
  ggplot(aes(x = site_eui, y = .pred)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title    = "Predicted vs Actual",
    subtitle = "Dashed line = perfect predictions",
    x        = "Actual",
    y        = "Predicted"
  ) +
  theme_minimal()

pred_vs_actual


# save plots ----
ggsave(here("final/plots/pred_vs_actual.png"), plot = pred_vs_actual, width = 8, height = 6, dpi = 300)

