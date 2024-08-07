library(tidyverse)
library(conmat)
library(mgcv)
library(gratia)
library(patchwork)

set.seed(2022 - 12 - 19)
polymod_contact_data <- get_polymod_setting_data()
polymod_survey_data <- get_polymod_population()

# Synthetic contact matrices for Perth -----

perth <- abs_age_lga("Perth (C)")

syncomat_setting_perth <- predict_setting_contacts(
  population = perth,
  contact_model = polymod_setting_models,
  age_breaks = c(seq(0, 85, 5), Inf)
)

# Partial dependency plots for POLYMOD GAM -----

polymod_setting_models <- fit_setting_contacts(
  contact_data_list = polymod_contact_data,
  population = polymod_survey_data
)

draw(polymod_setting_models$home, residuals = TRUE) +
  plot_annotation(title = "Home")
p_work <- draw(polymod_setting_models$work, residuals = TRUE)
p_school <- draw(polymod_setting_models$school, residuals = TRUE)
p_other <- draw(polymod_setting_models$other, residuals = TRUE)

wrap_plots(p_home, p_work, p_school, p_other)

# 01.07.24 Update: Matrices from Plots ------

p_home <- polymod_setting_models$home
summary(p_home)
sm_home <- smooth_estimates(p_home)

dat_home <- sm_home %>% 
  pivot_longer(
    cols = starts_with("gam_age"),
    names_prefix = "gam_age_",
    values_to = "x_value"
  ) %>% 
  filter(!is.na(x_value)) %>% 
  select(!c(`.type`, `.by`))

write_csv(dat, "./chitra/smooth-values.csv")

# Now to calculate the i's and j's for each value. 

fitted_values(p_home, data = polymod_contact_data$home)
