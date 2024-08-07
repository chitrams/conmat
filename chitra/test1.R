library(conmat)
library(mgcv)
library(gratia)

# Run conmat from beginning -----
set.seed(2022 - 12 - 19)
single_contact <- get_polymod_contact_data(setting = "all")
polymod_contact_data <- get_polymod_setting_data()
polymod_survey_data <- get_polymod_population()

#%% Single model fit -----

# First we create a GAM based on existing polymod data/contact survs
m_single <- fit_single_contact_model(
  contact_data = single_contact,
  population = polymod_survey_data
)

# Then we implement the GAM in Subiaco
subiaco <- abs_age_lga("Subiaco (C)")
syncomat_subiaco <- predict_contacts(
  model = m_single,
  population = subiaco,
  age_breaks = c(seq(0, 85, 5), Inf)
)

syncomat_subiaco %>% 
  predictions_to_matrix() %>% 
  autoplot()

#%% Multiple settings fit -----
# Creates a GAM model for each setting, instead of the one GAM in the 
# above section. 

# First we fit the model
polymod_setting_models <- fit_setting_contacts(
  contact_data_list = polymod_contact_data,
  population = polymod_survey_data
)

polymod_setting_models$home
polymod_setting_models$work
polymod_setting_models$school
polymod_setting_models$other

appraise(polymod_setting_models$home)
draw(polymod_setting_models$home, residuals = TRUE)

# Then we predict on our data using the GAMs
settings_syncomat_subiaco <- predict_setting_contacts(
  population = subiaco,
  contact_model = polymod_setting_models,
  age_breaks = c(seq(0, 85, 5), Inf)
)

autoplot(settings_syncomat_subiaco)

# Draw the partial dependency plots -----
# Uses gratia::draw()

draw(settings_syncomat_subiaco) # Doesn't work because this is not a model
draw(polymod_setting_models$home)

# To send to others ----

library(conmat)
library(gratia)
library(dharma)
library(tidyverse)

set.seed(2022 - 12 - 19)
polymod_contact_data <- get_polymod_setting_data()
polymod_survey_data <- get_polymod_population()

polymod_setting_models <- fit_setting_contacts(
  contact_data_list = polymod_contact_data,
  population = polymod_survey_data
)

draw(polymod_setting_models$home)
draw(polymod_setting_models$work)
draw(polymod_setting_models$school)
draw(polymod_setting_models$other)