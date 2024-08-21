# Set-up -----

#%% Load packages ------

library(socialmixr)
library(conmat)
library(tidyverse)

#%% Download and explore POLYMOD (control) ----

load("./data/polymod.rda")

View(polymod[["participants"]])
View(polymod[["contacts"]])

polymod_contact <- polymod$contacts

polymod_home_contact <- get_polymod_contact_data(setting = "home")
polymod_pop <- get_polymod_population()

#%% Download and explore other surveys ----

other_surveys <- socialmixr::list_surveys()
View(other_surveys)

# Different household structures;
# Somaliland's survey covers internally displaced peoples
somaliland_survey <- get_survey("https://doi.org/10.5281/zenodo.7071876")

# Warning message:
#   In load_survey(files) :
#   Could not merge C:\Users\CSARAS~1\AppData\Local\Temp\RtmpesxRsI/espicc_somaliland_digaale_survey_population.csv

# Thailand 2015
thailand_survey <- get_survey("https://doi.org/10.5281/zenodo.4739777")

# 2019, this one is a good survey
china_survey <- get_survey("https://doi.org/10.5281/zenodo.3878754") 

saveRDS(thailand_survey, "./data/thailand_survey.rda")
saveRDS(china_survey, "./data/china_survey.rda")

# Similar to POLYMOD
# BE, CH, NL, UK during Covid
comix_survey <- get_survey("https://doi.org/10.5281/zenodo.11154066")
saveRDS(comix_survey, "./data/comix_survey.rda")

# CoMix 2.0 arguably has a greater "mix" of the types of households?
# AT, BE, DK, HR (Croatia), EE (Estonia), GR, IT, PL, PT over covid
# Add these populations together? Or just grab one country?
comixv2_survey <- get_survey("https://zenodo.org/records/7331926")
saveRDS(comixv2_survey, "./data/comixV2_survey.rda")
# But there are issues: warning messages with merging. 

# UK 2022 (CoMix)
uk_survey <- get_survey("https://doi.org/10.5281/zenodo.6542524")
saveRDS(uk_survey, "./data/uk_survey.rda")

# China and comix (V2) might be the least problematic to use.
# No warning messages when "compiling", whereas Somaliland had issues.

# Surveys to use:
# To start small:
# - China 2019
# - Thailand 2015
# - UK 2022

#%% Load test data -----

china_survey <- readRDS("./data/china_survey.rda")
thailand_survey <- readRDS("./data/thailand_survey.rda")
uk_survey <- readRDS("./data/uk_survey.rda")

# Load functions
source("./chitra/functions.R")

# Clean all data ----

china_imputed <- impute_contact_data(china_survey)
china_imputed %>% filter_settings()

thailand_imputed <- impute_contact_data(thailand_survey)
thailand_imputed %>% filter_settings()

uk_imputed <- impute_contact_data(uk_survey)
uk_imputed %>% filter_settings()

uk_imputed %>% 
  select(starts_with("cnt_") & !contains("age") & !contains("gender")) %>% 
  summarise(across(everything(), ~table(.)))

china_filtered <- filter_china(china_imputed)
thailand_filtered <- filter_thailand(thailand_imputed)
uk_filtered <- filter_uk(uk_imputed)

#TODO Ask Nick: if the mutate is within the fn, the following doesn't work.
# See functions.R > sum_contacts_by_setting
# contact_home <- sum_contacts(cnt_home)

#%% Clean contact surveys to put into model fit -----
raw_contact_data_home <- contact_data_filtered %>% 
  mutate(
    contacted = cnt_home
  )
contact_home <- sum_contacts("home", raw_contact_data_home)

raw_contact_data_school <- contact_data_filtered %>% 
  mutate(
    contacted = cnt_school
  )
contact_school <- sum_contacts("school", raw_contact_data_school)

raw_contact_data_work <- contact_data_filtered %>% 
  mutate(
    contacted = cnt_work
  )
contact_work <- sum_contacts("work", raw_contact_data_work)

raw_contact_data_other <- contact_data_filtered %>% 
  mutate(
    contacted = pmax(cnt_transport, cnt_leisure, 
                     cnt_otherplace, cnt_otherpublicplace),
  )
contact_other <- sum_contacts("other", raw_contact_data_other)

rm(list = ls(pattern = "raw_contact_data_"))

# Upload population data from UNPD

china_pop <- read_csv("./data/china_pop_age_dist.csv")
china_pop_cm <- as_conmat_population(
  data = china_pop,
  age = lower.age.limit,
  population = population
)

#%% Fit to models -----

# Polymod example
mpolymod_home <- fit_single_contact_model(
  contact_data = polymod_home_contact,
  population = polymod_pop
)

#%%%% Home ----

mchina_home <- fit_single_contact_model(
  contact_data = contact_home,
  population = china_pop_cm
)

scm_china <- predict_contacts(
  model = mchina_home,
  population = china_pop_cm,
  age_breaks = c(seq(0, 80, by = 5), Inf)
)

china_home_plot <- scm_china %>% 
  predictions_to_matrix() %>% 
  autoplot()

#%%%% Work ----

mchina_work <- fit_single_contact_model(
  contact_data = contact_work,
  population = china_pop_cm,
  work_demographics = conmat_original_work_demographics
)

scm_china_work <- predict_contacts(
  model = mchina_work,
  population = china_pop_cm,
  age_breaks = c(seq(0, 80, by = 5), Inf)
)

china_work_plot <- scm_china_work %>% 
  predictions_to_matrix() %>% 
  autoplot()

#%%%% School ----

mchina_school <- fit_single_contact_model(
  contact_data = contact_school,
  population = china_pop_cm,
  school_demographics = conmat_original_school_demographics
)

scm_china_school <- predict_contacts(
  model = mchina_school,
  population = china_pop_cm,
  age_breaks = c(seq(0, 80, by = 5), Inf)
)

china_school_plot <- scm_china_school %>% 
  predictions_to_matrix() %>% 
  autoplot()

china_school_plot
