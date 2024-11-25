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

# Thailand 2015
thailand_survey <- get_survey("https://doi.org/10.5281/zenodo.4739777")

# China 2019
china_survey <- get_survey("https://doi.org/10.5281/zenodo.3878754") 

#saveRDS(thailand_survey, "./data/thailand_survey.rda")
#saveRDS(china_survey, "./data/china_survey.rda")

# UK 2022 (CoMix)
uk_survey <- get_survey("https://doi.org/10.5281/zenodo.6542524")
#saveRDS(uk_survey, "./data/uk_survey.rda")

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
get_contacts_from_survey <- function(survey) {
  contact_home <- sum_contacts(survey, "home")

  contact_school <- survey |>
    mutate(contacted = cnt_school) |>
    sum_contacts("school")

  contact_work <- survey |>
    mutate(contacted = cnt_work) |>
    sum_contacts("work")

  contact_other <- survey |>
    rowwise() |>
    mutate(
      contacted = max(c_across(any_of(c("cnt_transport", "cnt_leisure", "cnt_otherplace", "cnt_otherpublicplace"))))
    ) |>
    sum_contacts("other")

  list(
    "home" = contact_home,
    "work" = contact_work,
    "school" = contact_school,
    "other" = contact_other
  )
}




# Upload population data from UNPD

china_pop <- read_csv("./data/china_pop_age_dist.csv")
china_pop_cm <- as_conmat_population(
  data = china_pop,
  age = lower.age.limit,
  population = population
)

#%% Fit to models -----
fit_contact_singlesetting <- function(model, population, ...) {
  model <- fit_single_contact_model(
    contact_data = model,
    population = population,
    ...
  )

  predict_contacts(
    model = model,
    population = population,
    age_breaks = c(seq(0, 80, by = 5), Inf)
  )
}

fit_contacts <- function(models, population) {
  home <- fit_contact_singlesetting(models$home, population)

  work <- fit_contact_singlesetting(models$work, population, work_demographics = conmat_original_work_demographics)

  school <- fit_contact_singlesetting(models$school, population, school_demographics = conmat_original_school_demographics)

  other <- fit_contact_singlesetting(models$other, population)
  
  list(
    "home" = home,
    "work" = work,
    "school" = school,
    "other" = other
  )

}

china_survey_data <- get_contacts_from_survey(china_filtered)
thailand_survey_data <- get_contacts_from_survey(thailand_filtered)
polymod_survey_data <- list(
  "home" = get_polymod_contact_data("home"),
  "work" =  get_polymod_contact_data("work"),
  "school" = get_polymod_contact_data("school"),
  "other" = get_polymod_contact_data("other")
)

china_china <- fit_contacts(china_survey_data, china_pop_cm)
thailand_china <- fit_contacts(thailand_survey_data, china_pop_cm)
polymod_china <- fit_contacts(polymod_survey_data, china_pop_cm)

fit_list <- list("china" = china_china, "thailand" = thailand_china, "polymod" = polymod_china)

plot_comparisons <- function(fit_list, settings) {
  fit_names <- names(fit_list)

  home <- map2_dfr(fit_list, fit_names, .f = function(x, y) { 
    x$home |> mutate(survey_location = y) } 
  ) |> mutate(setting = "home")

  work <- map2_dfr(fit_list, fit_names, .f = function(x, y) { 
    x$work |> mutate(survey_location = y) } 
  ) |> mutate(setting = "work")

  school <- map2_dfr(fit_list, fit_names, .f = function(x, y) { 
    x$school |> mutate(survey_location = y) } 
  ) |> mutate(setting = "school")

  all_settings <- bind_rows(
    home, work, school
  )
  
  all_settings |>
    filter(setting %in% settings) |>
    ggplot(aes(x=age_group_from, y = age_group_to, fill = contacts)) +
      geom_tile() +
      coord_fixed() +
      scale_fill_distiller(direction = 1, trans = "sqrt") +
      theme_minimal() +
      theme(axis.text = element_text(size = 6, angle = 45, hjust = 1)) +
      facet_grid(setting ~ survey_location) +
      labs(x = "Age (from)", y = "Age (to)", fill = "Contacts")
}

plot_comparisons(fit_list[c("china", "polymod")], settings = c("home", "work", "school"))
