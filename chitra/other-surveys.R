load("./data/polymod.rda")

View(polymod[["participants"]])
View(polymod[["contacts"]])

library(socialmixr)
other_surveys <- socialmixr::list_surveys()

# Different household structures;
# Somaliland's survey covers internally displaced peoples
somaliland_survey <- get_survey("https://doi.org/10.5281/zenodo.7071876")

# Warning message:
#   In load_survey(files) :
#   Could not merge C:\Users\CSARAS~1\AppData\Local\Temp\RtmpesxRsI/espicc_somaliland_digaale_survey_population.csv

thailand_survey <- get_survey("https://doi.org/10.5281/zenodo.4086739")
china_survey <- get_survey("https://doi.org/10.5281/zenodo.3878754")

# There are some errors.
saveRDS(somaliland_survey, "./data/somaliland_survey.rda")
save(thailand_survey, "./data/thailand_survey.rda")
save(china_survey, "./data/china_survey.rda")

# Similar to POLYMOD
# BE, CH, NL, UK during Covid
comix_survey <- get_survey("https://doi.org/10.5281/zenodo.11154066")
saveRDS(comix_survey, "./data/comix_survey.rda")

# CoMix 2.0 arguably has a greater "mix" of the types of households?
# AT, BE, DK, HR (Croatia), EE (Estonia), GR, IT, PL, PT over covid
comixv2_survey <- get_survey("https://zenodo.org/records/7331926")
saveRDS(comixv2_survey, "./data/comixV2.rda")
# But there are issues: warning messages with merging. 

# China, Thailand and comix might be the least problematic to use.
# No warning messages when "compiling", whereas Somaliland had issues.
# To start small:
# - China 640.7 kb
# - Thailand 1.5 MB

# Set-up -----

#%% Load packages ------

library(socialmixr)
library(conmat)
library(tidyverse)

#%% Load test data -----

china_contact <- readRDS("./data/china_survey.rda")
comix_contact <- readRDS("./data/comix_survey.rda")
thailand_contact <- readRDS("./data/thailand_survey.rda")

#%% Load our ground truth -----

example_contact <- get_polymod_contact_data(setting = "home")

# Try -----

#%% Select contact data only for China ----

contact_data <- china_contact$contacts

#%% Cleaning 1 ------

# impute contact ages according to the required method

contact_data_imputed <- contact_data %>%
  dplyr::mutate(
    cnt_age_sampled = floor(
      # suppress warnings about NAs in runif
      suppressWarnings(
        stats::runif(
          n = dplyr::n(),
          min = cnt_age_est_min,
          max = cnt_age_est_max + 1
        )
      )
    ),
    cnt_age_mean = floor(
      cnt_age_est_min + (cnt_age_est_max + 1 - cnt_age_est_min) / 2
    ),
    cnt_age = dplyr::case_when(
      !is.na(cnt_age_exact) ~ as.numeric(cnt_age_exact),
      TRUE ~ NA_real_
    )
  )

participant_info <- china_contact$participants %>% 
  select(part_id, part_age, part_gender)

contact_data_participant_info <- left_join(
  contact_data_imputed, participant_info, by = "part_id"
)

# filter out any participants with missing contact ages or settings (can't
# just remove the contacts as that will bias the count)
contact_data_filtered <- contact_data_participant_info %>%
  dplyr::group_by(part_id) %>%
  dplyr::mutate(
    missing_any_contact_age = any(is.na(cnt_age_exact)),
    missing_any_contact_setting = any(
        is.na(cnt_home) |
        is.na(cnt_work) |
        is.na(cnt_school) |
        is.na(cnt_transport) |
        # is.na(cnt_leisure) |
        is.na(cnt_otherplace)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    !is.na(part_age),
    !missing_any_contact_age,
    !missing_any_contact_setting
  )

# Create new setting column here?

# get contacts by setting (keeping 0s, so we can record 0 contacts for some individuals)
contact_data_setting <- contact_data_filtered %>%
  dplyr::mutate(
    contacted = dplyr::case_when(
      setting == "all" ~ 1L,
      setting == "home" ~ cnt_home,
      setting == "school" ~ cnt_school,
      setting == "work" ~ cnt_work,
      setting == "other" ~ pmax(cnt_transport, cnt_otherplace),
    )
  )
