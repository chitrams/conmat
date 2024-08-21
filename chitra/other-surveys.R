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

home_contact <- get_polymod_contact_data(setting = "home")

#%% Download and explore other surveys ----

other_surveys <- socialmixr::list_surveys()

# Different household structures;
# Somaliland's survey covers internally displaced peoples
somaliland_survey <- get_survey("https://doi.org/10.5281/zenodo.7071876")

# Warning message:
#   In load_survey(files) :
#   Could not merge C:\Users\CSARAS~1\AppData\Local\Temp\RtmpesxRsI/espicc_somaliland_digaale_survey_population.csv

thailand_survey <- get_survey("https://doi.org/10.5281/zenodo.4086739")
china_survey <- get_survey("https://doi.org/10.5281/zenodo.3878754")

saveRDS(thailand_survey, "./data/thailand_survey.rda")
saveRDS(china_survey, "./data/china_survey.rda")

# Similar to POLYMOD
# BE, CH, NL, UK during Covid
comix_survey <- get_survey("https://doi.org/10.5281/zenodo.11154066")
saveRDS(comix_survey, "./data/comix_survey.rda")

# CoMix 2.0 arguably has a greater "mix" of the types of households?
# AT, BE, DK, HR (Croatia), EE (Estonia), GR, IT, PL, PT over covid
comixv2_survey <- get_survey("https://zenodo.org/records/7331926")
saveRDS(comixv2_survey, "./data/comixV2_survey.rda")
# But there are issues: warning messages with merging. 

# China, Thailand and comix (V2) might be the least problematic to use.
# No warning messages when "compiling", whereas Somaliland had issues.
# To start small:
# - China 640.7 kb
# - Thailand 1.5 MB

#%% Load test data -----

china_survey <- readRDS("./data/china_survey.rda")
comix_survey <- readRDS("./data/comixv2_survey.rda")
thailand_survey <- readRDS("./data/thailand_survey.rda")

# Try -----

#%% First clean, China ------

# Clean data for China

contact_data <- china_survey$contacts
participant_data <- china_survey$participants

complete_data <- left_join(
  contact_data, participant_data, by = "part_id"
)

# impute contact ages according to the required method

contact_data_imputed <- complete_data %>%
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

# filter out any participants with missing contact ages or settings (can't
# just remove the contacts as that will bias the count)
contact_data_filtered <- contact_data_imputed %>%
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

# Filter: home only -----

raw_contact_data_home <- contact_data_filtered %>% 
  mutate(
    contacted = cnt_home
    )

# Find all the settings within the China survey
contact_data_filtered %>% select(starts_with("cnt_"))

settings_china <- c(cnt_home, cnt_work, cnt_school, 
                    cnt_transport, cnt_leisure, 
                    cnt_otherplace, cnt_otherpublicplace)

# Only works for home, school, and work
# "Other" needs to be manually done

sum_contacts <- function(setting) {
  
  ages <- 0:100
  
  #TODO How do I make the following work?
  # The error msg says: object `cnt_home` not found, even after embracing. 
  indata <- contact_data_filtered %>% 
    mutate(
      contacted = pick(setting)
    )
  
  outdata <- indata %>%
    dplyr::select(
      part_id,
      age_from = part_age,
      age_to = cnt_age,
      contacted
    ) %>%
    tidyr::complete(
      tidyr::nesting(age_from, part_id),
      age_to = ages,
      fill = list(contacted = 0)
    ) %>%
    dplyr::group_by(
      age_from,
      age_to
    ) %>%
    dplyr::summarise(
      contacts = sum(contacted),
      participants = dplyr::n_distinct(part_id),
      .groups = "drop"
    ) %>%
    # add the setting information, so models can act differently for each
    # setting
    dplyr::mutate(
      setting = setting,
      .before = dplyr::everything()
    )
  
  outdata
}

contact_home <- sum_contacts(cnt_home)



