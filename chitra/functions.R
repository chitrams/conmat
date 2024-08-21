
impute_contact_data <- function(survey_data) {
  
  contact_data <- survey_data$contacts
  participant_data <- survey_data$participants
  
  complete_data <- left_join(
    contact_data, participant_data, by = "part_id"
  )
  
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
}

filter_contact_data <- function(contact_data_imputed) {
  contact_data_filtered <- contact_data_imputed %>%
    dplyr::group_by(part_id) %>%
    dplyr::mutate(
      missing_any_contact_age = any(is.na(cnt_age_exact)),
      missing_any_contact_setting = any(
        is.na(cnt_home) 
        | is.na(cnt_work) 
        | is.na(cnt_school) 
        | is.na(cnt_transport) 
        
        # The following should be commented out depending on data
        | is.na(cnt_leisure)
        | is.na(cnt_otherplace)
        | is.na(cnt_otherpublicplace)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(part_age),
      !missing_any_contact_age,
      !missing_any_contact_setting
    )
}


