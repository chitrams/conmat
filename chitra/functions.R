
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

filter_settings <- function(data) {
  data %>% 
    select(starts_with("cnt_") & !contains("age") & !contains("gender")) %>% 
    colnames()
} 

filter_china <- function(contact_data_imputed) {
  
  contact_data_filtered <- contact_data_imputed %>%
    dplyr::group_by(part_id) %>%
    dplyr::mutate(
      missing_any_contact_age = any(is.na(cnt_age_exact)),
      missing_any_contact_setting = any(
        is.na(cnt_home) | 
          is.na(cnt_work) | 
          is.na(cnt_school) | 
          is.na(cnt_transport) | 
          is.na(cnt_otherplace) | 
          is.na(cnt_otherpublicplace)
        ## | is.na(cnt_other_house)
        ## | is.na(cnt_worship)
        ## | is.na(cnt_supermarket)
        ## | is.na(cnt_shop)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(part_age),
      !missing_any_contact_age,
      !missing_any_contact_setting
    )
  
  contact_data_filtered
}

filter_thailand <- function(contact_data_imputed) {
  contact_data_filtered <- contact_data_imputed %>%
    dplyr::group_by(part_id) %>%
    dplyr::mutate(
      missing_any_contact_age = any(is.na(cnt_age_exact)),
      missing_any_contact_setting = any(
        is.na(cnt_home) | 
          is.na(cnt_work) | 
          is.na(cnt_school) | 
          is.na(cnt_transport) | 
          is.na(cnt_leisure) | 
          is.na(cnt_otherplace)
        ## | is.na(cnt_otherpublicplace)
        ## | is.na(cnt_other_house)
        ## | is.na(cnt_worship)
        ## | is.na(cnt_supermarket)
        ## | is.na(cnt_shop)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(part_age),
      !missing_any_contact_age,
      !missing_any_contact_setting
    )
  
  contact_data_filtered
}

filter_uk <- function(contact_data_imputed) {
  contact_data_filtered <- contact_data_imputed %>%
    dplyr::group_by(part_id) %>%
    dplyr::mutate(
      missing_any_contact_age = any(is.na(cnt_age_exact)),
      missing_any_contact_setting = any(
        is.na(cnt_home) | 
          is.na(cnt_work) | 
          is.na(cnt_school) | 
          is.na(cnt_transport) | 
          is.na(cnt_leisure) | 
          is.na(cnt_otherplace) |
          is.na(cnt_other_house) | 
          is.na(cnt_worship) | 
          is.na(cnt_supermarket) | 
          is.na(cnt_shop)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(part_age),
      !missing_any_contact_age,
      !missing_any_contact_setting
    )
  
  contact_data_filtered
}

# Attempt to clean:
#TODO Ask Nick how to do the following as it's not working
sum_contacts_by_setting <- function(in_data, setting) {
  
  cnt_col <- if_else(
    setting %in% c("home", "school", "work"),
    sym(paste0("cnt_", type)),
    sym(cnt_other)
  )
  
  out_data <- in_data %>% 
    mutate(contacted = !!cnt_col)
  
  return(out_data)
}


sum_contacts <- function(setting, data) {
  
  ages <- 0:100
  indata <- data
  
  #TODO How do I make the following work?
  # The error msg says: object `cnt_home` not found, even after embracing. 
  # indata <- contact_data_filtered %>%
  #   mutate(
  #     contacted = {{ setting }}
  #   )
  
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
