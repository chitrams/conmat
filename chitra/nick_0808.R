# Issue 151 - https://github.com/idem-lab/conmat/issues/151
# So what we want to do is take a different survey that isn't POLYMOD
# and tidy the data into a format that can be used with
# `fit_single_contact_model()`
# An example workflow is shown below where are using already tidied up
# data from polymod, which is provided with:
# `get_polymod_contact_data(setting = "home")`.

# So here is an example using POLYMOD data.
# Get the data from `get_polymod_contact_data(setting = "home")`
example_contact <- get_polymod_contact_data(setting = "home")
example_contact

# This has the format:
# setting (settings - home, school, work, other)
# age_from: The age of the people who did the survey
# age_to: The age of the people that the survey respondents had contact with
# contacts: The number of contacts from->to
# participants: The number of respondents in that age group

# This population is just our regular age-structured data
example_population <- get_polymod_population()

library(dplyr)

# this is getting the data down to a smaller size for faster model fitting
example_contact_20 <- example_contact %>%
  filter(
    age_to <= 20,
    age_from <= 20
  )

# This model takes in contact data and assumes a particular structure of the data
my_mod <- fit_single_contact_model(
  contact_data = example_contact_20,
  population = example_population
)

# so what we want to do is take some other contact survey data and
# get it into this format:
#### setting (settings - home, school, work, other)
#### age_from: The age of the people who did the survey
#### age_to: The age of the people that the survey respondents had contact with
#### contacts: The number of contacts from->to
#### participants: The number of respondents in that age group

library(socialmixr)

contact_survey_list <- list_surveys()

head(contact_survey_list)

zim <- socialmixr::get_survey(survey = "https://doi.org/10.5281/zenodo.3886638")

# mysterious black box method for getting data into the right format for
# use in socialmixr
clean_zim <- clean(zim)
clean_zim$participants

# instead, I'd recommend exploring the data cleaning steps from
# `get_polymod_contact_data()`
# and seeing if they relate back to some of the socialmixr data
# feel free to copy the code from `get_polymod_contact_data()`
# and try and run the socialmixr data into it.

# A few goals
# 1. Get a dataset from socialmixr into the right format to then do
#  `fit_single_contact_model()` on it (using relevant population data. e.g.,
# if you get a social contact survey from Zimbabwe, use census/age data from
# zimbabwe for the `population` argument in `fit_single_contact_model`)
# 2. Write out step 1 into a vignette, unpacking the data cleaning process
# And then also using `fit_single_contact_model` - and extending this
# to use other settings - work, school, other, all.
# And then explore using `fit_setting_contacts()`
# (stretch goal :2. Consider a way to turn our data cleaning steps in
# `get_polymod_contact_data` into another function that can tidy up a given
# dataset from socialmixr. Don't get too bogged down - there's a lot of contact
# surveys. This might also be similar to what `sociamixr::clean()` does,
# so that might also be worthwhile exploring?