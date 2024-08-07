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
saveRDS(thailand_survey, "./data/thailand_survey.rda")
saveRDS(china_survey, "./data/china_survey.rda")

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