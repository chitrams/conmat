load("./data/polymod.rda")

View(polymod[["participants"]])
View(polymod[["contacts"]])

library(socialmixr)
other_surveys <- socialmixr::list_surveys()
