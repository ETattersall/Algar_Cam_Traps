## Checking Species Names- could use for classifying "Other" images???
library(camtrapR)
# Need vector of names, either common or scientific
species_common <- c("woodland caribou", "grey wolf", "white-tailed deer", "black bear")

species.names.check1 <- checkSpeciesNames(speciesNames = species_common,
                                          searchtype   = "common",
                                          accepted     = TRUE,
                                          ask          = TRUE)
species.names.check1
#ask=TRUE when there are multiple matches, asks for input

#No match found for grey wolf, so let's check scientific names

species_scientific <- c("Canis lupus", "Canis latrans")
species.names.check2 <- checkSpeciesNames(speciesNames = species_scientific,
                                          searchtype   = "scientific",
                                          accepted     = TRUE,
                                          ask          = TRUE)
species.names.check2
