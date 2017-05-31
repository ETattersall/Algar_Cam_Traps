### Creating table of species and count data for software review test set
## Erin T., May 29, 2017


library(dplyr)

setwd("C:/Users/ETattersall/Documents/Sync/Camera Trap Image Processing/Test_images")
sof_test <- read.csv("Algar22_test.csv")
sof_sp <- sof_test %>% select(File, Date, Time, Species, SpeciesCount, GroupCount)
write.csv(sof_sp, "test_species.csv")
