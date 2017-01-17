#### Target predator data extraction and exploration #####
#### Created 16 January, 2017 by Erin Tattersall

###Isolating predator events into one csv ###

library(camtrapR)
library(dplyr)
library(tidyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps")

spec_col <- read.csv("Master_csv_specified.csv")

species_list <- unique(spec_col$Species)

## Pick out predators of interest: C_lupus, C_latrans, U_americanus, L_canadensis
pred_list <- c("C_lupus", "C_latrans","U_americanus","L_canadensis")

pred_csv <- spec_col %>% 
            filter(Species == pred_list) %>% 
            select(File, Folder, Species,Species_count)

### Number of events on Control lines vs Treatment lines? ##
