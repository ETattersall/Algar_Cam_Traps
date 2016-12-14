#### R script for organizing images by species
#### Started Dec. 12, 2016 by Erin Tattersall

#### Need a master csv of all stations

master_csv <- read.csv("Algar_Camera_Traps/Algar_master_12Dec_2016.csv")

library(dplyr)
library(camtrapR)
library(tidyr)
images_wd <- setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps")
images_wd

### Create a CSV with a species column (indicating species present in photo)
spec_col <- master_csv %>% 
  gather(Species, Species_count, starts_with("O_"), starts_with("R_"), starts_with("C_"), starts_with("A_"),starts_with("U_"), starts_with("G_"), starts_with("M_"), starts_with("V_"),starts_with("T_"),starts_with("H_"), starts_with("L_"), starts_with("Other")) %>% 
  filter(Species_count >0) %>% 
  select(File, Folder, Species, Species_count)

write.csv(spec_col,"Master_csv_specified.csv")


#### Vectors of all folder names and species to input into final loop

folder <- unique(master_csv$Folder)
species <- c("O_virginianus","R_tarandus", "C_elavus", "A_alces", "C_lupus", "C_latrans", "U_americanus", "L_canadensis", "G_gulo", "M_americana", "M_pennanti", "V_vulpes", "T_hudsonicus", "L_americanus", "H_sapiens", "G_canadensis", "Other_birds", "Other")
Stations <- list.files("Raw_images")

### Create species folders
SpecFolderCreate1 <- createSpeciesFolders (inDir               = "Raw_images",
                                           species             = species,
                                           hasCameraFolders = FALSE,
                                           removeFolders       = FALSE)

## Loop for organizing photos into species folders (1 station only)

## Algar 001
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_001")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar1" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
} 

## Algar 002
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_002")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar2" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
} 

## Algar 003
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_003")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar3" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 004
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_004")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar4" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 005
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_005")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar5" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 006
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_006")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar6" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 007
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_007")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar7" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 008
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_008")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar8" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 009
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_009")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar9" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 010
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_010")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar10" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 011
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_011")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar11" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 012
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_012")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar12" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 013
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_013")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar13" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 014
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_014")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar14" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 015
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_015")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar15" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 016
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_016")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar16" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 017
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_017")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar17" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 017
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_017")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar17" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 018
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_018")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar18" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 019
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_019")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar19" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 020
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_020")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar20" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 021
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_021")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar21" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 022
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_022")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar22" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 023
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_023")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar23" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 024
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images/Algar_024")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar24" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

