#### R script for organizing images by species
#### Started Dec. 12, 2016 by Erin Tattersall

#### Need a master csv of all stations

## master.csv.2016.01

library(dplyr)
library(camtrapR) ## req. package 'foreach' to be installed
library(tidyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01")


### Create a CSV with a species column (indicating species present in photo)
### For 2016.01 data, this step is unnecessary: already have species column
spec_col <- master.csv %>% 
  gather(Species, Species_count, starts_with("O_"), starts_with("R_"), starts_with("C_"), starts_with("A_"),starts_with("U_"), starts_with("G_"), starts_with("M_"), starts_with("V_"),starts_with("T_"),starts_with("H_"), starts_with("L_"), starts_with("Other")) %>% 
  filter(Species_count >0) %>% 
  select(File, Folder, Species, Species_count)

write.csv(spec_col,"Master_csv_specified.csv")

## table for species and files
spec_col <- master.csv.2016.01 %>% 
            filter(SpeciesCount >0) %>% 
            select(File, Folder, Species, SpeciesCount)


#### Vectors of all folder names and species to input into final loop

cams2016.01 <- cams2016$CamStation
unique(spec_col$Species) ## Some NAs and blanks (from unknown animals). Need vector without those
species <- c("O_virginianus","R_tarandus", "A_alces", "C_lupus", "C_latrans", "U_americanus", "L_canadensis", "M_americana", "M_pennanti", "V_vulpes", "T_hudsonicus", "L_americanus", "H_sapiens", "G_canadensis", "Other_birds", "Other", "P_concolor", "Mustelid spp.")
Stations <- list.files("Renamed_images")

### Create species folders
SpecFolderCreate1 <- createSpeciesFolders (inDir               = "Renamed_images",
                                           species             = species,
                                           hasCameraFolders = FALSE,
                                           removeFolders       = FALSE)



## Loop for organizing photos into species folders (1 station only)
##Still have to remove other images from each folder manually...


## Algar 01
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar01")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar01" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
} 

## Algar 02
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar02")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar02" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 03
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar03")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar03" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 04
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar04")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar04" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
############################################### RUN 1:58PM JUNE 2, 2017
## Algar 05
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar05")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar05" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 06
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar06")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar06" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 07
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar07")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar07" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 08
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar08")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar08" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 09
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar09")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar09" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 10
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar10")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar10" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 11
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar11")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar11" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 12
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar12")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar12" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 13
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar13")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar13" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 14
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar14")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar14" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 15
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar15")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar15" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 16
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar16")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar16" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 17
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar17")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar17" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 18
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar18")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar18" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 19
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar19")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar19" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 20
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar20")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar20" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 21
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar21")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar21" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 22
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar22")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar22" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 23
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar23")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar23" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 24
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar24")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar24" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 25
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar25")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar25" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 26
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar26")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar26" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 27
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar27")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar27" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 28
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar28")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar28" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 29
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar29")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar29" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 30
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar30")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar30" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 31
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar31")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar31" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 32
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar32")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar32" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 33
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar33")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar33" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 34
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar34")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar34" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 35
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar35")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar35" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 36
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar36")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar36" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 37
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar37")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar37" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 38
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar38")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar38" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 39
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar39")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar39" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 40
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar40")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar40" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}


## Algar 41
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar41")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar41" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 42
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar42")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar42" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 43
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar43")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar43" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 44
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar44")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar44" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 45
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar45")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar45" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 46
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar46")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar46" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 47
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar47")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar47" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 48
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar48")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar48" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 49
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar49")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar49" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 50
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar50")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar50" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 51
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar51")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar51" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 52
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar52")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar52" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 53
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar53")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar53" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 54
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar54")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar54" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 55
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar55")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar55" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}


## Algar 56
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar56")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar56" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 57
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar57")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar57" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 58
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar58")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar58" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 59
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar59")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar59" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 60
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar60")
for (sp in species) {
  f_sp_fol <- spec_col %>%
    filter(Folder == "Algar60" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}




## getSpeciesImages --> Use after creating record table to copy ALL species images across stations?
##### Only fetches one species at a time

specImagecopy <- getSpeciesImages(species                 = "U_americanus",
                                  recordTable             = rec.spec.ind,
                                  speciesCol              = "Species",
                                  stationCol              = "Station",
                                  outDir                  = "Species_org_01",
                                  createStationSubfolders = TRUE)

## Turning getSpeciesImages into a loop

image_identify <- unique(rec.spec.ind$Species)

for (id in image_identify) {
  getSpeciesImages(species                 = id,
                 recordTable             = rec.spec.ind,
                 speciesCol              = "Species",
                 stationCol              = "Station",
                 outDir                  = "Species_org_01",
                 createStationSubfolders = TRUE)
}

