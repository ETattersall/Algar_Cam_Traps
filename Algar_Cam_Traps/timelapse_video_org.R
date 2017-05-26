### Copying Timelapse images into folders for video clips
library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar_024")

f_sp_fol <- master_csv %>%
  filter(Folder == "Algar24" & Trigger_Mode == "Time Lapse") %>% 
  select(File)
vec_sp_fol <- unlist(f_sp_fol)
file.copy(vec_sp_fol,"Algar_024_timelapse")