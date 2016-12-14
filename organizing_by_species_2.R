#### Still working on species organization

folder <- unique(master_csv$Folder)
species <- c("O_virginianus","R_tarandus", "C_elavus", "A_alces", "C_lupus", "C_latrans", "U_americanus", "C_canadensis", "G_gulo", "M_americana", "M_pennanti", "V_vulpes", "T_hudsonicus", "L_americanus", "H_sapiens", "G_canadensis", "Other_birds", "Other")


Station_wd <- c("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_001", 
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_002", 
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_003",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_004", 
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_005",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_006",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_007",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_008",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_009",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_010",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_011",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_012",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_013",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_014",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_015",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_016",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_017",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_018",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_019",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_020",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_021",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_022",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_023",
                "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Renamed_images_Copy/Algar_024")

images_wd <- setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps")
images_wd

SpecFolderCreate1 <- createSpeciesFolders (inDir               = "Renamed_images_Copy",
                                           species             = species,
                                           hasCameraFolders = FALSE,
                                           removeFolders       = FALSE)


for (sp in species) {
  setwd(Station_wd)
  f_sp_fol <- spec_col %>%
    filter(Folder == folder & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
} 