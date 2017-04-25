#### camtrapR imageRename


library(camtrapR)



setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01")


#Check file paths, may need to include a subdirectory

# Load csv. 
cams2016 ##csv of 60 stations from nov. 2016

#Using ExifTool in R
#Allows exiftool to be used from anywhere on harddrive

exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir)   

#Checking whether it's been added to PATH

grepl(exiftool_dir, Sys.getenv("PATH"))




#imageRename function on OUR data (done on AITF computer, remember to change WD)
#Folder cannot contain any non-JPEG files (move to Extra Timelapse stuff file)

#Step 1: create a renaming table 

rename.tab <- imageRename(inDir            = "Raw_images",  
                          hasCameraFolders = FALSE, 
                          copyImages       = FALSE, 
                          writecsv         = FALSE)

 
#Station and camera IDs come from the raw images directory structure, 


#Step 3: Copy images into new folder
#outDir can't be same as inDir and can't be a subdirectory
Algar.rename <- imageRename(inDir            =   "Raw_images",
                            outDir           =   "Renamed_Images",
                            hasCameraFolders =  FALSE,
                            copyImages       =  TRUE,
                            writecsv         =  TRUE)

#Step 3: Checking if it worked (returns list of renamed JPEGs)
list.files("Renamed_Images", recursive = TRUE)







