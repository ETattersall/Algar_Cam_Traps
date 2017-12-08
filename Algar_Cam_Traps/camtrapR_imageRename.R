#### camtrapR imageRename

install.packages("camtrapR")

library(camtrapR)

getwd()



#Algar images from Nov 2017 are on external hard drive (path is E:/ or D:/... etc)
setwd("E:/Algar_Apr-Nov2017")
#Check file paths, may need to include a subdirectory

#Using ExifTool in R
#Allows exiftool to be used from anywhere on harddrive

# If exiftool is on Desktop
exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir)   

#Checking whether it's been added to PATH

grepl(exiftool_dir, Sys.getenv("PATH"))

# If exiftool is in "c:/Windows". Exiftool installin instructions will ask that exiftool be renamed to "exiftool.exe", but the below function only works when it is named "exiftool"
Sys.which("exiftool")


#### imageRename function on OUR data (done on AITF computer, remember to change WD) ####
#Folder cannot contain any non-JPEG files (move to Extra Timelapse stuff file)

#Step 1: create a renaming table (unnecessary for images to be copied and renamed)

rename.tab <- imageRename(inDir            = "Raw_images",  
                          hasCameraFolders = FALSE, 
                          copyImages       = FALSE, 
                          writecsv         = FALSE)

 
#Station and camera IDs come from the raw images directory structure






#### Step 3: Copy images into new folder ####
#outDir can't be same as inDir and can't be a subdirectory
Algar.rename <- imageRename(inDir            =   "Raw_images",
                            outDir           =   "Renamed_Images",
                            hasCameraFolders =  FALSE,
                            copyImages       =  TRUE,
                            writecsv         =  TRUE)

#Step 3: Checking if it worked (returns list of renamed JPEGs)
list.files("Renamed_Images", recursive = TRUE)


## Checking git connection




