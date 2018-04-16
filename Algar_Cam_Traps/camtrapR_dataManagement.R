##########################################
# camtrapR_dataManagement.R
# Aggregating useful camtrapR functions
# Started Apr. 3, 2018: creating station folders and renaming images
#########################################


library(camtrapR)

##### createStationFolders #####
## Set WD to desired location
setwd("D:/Algar_Nov2017-Apr2018")

## List of station names
Stations <- paste("Algar", formatC(1:73, width=2, flag="0"), sep="") #Formats numbers as fixed width of 2

## Create station folders
createStationFolders(inDir = "Raw_Images", 
                     stations = Stations,
                     createinDir = TRUE) #Creates Raw_Images folder

##### imageRename #####


#Using ExifTool in R
# Downloaded from https://www.sno.phy.queensu.ca/~phil/exiftool/
# Allows exiftool to be used from anywhere on harddrive



exiftool_dir<-"C:/Users/ETattersall/Desktop" #set location of exiftool 
exiftoolPath(exiftoolDir = exiftool_dir)   

#Checking whether it's been added to PATH

grepl(exiftool_dir, Sys.getenv("PATH"))

# If exiftool is in "c:/Windows". Exiftool installing instructions will ask that exiftool be renamed to "exiftool.exe", but the below function only works when it is named "exiftool"
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
Rich.rename <- imageRename(inDir            =   "Raw_images",
                            outDir           =   "Renamed_Images",
                            hasCameraFolders =  FALSE,
                            copyImages       =  TRUE,
                            writecsv         =  TRUE)

#Step 3: Checking if it worked (returns list of renamed JPEGs)
list.files("Renamed_Images", recursive = TRUE)
