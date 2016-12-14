#### camtrapR imageRename


library(camtrapR)
This is camtrapR version: 0.99.5 (CURRENT)

Algar_Pilot <- "C:/Users/ETattersall/OneDrive/Algar Camera Traps"
setwd(Algar_Pilot)


#Check file paths, may need to include a subdirectory

# Load csv. csv should include all stations of one strata? (All control, all treatment?)
AITFTestdata<- read.csv("Site 3-Template 2/AlgarImagesTemplate2.csv")
AITFTestdata

#Using ExifTool in R
#Allows exiftool to be used from anywhere on harddrive

exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir)   

#Checking whether it's been added to PATH

grepl(exiftool_dir, Sys.getenv("PATH"))




#imageRename function on OUR data (done on AITF computer, remember to change WD)
#Folder cannot contain any non-JPEG files (move to Extra Timelapse stuff file)

#Step 1: create a renaming table 
Algar_Raw_Con <- "Raw_Images/Control"
Algar_Raw_Treat <- "Raw_Images/Treat"

rename.table.Con <- imageRename(inDir            = Algar_Raw_Con,  
                                hasCameraFolders = FALSE, 
                                copyImages       = FALSE, 
                                writecsv         = FALSE)

 
#Station and camera IDs come from the raw images directory structure, 
#so for this to work, the raw images file has to be subdivided by Station ID and Camera ID (if applicable)

#Step 3: Copy images into new folder
#outDir can't be same as inDir and can't be a subdirectory
renaming.table.Con2 <- imageRename(inDir            =   Algar_Raw_Con,
                                   outDir           =   "Rename_Images/Control",
                                   hasCameraFolders =  FALSE,
                                   copyImages       =  TRUE,
                                   writecsv         =  TRUE)

#Step 3: Checking if it worked (returns list of renamed JPEGs)
list.files("Rename_Images/Control", recursive = TRUE)



#Other notes
#Copied new filenames into exising data frame in column File_rename


wd_images_raw_renamed <- file.path(tempdir(), "raw_images_renamed")
#When working with camtrapR sample data
#tempdir() creates a temporary directory in "C:/Users/ETattersall/AppData/Local/Temp"


#Isolating Species data 

DeerImages<-subset(AITFTestdata,O_virginianus>0)

#Now DeerImages=data frame only of deer pictures!
#How many photos have deer in them?

DeerCount<-summary(as.factor(DeerImages$O_virginianus))
DeerCount
1   2 
116  21 



