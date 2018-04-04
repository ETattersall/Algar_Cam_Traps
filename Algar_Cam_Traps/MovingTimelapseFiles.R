########################################################
## MovingTimelapseFiles.R
## Filtering out Timelapse images from image directory
## Resulting Timelapse Folder does not have subdirectories for folders
#######################################################

library(camtrapR)

setwd("D:/Algar_Apr-Nov2017")

# List all Images in directory
Images <- list.files('Renamed_Images', recursive = TRUE)
class(Images)
head(Images)

# Filter above list for Timelapse images taken at noon ("12-00-00(1))
Timelapse <- grep(pattern = "12-00-00(1)", x = Images, fixed = TRUE, value = TRUE) #fixed = TRUE uses exact matching of the pattern character string, value = TRUE returns image names themselves
head(Timelapse)
tail(Timelapse)


## Move Timelapse files into new folder
## Use small subset of images to test (created manually)

test <- list.files('Test_copy', recursive = TRUE)
t1 <- grep(pattern = "12-00-00(1)", x = test, fixed = TRUE, value = TRUE)
head(t1)
class(t1)


## Copy files from Station folders into Timelapse and remove from original folders
## Set working directory to Test_copy
setwd("D:/Algar_Apr-Nov2017/Test_copy")
#create a Timelapse folder
createStationFolders(inDir = getwd(),
                     stations = 'Timelapse',
                     createinDir = FALSE)

#### Function for moving files (needs to move files and remove them from previous folder)
file.move <- function(from, to){
  file.copy(from = from, to = to, recursive = TRUE)
  file.remove(from)
}

file.move(from = t1, to = 'Timelapse') #does not retain subdirectories

summary(list.files('Timelapse')) #606, same as in t1
summary(list.files(getwd(), recursive = TRUE)) #1295, same as test. All files kept!!

