########################################################
## MovingTimelapseFiles.R
## Filtering out Timelapse images from image directory
## Resulting Timelapse Folder does not have subdirectories for folders
#######################################################

library(camtrapR)

setwd("D:/CameraTrap_Images/Algar_Apr-Nov2018")

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
setwd("D:/CameraTrap_Images/Algar_Apr-Nov2018")

test <- list.files('Test_copy') ## Full list of images in test file
head(test)
t1 <- grep(pattern = "12-00-00(1)", x = test, fixed = TRUE, value = TRUE) ## List of Timelapse images in test file
head(t1)
class(t1)


## Copy files from Station folders into Timelapse and remove from original folders
## Set working directory to Test_copy
setwd("D:/CameraTrap_Images/Algar_Apr-Nov2018/Renamed_Images/Test_copy")
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


### Move Timelapse files for entire deployment
# Set working directory to Renamed_Images
setwd("D:/CameraTrap_Images/Algar_Apr-Nov2018/Renamed_Images")

#create a Timelapse folder
createStationFolders(inDir = getwd(),
                     stations = 'Timelapse',
                     createinDir = FALSE)


## Move files with above function

file.move(from = Timelapse, to = 'Timelapse') #does not retain subdirectories

summary(list.files('Timelapse'))
summary(list.files(getwd(), recursive = TRUE)) ## All files moved











#### But Timelapse folder does not have station subfolders
setwd("D:/CameraTrap_Images/Algar_Apr-Nov2018/Renamed_Images/Timelapse")

F1 <- list.files() ## List of Timelapse images without the Station subdirectories attached

tail(F1)
head(F1)

### Create station folders within Timelapse to move images into
## List of station names
Stations <- paste("Algar", formatC(1:73, width=2, flag="0"), sep="") #Formats numbers as fixed width of 2

## Create station folders
createStationFolders(inDir = getwd(), 
                     stations = Stations,
                     createinDir = TRUE)

head(Timelapse)
head(F1)


 ## Move Timelapse images into Timelapse station folders
## from = F1, to = Timelapse (still has subdirectory)
file.move(from = F1, to = Timelapse)

getwd()

summary(list.files('Timelapse', recursive = TRUE))


