### Gathering all CSVs and merging into master CSV for 2016.01
## Started May 30, 2017 by Erin T.

setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01")

## Creating objects for csv original and destination folders
csvs_2016.01 <- paste(cams2016.01, "csv", sep = ".")
from_csvs <- paste(cams2016.01, csvs_2016.01, sep = "/")
to_csvs <- paste("CSVs_2016.01", csvs_2016.01, sep = "/")

### Copying csvs into one folder
file.copy(from = from_csvs, to = to_csvs, overwrite = FALSE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
## Removing csvs from original folder
file.remove(from_csvs)

############ Original Algar code (Dec. 9, 2016), modified for 2016.01 deployment (June 1, 2017)
## Renaming CSVs with date
setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01/CSvs_2016.01")
n.name <- paste(cams2016.01,"01June_2017", sep = "_")
name.csv <- paste(n.name, "csv", sep = ".")

file.rename(csvs_2016.01,name.csv)


##OR do it this way

getwd()
startingDir<-"CSVs_2016.01"
filez<-list.files(startingDir, full.names=TRUE)
head(filez)

# Create a vector that consists of a bunch of file paths to then rename files in a consistent way
# Use sapply() to apply a function to every element of a vector:
# Use the file.rename() function along with the sub() function to change the filename

sapply(filez,FUN=function(eachPath){
  file.rename(from=eachPath,to=sub(pattern="Algar_",replacement="Algar_9Dec_2016.",eachPath, ))
})

##### Merging csvs ####
library(plyr)
library(dplyr)
setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01/CSvs_2016.01")

### rbind.fill (in plyr) merges csvs and fills missing columns with NA
filenames <- list.files()
master.csv.2016.01 <- do.call("rbind.fill", lapply(filenames, read.csv, header = TRUE))

### rbind.fill (in plyr) merges csvs and fills missing columns with NA


write.csv(master.csv.2016.01, "2016.01CSVs_02June_2017.csv")


###checking csv's for files to review
library(dplyr)
csv_toReview <- read.csv("C:/Users/ETattersall/Documents/Sync/Algar/2016.01/Algar57/Algar57.csv", header = TRUE)
Alg57_Rev <- csv_toReview %>% 
             filter(Review == "true") %>% 
             select(File, Folder, Species, Comments)
## Check all stations and create data frame of files marked for review

##csv of files to review
Rev_list <- rbind(Alg03_Rev, Alg10_Rev, Alg16_Rev, Alg20_Rev, Alg25_Rev, Alg27_Rev, Alg28_Rev, Alg34_Rev, Alg36_Rev, Alg37_Rev, Alg42_Rev, Alg45_Rev, Alg46_Rev, Alg47_Rev, Alg51_Rev, Alg57_Rev)

write.csv(Rev_list, "Algar_ReviewFiles.csv")

###########################################################################################################

