setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps")

file.rename("Algar_001.csv", "Algar_001_9Dec_2016.csv")


##############


startingDir<-"CSVs_12Dec_2016"
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
setwd("CSVs_12Dec_2016")

### rbind.fill (in plyr) merges csvs and fills missing columns with NA
filenames <- list.files()
Alg_master <- do.call("rbind.fill", lapply(filenames, read.csv, header = TRUE))

### rbind.fill (in plyr) merges csvs and fills missing columns with NA


write.csv(Alg_master, "Algar_master_12Dec_2016.csv")


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
