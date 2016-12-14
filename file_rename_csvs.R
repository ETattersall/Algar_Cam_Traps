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
