###############################
#
# AlgarCamera.R
# Cole Burton, started 21-Nov-2016
# script for exploratory analysis of camera trap data from the Algar Wildlife Monitoring Project
# 
###############################

# set directory
setwd("C:\\Users\\Cole\\Documents\\AITF Projects\\Algar\\Analysis\\Camera Data Analysis")

# save workspace
save.image("~/AITF Projects\\Algar\\Analysis\\Camera Data Analysis/AlgarCamera.RData")

# load packages (as needed)
library(camtrapR)
library(help=camtrapR)
vignette("ImageOrganisation",package="camtrapR")
vignette("SpeciesIndividualIdentification",package="camtrapR")
vignette("DataExtraction",package="camtrapR")
vignette("DataExploration",package="camtrapR")

# rename Algar camera trap images using imageRename function

image.table <- imageRename(inDir = "C:\\Users\\Cole\\Documents\\AITF Projects\\Algar\\Field Data\\Camera Photos",
                           outDir = "C:\\Users\\Cole\\Documents\\AITF Projects\\Algar\\Field Data\\RenamedImages",
                           hasCameraFolders = FALSE,
                           copyImages = TRUE, 
                           writecsv = TRUE)


### Load camera station data for the 24 cameras set in November 2015, checked in November 2016
cams2015 <- read.csv("Algar2015StationData.csv")

with(cams2015,plot(utmE,utmN))


# specify folder with renamed images
imgDir <- "C:\\Users\\Cole\\Documents\\AITF Projects\\Algar\\Field Data\\RenamedImages" 

# check number of JPG images [this stuff isn't needed now that we have tagged images]
length(list.files(imgDir, pattern = "JPG", recursive = TRUE)) # [1] 18036

# try this for each station
stations <- as.character(cams2015$CamStation)
stationDir <- rep(NA,length(stations))
stationDir <- paste("C:\\Users\\Cole\\Documents\\AITF Projects\\Algar\\Field Data\\RenamedImages",stations,sep="\\")

stn.img <- rep(NA,length(stations))
for (i in 1:length(stations)) {
  stn.img[i] <- length(list.files(stationDir[i], pattern = "JPG"))
}
sum(stn.img)

# add to camera station table
cams2015$img.cnt <- stn.img


# tally survey effort

camEff <- cameraOperation(cams2015, 
                stationCol = "CamStation", 
                setupCol = "DeployDate", 
                retrievalCol = "CheckDate1", 
                hasProblems = FALSE,
                dateFormat = "%d/%m/%Y", 
                writecsv = FALSE)

# total camera days
sum(camEff,na.rm=T) # 8911

# days per station
summary(rowSums(camEff,na.rm=T)) # median = 371, range = 370-373

# add to camera station table
cams2015$trapdays <- rowSums(camEff,na.rm=T)

# subtract days from total images for estimate of motion triggers [not needed, see image data below]
cams2015$EstTrig <- cams2015$img.cnt - cams2015$trapdays

# quick-and-dirty plot of number of motion triggers by treatment
boxplot(cams2015$EstTrig ~ cams2015$Treatment,col=c("orange","purple"),ylab="Number of Motion Triggers",
        cex.lab=1.5,cex.axis=1.5)


# check metadata tag names on images
exifTagNames(inDir = imgDir, 
             returnMetadata = FALSE)

# "TriggerMode" is tag 55

# try extracting metadata? # only gives it for first file
metaD <- exifTagNames(inDir = imgDir, 
             returnMetadata = TRUE)

# extract image data using recordTable
  # need to have species tagged

test <- recordTable(inDir = imgDir,
            IDfrom = "metadata",
            stationCol = "CamStation",
            writecsv = FALSE,
            outDir,
            metadataHierarchyDelimitor = "|",
            metadataSpeciesTag,
            additionalMetadataTags = "TriggerMode")


# could consider altering code from exifTagNames function to extract tags for all images in folder?
fix(exifTagNames)

###--- RAW CAMERA IMAGES

# load data files from Timelapse image analysis
  # done by Erin T and Joanna B, with quick review by me

# specify directory with image data

DR <- "C:\\Users\\Cole\\Documents\\AITF Projects\\Algar\\Field Data\\RenamedImages\\ColeTimelapseExports12Dec2016"

# create list to hold data for each site (each loaded from csv)
  # only columns 1:39, since some had empty last column
  # for some reason, some stations have lowercase true/false while others have uppercase
    # this creates problems, so I will specify all the column classes
    # I will skip some that aren't useful at this stage (but then need to adjust number of columns)
    # can specify dates and factors later, as needed
DataClass <- c("character","NULL","character","character","character","factor","logical","character","factor","logical",
               "character","logical","character",rep("integer",18),
               "character","logical","character","character","character",
               "NULL","NULL","character","factor","logical","logical")

Adata <- list()

for (SITE in 1:24) {
  Adata[[SITE]] <- read.csv(paste(DR,"/","Algar",SITE,"_12Dec2016.csv",sep = ""),colClasses=DataClass)[,1:39]
}

# combine into 1 dataset covering all sites ### CHECK THIS CAREFULLY ...
A <- do.call(rbind, Adata)  

# check for NA's in counts (shouldn't be any)
apply(apply(A[,13:30],2,is.na),2,sum)

# need to fix NA values in species counts ... [missing values in csv files]

# how many photos are of animals 
table(A$Animal)
table(A$Trigger_Mode)

# subset data to only animal photos that are not classified as unknown

A2 <- subset(A, Animal == TRUE & Unknown == FALSE)

# make a count variable for each detection
A2$count <- apply(A2[,13:30],1,max, na.rm=T)

# there are still a few zero counts to fix (should go back in csv files eventually ...)
A2[which(A2$count==0),]

# drop an image that didn't have an animal (checked, Algar6__2016-01-22__08-13-15(5).JPG)
A2 <- A2[-which(A2$count==0)[3],] 

# add missing counts for 2 bird photos (Algar4__2016-09-06__19-46-06(1).JPG and Algar4__2016-09-18__08-52-16(2).JPG 
A2[which(A2$count==0)[c(1,2)],"Other_birds"] <- 1

# add missing counts of snowshoe hare for others (Algar16__2016-09-25__21-40-28(1).JPG and subsequent)
A2[which(A2$count==0)[c(3:11)],"L_americanus"] <- 1

# update the count variable
A2$count <- apply(A2[,13:30],1,max, na.rm=T)

summary(A2$count) # all zeros now gone

# create variable with species name
for(i in 1:nrow(A2)) {
  A2$Species[i] <- names(which.max(A2[i,14:31]))
}

# some summaries
nrow(A2)

table(A2$Species)

# check the Other class
A2$Other_specify[which(A2$Species=="Other")]
A2[which(A2$Species=="Other"),]
A2[which(A2$Species=="Other"),c(31,36)]


table(A2$Folder)

# Add Treatment to detections 

A2$Treatment <- cams2015$Treatment[match(A2$Folder,cams2015$CamStation)]

table(A2$Treatment)


###--- DETECTION RATE INDEX

  # I will start with a liberal index of allowing 1 photo per minute (rather than hour)
  # to set up, follow code used for Boreal Deer dataset (e.g. from SpatialCountDensity.R)

# specify date formats 
A2$Date.Time <- paste(A2$Date,A2$Time,sep = " ")
A2$Date.Time <- as.POSIXct(strptime(A2$Date.Time, "%d-%b-%Y %H:%M:%S", tz="MST"))

# calculate a unique day for each day of study
# taking straight difference will include partial days, so is really 24-hour periods from time first camera set
# using "floor" so it doesn't round up to next day
A2$StudyDay <- floor(as.numeric(difftime(max(A2$Date.Time),min(A2$Date.Time),units="days")))

	### THERE ARE INCONSISTENCIES IN HOW YEAR IS SPECIFIED IN THESE DATES -- NEED TO FIX

	### FOR NOW I WILL PROCEED WITH SUMMARIES ON RAW DATA

# plot of detections across species
sp.plot <- rev(sort(table(A2$Species))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.4,ylim=c(0,2000))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot),xpd=TRUE)

# create Site x Species matrix
S <- as.data.frame.matrix(table(A2$Folder,A2$Species))

# species totals to compare with table(A2$Species)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- rev(sort(sp.sites)) 
xvals <- barplot(sp.plot2,names.arg = NA,col="royalblue4",ylab = "Number of sites",cex.lab=1.4)
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot2),xpd=TRUE)

# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S,1,function(x) sum(ifelse(x>0,1,0)))


# add coordinates and treatment to dataframe
S$utmE <- cams2015$utmE[match(row.names(S),cams2015$CamStation)]
S$utmN <- cams2015$utmN[match(row.names(S),cams2015$CamStation)]

S$Treatment <- cams2015$Treatment[match(row.names(S),cams2015$CamStation)]


# plot spatial variation by station [note: need to specify species columns now that other variables added]
with(S, symbols(x=utmE, y=utmN, circles=Total, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Total animal detections by camera station"))

with(S, symbols(x=utmE, y=utmN, circles=Richness, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Species richness by camera station"))

# look at detections relative to treatment [quick-and-dirty for exploration, not really appropriate tests]
  # total detections
boxplot(S$Total~S$Treatment,cex.axis=1.3,ylab="Total detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$Total,S$Treatment,mean)
tapply(S$Total,S$Treatment,sd)
t.test(S$Total~S$Treatment)

  # richness
boxplot(S$Richness~S$Treatment,cex.axis=1.3,ylab="Species Richness",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$Richness,S$Treatment,mean)
tapply(S$Richness,S$Treatment,sd)
t.test(S$Richness~S$Treatment)

# treatment effects by species

  # caribou
boxplot(S$R_tarandus~S$Treatment,cex.axis=1.3,ylab="Caribou detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$R_tarandus,S$Treatment,mean)
tapply(S$R_tarandus,S$Treatment,sd)
tapply(S$R_tarandus,S$Treatment,median)
t.test(S$R_tarandus~S$Treatment)

# wolf
boxplot(S$C_lupus~S$Treatment,cex.axis=1.3,ylab="Wolf detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$C_lupus,S$Treatment,mean)
tapply(S$C_lupus,S$Treatment,sd)
t.test(S$C_lupus~S$Treatment)
tapply(S$C_lupus,S$Treatment,median)

# bear
boxplot(S$U_americanus~S$Treatment,cex.axis=1.3,ylab="Black bear detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$U_americanus,S$Treatment,mean)
tapply(S$U_americanus,S$Treatment,sd)
tapply(S$U_americanus,S$Treatment,median)
t.test(S$U_americanus~S$Treatment)

# deer
boxplot(S$O_virginianus~S$Treatment,cex.axis=1.3,ylab="White-tailed deer detections",cex.lab=1.6,
        col=c("orange","purple"),boxwex=0.6)
  # remove outlier
with(S[which(S$O_virginianus<1000),],boxplot(O_virginianus~Treatment,cex.axis=1.3,ylab="White-tailed deer detections",
                                            cex.lab=1.6,col=c("orange","purple"),boxwex=0.6))

tapply(S$O_virginianus,S$Treatment,mean)
with(S[which(S$O_virginianus<1000),],tapply(O_virginianus,Treatment,mean))

tapply(S$O_virginianus,S$Treatment,sd)
tapply(S$O_virginianus,S$Treatment,median)
t.test(S$O_virginianus~S$Treatment)

# moose
boxplot(S$A_alces~S$Treatment,cex.axis=1.3,ylab="Moose detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$A_alces,S$Treatment,mean)
tapply(S$A_alces,S$Treatment,sd)
tapply(S$A_alces,S$Treatment,median)
t.test(S$A_alces~S$Treatment)


# save workspace
save.image("~/AITF Projects\\Algar\\Analysis\\Camera Data Analysis/AlgarCamera.RData")






