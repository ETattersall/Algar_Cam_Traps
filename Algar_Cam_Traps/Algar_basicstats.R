###### Summary stats for first 2 Algar deployments
### Started by Erin 7 June 7, 2017
## Using some repeat code from 'Algar_code_organize.R'
### Pertinent dataframes
## Cam Station info = cams2015, cams2016
## Master CSVs = x2016.01, need to read in 2015
## Record tables = ani.2015 and ani.rec
library(dplyr)

setwd(images_wd) ##Algar_Camera_Traps


########## No. of images############### 
## 2015
x2015.01 <- read.csv("2015.01/Algar_master_12Dec_2016.csv")
imgDir.2015 <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01/Raw_images" 
length(list.files(imgDir.2015, pattern = "JPG", recursive = TRUE)) ##18036, matches entries in master CSV because no cameras malfunctioned

## 2016
imgDir.2016 <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images" 
length(list.files(imgDir.2016, pattern = "JPG", recursive = TRUE)) ##30632 
##x2016.01 has 13955 entries. Did not process Algar32 and Algar50 because of corrupt files/malfunctioning camera

################ Camera Effort ########
##2015
camEff <- cameraOperation(cams2015, 
                          stationCol = "CamStation", 
                          setupCol = "DeployDate", 
                          retrievalCol = "CheckDate1",
                          hasProblems = FALSE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)
# total camera days
sum(camEff,na.rm=T) #8911
##2016 - account for malfunctioning cameras
camEff <- cameraOperation(cams2016, 
                          stationCol = "CamStation", 
                          setupCol = "CheckDate1", 
                          retrievalCol = "CheckDate2",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

sum(camEff,na.rm=T) ## 9025

######## No. of Species #######
##2015, where 'Other' = cougar and mustelid spp.
unique(ani.2015$Species)
## [1] "A_alces"       "O_virginianus" "U_americanus"  "C_latrans"     "C_lupus"       "G_canadensis"  "L_canadensis"  "R_tarandus"   
## [9] "V_vulpes"      "L_americanus"  "M_pennanti"    "T_hudsonicus"  "Other"         "G_gulo"        "M_americana"  
## 14 mammal species, plus sandhill crane

## Other birds?
birds.2015 <- x2015.01 %>% filter(Other_birds >0) %>% select(File, Other_birds, Other_specify, Comments) ### 179 images. Not going to go through all of them...
## How many detections of other birds?
bird.dec <- rec.spec %>% filter(Species == "Other_birds") %>% select(Species, FileName) ## 68 detections of other birds


## 2016
unique(ani.rec$Species)
## [1] "A_alces"       "L_americanus"  "O_virginianus" "R_tarandus"    "C_lupus"       "L_canadensis"  "M_americana"   "T_hudsonicus" 
## [9] "C_latrans"     "U_americanus"  "G_canadensis"  "V_vulpes"      "M_pennanti"    "P_concolor" 
## 13 mammal species, plus sandhill crane
## other birds = Great grey owl, grouse, grey jays

########## Detection rates (no. detections per 1000 TD) ######
## Mean trap days for 2015.01 = 8911
## Mean trap days for 2016.01 = 9025

##### 2015.01 full year
sp_detect <- ani.2015$Species
st_detect <- ani.2015$Station
S2015.01 <- as.data.frame.matrix(table(st_detect,sp_detect))

# add total count and total species (richness) for each site
S2015.01$Total <- apply(S2015.01,1,sum)

S2015.01$Richness <- apply(S2015.01[,1:15],1,function(x) sum(ifelse(x>0,1,0)))

(sum(S2015.01$R_tarandus)/8911)*1000 ## 4.94 caribou detections per 1000 trap days
(sum(S2015.01$C_lupus)/8911)*1000 ## 11.11 wolves
(sum(S2015.01$U_americanus)/8911)*1000 ## 16.61 black bears
(sum(S2015.01$O_virginianus)/8911)*1000 ##27.38 white-tailed deer
(sum(S2015.01$A_alces)/8911)*1000 ## 3.59 moose
(sum(S2015.01$C_latrans)/8911)*1000 ##5.72 coyote
(sum(S2015.01$L_canadensis)/8911)*1000 ##3.47 lynx




##### 2016.01
# create Site x Species matrix
sp_detect <- ani.rec$Species
ani.rec$Station <- toupper(ani.rec$Station) ### Need to do to match CamStations in cam2016
st_detect <- ani.rec$Station
S2016.01 <- as.data.frame.matrix(table(st_detect,sp_detect))

S2016.01$Total <- apply(S2016.01,1,sum)

S2016.01$Richness <- apply(S2016.01[,1:15],1,function(x) sum(ifelse(x>0,1,0)))

(sum(S2016.01$R_tarandus)/9025)*1000 ## 2.43 caribou detections per 1000 trap days
(sum(S2016.01$C_lupus)/9025)*1000 ## 10.19 wolves
(sum(S2016.01$U_americanus)/9025)*1000 ## 0.33 black bears
(sum(S2016.01$O_virginianus)/9025)*1000 ## 10.97 white-tailed deer
(sum(S2016.01$A_alces)/9025)*1000 ## 4.99 moose
(sum(S2016.01$C_latrans)/9025)*1000 ## 5.21 coyote
(sum(S2016.01$L_canadensis)/9025)*1000 ## 2.99 lynx

#### 2015.01 Separate winter data for comparison
w2015.01 <- ani.2015 %>% filter(Date <"2016-04-20") ### for summer should be Date>"2016-04-19" to include April 20, 2017
### Order by date
w2015.01 <- w2015.01[order(w2015.01$Date), ]

sp_detect <- w2015.01$Species
st_detect <- w2015.01$Station
win2015 <- as.data.frame.matrix(table(st_detect,sp_detect)) 

win2015$Total <- apply(win2015,1,sum)

win2015$Richness <- apply(win2015[,1:15],1,function(x) sum(ifelse(x>0,1,0)))

write.csv(win2015, "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/recordsNov2015-Apr2016.csv")

### To calculate camera effort between 2 seasons, need to add column with SeasonBreak date
cams2015$SeasonBreak <- rep("20/04/2016", length(cams2015$CamStation))

camEff <- cameraOperation(cams2015, 
                          stationCol = "CamStation", 
                          setupCol = "DeployDate", 
                          retrievalCol = "SeasonBreak",
                          hasProblems = FALSE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)
# total camera days
sum(camEff,na.rm=T) ## 4009 in winter

(sum(win2015$R_tarandus)/4009)*1000 ## 1.75 caribou detections per 1000 trap days
(sum(win2015$C_lupus)/4009)*1000 ## 8.23 wolves
(sum(win2015$U_americanus)/4009)*1000 ## 0.25 black bears
(sum(win2015$O_virginianus)/4009)*1000 ## 19.21 white-tailed deer
(sum(win2015$A_alces)/4009)*1000 ## 3.24 moose
(sum(win2015$C_latrans)/4009)*1000 ## 5.49 coyote
(sum(win2015$L_canadensis)/4009)*1000 ## 3.74 lynx


### Separate summer data
sum2015.01 <- ani.2015 %>% filter(Date >"2016-04-19") ### for summer should be Date>"2016-04-19" to include April 20, 2017
### Order by date
sum2015.01 <- sum2015.01[order(sum2015.01$Date), ]

sp_detect <- sum2015.01$Species
st_detect <- sum2015.01$Station
summer2016 <- as.data.frame.matrix(table(st_detect,sp_detect)) 

summer2016$Total <- apply(summer2016,1,sum)

summer2016$Richness <- apply(summer2016[,1:15],1,function(x) sum(ifelse(x>0,1,0)))

write.csv(summer2016, "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/recordsApr-Nov2016.csv")

### To calculate camera effort between 2 seasons, need to add column with SeasonBreak date
cams2015$SeasonBreak <- rep("20/04/2016", length(cams2015$CamStation))

camEff <- cameraOperation(cams2015, 
                          stationCol = "CamStation", 
                          setupCol = "SeasonBreak", 
                          retrievalCol = "CheckDate1",
                          hasProblems = FALSE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)
# total camera days
sum(camEff,na.rm=T) ## 4926 in summer

(sum(summer2016$R_tarandus)/4926)*1000 ## 7.51 caribou detections per 1000 trap days
(sum(summer2016$C_lupus)/4926)*1000 ## 13.40 wolves
(sum(summer2016$U_americanus)/4926)*1000 ## 29.84 black bears
(sum(summer2016$O_virginianus)/4926)*1000 ## 33.90 white-tailed deer
(sum(summer2016$A_alces)/4926)*1000 ## 3.86 moose
(sum(summer2016$C_latrans)/4926)*1000 ## 5.89 coyote
(sum(summer2016$L_canadensis)/4926)*1000 ## 3.25 lynx

########## Detections at first 24 cameras in 2016.01
f24Stat <- cams2016[1:24,]


sum(camEff,na.rm=T) ## 3559

f24 <- f24Stat$CamStation
win2016.f24 <- ani.rec[1:167,] ## subsetted a few extra rows of Algar25....b/c ALgar18 malfunctioned?
win2016.f24 <- win2016[!win2016$Station == "ALGAR25", ]
win2016.f24 <- win2016.f24[order(win2016.f24$Date),]

sp_detect <- win2016.f24$Species
st_detect <- win2016.f24$Station
f24.2016.01 <- as.data.frame.matrix(table(st_detect,sp_detect)) 

f24.2016.01$Total <- apply(f24.2016.01,1,sum)

f24.2016.01$Richness <- apply(f24.2016.01[,1:12],1,function(x) sum(ifelse(x>0,1,0)))

(sum(f24.2016.01$R_tarandus)/3559)*1000 ## 1.97 caribou detections per 1000 trap days
(sum(f24.2016.01$C_lupus)/3559)*1000 ## 8.71 wolves
(sum(f24.2016.01$U_americanus)/3559)*1000 ## 0.56 black bears
(sum(f24.2016.01$O_virginianus)/3559)*1000 ## 13.49 white-tailed deer
(sum(f24.2016.01$A_alces)/3559)*1000 ## 2.25 moose
(sum(f24.2016.01$C_latrans)/3559)*1000 ## 6.74 coyote
(sum(f24.2016.01$L_canadensis)/3559)*1000 ## 3.09 lynx

######## Counting Animal images in 2015.01 (again) ####
### Discrepancies in  case for True/False
class(x2015.01$File)
class(x2015.01$Folder)
class(x2015.01$Animal)
class(x2015.01$Unknown)


### all factors
### Coerce True/Falses into characters (just Animal and Unknown for now)
x2015.01$Animal <- as.character(x2015.01$Animal)
x2015.01$Unknown <- as.character(x2015.01$Unknown)

x2015.01 <- data.frame(lapply(x2015.01, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})) 
table(x2015.01$Animal)
table(x2015.01$Unknown)
apply(apply(x2015.01[,15:32],2,is.na),2,sum) ## 3


A1 <- subset(x2015.01, Animal == TRUE & Unknown == FALSE)
A1$X <- NULL
