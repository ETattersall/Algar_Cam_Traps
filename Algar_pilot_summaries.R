### Data summarising using camtrapR record tables (rec.noTimelapse), graphing results
### Started by Erin Tattersall on Jan. 26, 2017

library(dplyr)
library(tidyr)
library(ggplot2)

##Read in station data
setwd(images_wd)
cams2015 <- read.csv("Algar2015StationData.csv")

## Plot station locations in utm
with(cams2015,plot(utmE,utmN))

## No. detections by species and station as data frames 
spec_detect <- data.frame (rec.noTimelapse %>% count(Species))


Station_detect <- data.frame (rec.noTimelapse %>% count(Station))

### Or you can take it directly from rec.noTimelapse

sp_detect <- rec.noTimelapse$Species
st_detect <- rec.noTimelapse$Station

### Frequency histograms, using Cole's code (Algar_pilot_Cole.R)

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.4,ylim=c(0,250))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot),xpd=TRUE)

# create Site x Species matrix
S <- as.data.frame.matrix(table(st_detect,sp_detect))

# species totals to compare with table(sp_detect)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- rev(sort(sp.sites)) 
xvals <- barplot(sp.plot2,names.arg = NA,col="royalblue4",ylab = "Number of sites",cex.lab=1.4)
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot2),xpd=TRUE)

# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S[,1:17],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:17] to specify counting only species 
## columns

# add coordinates and treatment to dataframe
S$utmE <- cams2015$utmE[match(row.names(S),cams2015$CamStation)]
S$utmN <- cams2015$utmN[match(row.names(S),cams2015$CamStation)]

S$Treatment <- cams2015$Treatment[match(row.names(S),cams2015$CamStation)]

