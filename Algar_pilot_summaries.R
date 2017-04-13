### Data summarising using camtrapR record tables (rec.noTimelapse), graphing results
### Started by Erin Tattersall on Jan. 26, 2017

library(dplyr)
library(tidyr)
library(ggplot2)
library(camtrapR)

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

## t = -0.99928, df = 19.572, p-value = 0.3299
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -44.55328  15.71994
## sample estimates:
## mean in group Control mean in group Site Prep Plant 
## 37.16667                      51.58333 

# richness
boxplot(S$Richness~S$Treatment,cex.axis=1.3,ylab="Species Richness",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$Richness,S$Treatment,mean)
tapply(S$Richness,S$Treatment,sd)
t.test(S$Richness~S$Treatment)

## data:  S$Richness by S$Treatment
## t = -2.2385, df = 22, p-value = 0.03563
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -4.1740103 -0.1593231
## sample estimates:
##  mean in group Control mean in group Site Prep Plant 
##   6.000000                      8.166667 

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

tapply(S$O_virginianus,S$Treatment,sd)
tapply(S$O_virginianus,S$Treatment,median)
t.test(S$O_virginianus~S$Treatment)

# moose
boxplot(S$A_alces~S$Treatment,cex.axis=1.3,ylab="Moose detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$A_alces,S$Treatment,mean)
tapply(S$A_alces,S$Treatment,sd)
tapply(S$A_alces,S$Treatment,median)
t.test(S$A_alces~S$Treatment)