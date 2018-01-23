############################################
# Camelot_rtDataextract.R
# Extracting detection data from recordTable's created by Camelot
# Original code from 'Algar_code_organize.R', 'SnowData_extract.R'
# Started by E Tattersall on Jan. 23, 2018
############################################

library(dplyr)
library(tidyr)
library(ggplot2)

## Import recordTables created by Camelot
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
rt <- read.csv("2017.01_rawrecordTable.csv") #Other record table has already had 'No Animal' detections removed

str(rt)
Nan <- rt %>% filter(Species == "No Animal") %>% select(Station,Species, DateTimeOriginal,Date,Time)
TL <- rt %>% filter(Time == "12:00:00") #7640 (26 of which are marked as a Species), closer than 7492 but not all
wtf <- rt %>% filter(Time == "12:00:00" & Species != "No Animal") #0 --> Discrepancy resolved :) 

## Remove Species = No Animal (Timelapse and Misfires)
rt <- rt[!rt$Species == "No Animal", ] # 1221 obs., fewer than the 1410 reported in other Camelot summaries
unique(rt$Species)
summary(rt$Species)
# Camelot summary output determines independent obs. bade on specific criteria. For consistency with early datam use detections from record table

write.csv(rt, "2017.01_recordTable.csv")

### Data summarising using camtrapR record tables (rec.spec), graphing results
### mostly Cole's code

## No. of detections by species and station
sp_detect <- rt$Species
st_detect <- rt$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.5,ylim=c(0,350))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1.2,labels=names(sp.plot),xpd=TRUE)

## Remove humans, mustelid spp and other birds (in the most roundabout way ever...)
ani.rec <- rt[!rt$Species == "Homo sapiens", ]
ani.rec <- ani.rec[!ani.rec$Species == "Unknown species", ] ## Omits unknowns
## Gather all birds except sandhill crane into 1 category "Bird spp."
ani.rec$Species <- gsub("Perisoreus canadensis", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Colaptes auratus", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Tympanuchus phasianellus", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Strix nebulosa", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Canachites canadensis", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Branta canadensis", "Bird spp.", ani.rec$Species)

unique(ani.rec$Species)




sp_detect <- ani.rec$Species
st_detect <- ani.rec$Station

sp.plot1 <- rev(sort(table(sp_detect)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)
colnames(sp.plot1) <- c("Species", "Freq")
write.csv(sp.plot1, "SpDetectionSummary_nov2017.csv")


### Frequency histograms
sp.1 <- ani.rec$Species
sp.plot1 <- rev(sort(table(sp.1)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)
ggplot(data = sp.plot1, aes(x = sp.1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Sandhill crane", "Black bear", "White-tailed deer", "Grey wolf", "Snowshoe hare", "Bird spp.", "Woodland caribou",  "Moose", "Coyote",  "Canada lynx", "Red squirrel", "Red fox", "American marten", "River otter", "Beaver"))

