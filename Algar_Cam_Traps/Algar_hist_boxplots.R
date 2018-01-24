###############################################
# Script for creating Algar detection plots
# Erin T., Jan. 24, 2018
# Input = camera trap record table, where each row = detection
# Columns = Station, Species, Date, Time, etc
# Separate data frame for station-specific data (utmE, utmN, Treatment, etc)
##############################################


library(ggplot2)

#### create Site x Species matrix ####
All.rec <- read.csv("recordTable_nov2015-nov2017.csv") #Data already cleaned, just loading in
# All.rec$Station <- toupper(All.rec$Station) ### Need to do to match CamStations in cam2016
st_detect <- All.rec$Station ##Will need to add row for Algar32 at some point (no detections)
sp_detect <- All.rec$Species

sp.plot1 <- rev(sort(table(sp_detect)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)



### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

ggplot(data = sp.plot1, aes(x = sp_detect, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("White-tailed deer", "Sandhill crane", "Black bear", "Grey wolf", "Snowshoe hare", "Bird spp.", "Coyote", "Moose", "Woodland caribou", "Human", "Canada lynx", "Red squirrel", "Red fox", "American marten", "Cougar", "Fisher", "Wolverine", "River otter", "Beaver"))

S <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns

# species totals to compare with table(sp_detect)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot2) <- c("NumSites")
fix(sp.plot2) #Changing scientific names to common


## Plot for number of sites per species
ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = NumSites))+ geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Number of Sites") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Black bear", "Grey wolf", "Sandhill crane", "Bird spp.", "Moose", "White-tailed deer", "Woodland caribou", "Canada lynx", "Snowshoe hare", "Coyote", "Human", "Red fox", "Red squirrel", "American marten", "Cougar", "Fisher","River otter", "Wolverine", "Beaver"))



# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S[,1:19],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:19] to specify counting only species 
## columns

# add coordinates and treatment to dataframe
S$utmE <- camdata$utmE[match(row.names(S),camdata$CamStation)]
S$utmN <- camdata$utmN[match(row.names(S),camdata$CamStation)]

S$Treatment <- camdata$Treatment[match(row.names(S),camdata$CamStation)]
str(S)
unique(S$Treatment)
fix(S) #Species columns need to be one word

# plot spatial variation by station [note: need to specify species columns now that other variables added]
with(S, symbols(x=utmE, y=utmN, circles=Total, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Total animal detections by camera station"))

with(S, symbols(x=utmE, y=utmN, circles=Richness, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Species richness by camera station"))

# Box plots for detections and richness by treatment
ggplot(data = S, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

ggplot(data = S, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Species Richness") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))
