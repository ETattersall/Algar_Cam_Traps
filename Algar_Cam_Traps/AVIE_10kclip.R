###########################
# AVIE_10kclip.R
# Deriving Veg_simple for larger buffer area surrounding Algar study site
# Started March 16, 2018 by Erin T.
##########################

require(raster)

require(sp)

require(rgdal)

require(rgeos)

require(dplyr) # for glimpse function

require(tidyr) #for gather function

require(ggplot2) #for plotting data across scales

# Spatial data for Algar located on Algar Project Google Drive
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/3. Data/3.1 GIS")

# External hard drive directory (when working on lab computer)
setwd("F:/AVIE_dataR")

## Veg_Simple was derived from AVIE moisture regime and forest cover. Need to created the same field
# for the new AVIE data (Algar + 10k buffer)
# Should be able to relate Veg_Simple from old AVIE to new
# Need to create a field that aggregates moisture regime and FCComplex in both lookup and new AVIE attribute table


#### If lookup tables are available ######

## Load in lookup table from AVIE_DeerTypes
#Lookup using dominant forest cover + moisture regime (shouldn't need this one)
VS.DOM <- read.csv("AVIE_VegSimpleLookUp.csv")
VS.DOM$X <- NULL
VS.DOM$X.1 <- NULL
VS.DOM$X.2 <- NULL

## Load in attribute table for AVIE_with_DeerTypes for comparison
DeerTypes <- read.csv("AVIE_with_DeerTypes.csv")
#######
## No lookup table --> open AVIE_with_DeerTypes layer
DeerTypes <- readOGR(dsn = "Algar_data_DPan/data.gdb", layer = "AVIE_with_DeerTypes")
DeerTypes <- DeerTypes@data ## attribute table
colnames(DeerTypes)

unique(DeerTypes$MOIST_REG) #3 levels: m,w,a
unique(DeerTypes$FC_COMPLEX) #148 levels
unique(DeerTypes$FC_DOM)#40 levels
unique(DeerTypes$FC_SUBDOM)#24 levels
unique(DeerTypes$Deer_Type)
DeerTypes$MOIST_FC <- paste(DeerTypes$MOIST_REG, DeerTypes$FC_COMPLEX)
unique(DeerTypes$MOIST_FC) #164. Matches lookup table

#Lookup using complex forest cover (dominant + subdom) + moisture regime
VS.complex <- read.csv("AVIE_VegSimpleLookUpFCCOMPLEX.csv")
#New column combining moisture and forest cover
VS.complex$MOIST_FC <- paste(VS.complex$MOIST_REG, VS.complex$FC_COMPLEX)

### Combine FC_DOM, "Deer_Type", and "Veg_Type" to compare
tree <- cbind.data.frame(DeerTypes$FC_DOM, DeerTypes$Deer_Type)
tree$Veg_Type <- DeerTypes$Veg_Type
tree$Veg_Simple <- DeerTypes$Veg_Simple
colnames(tree) <- c("DOM", "Deer", "Veg")

unique(tree$Veg)
unique(tree$Deer)

# Load in new AVIE layer
AVIE <- readOGR("AVIE_data", "AVIE_Algar_10k_Clip")
proj4string(AVIE) #tmerc lat-long, NAD83 (units = m)
class(AVIE) #spatial polygons data frame
att <- AVIE@data #attribute table

# Combine MOIST and FC_COMPLEX in attribute table?
att$MOIST_FC <- paste(att$MOIST_REG, att$FC_COMPLEX)
head(att$MOIST_FC)
attMOIST_FC <- as.data.frame(unique(att$MOIST_FC)) #240 unique combinations, does not match lookup table. Investigate
unique(att$MOIST_REG) #4 levels: w, m , d, a --> d = ??
unique(att$FC_COMPLEX) #212 levels
unique(att$FC_DOM) #43
unique(att$FC_SUBDOM) #27
unique(att$MOIST_FC)#240


## Test on subset of attribute table first
T <- cbind.data.frame(att$MOIST_REG, att$FC_COMPLEX, att$MOIST_FC)
colnames(T) <- c("MOIST_REG", "FC_COMPLEX", "MOIST_FC")
unique(T$MOIST_FC)#240
table(T$MOIST_REG) #3 d's
T[which(T$MOIST_REG=="d"),] #d = dry --> either upland (Aspen/Grassland Dry) or Non-forest (industrial)

##Compare MOIST_FC between old and new AVIE
#append, then check unique
MOIST_FC <- append(unique(att$MOIST_FC), VS.complex$MOIST_FC)
unique(MOIST_FC)# 240 unique levels (where old does not include NA in MOIST_REG for some features)


## Can I match Veg_Simple between lookup table and attribute table? Will be lots of blanks for combinations in new AVIE that were not present in old
att$Veg_Simple <- VS.complex$Veg_simple[match(att$MOIST_FC, VS.complex$MOIST_FC)]
## Test on subset of attribute table first (only contains desired fields)
T <- cbind.data.frame(att$MOIST_REG, att$FC_COMPLEX, att$MOIST_FC)
colnames(T) <- c("MOIST_REG", "FC_COMPLEX", "MOIST_FC")
unique(T$MOIST_FC)#240
table(T$MOIST_REG) #3 d's
T[which(T$MOIST_REG=="d"),] #d = dry --> either upland (Aspen/Grassland Dry) or Non-forest (industrial)
# Matching Veg_Simple
T$Veg_Simple <- VS.complex$Veg_simple[match(T$MOIST_FC, VS.complex$MOIST_FC)]

#How many elements do not have a Veg_Simple category?
No.VS <- T[is.na(T$Veg_Simple), ] #485 elements/22746 --> not many...
#Unique MOIST_FC from new AVIE with no Veg_Simple
NoVS <- as.data.frame(unique(No.VS$MOIST_FC))#81 levels of MOIST_FC have no classification
colnames(NoVS) <- "MOIST_FC"

#Create a Veg_Simple classification for new MOIST_FC, based off of old criteria
NoVS$Veg_Simple <- rep(NA,81)
# River, Lakes/Ponds, Plant Sites/Sewage Lagoons, Cutbank/Sand, Flooded --> Non-forest
NoVS[which(NoVS$MOIST_FC=="NA Flooded" | NoVS$MOIST_FC=="NA River"| NoVS$MOIST_FC=="NA Lakes/Ponds"| NoVS$MOIST_FC=="NA Plant Sites/Sewage Lagoons"| NoVS$MOIST_FC=="NA Cutbank/Sand"| NoVS$MOIST_FC=="NA Flooded" ), "Veg_Simple"] <- "Non-forest"
# New categories assessed individually (Rock Barren = NF, Rural residential = NF)
NoVS[which(NoVS$MOIST_FC=="NA Rock Barren" |NoVS$MOIST_FC=="NA Rural Residential"), "Veg_Simple"] <- "Non-forest"

#Add in MOIST_REG and FC_COMPLEX for classification
NoVS$MOIST_REG <- T$MOIST_REG[match(NoVS$MOIST_FC, T$MOIST_FC)]
NoVS$FC_COMPLEX <- T$FC_COMPLEX[match(NoVS$MOIST_FC, T$MOIST_FC)]

#2 MOIST_FC classified as d for moisture - either upland (Aspen/Grassland Dry) or Non-forest (industrial)
NoVS[which(NoVS$MOIST_FC== "d Aspen / Grassland Dry"), "Veg_Simple"] <- "Upland"
NoVS[which(NoVS$MOIST_FC== "d Industrial Reclamation-Vegetated"), "Veg_Simple"] <- "Non-forest"


# Non-forest: FC_COMPLEX = Bryophytic, Forb Meadow, Grassland Mesic, Herbaceous Clearing, Herbaceous Clearcut,
# Industrial Reclamation-Vegetated, Shrub Meadow, Shrub Grassland
NoVS[which(NoVS$FC_COMPLEX=="Bryophytic" | NoVS$FC_COMPLEX=="Forb Meadow"), "Veg_Simple"] <- "Non-forest"
# MOIST_REG = w --> Lowland. Exceptions are Non-forest mentioned above
## For complicated FC_COMPLEX names, change manually
fix(NoVS)
table(NoVS$Veg_Simple)#31 Lowland, 15- Non-forest, 1 Upland accounted for --> 34 to go!
# MOIST_REG = m --> Lowland, Exceptions being non-forest mentioned above
fix(NoVS)
table(NoVS$Veg_Simple)#81 --> all now have Veg_Simple

## Relate back to test table of relevant attributes
table(is.na(T$Veg_Simple))#485 rows with NAs

## Need to combine Veg_simple from VS.complex and NoVS
VEGCOM.old <- cbind.data.frame(VS.complex$MOIST_FC, VS.complex$Veg_simple)
colnames(VEGCOM.old) <- c("MOIST_FC", "Veg_Simple")
VEGCOM.new <- cbind.data.frame(NoVS$MOIST_FC, NoVS$Veg_Simple)
colnames(VEGCOM.new) <- c("MOIST_FC", "Veg_Simple")
NEWveg <- rbind.data.frame(VEGCOM.old, VEGCOM.new) #5 more MOIST_FC than new because some Non-forest are duplicated (don't include NA in old set but do in new)

T$Veg_Simple <- NEWveg$Veg_Simple[match(T$MOIST_FC, NEWveg$MOIST_FC)]
table(is.na(T$Veg_Simple)) # No NAs!!

## Attach new Veg_Simple to attribute table
att$Veg_Simple <- T$Veg_Simple[match(T$MOIST_FC, att$MOIST_FC)]
## But is the Veg_Simple field now available spatially??? Probably not

## Export NEWveg as new lookup table to perform join in Arc
write.csv(NEWveg, "Aggregate_LookUp.csv")


#### March 19, 2018: Clipping buffers and calculating lowland habitat for new AVIE layer ####
## Most code copied from Algar_scale_analysis.R
AVIE <- readOGR("AVIE_data", "Algar10k_VegSimple")

summary(AVIE) # Spatial polygons data frame, projected in NAD83. 
class(AVIE) #Veg_Simple has 1533 NAs = 6.7%
# Check MOIST_FC for NAs in Veg_Simple
AVIEatt <- AVIE@data
NAVS <- unique(AVIEatt[is.na(AVIEatt$Veg_Simple), "MOIST_FC"]) #53 unique entries, but 237 levels...
#leave for now, compare ending proportions to see if it makes a large difference
plot(AVIE)

Algcoord <- readOGR("AVIE_data", "AlgarSites_April2017")
summary(Algcoord) #in utm
plot(Algcoord, pch = 18, col = "red")
Cams <- Algcoord@coords #matrix of coordinates
Algcoord@data

### Will do 8 buffers for scale analysis. Start with 500m to compare to old AVIE layer, then complete for other scales
b500 <- gBuffer(Algcoord, width = 500, byid = TRUE) #byid=TRUE to apply function to subgeometries
summary(b500)#SpatialPolygons class, projected in utm NAD83, retains all station data
# Confirming buffers were drawn
plot(Algcoord, pch = 18, col = "red")
plot(b500)

#### Extracting landcover data for that buffer ####
# comparing CRS between layers
proj4string(b500)# NAD83 utm zone 12
proj4string(AVIE)# NAD83 tmerc

# Converting AVIE to UTM
AVIE_UTM <- spTransform(AVIE, CRSobj = CRS(proj4string(Algcoord)))
proj4string(AVIE_UTM)
AVIE <- AVIE_UTM #Overwrite original AVIE CRS

#Clipping landcover data for buffers using raster::intersect 

int500 <- raster::intersect(AVIE,b500)
plot(int500)
summary(int500)
data500 <- int500@data
str(data500)
## Only need Camera data + Veg_Simple
data500 <- data500 %>% select(CamStation, utmE,utmN, Treatment, lat_decdeg,lon_decdeg, Veg_Simple)
## Rows = each polygon at each site. Need to be aggregated (will do after areas calculated)

A500 <- gArea(int500, byid = TRUE)#Calculates area for each landcover type at each station 
class(A500) #Returns numeric object containing 109 areas
data500$AREA_m <- A500 # Adding cover area to data

# Aggregate same land cover polygons at each site
data500 <- aggregate(.~ CamStation + utmE + utmN + Treatment + lat_decdeg + lon_decdeg + Veg_Simple, data = data500, sum)

## Order data by Veg_Simple, then by CamStation (want all Lowland to be at top)
data500 <- data500[with(data500, order(Veg_Simple, CamStation)), ]
summary(data500)# Lowland at all 50 sites, Non-forest at 48, Upland at 43

#Function for calculating total area of a buffer
bufferArea <- function(r){
  Area <- pi*(r)^2
  print(Area)
}

data500$TOTAREA <- bufferArea(500) #Adding total area 
data500$Percent_cover <-  data500$AREA_m/data500$TOTAREA #Calculating percent landcover for each Veg_Simple type at each station

#### Compiling a dataset of % lowland cover
lowland <- data500[1:60, ]
hist(lowland$Percent_cover)

## Compare to prop. landcover from old AVIE
old.low <- read.csv("Lowlandcover_9buffersizes.csv")
compare <- as.data.frame(lowland$CamStation)
compare$low500NEW <- lowland$Percent_cover
compare$low500OLD <- old.low$Prop500

plot(compare$low500NEW, compare$low500OLD)## should be ~1:1
#Not quite 1:1 --> old proportions of lowland higher than new at many sites

#Calculate difference 
compare$DIFF <- compare$low500OLD - compare$low500NEW
hist(compare$DIFF)
summary(compare$DIFF)
##      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.40562  0.00000  0.01361  0.05876  0.15611  0.65768 

## Export new prop.low at 500m buffer for modelling to compare results to old AVIE data
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
write.csv(lowland, "proplowland_500mbuffer_newAVIE.csv")

## Given that I know the decisions going into creating new Veg_Simple and am aware of its' flaws (whereas I know neither for old Veg_Simple), I will continue to use new Veg_Simple

#### Mar. 21, 2018: Calculating other buffer areas and prop. lowland ####
## Need data frame for proportion lowland habitat calculated at each buffer size
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
#Start with 500mVegSimple already created
low500 <- read.csv("proplowland_500mbuffer_newAVIE.csv")
low500 <- low500 %>% select(CamStation, Treatment, Percent_cover)
colnames(low500) <- c("CamStation", "Treatment", "low500")
# Spatial data for Algar located on Algar Project Google Drive
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data")
AVIE <- readOGR("AVIE_InnotechMar2018", "Algar10k_VegSimple")
summary(AVIE)
proj4string(AVIE) #tmerc

# Converting AVIE to UTM
AVIE_UTM <- spTransform(AVIE, CRSobj = CRS(proj4string(Algcoord)))
proj4string(AVIE_UTM)
AVIE <- AVIE_UTM #Overwrite original AVIE CRS

## Starting dataframe to hold all lowland props
prop.low <- as.data.frame(low500$CamStation)
colnames(prop.low) <- "CamStation"

## Function for extracting AVIE at various buffer scales (m) around camera sites and calculating proportion of lowland habitat
#Req. packages: rgeos, raster, dplyr
propLow <- function(buffer){
  a <- gBuffer(Algcoord, width = buffer, byid = TRUE) #Create buffer
  b <- raster::intersect(AVIE, a)#Clip AVIE data
  data <- b@data #Isolate dataframe
  data <- data %>% select(CamStation, utmE,utmN, Treatment, lat_decdeg,lon_decdeg, Veg_Simple)#Select relevant columns
  data$Area <- gArea(b, byid=TRUE) #Calculate area
  data <- aggregate(.~ CamStation + utmE + utmN + Treatment + lat_decdeg + lon_decdeg + Veg_Simple, data = data, sum) #Aggregate same landcover polygons at each site
  data <- data[with(data, order(Veg_Simple, CamStation)), ] #Order by Veg_Simple and CamStation
  data$BufferArea <- bufferArea(buffer) #bufferArea function created above
  data$Percent_cover <- data$Area/data$BufferArea
  data <- data %>% select(CamStation, Treatment,Percent_cover)
  data <- data[1:60, ] #Only want Lowland habitat
}

## Calculating lowland habitat at various scales
low250 <- propLow(250)
colnames(low250) <- c("CamStation", "Treatment", "low250")
low750 <- propLow(750)
colnames(low750) <- c("CamStation", "Treatment", "low750")
low1000 <- propLow(1000)
colnames(low1000) <- c("CamStation", "Treatment", "low1000")
low1250 <- propLow(1250)
colnames(low1250) <- c("CamStation", "Treatment", "low1250")
low1500 <- propLow(1500)
colnames(low1500) <- c("CamStation", "Treatment", "low1500")
low1750 <- propLow(1750)
colnames(low1750) <- c("CamStation", "Treatment", "low1750")
low2000 <- propLow(2000)
colnames(low2000) <- c("CamStation", "Treatment", "low2000")

prop.low$low250 <- low250$low250
prop.low$low500 <- low500$low500
prop.low$low750 <- low750$low750
prop.low$low1000 <- low1000$low1000
prop.low$low1250 <- low1250$low1250
prop.low$low1500 <- low1500$low1500
prop.low$low1750 <- low1750$low1750
prop.low$low2000 <- low2000$low2000

write.csv(prop.low, "newAVIE_lowland8buffersizes.csv")

# Rename Proportion columns (need to be numeric before plotting)
colnames(prop.low) <- c("CamStation", "250", "500", "750", "1000", "1250", "1500", "1750", "2000")

#Gather Scales into one colum
P1 <- gather(data = prop.low, key = Scale, value = Prop_low, 2:9)
str(P1) #Scale is character class, convert to numeric
P1$Scale <- as.numeric(P1$Scale)
str(P1)

## Plotting lowland proportions
hist(P1$Prop_low) #Skewed to higher %, but not as much as previously

#Plotting %lowland against scale
fig.a <- ggplot(data = P1, aes(x = Scale, y = Prop_low, fill = Scale)) + geom_point()
fig.a + geom_smooth(method = "lm")

## Checking R-sq. in linear model
library(lme4)
low.scale <- lm(formula = Prop_low~Scale, data = P1)
summary(low.scale) # R-squared: 0.005 --> 

### Little change in prop.lowland across scales: stick with 500m

