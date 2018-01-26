###############################################################
# AVI_explore.R
# Code for exploring available AVI data, extracting desired
# Started Jan. 26, 2018
###############################################################

require(raster)

require(sp)

require(rgdal)

require(rgeos)

require(dplyr) # for glimpse function

#Loading a feature class from a geodatabase (gdb)

# The input file geodatabase
fgdb <- "C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data/GIS/Algar_data_DPan/data.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="AVIE_with_DeerTypes")

# Determine the FC extent, projection, and attribute information
summary(fc) #Spatial polygons dataframe, 155 attribute fields, projection = tmerc

# View the feature class
plot(fc)

str(fc@data)
glimpse(fc@data) # Attribute explanations at: https://geodiscover.alberta.ca/geoportal/catalog/search/resource/fullMetadata.page?uuid=%7B3DBCFA02-E97A-4059-9414-1ED8E0700E80%7D

unique(fc@data$Veg_Type) #9 levels
unique(fc@data$Deer_Type) #9 levels
unique(fc@data$FC_DOM) #40 levels
unique(fc@data$FC_COMPLEX)#148 levels
unique(fc@data$MOIST_REG) # 3 Levels: a = aquatic, m =  mesic, w = wet (dry is also an moisture class but lacking here)
# AVIE_Veg_simple aggregates Veg_Type into 3 classes: Lowland (LL spruce, LL deciduous, LL mixedwood, tamarack), Upland (Upland spruce, Upland deciduous, Upland mixedwood, Pine), and Non-forest (water and anth. features)

#Visualise Veg_simple
plot(fc, col = "lightgreen") 
# Addin colour to lowland areas 
low <- fc$Veg_simple == "Lowland"
plot(fc[ low, ], col = "darkgreen", add = TRUE)
# Adding colour to nonforest areas
nofo <- fc$Veg_simple == "Non-forest"
plot(fc[ nofo, ], col = "lightblue", add = TRUE) #Comparing to WAM, there are some non-forest/lowland distinctions in AVI not picked up by WAM

# Visualise moisture regimes
mesic <- fc$MOIST_REG == "m"
plot(fc[mesic, ], col = "orange", add = TRUE) #seems to map well to upland

wet <- fc$MOIST_REG == "w"
plot(fc[wet, ], col = "magenta", add = TRUE) #wet overlaps with non-forest and lowland in places

aq <- fc$MOIST_REG == "a"
plot(fc[aq, ], col = "blue", add = TRUE) 

