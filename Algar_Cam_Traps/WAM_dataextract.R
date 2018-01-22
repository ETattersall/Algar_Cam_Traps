##################################
# WAM_dataextract.R
# Extracting WAM data for area wetness along seismic lines
# Using code from Doug MacNearney, fRI
# Modifying for Algar, started by Erin, January 18, 2018
##################################

# Add required libraries

require(raster)

require(sp)

require(rgdal)



#set working directory

setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data/GIS")



#read in raster and seismic line Camera sites:

r<-raster("WAM_250mcliip.tif")
plot(r)


cams<-readOGR(dsn=getwd(), layer="AlgarSites_April2017")

#check that projections are the same

r
## class       : RasterLayer 
## dimensions  : 88835, 67595, 6004801825  (nrow, ncol, ncell)
## resolution  : 1, 1  (x, y)
## extent      : 373262, 440857, 6204519, 6293354  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 
## data source : C:\Users\ETattersall\Google Drive\Algar Seismic Restoration Project\GIS data\GIS\AlgarWAM.tif 
## names       : AlgarWAM 
## values      : 1, 7  (min, max)--> not correct symbology


## Symbology conversion (colour map layers to Wetness index categories, see DepthToWater metadata):
# Colour map layers     Wetness Index     Interpretation
#         1                 0 - 0.1           Surface water or very wet (water near surface)
#        1-2                0.1 - 0.25          
#        2-4                0.25 - 0.5
#        4-7                0.5 - 1           Less wet area, though could be 'hyrdologically sensitive'
cams



#make sure they stack on top of each other

plot(r)

plot(cams,add=T) #Stations stack onto 250m-buffered data. Station symbols extend, but shouldn't be a problem 



## Doug's code is based on a spatial lines file that needs to be rasterized and clipped. Try for Algar cam stations (spatial points)

# Write a function using a simple functions

# seis_SLDF is a SpatialLinesDataFrame of the seismic lines.Also works for spatial points

# wam_rast is the raster of WAM values

AddWAM<-function(seis_SLDF,wam_rast){
  
  start.time<-paste("start time:", Sys.time())
  
  for(i in 1:nrow(seis_SLDF)) {
    
    tryCatch({ #tryCatch allows the rest of the function to run even if one line encounters an error
      
      clip1 <- crop(wam_rast, extent(seis_SLDF[i,])+1) #crop raster to extent of line segment.
      
      #+1 is necessary for perfectly horizontal lines
      
      #where the 'y' extent is only one cell wide.
      
      #It needs to be at least 2 wide to be considered a valid extent.
      
      clip2 <- rasterize(seis_SLDF[i,], clip1, mask=TRUE) #take the line segment and rasterize it using clip1
      
      ext <- getValues(clip2) #much faster than extract()
      
      seis_SLDF@data[i,"wam_mean"]<- mean(ext, na.rm=T) #add 'mean' attribute to seismic data
      
      seis_SLDF@data[i,"wam_min"]<- min(ext, na.rm=T)#add 'min' attribute to seismic data
      
      seis_SLDF[i,"wam_max"]<- max(ext, na.rm=T)#add 'max' attribute to seismic data
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  end.time<-paste("end time:", Sys.time())
  
  print(start.time)
  
  print(end.time)
  
  return(seis_SLDF)
  
}



#Testing function


AddWAM1<-AddWAM(cams,r) #Ran for 24 seconds




str(AddWAM1@data) #added data to data frame

head(AddWAM1@data)

#### Test 2: Removing steps to crop and rasterize from function
# spatial.df = spatial data frame object to add attribute data to
# wam_rast = raster of WAM values, already clipped to desired buffer size

AddWAM<-function(spatial.df,wam_rast){
  
  start.time<-paste("start time:", Sys.time())
  
  for(i in 1:nrow(spatial.df)) {
    
    tryCatch({ #tryCatch allows the rest of the function to run even if one line encounters an error
      
      # clip1 <- crop(wam_rast, extent(spatial.df[i,])+1) #crop raster to extent of line segment.
      
      #+1 is necessary for perfectly horizontal lines
      
      #where the 'y' extent is only one cell wide.
      
      #It needs to be at least 2 wide to be considered a valid extent.
      
      # clip2 <- rasterize(spatial.df[i,], clip1, mask=TRUE) #take the line segment and rasterize it using clip1
      
      ext <- getValues(wam_rast) #much faster than extract()
      
      spatial.df@data[i,"wam_mean"]<- mean(ext, na.rm=T) #add 'mean' attribute to seismic data
      
      spatial.df@data[i,"wam_min"]<- min(ext, na.rm=T)#add 'min' attribute to seismic data
      
      spatial.df[i,"wam_max"]<- max(ext, na.rm=T)#add 'max' attribute to seismic data
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  end.time<-paste("end time:", Sys.time())
  
  print(start.time)
  
  print(end.time)
  
  return(spatial.df)
  
}


## Testing
AddWAM2 <- AddWAM(spatial.df = cams, wam_rast = r) # Taking longer to run...makes sense because raster is bigger
# ERROR: cannot allocate vector of size 2.1 GB


plot(r)

plot(seis_test_out,add=T)

plot(seis_test_out[is.na(seis_test_out@data$wam_mean),],add=T, col="red")



diff_mean<-round(seis_out@data$WDEPTH_MEA-seis_out@data$wam_mean,0)

diff_min<-round(seis_out@data$WDEPTH_MIN-seis_out@data$wam_min,0)



hist(diff_mean)

hist(diff_min)



table(diff_mean)

table(diff_min)



#Third attempt: using parallel processing to speed things up:

#initiate multicore cluster and load packages

library(foreach)

library(doParallel)

library(tcltk)

library(sp)

library(raster)



cores<- detectCores()-1

cl <- makeCluster(cores, output="") #output should make it spit errors

registerDoParallel(cl)

#getDoParWorkers()





#Heres the function:

#Adapted from http://gis.stackexchange.com/questions/130522/increasing-speed-of-crop-mask-extract-raster-by-many-polygons-in-r

#must add uniqID field to spatial data first.

multicore.tabulate.intersect<- function(cores, lineSDF, rasterlayer){
  
  linelist<-split(lineSDF, rep(1:cores, len=nrow(lineSDF@data)))
  
  foreach(i=1:cores, .packages= c("raster","tcltk","foreach"), .combine = rbind) %dopar% {
    
    
    
    mypb <- tkProgressBar(title = "R progress bar", label = "", min = 0, max = length(linelist[[i]]), initial = 0, width = 300)
    
    
    
    foreach(j = 1:length(linelist[[i]]), .combine = rbind) %do% {
      
      final<-data.frame()
      
      tryCatch({ #not sure if this is necessary now that I'm using foreach, but it is useful for loops.
        
        
        
        single <- linelist[[i]][j,] #pull out individual line to be tabulated
        
        
        
        dir.create (file.path("C:/Users/dmacnearney_FOO/Desktop/rtemp",i,j,single@data$uniqID), showWarnings = FALSE) #creates unique filepath for temp directory
        
        rasterOptions(tmpdir=file.path("C:/Users/dmacnearney_FOO/Desktop/rtemp",i,j, single@data$uniqID))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of lines
        
        
        
        clip1 <- crop(rasterlayer, extent(single)+res(rasterlayer)[1]) #crop to extent of line, + another cell on each side for those that have non-valid extent
        
        clip2 <- rasterize(single, clip1, mask=TRUE) #crops to line edge & converts to raster
        
        ext <- getValues(clip2) #much faster than extract
        
        
        
        wam_mean<-mean(ext, na.rm=T)
        
        wam_min<-min(ext, na.rm=T)
        
        wam_max<-max(ext, na.rm=T)
        
        
        
        mat<- as.data.frame(cbind(wam_mean,wam_min,wam_max),col.names=c("wam_mean","wam_min","wam_max"))
        
        final<-cbind(single@data$uniqID,mat) #combines it with the name of the line
        
        unlink(file.path("C:/Users/dmacnearney_FOO/Desktop/rtemp",i,j,single@data$uniqID), recursive = TRUE,force = TRUE) #delete temporary files
        
        setTkProgressBar(mypb, j, title = "number complete", label = j)
        
        
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #trycatch error so it doesn't kill the loop
      
      
      
      return(final)
      
    } 
    
    #close(mypb) #not sure why but closing the pb while operating causes it to return an empty final dataset... dunno why.
    
    
    
  }
  
  
  
}



#Running the function:

#First add uniqID field to seismic data:

seis@data$uniqID <- sapply(seis@lines, function(x) {x@ID} )

nrow(seis@data[unique(seis@data$uniqID),])

#testing subset:

#seis_test<-seis[1:100,]



#run the function to get the attribute table to append:

Sys.time()

myoutput <- multicore.tabulate.intersect(cores,seis, r)

Sys.time()

registerDoSEQ()

close()



#roughly 1500/minute, so 15x faster (makes sense, using 15 cores)



#merge output with seismic line data to create new seismic line object:

seis_out<-merge(seis,myoutput,by.x="uniqID",by.y="single@data$uniqID",all.x=T)



str(seis_out@data)

head(seis_out@data)



plot(r)

plot(seis_out,add=T)

plot(seis_out[is.na(seis_out@data$wam_mean),],add=T, col="red")



diff_mean<-round(seis_out@data$WDEPTH_MEA-seis_out@data$wam_mean,0)

diff_min<-round(seis_out@data$WDEPTH_MIN-seis_out@data$wam_min,0)



hist(diff_mean)

hist(diff_min)



table(diff_mean)

table(diff_min)



seis_out

#write the shapefile

writeOGR(obj=seis_out, dsn=getwd(), layer="seismic_test_out", driver="ESRI Shapefile")

