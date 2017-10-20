#################################################
# Combining Algar survey data sets
# Pilot data (Algar01 - Algar 24) with Winter 2016-17 data (Algar01-60)
# Started by Erin T. on Oct. 19, 2017
#################################################

library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

# No. detections/ month data
pilot <- read.csv("2015.01_monthlydetections.csv")
pilot$X <- NULL
win60 <- read.csv("2016.01_monthlydetections.csv")
win60$X <- NULL

# Full dataset will have NAs for the months that Algar25-60 were inactive (Nov. 2015-Oct.2016)
# 11 months, 36 cameras = 396 rows of NAs

# Algar01 - 24 from 2nd deployment
Alg24 <- win60[1:138, ]

# Combine all 24 cameras for full survey
alldat <- rbind(pilot,Alg24)

## Double counts for Nov. 2016 (counted in both surveys)?
nov2016 <- alldat %>% filter(Yr_Month == "2016-11") ## Yes. Need to be combined

## Use aggregate function?

