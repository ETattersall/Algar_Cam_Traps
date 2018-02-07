##############################################
# Species_plots.R
# Plotting species detection by treatment
# Started Feb5. 2018
##############################################

library(ggplot2)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)
str(dat)

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

w <- ggplot(data = dat, aes(x = Treatment, y = Wolf, fill = Treatment)) + geom_boxplot()
w + theme_classic() + xlab("Sampling Strata") + ylab("Wolf detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

wsnow <- ggplot(data = dat, aes(x=SnowDays, y = Wolf)) +geom_point()+ theme_classic() + xlab("Snow Days/month") + ylab("Wolf detections/month")
wsnow

plot(Wolf~SnowDays,
     data = dat,
     col = alpha("azure4", 1),
     pch = 16, cex = 1.0,
     xlab = "Snow Days/month",
     ylab = "Wolf detections/month")

wolf.snow <- ggplot(data = dat, aes(x = SnowDays, y = Wolf)) + geom_point() + theme_classic() + xlab("Snow Days/month") + ylab("Wolf detections/month")
wolf.snow


#### Caribou ####
CA <- ggplot(data = dat, aes(x = Treatment, y = Caribou, fill = Treatment)) + geom_boxplot()
CA + theme_classic() + xlab("Sampling Strata") + ylab("Caribou detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

ca.low <- ggplot(data = dat, aes(x = low500, y = Caribou)) + geom_point() + theme_classic() + xlab("Proportion lowland habitat (500m buffer)") + ylab("Caribou detections/month")
ca.low


#### WTDeer ####
WTD <- ggplot(data = dat, aes(x = Treatment, y = WTDeer, fill = Treatment)) + geom_boxplot()
WTD + theme_classic() + xlab("Sampling Strata") + ylab("WTDeer detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

WTD.low <- ggplot(data = dat, aes(x = low500, y = WTDeer)) + geom_point() + theme_classic() + xlab("Proportion lowland habitat (500m buffer)") + ylab("WTDeer detections/month")
WTD.low

WTD.snow <- ggplot(data = dat, aes(x = SnowDays, y = WTDeer)) + geom_point() + theme_classic() + xlab("Snow Days/month") + ylab("WTDeer detections/month")
WTD.snow

#### Updated individual species plots
updat <- read.csv("MonthlyDetections_nov2015-nov2017.csv")

## Wolf
ggplot(data = updat, aes(x = Treatment, y = Wolf, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Wolf detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Caribou
ggplot(data = updat, aes(x = Treatment, y = Caribou, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Caribou detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# WTDeer
ggplot(data = updat, aes(x = Treatment, y = WTDeer, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("WT deer detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Moose
ggplot(data = updat, aes(x = Treatment, y = Moose, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Moose detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))
