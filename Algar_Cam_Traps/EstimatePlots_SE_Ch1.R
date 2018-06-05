#############################################
## EstimatePlots_SE_Ch1.R
## Plotting predictor estimates with their SE, instead of CI
## Using model averaging outputs from the each species .Rmd file (Ch1_models_SPECIES.Rmd)
## June 4, 2018
############################################

library(ggplot2)

## Caribou
Caribou <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Caribou) <- "Predictor"

#Estimates
Caribou$Estimate <- c(-1.523, 0.554, -0.173, 2.950, -1.431, -0.486, 0.025, -0.269)
Caribou$StdError <- c(1.204, 0.549, 0.663, 0.643, 0.348, 0.589, 0.167, 0.629)

ggplot(data = Caribou, aes( x = Predictor, y = Estimate)) + geom_point() + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + scale_y_continuous(limits = c(-4, 4))

## Wolf
Wolf <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Wolf) <- "Predictor"

#Estimates
Wolf$Estimate <- c(0.062, 0.475, 1.379, -0.571, -0.729, 0.061, 0.022, 0.036)
Wolf$StdError <- c(0.590, 0.509, 0.542, 0.434, 0.263, 0.200, 0.151, 0.177)

ggplot(data = Wolf, aes( x = Predictor, y = Estimate)) + geom_point() + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 14)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 15)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 15)) + theme(strip.text = element_text(colour = "black", size = 14)) + scale_y_continuous(limits = c(-2, 2))

## Blackbear
Blackbear <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "VegHt"))
colnames(Blackbear) <- "Predictor"

#Estimates
Blackbear$Estimate <- c(-0.110, 0.139, 0.367, -0.063, 0.522)
Blackbear$StdError <- c(0.386, 0.339, 0.504, 0.197, 0.553)

ggplot(data = Blackbear, aes( x = Predictor, y = Estimate)) + geom_point() + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 14)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 15)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 15)) + theme(strip.text = element_text(colour = "black", size = 14)) + scale_y_continuous(limits = c(-1.25, 1.25))

## WTD
WTD <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(WTD) <- "Predictor"

#Estimates
WTD$Estimate <- c(-0.319, -1.448, -0.512, -1.970, -0.171, 0.783, -0.007, 0.185)
WTD$StdError <- c(0.490, 0.487, 0.544, 0.408, 0.322, 0.358, 0.125, 0.334)

ggplot(data = WTD, aes( x = Predictor, y = Estimate)) + geom_point() + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 14)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 15)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 15)) + theme(strip.text = element_text(colour = "black", size = 14)) + scale_y_continuous(limits = c(-2.5, 2.5))

##Moose
Moose <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Moose) <- "Predictor"

#Estimates
Moose$Estimate <- c(-0.306, -0.168, 0.376, -0.064, 0.005, -0.042, 0.550, 0.875)
Moose$StdError <- c(0.523, 0.451, 0.516, 0.201, 0.169, 0.168, 0.426, 0.358)

ggplot(data = Moose, aes( x = Predictor, y = Estimate)) + geom_point() + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 14)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 15)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 15)) + theme(strip.text = element_text(colour = "black", size = 14)) + scale_y_continuous(limits = c(-1.25, 1.25))
