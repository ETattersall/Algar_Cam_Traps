#############################################
## EstimatePlots_SE_Ch1.R
## Plotting predictor estimates with their SE, instead of CI
## Using model averaging outputs from the each species .Rmd file (Ch1_models_SPECIES.Rmd)
## June 4, 2018
############################################

library(ggplot2)

## Caribou
Caribou <- as.data.frame(c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Caribou) <- "Predictor"

#Estimates
Caribou$Estimate <- c(-1.523, 0.554, -0.173, 2.950, -1.431, -0.486, 0.025, -0.269)
Caribou$StdError <- c(1.204, 0.549, 0.663, 0.643, 0.348, 0.589, 0.167, 0.629)

ggplot(data = Caribou, aes(x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-4, 4))

## Wolf
Wolf <- as.data.frame(c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Wolf) <- "Predictor"

#Estimates
Wolf$Estimate <- c(0.062, 0.475, 1.379, -0.571, -0.729, 0.061, 0.022, 0.036)
Wolf$StdError <- c(0.590, 0.509, 0.542, 0.434, 0.263, 0.200, 0.151, 0.177)

ggplot(data = Wolf, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-2, 2))

## Blackbear
Blackbear <- as.data.frame(c("NatRegen", "Treated", "HumanUse", "Lowland", "VegHt"))
colnames(Blackbear) <- "Predictor"

#Estimates
Blackbear$Estimate <- c(-0.110, 0.139, 0.367, -0.063, 0.522)
Blackbear$StdError <- c(0.386, 0.339, 0.504, 0.197, 0.553)

ggplot(data = Blackbear, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "Treated", "HumanUse", "Lowland", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-1.25, 1.25))

## WTD
WTD <- as.data.frame(c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(WTD) <- "Predictor"

#Estimates
WTD$Estimate <- c(-0.319, -1.448, -0.512, -1.970, -0.171, 0.783, -0.007, 0.185)
WTD$StdError <- c(0.490, 0.487, 0.544, 0.408, 0.322, 0.358, 0.125, 0.334)

ggplot(data = WTD, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-2.5, 2.5))

##Moose
Moose <- as.data.frame(c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Moose) <- "Predictor"

#Estimates
Moose$Estimate <- c(-0.306, -0.168, 0.376, -0.064, 0.005, -0.042, 0.550, 0.875)
Moose$StdError <- c(0.523, 0.451, 0.516, 0.201, 0.169, 0.168, 0.426, 0.358)

ggplot(data = Moose, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "Treated", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-1.25, 1.25))



############################################
## Converting plots to estimates FROM THE FULL MODEL
Caribou <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Caribou) <- "Predictor"

Caribou$Estimate <- c(-1.292, 0.539, -0.448, 2.931,-1.414, -0.797, 0.445, -1.199) # Coefficients from FM in order for graphical presentation
Caribou$StdError <- c(1.189,0.530, 0.693, 0.643, 0.350, 0.537, 0.460, 0.951)

ggplot(data = Caribou, aes(x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-4, 4))

## WTD
WTD <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(WTD) <- "Predictor"

WTD$Estimate <- c(-0.319, -1.393, -0.321 ,-1.982, -0.467, 0.784, -0.197, 0.550) # Coefficients from FM in order for graphical presentation
WTD$StdError <- c(0.480, 0.479, 0.555, 0.402, 0.382, 0.351, 0.374, 0.400)

ggplot(data = WTD, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-2.5, 2.5))

## Wolf
Wolf <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Wolf) <- "Predictor"

Wolf$Estimate <- c(0.025, 0.533, 1.472, -0.716, -0.734, 0.332, 0.022, 0.260) # Coefficients from FM in order for graphical presentation
Wolf$StdError <- c(0.586, 0.497, 0.562, 0.386, 0.263, 0.364, 0.405, 0.422)

ggplot(data = Wolf, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-2, 2.5))
## Blackbear
Blackbear <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "LineDens", "LineWidth", "VegHt"))
colnames(Blackbear) <- "Predictor"

Blackbear$Estimate <- c(-0.210, 0.299, 0.763, -0.324, -0.079, -0.062, 0.956) # Coefficients from FM in order for graphical presentation
Blackbear$StdError <- c(0.524, 0.434, 0.533, 0.350, 0.327, 0.357, 0.374)

ggplot(data = Blackbear, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "VegHt"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-2, 2))

## Moose
Moose <- as.data.frame(c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))
colnames(Moose) <- "Predictor"

Moose$Estimate <- c(-0.293, -0.106, 0.365, -0.345, 0.041, -0.281, 0.704, 0.741) # Coefficients from FM in order for graphical presentation
Moose$StdError <- c(0.510, 0.445, 0.502, 0.341, 0.481, 0.341, 0.374, 0.348)

ggplot(data = Moose, aes( x = Predictor, y = Estimate)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Estimate - StdError, ymax = Estimate + StdError, width = 0.3))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("NatRegen", "SPP", "HumanUse", "Lowland", "pSnow", "LineDens", "LineWidth", "VegHt"))+theme_classic() + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-1.25, 1.25))

