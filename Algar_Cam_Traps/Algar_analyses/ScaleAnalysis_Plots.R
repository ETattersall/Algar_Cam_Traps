###############################################################
## ScaleAnalysis_Plots.R
## See Ch3_LineDensity_scaleanalysis.Rmd for full code
## For tab data frames, see "LDscaleanalysis_species.csv" (insert Lynx, Coyote, or Blackbear as species)
## Code for plot is same for each species (except for title), as long as tab is species-specific

##Line Density and Habitat scale analysis

## tab = data frame of AIC model weights, with scales represented numerically in order of decreasing model weight
op <- par(mar = c(6,6,5,3) + 0.2)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "", ylab = "", pch=20, cex = 4, cex.axis = 2, cex.lab = 4)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))
title("Blackbear", line = -4, cex.main = 5)
title(xlab = "Scale (m)", line = 4, cex.lab = 4)
title(ylab = "AIC weight", cex.lab = 4)

op <- par(mar = c(6,6,5,3) + 0.2)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "", ylab = "", pch=20, cex = 4, cex.axis = 2, cex.lab = 4)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))
title("Black bear", line = -4, cex.main = 5)
title(xlab = "Scale (m)", line = 4, cex.lab = 4)
title(ylab = "AIC weight", cex.lab = 4)

