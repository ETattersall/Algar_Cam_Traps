###### Graphing summary stats for Algar 2016.01 deployment
### Started 6 June, 2017 by Erin. T

library(ggplot2)
## S = dataframe of detections by station and species for 2016.01 deployment
ggplot(data = S, aes(x = Treatment, y = Total)) + geom_boxplot()

## Compare to base R boxplot
boxplot(S$Total~S$Treatment,cex.axis=1.3,ylab="Total detections",cex.lab=1.6,col=c("royalblue","orange","green", "red"),boxwex=0.6)