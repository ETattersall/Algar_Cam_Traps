#### Basic summary stats of CT data
#### Data exploration for Algar pilot data
### Started April 11, 2017 by Erin Tattersall

library(ggplot2)

### Uses dataframe "S": species detections by site

## Naive occupancy (Stations with sp. present/Total stations)

sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
naive.occ <- sp.sites/24
naive.plot <- data.frame(rev(sort(naive.occ))) 
Species <- row.names(naive.plot)

ggplot(naive.plot, aes(x=Species)) + geom_bar()

### Can't get graphs to work yet. Gave up and did it in Excel :P

### Photographic event rate = # detection events/ sampling effort (Sampling effort = Camera days)
### Camera days = 8911
## Event rate gives detections per camera day
ev.rate <- sp.plot/8911
ev.rate.plot <- data.frame(rev(sort(ev.rate)))
sum(sp.plot) ## 1065
