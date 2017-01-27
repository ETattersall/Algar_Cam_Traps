### Data summarising using camtrapR record tables (rec.noTimelapse), graphing results
### Started by Erin Tattersall on Jan. 26, 2017

library(dplyr)
library(tidyr)
library(ggplot2)

## No. detections by species and station as data frames
spec_detect <- data.frame (rec.noTimelapse %>% count(Species))


Station_detect <- data.frame (rec.noTimelapse %>% count(Station))

### Or you can take it directly from rec.noTimelapse

species_seen <- rec.noTimelapse$Species

species_hist <- ggplot(data.frame(species_seen), aes(x=species_seen)) +
  geom_bar()