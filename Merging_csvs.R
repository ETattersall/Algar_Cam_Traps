### Merging csvs in R
### Started 9 Dec 2016 by Erin T

###So far nothing works!!

filenames <- list.files(path = "~/")
do.call("rbind", lapply(filenames, read.csv, header = TRUE))

setwd("CSVs_Copy")
csvs <- list.files()
csvs
do.call("rbind", lapply(csvs, read.csv, header = TRUE))

for (csv in csvs) {
  read.csv("csv", header = TRUE)
 }

read.csv("Algar_001.csv")
