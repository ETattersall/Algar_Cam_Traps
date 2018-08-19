### Subsetting data frames for certain values within a column
## Data frame win2015.1 has column species with various scientific names of species. I want to subset entire data frame for seven species only

win2015.1 <- win2015.1[(win2015.1$Species == "A_alces") | (win2015.1$Species =="C_latrans") | (win2015.1$Species =="C_lupus")| (win2015.1$Species =="L_canadensis") | (win2015.1$Species =="O_virginianus") | (win2015.1$Species =="R_tarandus"), ]