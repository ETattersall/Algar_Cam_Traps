### Folder organization for Algar project- cleaning up Timelapse files (csvs,ddbs, backups)
### Started June 1, 2017 by Erin T.

setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01")

### CSVs
## Creating objects for csv original and destination folders
csvs_2016.01 <- paste(cams2016.01, "csv", sep = ".")
from_csvs <- paste(cams2016.01, csvs_2016.01, sep = "/")
to_csvs <- paste("CSVs_2016.01", csvs_2016.01, sep = "/")

### Copying csvs into one folder
file.copy(from = from_csvs, to = to_csvs, overwrite = FALSE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
## Removing csvs from original folder
file.remove(from_csvs)

### ddbs
dir.create("DDB_files")
ddb_2016.01 <- paste(cams2016.01, "ddb", sep = ".")
from_ddb <- paste(cams2016.01, ddb_2016.01, sep = "/")
to_ddb <- paste("DDB_files", ddb_2016.01, sep = "/")

file.copy(from = from_ddb, to = to_ddb, overwrite = FALSE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
file.remove(from_ddb)

### Backups
### Got a bit loopy. Folders need to be renamed with station ID before being moved. Did manually (backups were moved out of stations but not into "Backups_all")
dir.create("Backups_all")




backups_ren <- paste("Backups", cams2016.01, sep = "_")
from_backups <- paste(cams2016.01, backups_ren, sep = "/")
to_backups <- paste("Backups_all", backups_ren, sep = "/")
file.copy(backups_ren, to_backups)


