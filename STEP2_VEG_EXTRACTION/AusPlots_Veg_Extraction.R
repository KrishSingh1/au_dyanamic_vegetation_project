###### AusPlots Vegetation Data Extraction ######
## Author: Krish Singh
## Date: 230803
## Purpose: To extract all vegetation data from 713 different site locations from AusPlots


query.dir <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
files <- list.files(query.dir, pattern = "\\.csv$", full.names = FALSE)
site.names <- tools::file_path_sans_ext(files)


library(ausplotsR)
#my.data <- get_ausplots(my.Plot_IDs = site.names)
#saveRDS(my.data, file = "site_veg.rds")

veg_info <- readRDS("site_veg.rds")
