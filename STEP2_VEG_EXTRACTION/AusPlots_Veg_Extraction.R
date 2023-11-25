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


# Obtain up-to-date data --------------------------------------------------

library(ausplotsR)
my.data <- get_ausplots(my.Plot_IDs = site.names, structural_summaries = T,
                        veg.PI = T, site_info = T, 
                        dictionary = T, veg.vouchers = T)
saveRDS(my.data, file = "site_veg_2-0-3.rds")


veg_info <- readRDS("site_veg_2-0-3.rds")

dictionary <- as.data.frame(veg_info$metadata.dictionary)


# Get Soil Data -----------------------------------------------------------

my.data <- get_ausplots(my.Plot_IDs = site.names, soil_subsites = T,
                        site_info = T, 
                        soil_bulk_density = T,
                        soil_character = T)

saveRDS(my.data, file = "site_soil_2-0-3.rds")
