###### AusPlots Vegetation Data Extraction ######
## Author: Krish Singh
## Date: 240105
## Purpose: To extract all vegetation data from 713 different site locations from AusPlots


# Libraries ---------------------------------------------------------------

library(ausplotsR)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------

# Get list of sites
query.dir <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
files <- list.files(query.dir, pattern = "\\.csv$", full.names = FALSE)
site.names <- tools::file_path_sans_ext(files)

# Obtain up-to-date data --------------------------------------------------

version <- gsub('\\.', '-', packageVersion("ausplotsR"))
my.data <- get_ausplots(my.Plot_IDs = site.names, veg.PI = T, site_info = T, dictionary = T)
file <- paste0("../DATASETS/","site_veg_", version, ".rds")
saveRDS(my.data, file = file)
veg_info <- readRDS(file)

# Get Soil Data -----------------------------------------------------------

# my.data <- get_ausplots(my.Plot_IDs = site.names, soil_subsites = T,
#                         site_info = T, 
#                         soil_bulk_density = T,
#                         soil_character = T)
# 
# saveRDS(my.data, file = "site_soil_2-0-3.rds")