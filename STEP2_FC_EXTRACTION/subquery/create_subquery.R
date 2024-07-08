### Subquery Extractor ###
## Author: Krish Singh
## Date: 08-07-24
## Objective: Extract a subset of the query dataset (used for GADI) based on some criteria,
##            namely due to some issue for some sites. Used to separately query the 'problem' sites


# Libraries ---------------------------------------------------------------

library(dplyr)

# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------



query <- read.csv('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Merged_Polygons/AusPlots_Merged_Completed.csv')
site_info <- read.csv('../../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv')
site_info_not_100 <- site_info %>%
  subset(site.info.plot_is_100m_by_100m == F)

subquery <- query %>%
  subset(site_location_name %in% unique(site_info_not_100$site.info.site_location_name))
  
write.csv(subquery,'../../DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Merged_Polygons/AusPlots_Merged_Completed_not_100.csv')


# Alternative Subquerying:

# directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/New_Batch/"
# files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
# fileNames <- tools::file_path_sans_ext(files)
# length(unique(fileNames))

#subquery <- query[query$site_location_name %in% difference,] # Use this to find the difference between what was obtained and what was expected to be obtained
