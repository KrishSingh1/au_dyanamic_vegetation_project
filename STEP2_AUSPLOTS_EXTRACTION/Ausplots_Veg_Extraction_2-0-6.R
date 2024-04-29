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
my.data <- get_ausplots(my.Plot_IDs= site.names, site_info=TRUE, structural_summaries=TRUE,
                        veg.vouchers=TRUE, veg.PI=TRUE, basal.wedge=TRUE, soil_subsites=TRUE,
                        soil_bulk_density=TRUE, soil_character=TRUE, dictionary= TRUE)

file <- paste0("../DATASETS/AusPlots_Extracted_Data/","site_veg_", version, ".rds")
saveRDS(my.data, file = file)
veg_info <- readRDS(file)

# Transform columns to allow export - record 2 of 'variableLabel' is a list, need to concat the elements
veg_info$metadata.dictionary$variableLabel <- unlist(lapply(veg_info$metadata.dictionary$variableLabel, FUN = function(x){
  if(class(x) == "list"){
    x <- paste(x, collapse = ",")
  }
  return(x)
}))

# Export each dataframe from the downloaded r object as a csv file 
for (i in names(veg_info)) {
  if (i != 'citation') {
    name <- gsub('\\.','_',i) # replace '.' with '_' for file naming
    file <- paste0("../DATASETS/AusPlots_Extracted_Data/","extracted_",name,"_",version ,  ".csv")
    write.csv(veg_info[i], file)
    print(paste0('Exported ', i, ' as a csv file.'))
  }
}


# For reading -------------------------------------------------------------

version <- gsub('\\.', '-', packageVersion("ausplotsR"))
file <- paste0("../DATASETS/AusPlots_Extracted_Data/","site_veg_", version, ".rds")
veg_info <- readRDS(file)
