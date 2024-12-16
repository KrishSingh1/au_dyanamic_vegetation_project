#### Convert NC to csv ####
## Krish Singh
## 07022024
##


# Library -----------------------------------------------------------------

library(ncdf4)
library(data.table)

# Functions ---------------------------------------------------------------

get_nc_data <- function(site.focus, directory, variable_name){
  variable.path <- file.path(directory, variable_name)
  nc.path <- file.path(variable.path, paste0(site.focus, "_1980_2022.nc")) # initially 1987
  nc <- nc_open(filename = nc.path)
  time.var <- ncvar_get(nc, "time")
  dates <- as.Date(time.var, origin = "1850-01-01")
  
  # Account for exception for naming scheme for vapourpres
  variable_split <- strsplit(variable_name, split = "_")[[1]][[1]]
  if(variable_split == 'vapourpres'){
    var <- ncvar_get(nc, variable_split)
  } else {
    var <- ncvar_get(nc, variable_name)
  }
  
  daily.variable <- data.table(variable_name = var, time = dates)
  return(daily.variable)
}

# Main --------------------------------------------------------------------


# # This is for initial subset dataset 
# site.list <- read.csv('../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
# site.unique.names <- unique(site.list$site_location_name)
# climate.variables <- c('precip', 'tmax', 'tmin', 'vapourpres_h09', 'vapourpres_h15') # climate variables of interest 
# directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/ausplots_agcd/"
# 
# for (site.name in site.unique.names) {
# 
#   file.output.name <- paste0(site.name, '_1987_2022.csv') # get name of file
#   
#   for(variable in climate.variables){ # for each climate variable, export the associated site data
#     data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
#     colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
#     write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
#     print(paste0("Exported ",variable," data for ",site.name)) 
#   }
# }
# 
# 
# # For bigger subset
# 
# site.list <- read.csv('../DATASETS/Sites_Bigger_Subset_20240124/ausplots_bigger_subset.csv')
# site.unique.names <- unique(site.list$site_location_name)
# 
# for (site.name in site.unique.names) {
#   
#   file.output.name <- paste0(site.name, '_1987_2022.csv') # get name of file
#   
#   for(variable in climate.variables){ # for each climate variable, export the associated site data
#     data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
#     colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
#     write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
#     print(paste0("Exported ",variable," data for ",site.name)) 
#   }
# }


### Now since we got data with larger time coverage: 

# This is for initial subset dataset 
site.list <- read.csv('../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.unique.names <- unique(site.list$site_location_name)
climate.variables <- c('precip', 'tmax', 'tmin', 'vapourpres_h09', 'vapourpres_h15') # climate variables of interest 
directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/ausplots_agcd/"

for (site.name in site.unique.names) {
  
  file.output.name <- paste0(site.name, '_1980_2022.csv') # get name of file
  
  for(variable in climate.variables){ # for each climate variable, export the associated site data
    data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
    colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
    write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
    print(paste0("Exported ",variable," data for ",site.name)) 
  }
}


# For bigger subset

site.list <- read.csv('../DATASETS/Sites_Bigger_Subset_20240124/ausplots_bigger_subset.csv')
site.unique.names <- unique(site.list$site_location_name)

for (site.name in site.unique.names) {
  
  file.output.name <- paste0(site.name, '_1980_2022.csv') # get name of file
  
  for(variable in climate.variables){ # for each climate variable, export the associated site data
    data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
    colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
    write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
    print(paste0("Exported ",variable," data for ",site.name)) 
  }
}

# For all Sites:


file.list.dir <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(file.list.dir, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/ausplots_agcd/"
climate.variables <- c('precip', 'tmax', 'tmin', 'vapourpres_h09', 'vapourpres_h15') # climate variables of interest 
for (site.name in fileNames) {
  
  file.output.name <- paste0(site.name, '_1980_2022.csv') # get name of file
  
  for(variable in climate.variables){ # for each climate variable, export the associated site data
    data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
    colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
    write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
    print(paste0("Exported ",variable," data for ",site.name)) 
  }
}


# For the Left Over Sites:


file.list.dir <- read.csv("../DATASETS/AusPlots_Location/New_AusPlots_Locations_SW_Points.csv")
fileNames <- file.list.dir$site_location_name
directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/ausplots_agcd_b/"
climate.variables <- c('precip', 'tmax', 'tmin', 'vapourpres_h09', 'vapourpres_h15') # climate variables of interest 
for (site.name in fileNames) {
  
  file.output.name <- paste0(site.name, '_1980_2022.csv') # get name of file
  
  for(variable in climate.variables){ # for each climate variable, export the associated site data
    data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
    colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
    write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
    print(paste0("Exported ",variable," data for ",site.name)) 
  }
}




