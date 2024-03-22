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
  nc.path <- file.path(variable.path, paste0(site.focus, "_1987_2022.nc"))
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


# This is for initial subset dataset 
site.list <- read.csv('../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.unique.names <- unique(site.list$site_location_name)
climate.variables <- c('precip', 'tmax', 'tmin', 'vapourpres_h09', 'vapourpres_h15') # climate variables of interest 
directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd"

for (site.name in site.unique.names) {

  file.output.name <- paste0(site.name, '_1987_2022.csv') # get name of file
  
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
  
  file.output.name <- paste0(site.name, '_1987_2022.csv') # get name of file
  
  for(variable in climate.variables){ # for each climate variable, export the associated site data
    data.export.precip <- get_nc_data(site.focus = site.name, directory = directory, variable_name = variable) # get data
    colnames(data.export.precip)[colnames(data.export.precip) == 'variable_name'] <- variable # rename variable column
    write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/", variable, "/", file.output.name))  # write to csv
    print(paste0("Exported ",variable," data for ",site.name)) 
  }
}


