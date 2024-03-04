#### Convert NC to csv ####
## Krish Singh
## 07022024
##


# Library -----------------------------------------------------------------

library(ncdf4)
library(data.table)

# Functions ---------------------------------------------------------------

get_precip_data <- function(site.focus, directory) {
  precip.path <- file.path(directory, "precip")
  nc.path <- file.path(precip.path, paste0(site.focus, "_1987_2022.nc"))
  nc <- nc_open(filename = nc.path)
  time.var <- ncvar_get(nc, "time")
  dates <- as.Date(time.var, origin = "1850-01-01")
  var <- ncvar_get(nc, 'precip')
  daily.precip <- data.table(precip = var, time = dates)
  return(daily.precip)
}

get_tmax_data <- function(site.focus, directory) {
  tmax.path <- file.path(directory, "tmax")
  nc.path <- file.path(tmax.path, paste0(site.focus, "_1987_2022.nc"))
  nc <- nc_open(filename = nc.path)
  time.var <- ncvar_get(nc, "time")
  dates <- as.Date(time.var, origin = "1850-01-01")
  var <- ncvar_get(nc, 'tmax')
  daily.tmax <- data.table(tmax = var, time = dates)
  return(daily.tmax)
}

# Main --------------------------------------------------------------------



site.list <- read.csv('../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.unique.names <- unique(site.list$site_location_name)

for (site.name in site.unique.names) {

  directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd"
  file.output.name <- paste0(site.name, '_1987_2022.csv')
  
  # Precip data 
  data.export.precip <- get_precip_data(site.name, directory = directory)
  write.csv(data.export.precip, paste0("../DATASETS/Climate_Gridded/Precip/", file.output.name))
  print(paste0("Exported precip data for ", site.name))
  
  # Tmax data 
  
  data.export.tmax <- get_tmax_data(site.name, directory = directory)
  write.csv(data.export.tmax, paste0("../DATASETS/Climate_Gridded/tmax/", file.output.name))
  print(paste0("Exported tmax data for ", site.name))
}
