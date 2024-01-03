#### Exploring time series of different time windows for precipitation ###
## Author: Krish Singh
## Date: 20231211
## Purpose: To create a time-window for precipitation



# Algorithm ---------------------------------------------------------------

# Output: 
# - A precipitation dataset that has a sliding window, start date, and time lag applied to it
# Input:
# - Precipitation dataset 
# Process:
# - Get precipitation dataset 
# - Apply a sliding window to the dataset 
# - Generate visualisations 
# - Apply a time lag
# - Generate visualisations 
# - Apply a start date
# - Generate visualisations 


# Library -----------------------------------------------------------------

library(ncdf4)
library(zoo)
library(TSstudio)
library(dplyr)

# Functions ---------------------------------------------------------------

apply_sliding_window <- function(file, window){
  
  test.zoo <- read.zoo(file, index.column = 'time')
  window.sum <- rollapply(test.zoo, window, FUN = sum, na.rm = T, fill = NA, align = 'right')
  
  return(fortify.zoo(window.sum))
}


get_precip_data <- function(site.focus, directory) {
  precip.path <- file.path(directory, "precip")
  nc.path <- file.path(precip.path, paste0(site.focus, "_1987_2022.nc"))
  nc <- nc_open(filename = nc.path)
  time.var <- ncvar_get(nc, "time")
  dates <- as.Date(time.var, origin = "1850-01-01")
  var <- ncvar_get(nc, 'precip')
  daily.precip <- data.frame(precip = var, time = dates)
  return(daily.precip)
}

test.window <- function(window) {
  
  precip.window <- apply_sliding_window(get_precip_data(site.focus,directory),
                                        window)
  colnames(precip.window)[1] <- 'date'
  precip.window$date <- as.Date(precip.window$date)
  
  # Visualise the dataset 
  site.greenness <- read.csv(paste0('../STEP6_PARAMETER_ENGINEERING/combined_data_', site.focus, '.csv'))
  site.greenness$date <- as.Date(site.greenness$date)
  
  test <- precip.window %>% full_join(site.greenness, by = 'date')
  test <- test[order(test$date),]
  rownames(test) <- 1:nrow(test)
  test <- aggregate(test[,c('window.sum','NDVI', 'green_mean', 'pv', 'green')], 
                    by = list(test$date), FUN = mean, na.rm = T)
  
  
  return(summary(lm(pv ~ window.sum, test))$adj.r.squared)
}


# Main --------------------------------------------------------------------

# Get names of the site subset 
site.subset <- read.csv('../STEP2_NDVI_EXTRACTION/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.subset.names <- unique(site.subset$site_location_name)
site.focus <- site.subset.names[which(site.subset.names == 'TCATCH0006')] # pick a focus site 
#site.focus <- site.subset.names[11]

directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd"

# Get the precipitation data for the chosen site subset and apply window function

window <- 641
precip.window <- apply_sliding_window(get_precip_data(site.focus,directory), window)
colnames(precip.window)[1] <- 'date'
precip.window$date <- as.Date(precip.window$date)

# Visualise the dataset 
ts_plot(precip.window)
site.greenness <- read.csv(paste0('../STEP6_PARAMETER_ENGINEERING/combined_data_', site.focus, '.csv'))
site.greenness$date <- as.Date(site.greenness$date)

test <- precip.window %>% full_join(site.greenness, by = 'date')
test <- test[order(test$date),]
rownames(test) <- 1:nrow(test)
test <- aggregate(test[,c('window.sum','NDVI', 'green_mean', 'pv', 'green')], 
                                 by = list(test$date), FUN = mean, na.rm = T)


ts_plot(na.locf(test[,c("Group.1", 'window.sum', 'NDVI', 'pv', 'green_mean')]), type = 'multiple', title =  site.focus)

plot(test$window.sum, test$pv)

summary(lm(NDVI ~ window.sum, test))

windows <- seq(1,1000,40)
values <- sapply(windows, FUN = test.window)
plot(x = windows, y =  values)

windows[which.max(values)]
 



