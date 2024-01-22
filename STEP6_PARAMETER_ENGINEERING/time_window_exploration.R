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
library(plotly)
library(foreach)
library(doParallel)
library(data.table)

# Functions ---------------------------------------------------------------

apply_sliding_window <- function(file, window) {
  test.zoo <- read.zoo(file, index.column = 'time')
  window.sum <- rollsum(test.zoo, k = window, align = 'right', fill = NA)
  
  result <- data.table(
    date = index(window.sum),
    window.sum = coredata(window.sum)
  )
  
  return(result)
}


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

test.window <- function(window, site.focus, directory, site.greenness, precip.data) {
  
  precip.window <- apply_sliding_window(precip.data, window)
  colnames(precip.window)[1] <- 'date'
  precip.window$date <- as.Date(precip.window$date)
  
  test <- precip.window %>% full_join(site.greenness, by = 'date')
  test <- test[order(test$date),]
  rownames(test) <- 1:nrow(test)
  test <- aggregate(test[,c('window.sum', 'pv')], 
                    by = list(test$date), FUN = mean, na.rm = T)
  
  model <- lm(pv ~ window.sum, data = test)
  ret <- c("window" =  window, summary(model)$adj.r.squared)
  
  name.score <- paste0(site.focus, ".adjusted.R2")
  names(ret) <- c("window", name.score)
  
  return(ret)
}

fit_linear_models_2 <- function(site.names, windows,
                              directory = "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd",
                              num_cores = 4) {
  registerDoParallel(cores = num_cores)
  
  first <- foreach(ssn = site.subset.names, .combine = cbind, .export = c("test.window", "get_precip_data", "windows", "directory", "apply_sliding_window"), 
                   .packages = c("ncdf4", "zoo", "dplyr", "data.table")) %dopar% {
                     
    site.greenness <- fread(paste0('../STEP6_PARAMETER_ENGINEERING/combined_data_', ssn, '.csv'), select = c("date", "pv"))
    site.greenness$date <- as.Date(site.greenness$date)
    precip.data <- get_precip_data(ssn, directory)
    
    site.r2 <- data.table(t(sapply(windows, FUN = test.window, 
                                      site.focus = ssn,
                                      directory = directory,
                                      site.greenness = site.greenness,
                                      precip.data = precip.data)))
    
    return(site.r2)
  }

  stopImplicitCluster()
  return(first)
}

# Main --------------------------------------------------------------------



directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd"

# Get names of the site subset 
site.subset <- read.csv('../STEP2_NDVI_EXTRACTION/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.subset.names <- unique(site.subset$site_location_name)
exception <- "NSABHC0023"
site.subset.names <- site.subset.names[-which(site.subset.names == exception)] 


# Get the precipitation data for the chosen site subset and apply window function

windows <- seq(1,365*3,5)

time.res <- system.time({
  r2s <- fit_linear_models_2(site.subset.names, windows = windows, num_cores = 7)
})

# 4 cores: 6 min 1 sec
# 7 cores: 4 min 30 sec, sys-time: 261.21sec
# Optimised 'apply window size': 198.57 sec - 3min 18sec
# Optimise reading site.greenness csv: 104.82 sec - 1 min 45 sec [Use this vers]
# Use biglm instead of lm: 89.73 sec: - 1 min 29 sec


#write.csv(r2s, "../DATASETS/linear_model_fitted_r2.csv")
r2s.t <- read.csv("../DATASETS/linear_model_fitted_r2.csv")

round(r2s.t$WAAPIL0003.adjusted.R2 - r2s$WAAPIL0003.adjusted.R2,10)
round(r2s.t$TCATCH0006.adjusted.R2 - r2s$TCATCH0006.adjusted.R2,10)

plot(r2s$window,r2s$WAAPIL0003.adjusted.R2) # Grass, consider fire history  
plot(r2s$window,r2s$TCATCH0006.adjusted.R2) # Grass
plot(r2s$window,r2s$NSAMDD0014.adjusted.R2)

plot(r2s$window,r2s$SATSTP0005.adjusted.R2)
plot(r2s$window,r2s$QDASSD0015.adjusted.R2)

# Pattern
plot(r2s$window,r2s$WAAGAS0002.adjusted.R2) # Grass
plot(r2s$window,r2s$NTAGFU0021.adjusted.R2) # Grass
plot(r2s$window,r2s$NTAFIN0002.adjusted.R2) # Grass
plot(r2s$window,r2s$QDAEIU0010.adjusted.R2) # Grass


plot(r2s$window,r2s$NSANAN0002.adjusted.R2) # Tree
plot(r2s$window,r2s$NSANSS0001.adjusted.R2) # Tree

# By vegetation type 

classifications <- read.csv("../DATASETS/AusPlots_Sites_Classified_2-0-6.csv")
# Grass 
classifications[classifications$site_location_name == 'WAAPIL0003',]$vegetation_type
classifications[classifications$site_location_name == 'TCATCH0006',]$vegetation_type
classifications[classifications$site_location_name == 'WAAGAS0002',]$vegetation_type
classifications[classifications$site_location_name == 'NSAMDD0014',]$vegetation_type
classifications[classifications$site_location_name == 'NTAGFU0021',]$vegetation_type
classifications[classifications$site_location_name == 'SATSTP0005',]$vegetation_type
classifications[classifications$site_location_name == 'QDASSD0015',]$vegetation_type
classifications[classifications$site_location_name == 'NTAFIN0002',]$vegetation_type
classifications[classifications$site_location_name == 'QDAEIU0010',]$vegetation_type

# Tree
classifications[classifications$site_location_name == 'NSANSS0001',]$vegetation_type
classifications[classifications$site_location_name == 'NSANAN0002',]$vegetation_type

# Relationship with MAP

load('../STEP4_EXPORE_DATA/annual.precip.data.RData')

site.names <- mapply(colnames(r2s)[-c(1:2)],FUN = function(str) {
  return(strsplit(str, split = ".", fixed = T)[[1]][1])
})
max.adj.r2 <- mapply(r2s[,-c(1:2)], FUN = max, na.rm = F)
max.adj.r2.df <- data.frame("site_location_name" = site.names,max.adj.r2)

r2.map <- merge(annual.precip.data, max.adj.r2.df, by =  "site_location_name")

plot_ly(r2.map, x = ~precip_mean, y = ~max.adj.r2)



# Junk Script (Don't Run) -------------------------------------------------

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


# ts_plot(na.locf(test[,c("Group.1", 'window.sum', 'NDVI', 'pv', 'green_mean')]),
#         type = 'multiple', title =  site.focus)

#plot(test$window.sum, test$pv)

summary(lm(NDVI ~ window.sum, test))

windows <- seq(1,365*3,5)

values <- sapply(windows, FUN = test.window, 
                 site.focus = site.name.focus,
                 directory = directory)

plot(x = windows, y = values, main = site.name.focus)



# Testing test.window function --------------------------------------------

site.greenness <- fread(paste0('../STEP6_PARAMETER_ENGINEERING/combined_data_', 'TCATCH0006', '.csv'), select = c("date", "pv"))
site.greenness$date <- as.Date(site.greenness$date)
precip.data <- get_precip_data('TCATCH0006', directory)
site.r2 <- test.window(site.focus = 'TCATCH0006',
                       directory = directory,
                       site.greenness = site.greenness,
                       precip.data = precip.data,
                       window = 10)