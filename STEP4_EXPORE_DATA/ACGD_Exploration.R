library(ncdf4)
library(plotly)
library(timetk)
library(dplyr)
library(ggplot2)
library(lubridate)
library(imputeTS)



directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd/"

precip <- file.path(directory, "precip")
tmax <- file.path(directory, "tmax")
tmin <- file.path(directory, "tmin")
vapourpres_h09 <- file.path(directory, "vapourpres_h09")
vapourpres_h15 <- file.path(directory, "vapourpres_h15")

files <- list.files(precip, pattern = "\\.nc$", full.names = FALSE) # same for each directory
fileNames <- tools::file_path_sans_ext(files)
DI = 81 # directory index 


### 

get_climate_tibble <- function(index, files, variable, directory, window){
  
  data.directory <- file.path(directory, variable)
  nc.path <- file.path(data.directory, files[index])
  print("Open nc")
  nc <- nc_open(nc.path)
  print("Opened")
  
  if(variable == "vapourpres_h09" || variable == "vapourpres_h15") {
    var <- ncvar_get(nc, "vapourpres")
  }
  var <- ncvar_get(nc, variable)
  time.var <- ncvar_get(nc, "time")
  
  dates <- as.Date(time.var, origin = "1850-01-01")
  
  daily.var <- zoo::zoo(var,dates)
  window.sum <- zoo::rollsum(daily.var,k = window)
  
  data.df <- merge(daily.var, window.sum) %>% as.data.frame()
  colnames(data.df)[which(colnames(data.df) == "daily.var")] <- variable
  data.df$time <- as.Date(rownames(data.df))
  
  data.df$window.sum <- na_interpolation(data.df$window.sum)
  colnames(data.df)[which(colnames(data.df) == "window.sum")] <- paste0("window.sum", ".", window)
  
  return(as_tibble(data.df))
}



## Precipitation 

precip.tibble <- get_climate_tibble(DI, files, "precip", directory, 16)

precip.tibble %>% plot_time_series_regression(
    .date_var     = time,
    .formula      = precip ~ as.numeric(time) + month(time, label = TRUE),
    .show_summary = T)

precip.tibble <- get_climate_tibble(DI, files, "precip", directory, 16)
precip.tibble %>% plot_time_series_regression(
  .date_var     = time,
  .formula      = window.sum.16 ~ as.numeric(time) + month(time, label = TRUE),
  .show_summary = T)


write.csv(precip.tibble, paste0("Precip_",fileNames[DI], ".csv"))



## Tmax

tmax.tibble <- get_climate_tibble(DI, files, "tmax", directory, 16)
tmax.tibble %>% plot_time_series_regression(
  .date_var     = time,
  .formula      = tmax ~ as.numeric(time) + month(time, label = TRUE),
  .show_summary = T)


## Tmin

tmin.tibble <- get_climate_tibble(DI, files, "tmin", directory)
tmin.tibble %>% plot_time_series_regression(
  .date_var     = time,
  .formula      = weekly.sum ~ as.numeric(time) + month(time, label = TRUE),
  .show_summary = T)


## vapourpres_09

vapourpres_h09.tibble <- get_climate_tibble(DI, files, "vapourpres_h09", directory)
vapourpres_h09.tibble %>% plot_time_series_regression(
  .date_var     = time,
  .formula      = weekly.sum ~ as.numeric(time) + month(time, label = TRUE),
  .show_summary = T)


## Vapourpres_15

vapourpres_h15.tibble <- get_climate_tibble(DI, files, "vapourpres_h15", directory)
vapourpres_h15.tibble %>% plot_time_series_regression(
  .date_var     = time,
  .formula      = weekly.sum ~ as.numeric(time) + month(time, label = TRUE),
  .show_summary = T)















