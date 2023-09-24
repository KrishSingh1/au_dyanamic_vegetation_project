library(ncdf4)
library(plotly)
library(timetk)
library(dplyr)
library(ggplot2)
library(lubridate)


directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd/"

precip <- file.path(directory, "precip")
tmax <- file.path(directory, "tmax")
tmin <- file.path(directory, "tmin")
vapourpres_h09 <- file.path(directory, "vapourpres_h09")
vapourpres_h15 <- file.path(directory, "vapourpres_h15")

files <- list.files(precip, pattern = "\\.nc$", full.names = FALSE) # same for each directory
fileNames <- tools::file_path_sans_ext(files)

### 

timeseries_plot <- function(index, files, variable, directory){
  
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
  
  monthly.var <- rollapply(zoo(var,dates), width = 1, FUN = sum, by = "months", fill = NA)
  print(monthly.var)
  
  data.df <- data.frame(time = dates,variable = monthly.var) %>% as_tibble()
  
  return(
  data.df %>% plot_time_series_regression(
    .date_var     = time,
    .formula      = variable ~ as.numeric(time) + month(time, label = TRUE),
    .show_summary = T)
  )
}

timeseries_plot(3, files, "precip", directory)



RI <- 3
nc.data.site <- fileNames[RI]


### Precipitation 

nc.precip.path <- file.path(precip, files[RI])

nc <- nc_open(nc.precip.path)
print(nc)

attributes(nc$var)
precip.var <- ncvar_get(nc, "precip")
time.var <- ncvar_get(nc, "time")

ncatt_get(nc, "time", "units")

start.date <- as.Date('1986-12-31')
end.date <- as.Date('2022-12-30')
dates <- seq(from = start.date, by='1 days', to = end.date)
length(dates)
as.Date(time.var, origin = "1850-01-01")
range(as.Date(time.var, origin = "1850-01-01"))


precip.df <- data.frame(time = dates, precip = precip.var) %>% as_tibble()
precip.df %>% plot_time_series(time, precip)

precip.df %>% plot_time_series_boxplot(time, precip, .period = "1 year")

summary(precip.var)
var(precip.var)

precip.df %>% plot_time_series_regression(
  .date_var     = time,
  .formula      = precip ~ as.numeric(time) + month(time, label = TRUE),
  .show_summary = T
)

### min

timeseries_plot(3, files, "tmax", directory)

timeseries_plot(3, files, "tmin", directory)


timeseries_plot(3, files, "vapourpres_h09", directory)
timeseries_plot(3, files, "vapourpres_h15", directory)



RI <- 3
nc.data.site <- fileNames[RI]


### Precipitation 

nc.precip.path <- file.path(vapourpres_h15, files[RI])

nc <- nc_open(nc.precip.path)
print(nc)

attributes(nc$var)
precip.var <- ncvar_get(nc, "vapourpres")
time.var <- ncvar_get(nc, "time")

ncatt_get(nc, "time", "units")


library(zoo)











