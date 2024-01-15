#### Exploring Modis Burn Dataset
## Krish Singh
## 14/01/2024
##



# Library -----------------------------------------------------------------
library(ausplotsR)
library(TSstudio)



# Functions ---------------------------------------------------------------

apply_sliding_window <- function(file, window){
  
  test.zoo <- read.zoo(file, index.column = 'time')
  window.sum <- rollapply(test.zoo, window, FUN = sum, na.rm = T, fill = NA, align = 'right')
  
  return(fortify.zoo(window.sum))
}

NBR <- function(NIR, SWIR) {
  return((NIR-SWIR)/(NIR+SWIR))
}

BAI <- function(RED, NIR) {
  denominator <- (0.1 - RED)^2 + (0.06 - NIR)^2
  return(1/denominator)
}

plot_NBR <- function(site.name) {
  burn.date <- read.csv('../DATASETS/AusPlots_BurnDate.csv')
  burn.reflectances <- read.csv('../DATASETS/AusPlots_BurnReflectances.csv')
  
  site.focus <- subset(burn.reflectances, subset = (site_location_name ==  site.name))
  site.focus$NBR <- NBR(site.focus$modis_nir, site.focus$modis_swir)
  site.focus$date <- as.Date(site.focus$date)
  
  return(ts_plot(site.focus[,c("date", "NBR")]))
}


plot_BAI <- function(site.name) {
  burn.date <- read.csv('../DATASETS/AusPlots_BurnDate.csv')
  burn.reflectances <- read.csv('../DATASETS/AusPlots_BurnReflectances.csv')
  
  site.focus <- subset(burn.reflectances, subset = (site_location_name ==  site.name))
  site.focus$BAI <- BAI(site.focus$modis_red, site.focus$modis_nir)
  site.focus$date <- as.Date(site.focus$date)
  
  return(ts_plot(site.focus[,c("date", "BAI")]))
}

# Main --------------------------------------------------------------------

# Individual
site.name <- 'NSANAN0002'
burn.date <- read.csv('../DATASETS/AusPlots_BurnDate.csv')
burn.reflectances <- read.csv('../DATASETS/AusPlots_BurnReflectances.csv')
site.focus <- subset(burn.reflectances, subset = (site_location_name == 'NSANAN0002'))
site.focus$NBR <- NBR(site.focus$modis_nir, site.focus$modis_swir)
site.focus$date <- as.Date(site.focus$date)
ts_plot(site.focus[,c("date", "NBR")])


# Using function
plot_NBR('NSANAN0002')
plot_NBR('NSAMDD0014')




plot_BAI('NSANAN0002')



