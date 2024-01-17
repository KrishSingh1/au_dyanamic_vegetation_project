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

plot_NBR <- function(site.name,burn.date, burn.reflectances) {
  site.focus <- subset(burn.reflectances, subset = (site_location_name ==  site.name))
  site.focus$NBR <- NBR(site.focus$modis_nir, site.focus$modis_swir)
  site.focus$date <- as.Date(site.focus$date)
  return(ts_plot(site.focus[,c("date", "NBR")]))
}

plot_NBR_gg <- function(site.name, burn.date, burn.reflectances) {
  site.focus <- subset(burn.reflectances, subset = (site_location_name ==  site.name))
  site.focus$NBR <- NBR(site.focus$modis_nir, site.focus$modis_swir)
  site.focus$date <- as.Date(site.focus$date)
  
  burn.date <- subset(burn.date, subset = (site_location_name ==  site.name))
  burn.date$date <- as.Date(burn.date$date)
  
  g <- ggplot(data = site.focus, mapping = aes(x = date, y = NBR)) +
    geom_line() + labs(title = paste0(site.name, " NBR and Burn Dates")) +
    geom_hline(yintercept = 0, linetype = 4, color = 'green')
  
  for (d in 1:nrow(burn.date)) {
    print(burn.date[d,]$date)
    g <- g + geom_vline(xintercept = as.numeric(burn.date[d,]$date), 
                          linetype = 4, color = 'red')
  }
  
  return(g)
}



plot_BAI <- function(site.name, burn.date, burn.reflectances) {
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


# Using function

plot_NBR_gg('NSANAN0002', burn.date, burn.reflectances)
plot_NBR_gg('NTAGFU0021', burn.date, burn.reflectances)
plot_NBR_gg('QDAEIU0010', burn.date, burn.reflectances)
plot_NBR_gg('WAAPIL0003', burn.date, burn.reflectances)
plot_NBR_gg('NSANSS0001', burn.date, burn.reflectances)

