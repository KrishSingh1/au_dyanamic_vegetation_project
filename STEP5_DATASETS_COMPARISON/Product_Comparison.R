#### FC and NDVI product comparisons ####
## Author: Krish Singh
## DATE: 20231201
## Purpose: to explore the similarities between the FC and NDVI data products from
## - Vegmachine
## - DEA FC
## - NDVI


# Load Libraries ----------------------------------------------------------
library(TSstudio)
library(readr) # needed to resolve delimitor issue in sfc csv files 



# Load Datasets -----------------------------------------------------------
focus.site.name <- 'NSANAN0002'

# Load NDVI
NDVIs.path <- '../STEP2_NDVI_EXTRACTION/EarthEngine/Output/NDVI_Extraction_2/'
focus.site.file.name <- paste0(focus.site.name, '_NDVI', '.csv')
focus.site.ndvi <- read.csv(paste0(NDVIs.path,focus.site.file.name))

## Need to extract the dates from system index (left of the last '_' seperator)
focus.site.ndvi$date <- unlist(lapply(as.character(focus.site.ndvi.dates),
                                         FUN = function(string){
                                           splitted.str <- strsplit(string, '_')
                                           return(splitted.str[[1]][[length(unlist(splitted.str)) - 1]])
                                           }
))

focus.site.ndvi$date <- as.Date(focus.site.ndvi$date, format = '%Y%m%d')
focus.site.ndvi <- focus.site.ndvi[order(focus.site.ndvi$date),]

# Load SFC
SFC.path <- '../STEP2_FC_EXTRACTION/VegMachine/SFC/'
focus.site.file.name <- paste0(focus.site.name, '_SFC', '.csv')
focus.site.sfc <- readr::read_csv(paste0(SFC.path,focus.site.file.name))

# To convert to a date, I needed to append a day ('01') while in actuality
# there are no specific dates 
focus.site.sfc$date <- as.Date(unlist(lapply(focus.site.sfc$date,
                      function(date) 
                        {return(paste0(date, '-01'))}
                      )))

# Load DEA FC

dea.path <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
focus.site.file.name <- paste0(focus.site.name, '.csv')
focus.site.dea <- read.csv(paste0(dea.path,focus.site.file.name))
focus.site.dea <- aggregate(focus.site.dea, by = list(focus.site.dea$time), FUN = mean, na.rm = T)
focus.site.dea$date <- as.Date(focus.site.dea$Group.1)
class(focus.site.dea$pv)


# Explore time series -----------------------------------------------------

# NDVI time series 
ts_plot(focus.site.ndvi[,c("date", "NDVI")])

#ts_plot(focus.site.sfc[,c("date", "green_mean")])

plot(focus.site.sfc[,c("date", "green_mean")])

ts_plot(focus.site.dea[,c("date","pv")])


# Process time series -----------------------------------------------------




# Compare time series  ----------------------------------------------------




# Key results  ------------------------------------------------------------





