#### FC and NDVI product Comparisons Single Site ####
## Author: Krish Singh
## DATE: 20231201
## Purpose: to explore the similarities between the FC and NDVI data products from
## - Vegmachine
## - DEA FC
## - NDVI


# Load Libraries ----------------------------------------------------------
library(TSstudio)
library(readr) # needed to resolve delimiter issue in sfc csv files 
library(ausplotsR)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(tune)


# Functions ---------------------------------------------------------------

get_NDVI <- function(focus.site.name, path) {

  # Load NDVI
  focus.site.file.name <- paste0(focus.site.name, '_NDVI', '.csv')
  focus.site.ndvi <- read.csv(paste0(path,focus.site.file.name))

  ## Need to extract the dates from system index (left of the last '_' seperator)
  focus.site.ndvi$date <- unlist(lapply(focus.site.ndvi$system.index,
                                        FUN = function(string){
                                          splitted.str <- strsplit(string, '_')
                                          return(splitted.str[[1]][[length(unlist(splitted.str)) - 1]])
                                        }
  ))


  focus.site.ndvi$date <- as.Date(focus.site.ndvi$date, format = '%Y%m%d')
  focus.site.ndvi <- focus.site.ndvi[order(focus.site.ndvi$date),] 
  return(focus.site.ndvi)
}


get_SFC <- function(focus.site.name, path){
  # Load SFC
  SFC.path <- '../STEP2_FC_EXTRACTION/VegMachine/SFC/'
  focus.site.file.name <- paste0(focus.site.name, '_SFC', '.csv')
  focus.site.sfc <- readr::read_csv(paste0(path,focus.site.file.name))
  
  # To convert to a date, I needed to append a day ('01') while in actuality
  # there are no specific dates 
  focus.site.sfc$date <- as.Date(unlist(lapply(focus.site.sfc$date,
                                               function(date) 
                                               {return(paste0(date, '-01'))}
  )))
  
  focus.site.sfc$green_mean <- as.numeric(focus.site.sfc$green_mean)
  return(focus.site.sfc)
}


get_DEA <- function(focus.site.name, path){
  focus.site.file.name <- paste0(focus.site.name, '.csv')
  focus.site.dea <- read.csv(paste0(path,focus.site.file.name))
  focus.site.dea <- aggregate(focus.site.dea, by = list(focus.site.dea$time), FUN = mean, na.rm = T)
  focus.site.dea$date <- as.Date(focus.site.dea$Group.1)
  return(focus.site.dea)
}


create_error_fc_data <- function(site.info, error){
  one.veg.fc <- data.frame(site_unique = site.info$site_unique,
                           green = rep(NA, nrow(site.info)), 
                           brown = rep(NA, nrow(site.info)),
                           bare = rep(NA, nrow(site.info)),
                           error = rep(error, nrow(site.info)))
  return(one.veg.fc)
}

# iterating fractional cover on each site individually
get_individual_fcs <- function(site.names){
  
  fc <- data.frame(site_unique = NA, green = NA,
                   brown = NA, bare = NA, error = NA)
  
  for(name in site.names) {
    error <- 0 # Error 0: No error
    one.veg.info <- get_ausplots(name, veg.PI = T, site_info = T)
    
    if(nrow(one.veg.info$veg.PI) == 0) {
      error <- 1 # Error 1: missing veg.PI 
      one.veg.fc <- create_error_fc_data(one.veg.info$site.info, error)
    } else {
      
      one.veg.fc <- fractional_cover(one.veg.info$veg.PI) # retrieve fc data
      if(is.character(one.veg.fc)){ # empty fc dataset is in the form of an empty character
        error <- 2 # Error 2: fractional_cover returns empty character 
        one.veg.fc <- create_error_fc_data(one.veg.info$site.info, error)
      } else {
        one.veg.fc$error <- rep(error, nrow(one.veg.fc)) 
        
        # Check if all columns (exclude 'other') are present; 'site_unique' 'bare', 'green', 'brown' 
        columns.diff <- setdiff(c("site_unique", "bare", "green", "brown", "error"), colnames(one.veg.fc))
        # When we get partially missing data - either one or more of bare, green, or brown is missing 
        if(length(columns.diff) > 0){
          error <- 4 # Error 4: Site obtains partially missing fractional cover data column (eg. missing one of or more between bare, green, brown)
          for(i in columns.diff) {
            if (i != "other") { 
              one.veg.fc[i] <- rep(NA, nrow(one.veg.info$site.info))
            }
          }
          one.veg.fc$error <- rep(error, nrow(one.veg.info$site.info)) # change error to 4 for entire subset
        } 
        
        one.veg.fc <- one.veg.fc[,c("site_unique", "green", "brown", "bare", "error")]
        # check for missing FC for site observations 
        missing.obs <- setdiff(one.veg.info$site.info$site_unique, one.veg.fc$site_unique)
        if(length(missing.obs) > 0){
          error <- 3 # Error 3: site obtains partially missing fractional cover data row-wise (missing observations entirely)
          temp <- create_error_fc_data(
            subset(one.veg.info$site.info,subset = (site_unique %in% missing.obs)), error)
          one.veg.fc <- rbind(one.veg.fc, temp)
        }
      }
    }
    fc <- rbind(fc, one.veg.fc)
  }
  fc <- fc[-1,]
  rownames(fc) <- 1:nrow(fc)
  return(fc)
}



# Load Datasets -----------------------------------------------------------
focus.site.name <- 'NSANAN0002'

# Load NDVI
NDVIs.path <- '../STEP2_NDVI_EXTRACTION/EarthEngine/Output/NDVI_Extraction_2/'
focus.site.file.name <- paste0(focus.site.name, '_NDVI', '.csv')
focus.site.ndvi <- read.csv(paste0(NDVIs.path,focus.site.file.name))

## Need to extract the dates from system index (left of the last '_' seperator)
focus.site.ndvi$date <- unlist(lapply(focus.site.ndvi$system.index,
                                      FUN = function(string){
                                        splitted.str <- strsplit(string, '_')
                                        return(splitted.str[[1]][[length(unlist(splitted.str)) - 1]])
                                           }
))


focus.site.ndvi$date <- as.Date(focus.site.ndvi$date, format = '%Y%m%d')
focus.site.ndvi <- focus.site.ndvi[order(focus.site.ndvi$date),]
focus.site.ndvi$NDVI_std <- (focus.site.ndvi$NDVI - mean(focus.site.ndvi$NDVI, na.rm = T))/
  sd(focus.site.ndvi$NDVI, na.rm = T)

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

focus.site.sfc$green_mean <- as.numeric(focus.site.sfc$green_mean)
focus.site.sfc$green_mean_std <- (focus.site.sfc$green_mean - mean(focus.site.sfc$green_mean, na.rm = T))/
  sd(focus.site.sfc$green_mean, na.rm = T)

focus.site.sfc <- as.data.frame(focus.site.sfc) # note parsing issue with the last column

# Load DEA FC

dea.path <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
focus.site.file.name <- paste0(focus.site.name, '.csv')
focus.site.dea <- read.csv(paste0(dea.path,focus.site.file.name))
focus.site.dea <- aggregate(focus.site.dea, by = list(focus.site.dea$time), FUN = mean, na.rm = T)
focus.site.dea$date <- as.Date(focus.site.dea$Group.1)

focus.site.dea$pv_std <- (focus.site.dea$pv - mean(focus.site.dea$pv, na.rm = T))/
                      sd(focus.site.dea$pv, na.rm = T)


# Load AusPlots datasets

ausplots.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg_2-0-3.rds")
ausplots.focus.fc <- readRDS("../STEP2_VEG_EXTRACTION/insitu_fractional_cover_default_2-0-3.rds")
ausplots.info.focus <- ausplots.info$site.info[ausplots.info$site.info$site_location_name == focus.site.name,]
site.id <- ausplots.info.focus$site_unique
ausplots.fc.data <- merge(ausplots.info.focus, ausplots.focus.fc, by = 'site_unique')
date.visit <- as.Date(ausplots.fc.data$visit_start_date)


# Explore time series -----------------------------------------------------


# NDVI time series 
ts_plot(focus.site.ndvi[,c("date", "NDVI")])

#ts_plot(focus.site.sfc[,c("date", "green_mean")])

ts_plot(focus.site.sfc[,c("date", "green_mean")]) # Not sure why this shows nothing

plot(focus.site.sfc[,c("date", "green_mean")])

ts_plot(focus.site.dea[,c("date","pv")])


# Compare time series  ----------------------------------------------------

# define date range to aggregate RS data for comparison  
times.forwards <- seq(date.visit, by='1 days', length = 31)
times.backwards <- seq(date.visit, by='-1 days', length = 31)

closest.times.dea <- rbind(focus.site.dea[focus.site.dea$date %in%times.forwards,],
                           focus.site.dea[focus.site.dea$date %in%times.backwards,])
closest.times.dea <- closest.times.dea[order(closest.times.dea$date),]

closest.times.ndvi <- rbind(focus.site.ndvi[focus.site.ndvi$date %in%times.forwards,],
                           focus.site.ndvi[focus.site.ndvi$date %in%times.backwards,])
closest.times.ndvi <- closest.times.ndvi[order(closest.times.ndvi$date),]

# special case for sfc as it is seasonal - over 3 
times.forwards.sfc <- seq(date.visit, by='1 days', length = 31*3) 
# I generally want the earliest occurance of a non-na green_mean 
closest.times.sfc <- rbind(focus.site.sfc[focus.site.sfc$date %in%times.forwards.sfc,])
closest.times.sfc <- closest.times.sfc[order(closest.times.sfc$date),]
closest.times.sfc <- subset(closest.times.sfc, !is.na(green_mean))
closest.times.sfc <- closest.times.sfc[which.min(closest.times.sfc$date),]


# Plot the nearest time series 
ts_plot(closest.times.dea[,c("date", "pv")]) # dea time series 

ts_plot(closest.times.ndvi[,c("date", "NDVI")]) # EE time series 

ts_plot(closest.times.sfc[,c("date", "green_mean")]) # still empty 


# merge dea and ee for side by side comparison 
closest.times.binded <- merge(closest.times.ndvi, closest.times.dea, by = 'date')
closest.times.binded <- closest.times.binded[,c("date", "NDVI", "pv", "NDVI_std", "pv_std")] 


# normalise NDVI and pv locally 
closest.times.binded$NDVI_norm <- (
  closest.times.binded$NDVI - mean(closest.times.binded$NDVI, na.rm = T))/
  sd(closest.times.binded$NDVI, na.rm = T
     )

closest.times.binded$pv_norm <- (
  closest.times.binded$pv - mean(closest.times.binded$pv, na.rm = T))/
  sd(closest.times.binded$pv, na.rm = T)


ts_plot(closest.times.binded[,c("date", "NDVI_std", "pv_std")]) # plot side by side comparison 

ts_plot(closest.times.binded[,c("date", "NDVI_norm", "pv_norm")])

# combine into one dataset 
site.focus.agg.nearest <- data.frame("site_unique" = site.id, lapply(
  closest.times.binded[,c("pv","NDVI", "pv_norm", "NDVI_norm", "pv_std", "NDVI_std")],
  FUN = mean, na.rm = T))
site.focus.agg.nearest$green_mean <- closest.times.sfc$green_mean
site.focus.agg.nearest$date <- date.visit
site.focus.agg.nearest$aus_green <- ausplots.fc.data$green


# Create a matrix like graph comparing each dataset with each other 

subsets.info <- read.csv('../STEP2_NDVI_EXTRACTION/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.names <- unique(subsets.info$site_location_name)

multi.rs.evaluation <- data.frame(site_unique = as.character(NA), 
                                  date = as.Date(NA),
                                  pv = as.numeric(NA),
                                  NDVI = as.numeric(NA),
                                  green_mean = as.numeric(NA),
                                  aus_green = as.numeric(NA))

for (site in site.names) {
 
  if(site != 'NSABHC0023') { # IF: not that site I don't have the data for (NSABC0023)
    
    ## Load the RS datasets 
    focus.site.ndvi <- get_NDVI(site, 
                                '../STEP2_NDVI_EXTRACTION/EarthEngine/Output/NDVI_Extraction_2/')
    focus.site.ndvi$NDVI <- focus.site.ndvi$NDVI * 100 # scale the NDVI 
    focus.site.sfc <- get_SFC(site,
                              '../STEP2_FC_EXTRACTION/VegMachine/SFC/')
    focus.site.dea <- get_DEA(site,
            '/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/')
    
    
    ## Load the AusPlots datasets
    ausplots.info.focus <- subsets.info[subsets.info$site_location_name == site,]
    site.id <- ausplots.info.focus$site_unique
    ausplots.focus.fc <- get_individual_fcs(site) # Fractional cover 
    ausplots.fc.data <- merge(ausplots.info.focus, ausplots.focus.fc, by = 'site_unique')
    #date.visit <- as.Date(ausplots.fc.data$visit_start_date)
    
    # Need to iterate through dates in case of multiple visits 
    
    for (r in  1:nrow(ausplots.fc.data)) {
      date <- as.Date(ausplots.fc.data$visit_start_date[r])
    
      # define date range to aggregate RS data for comparison  
      times.forwards <- seq(date, by='1 days', length = 31)
      times.backwards <- seq(date, by='-1 days', length = 31)
      
      closest.times.dea <- rbind(focus.site.dea[focus.site.dea$date %in%times.forwards,],
                                 focus.site.dea[focus.site.dea$date %in%times.backwards,])
      closest.times.dea <- closest.times.dea[order(closest.times.dea$date),]
      
      closest.times.ndvi <- rbind(focus.site.ndvi[focus.site.ndvi$date %in%times.forwards,],
                                  focus.site.ndvi[focus.site.ndvi$date %in%times.backwards,])
      closest.times.ndvi <- closest.times.ndvi[order(closest.times.ndvi$date),]
      closest.times.binded <- merge(closest.times.ndvi, closest.times.dea, by = 'date')
      
      # special case for sfc as it is seasonal - over 3 
      times.forwards.sfc <- seq(date, by='1 days', length = 31*3) 
      # I generally want the earliest occurance of a non-na green_mean 
      closest.times.sfc <- rbind(focus.site.sfc[focus.site.sfc$date %in%times.forwards.sfc,])
      closest.times.sfc <- closest.times.sfc[order(closest.times.sfc$date),]
      closest.times.sfc <- subset(closest.times.sfc, !is.na(green_mean))
      closest.times.sfc <- closest.times.sfc[which.min(closest.times.sfc$date),]
      if(nrow(closest.times.sfc) < 1){
        green_mean <- NA
      } else {
        green_mean <- closest.times.sfc$green_mean
      }
      
      # combine into one dataset 
      site.focus.agg.nearest <- data.frame("site_unique" = ausplots.fc.data$site_unique[r],
                                           lapply( 
                                             closest.times.binded[,c("pv","NDVI")],
                                             FUN = mean, na.rm = T))
      
      site.focus.agg.nearest$green_mean <- green_mean
      site.focus.agg.nearest$date <- date
      site.focus.agg.nearest$aus_green <- ausplots.fc.data$green[r]
    
      print(site.focus.agg.nearest)
      multi.rs.evaluation <- rbind(multi.rs.evaluation,site.focus.agg.nearest)
    }
  }
}


# Key results  ------------------------------------------------------------

# Against each other
pv.NDVI <- ggplot(data = multi.rs.evaluation, aes(x = pv, y = NDVI)) +
  geom_point() + labs(x = "green fraction (DEA)", y = "NDVI (Landsat)") + 
  coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = F,
              fullrange = T, colour = 'black', linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, fullrange = T)
pv.NDVI 

pv.sfc <- ggplot(data = multi.rs.evaluation, aes(x = pv, y = green_mean)) +
  geom_point() + labs(x = "green fraction (DEA)", y = "SFC green (Vegmachine)") + 
  coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = F,
              fullrange = T, colour = 'black', linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, fullrange = T)
pv.sfc 

NDVI.sfc <- ggplot(data = multi.rs.evaluation, aes(x = NDVI, y = green_mean)) +
  geom_point() + labs(x = "NDVI (LandSat)", y = "SFC green (Vegmachine)") + 
  coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = F,
              fullrange = T, colour = 'black', linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, fullrange = T)
NDVI.sfc 


# With in-situ

aus.pv <- ggplot(data = multi.rs.evaluation, aes(x = aus_green, y = pv)) +
  geom_point() + labs(x = "Insitu green (AusPlots)", y = "RS green (DEA)") + 
  coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = F,
              fullrange = T, colour = 'black', linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, fullrange = T)
aus.pv


aus.NDVI <- ggplot(data = multi.rs.evaluation, aes(x = aus_green, y = NDVI)) +
  geom_point() + labs(x = "Insitu green (AusPlots)", y = "NDVI (LandSat)") + 
  coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = F,
              fullrange = T, colour = 'black', linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, fullrange = T)
aus.NDVI


aus.sfc <- ggplot(data = multi.rs.evaluation, aes(x = aus_green, y = green_mean)) +
  geom_point() + labs(x = "Insitu green (AusPlots)", y = "RS green (Vegmachine)") + 
  coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = F,
              fullrange = T, colour = 'black', linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, fullrange = T)
aus.sfc


