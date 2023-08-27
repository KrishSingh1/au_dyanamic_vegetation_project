#### DEA Data Exploration Advanced #### 
# By Krish Singh
# Date: 230827
# Purpose: To visually explore the alignment of AusPlots and DEA data. 

debug <- T

debug_msg <- function(msg) {
  print(msg)
}


library(ausplotsR)

directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")

##### Algorithm #####
# 1.  Obtain the insitu fractional cover 
# 2.  In a site, obtain corresponding fractional cover 
#       from the remote sensing data at the timestamps of ausplots obervations
#        - Note: they need to be within 1 month to control error 
# 3.  Take the difference between FC AusPlots and FC remotely sensed data  
# 4.  Repeat 2-3 until data for all observations have been found 
# 5.  Plot the data 


### Some functions ####

get_1_directional_nearest_timestep <- function(time.seq, dea.fc.input, aus.date.input) {
  
  for (d in time.seq) {
    subsetter <- subset(dea.fc.input, subset = (time == d))
    if(nrow(subsetter) > 0 & !all(is.na(subsetter$pv)) ) {
      break
    }
  }
  
  time.stamp <- paste(subsetter$time[1])
  time.diff <-  as.numeric(difftime(subsetter$time[1], aus.date.input,
                                    units = 'days'))
  return(c(time.stamp, time.diff))
}


get_nearest_timestep <- function(fowards.nearest, backwards.nearest) {

  is.for.na <- is.na(fowards.nearest[2])
  is.bac.na <- is.na(backwards.nearest[2])
  
  if(is.for.na & is.bac.na){
    return(NA)
  } else if (is.for.na) {
    timestamp.nearest <- backwards.nearest
  } else if (is.bac.na) {
    timestamp.nearest <- forward.nearest
  } else if (as.numeric(forward.nearest[2]) < abs(as.numeric((backwards.nearest[2])))) {
    timestamp.nearest <- forward.nearest
  } else {
    timestamp.nearest <- backwards.nearest
  }
  
  return(timestamp.nearest)
}

########################

dea.fc.sites.nearest <- data.frame("site_unique" = NA,
                     "time" = NA,
                     "diff" = NA,
                     "bs" = NA,
                     "pv" = NA,
                     "npv" = NA,
                     "ue" = NA)

for (file.i in fileNames) {
  
  ausplots.fc.i.index <- grep(file.i, insitu.fractional.cover$site_unique)
  ausplots.fc.i <- insitu.fractional.cover[ausplots.fc.i.index,]
  
  dea.file.path <- file.path(directory, paste0(file.i, ".csv"))
  dea.fc.i <- read.csv(dea.file.path)
  dea.fc.i$time <- as.Date(dea.fc.i$time)
  
  for (i in ausplots.fc.i$site_unique) {
    
    ausplots.info.i.index <- grep(i, veg.info$site.info$site_unique)
    debug_msg(ausplots.info.i.index)
    
    ausplots.date.i <- as.Date(veg.info$site.info$visit_start_date[ausplots.info.i.index])
    debug_msg(ausplots.date.i)
    
    times.forwards <- seq(ausplots.date.i, by='1 days', length = 31)
    debug_msg(times.forwards)
    times.backwards <- seq(ausplots.date.i, by='-1 days', length = 31)
    debug_msg(times.backwards)
    
    
    forward.nearest <- get_1_directional_nearest_timestep(times.forwards, 
                                                          dea.fc.i, ausplots.date.i)
    
    backwards.nearest <- get_1_directional_nearest_timestep(times.backwards,
                                                            dea.fc.i, ausplots.date.i)
    
    timestamp.nearest <- get_nearest_timestep(forward.nearest, backwards.nearest)
    debug_msg(timestamp.nearest)
    
    dea.fc.nearest <- subset(dea.fc.i, subset = (time == timestamp.nearest))
    debug_msg(dea.fc.nearest)
    
    ## To do: Cut the dataset further to reduce excess data
    
    dea.fc.agg.nearest <- data.frame("site_unique" = veg.info$site.info$site_unique[ausplots.info.i.index], 
                                     "time" = dea.fc.nearest$time[1], "diff" = abs(as.numeric(timestamp.nearest[2])),
               lapply(dea.fc.nearest[,c("bs","pv","npv","ue")],mean, na.rm = T))
    
    dea.fc.sites.nearest <- rbind(dea.fc.sites.nearest, dea.fc.agg.nearest)
    debug_msg(dea.fc.sites.nearest)
    
  }
}


# write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest.csv")

####### Generating plots ########

library(ggplot2)


#opaque.fc <- fractional_cover(veg.PI = veg.info$veg.PI, in_canopy_sky = "TRUE") 
#dea.fc.sites.plotting <- merge(dea.fc.sites.nearest, opaque.fc, by = 'site_unique')

dea.fc.sites.plotting <- merge(dea.fc.sites.nearest, insitu.fractional.cover, by = 'site_unique')

# Greenness 
ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100)

# Bare
ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point() + geom_abline() +
  xlim(0,100) + ylim(0,100)

# Brown
ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point() + geom_abline() +
  xlim(0,100) + ylim(0,100)



