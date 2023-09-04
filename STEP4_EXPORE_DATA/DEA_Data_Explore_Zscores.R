#### DEA Data Exploration Advanced #### 
# By Krish Singh
# Date: 230827
# Purpose: To visually explore the alignment of AusPlots and DEA data. 

debug <- F

debug_msg <- function(msg) {
  if (debug) {
    print(msg)
  }
}


library(ausplotsR)

directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")


ausplots.fc.i.index <- grep("NSABBS0003", insitu.fractional.cover$site_unique)
ausplots.fc.i <- insitu.fractional.cover[ausplots.fc.i.index,]

dea.file.path <- file.path(directory, paste0("NSABBS0003", ".csv"))
dea.fc.i <- read.csv(dea.file.path)
dea.fc.i$time <- as.Date(dea.fc.i$time)


ausplots.info.i.index <- grep("NSABBS0003-58581", veg.info$site.info$site_unique)

ausplots.date.i <- as.Date(veg.info$site.info$visit_start_date[ausplots.info.i.index])


times.forwards <- seq(ausplots.date.i, by='1 days', length = 31)
times.backwards <- seq(ausplots.date.i, by='-1 days', length = 31)

forward.nearest <- get_1_directional_nearest_timestep(times.forwards, 
                                                      dea.fc.i, ausplots.date.i)

backwards.nearest <- get_1_directional_nearest_timestep(times.backwards,
                                                        dea.fc.i, ausplots.date.i)

timestamp.nearest <- get_nearest_timestep(forward.nearest, backwards.nearest)

dea.fc.nearest <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.i)

agg.time.series <- aggregate(dea.fc.nearest[,c("time", "bs", "pv", "npv", "ue")], 
                             by = list(dea.fc.nearest$time), FUN = mean, na.rm = T)

sds <- mapply(agg.time.series[,c("bs", "pv", "npv", "ue")], FUN = sd, na.rm = T)
means <- mapply(agg.time.series[,c("bs", "pv", "npv", "ue")], FUN = mean, na.rm = T)

zscores <- (agg.time.series[,c("bs", "pv", "npv", "ue")] - means)/sds
zscore.timestamp <- zscores[which(agg.time.series$time == timestamp.nearest),]



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
  
  na <- T
  
  for (d in time.seq) {
    subsetter <- subset(dea.fc.input, subset = (time == d))
    if(nrow(subsetter) > 0 & mean(is.na(subsetter$pv)) < 0.3 ) {
      na <- F
      break
    }
  }
  
  if(na) {
    return(c(NA, NA))
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
    timestamp.nearest <- c(NA,NA)
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


trim_to_nearest_coord <- function(ausplots.info.i.index, veg.info, dea.fc.i ) {
  
  # Site End Points:   
  W.site <- veg.info$site.info$pit_marker_easting[ausplots.info.i.index]
  S.site <- veg.info$site.info$pit_marker_northing[ausplots.info.i.index]
  N.site <- S.site + 100
  E.site <- W.site + 100
  
  #print(W.site)
  
  # Remote End Points: 
  E.remote.incre <- unique(dea.fc.i$x)
  N.remote.incre <- unique(dea.fc.i$y)
  
  # Find Closest Points:
  W.closest <- E.remote.incre[which.min(abs(E.remote.incre - W.site))]
  E.closest <- E.remote.incre[which.min(abs(E.remote.incre - E.site))]
  N.closest <- N.remote.incre[which.min(abs(N.remote.incre - N.site))]
  S.closest <- N.remote.incre[which.min(abs(N.remote.incre - S.site))]
  
  #print(W.closest)
  #print(E.closest)
  #print(N.closest)
  #print(S.closest)
  
  # Trim dataset:
  trimmed <- subset(dea.fc.i, subset = (x >= W.closest & x <= E.closest &
                                          y >= S.closest & y <= N.closest))
  
  #print(unique(trimmed$x))
  #print(unique(trimmed$y))
  
  return(trimmed)
}


plot_site_markings <- function(easting.site, northing.site, dea.fc.i) {
  
  MASS::eqscplot(dea.fc.i$x, dea.fc.i$y,tol = .5, xlab = "easting", ylab = "northing")
  
  points(x = easting.site,y = northing.site, pch = 2, col = 'red')
  points(x = easting.site+100,y = northing.site, pch = 2, col= 'red')
  points(x = easting.site,y = northing.site+100, pch = 2, col= 'red')
  points(x = easting.site+100,y = northing.site+100, pch = 2, col= 'red')
  
}




######### Obtain Dataset for Exploration ###########

dea.fc.sites.nearest <- data.frame("site_unique" = NA,
                                   "time" = NA,
                                   "diff" = NA,
                                   "bs" = NA,
                                   "pv" = NA,
                                   "npv" = NA,
                                   "ue" = NA,
                                   "npixels" = NA)

no.files <- length(fileNames)
no.files.processed <- 0

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
    
    dea.fc.nearest <- subset(dea.fc.i, subset = (time == timestamp.nearest[1]))
    debug_msg(dea.fc.nearest)
    
    #dea.fc.nearest.test <- dea.fc.nearest
    
    dea.fc.nearest <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.nearest)
    
    dea.fc.agg.nearest <- data.frame("site_unique" = i, "time" = timestamp.nearest[1],
                                     "diff" = as.numeric(timestamp.nearest[2]),
                                     lapply(dea.fc.nearest[,c("bs","pv","npv","ue")], mean, na.rm = T), 
                                     "npixels" = nrow(dea.fc.nearest))
    
    dea.fc.sites.nearest <- rbind(dea.fc.sites.nearest, dea.fc.agg.nearest)
    debug_msg(dea.fc.sites.nearest)
  }
  
  if(debug) {
    break
  }
  no.files.processed <- no.files.processed + 1
  print(paste(no.files - no.files.processed, "left"))
}


#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest.csv")
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_pixel_inc.csv")


if(debug) {
  
  test.trim <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.i)
  
  plot_site_markings(veg.info$site.info$pit_marker_easting[ausplots.info.i.index],
                     veg.info$site.info$pit_marker_northing[ausplots.info.i.index],
                     dea.fc.nearest.test)
  
  plot_site_markings(veg.info$site.info$pit_marker_easting[ausplots.info.i.index],
                     veg.info$site.info$pit_marker_northing[ausplots.info.i.index],
                     dea.fc.nearest)
  
}





####### Generating plots ########

library(ggplot2)