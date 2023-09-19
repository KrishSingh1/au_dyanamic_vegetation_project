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
                                   "latitude" = NA,
                                   "longitude" = NA,
                                   "diff" = NA,
                                   "bs" = NA,
                                   "pv" = NA,
                                   "npv" = NA,
                                   "ue" = NA,
                                   "timesteps" = NA)

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
    
    
    dea.fc.nearest <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.i)
    
    
    if(nrow(dea.fc.nearest) > 0) { # Check if there is an issue with spatial trimming
    
      agg.time.series <- aggregate(dea.fc.nearest[,c("time", "bs", "pv", "npv", "ue")], 
                                   by = list(dea.fc.nearest$time), FUN = mean, na.rm = T)
      
      
      zscores <- sapply(agg.time.series[,c("bs", "pv", "npv", "ue")], FUN = scale)
      zscores.nearest <- zscores[which(agg.time.series$time == timestamp.nearest[1]),]
      
      
      dea.fc.agg.nearest <- data.frame("site_unique" = i, 
                                       "time" = timestamp.nearest[1],
                                       "latitude" = veg.info$site.info$latitude[ausplots.info.i.index],
                                       "longitude" = veg.info$site.info$longitude[ausplots.info.i.index],
                                       "diff" = as.numeric(timestamp.nearest[2]), 
                                       "bs" = zscores.nearest["bs"],
                                       "pv" = zscores.nearest["pv"],
                                       "npv"= zscores.nearest["npv"],
                                       "ue" = zscores.nearest["ue"],
                                       "timesteps" = nrow(agg.time.series), row.names =  NULL)
    } else {
      dea.fc.agg.nearest <- data.frame("site_unique" = i, 
                                       "time" = timestamp.nearest[1],
                                       "latitude" = veg.info$site.info$latitude[ausplots.info.i.index],
                                       "longitude" = veg.info$site.info$longitude[ausplots.info.i.index],
                                       "diff" = as.numeric(timestamp.nearest[2]), 
                                       "bs" = NA,
                                       "pv" = NA,
                                       "npv"= NA,
                                       "ue" = NA,
                                       "timesteps" = NA, row.names =  NULL)
    }
    
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
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_zscores.csv")





####### Generating plots ########

library(leaflet)
library(dplyr)
dea.fc.sites.nearest <- read.csv("dea_fc_sites_nearest_zscores.csv")

plot_aus_map <- function(df, variable){
  
  df["var"] <- df[variable]
  col.var <- colorNumeric(palette = c("green", "red"), domain = df$var)
  aus.map.var <- leaflet(df) %>% addTiles %>%
    addCircleMarkers(data = df,
                     lat = ~latitude,
                     lng = ~longitude,
                     color = ~col.var(var),
                     radius = 3,
                     fillOpacity = 1, 
                     popup= ~paste0(variable, " :", var, "<br>", 
                                    "time: ", time, "<br>",
                                    "diff: ", diff,"<br>",
                                    "site_unique", site_unique)
                     )  %>%
    addLegend("bottomright", pal = col.var, values = ~var,
              title = variable, opacity = 1)
  
  
  return(aus.map.var)
}


dea.fc.sites.nearest <- na.omit(dea.fc.sites.nearest)
plot_aus_map(dea.fc.sites.nearest, "bs")
plot_aus_map(dea.fc.sites.nearest,  "pv")
plot_aus_map(dea.fc.sites.nearest, "npv")






