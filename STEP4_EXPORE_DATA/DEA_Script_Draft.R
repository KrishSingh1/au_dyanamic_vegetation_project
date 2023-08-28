directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")



### Some functions ####

get_1_directional_nearest_timestep <- function(time.seq, dea.fc.input, aus.date.input) {
  
  na <- T
  
  for (d in time.seq) {
    subsetter <- subset(dea.fc.input, subset = (time == d))
    if(nrow(subsetter) > 0 & !all(is.na(subsetter$pv)) ) {
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
  
  print(W.site)
  
  # Remote End Points: 
  E.remote.incre <- unique(dea.fc.i$x)
  N.remote.incre <- unique(dea.fc.i$y)
  
  # Find Closest Points:
  W.closest <- E.remote.incre[which.min(abs(E.remote.incre - W.site))]
  E.closest <- E.remote.incre[which.min(abs(E.remote.incre - E.site))]
  N.closest <- N.remote.incre[which.min(abs(N.remote.incre - N.site))]
  S.closest <- N.remote.incre[which.min(abs(N.remote.incre - S.site))]
  
  print(W.closest)
  print(E.closest)
  print(N.closest)
  print(S.closest)
  
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


ausplots.fc.i.index <- grep("SAAKAN0002", insitu.fractional.cover$site_unique)
ausplots.fc.i <- insitu.fractional.cover[ausplots.fc.i.index,]

dea.file.path <- file.path(directory, paste0("SAAKAN0002", ".csv"))
dea.fc.i <- read.csv(dea.file.path)
dea.fc.i$time <- as.Date(dea.fc.i$time)

ausplots.info.i.index <- grep("SAAKAN0002-58876", veg.info$site.info$site_unique)
ausplots.date.i <- as.Date(veg.info$site.info$visit_start_date[ausplots.info.i.index])


veg.info$site.info$site_unique[ausplots.info.i.index]
as.Date(veg.info$site.info$visit_start_date[ausplots.info.i.index])


times.forwards <- seq(ausplots.date.i, by='1 days', length = 31)
times.backwards <- seq(ausplots.date.i, by='-1 days', length = 31)



forward.nearest <- get_1_directional_nearest_timestep(times.forwards, 
                                                      dea.fc.i, ausplots.date.i)

backwards.nearest <- get_1_directional_nearest_timestep(times.backwards,
                                                        dea.fc.i, ausplots.date.i)

timestamp.nearest <- get_nearest_timestep(forward.nearest, backwards.nearest)

dea.fc.nearest <- subset(dea.fc.i, subset = (time == timestamp.nearest[1]))
dea.fc.nearest.t <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.nearest)




test.trim <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.i)

plot_site_markings(veg.info$site.info$pit_marker_easting[ausplots.info.i.index],
                   veg.info$site.info$pit_marker_northing[ausplots.info.i.index],
                   dea.fc.nearest)

plot_site_markings(veg.info$site.info$pit_marker_easting[ausplots.info.i.index],
                   veg.info$site.info$pit_marker_northing[ausplots.info.i.index],
                   dea.fc.nearest.t)




