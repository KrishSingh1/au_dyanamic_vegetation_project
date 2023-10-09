#### Finding the Correlation between precipitation and fractional covers #####
# Krish Singh
# 2023109

#### General Process
# Objective: Finding the correlation between mean annual precipitation and mean fractional cover for each site 
# Trying with one site
# 1. Find mean annual precipitation of a site 

library(stringr)
library(ncdf4)
library(dplyr)
library(lubridate)

directory <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/ausplots_agcd"
precip.path <- file.path(directory, "precip")
files <- list.files(precip.path, pattern = "\\.nc$", full.names = FALSE) # same for each directory
fileNames <- tools::file_path_sans_ext(files)

annual.precip.data <- data.frame(site_location_name = NA, 
                                 precip_sd = as.numeric(NA),
                                 precip_mean = as.numeric(NA),
                                 precip_cv = as.numeric(NA))

for(RI in 1:length(fileNames)) {
  
  site.location.name <- stringr::str_split(fileNames[RI], "_")[[1]][1]
  
  nc.path <- file.path(precip.path, files[RI])
  nc <- nc_open(filename = nc.path)
  time.var <- ncvar_get(nc, "time")
  dates <- as.Date(time.var, origin = "1850-01-01")
  var <- ncvar_get(nc, 'precip')
  daily.precip <- data.frame(precip = var, time = dates)
  
  # So its in the range of July-June
  daily.precip <- subset(daily.precip, subset = (time >= "1987-07-01" & time <= "2022-06-30"))
  rownames(daily.precip) <- 1:nrow(daily.precip)
  
  daily.precip$group.col <- cut(daily.precip$time, breaks = '365 days', labels = FALSE)
  
  single.annual.mean <- aggregate(daily.precip[,c('precip')], by = list(daily.precip$group.col), FUN = mean)
  annual.sd <- sd(single.annual.mean$x)
  annual.mean <- mean(single.annual.mean$x)
  annual.cv <- annual.sd / annual.mean
  
  annual.precip.data.i <- data.frame(site_location_name = site.location.name, 
                                     precip_sd = annual.sd,
                                     precip_mean = mean(single.annual.mean$x),
                                     precip_cv = annual.cv)
  
  annual.precip.data <- rbind(annual.precip.data,annual.precip.data.i)
}

annual.precip.data <- annual.precip.data[-1,]
save(... = annual.precip.data, file = 'annual.precip.data.RData')

load('annual.precip.data.RData')


# 2. Get FC mean annual statistics 

trim_to_nearest_coord <- function(ausplots.info.i.index, veg.info, dea.fc.i, reference.query ) {
  
  reference.query.index <- which(reference.query$site_location_name == veg.info$site.info$site_location_name[ausplots.info.i.index][1])
  #print(reference.query.index)
  
  # Site End Points:   
  #W.site <- veg.info$site.info$pit_marker_easting[ausplots.info.i.index][2]
  #S.site <- veg.info$site.info$pit_marker_northing[ausplots.info.i.index][2]
  W.site <- reference.query$pit_marker_easting[reference.query.index]
  S.site <- reference.query$pit_marker_northing[reference.query.index]
  
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



# Get Needed datasets
directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")

annual.fc.data <- data.frame(site_location_name = NA, 
                                 bs_mean = as.numeric(NA),
                                 npv_mean = as.numeric(NA),
                                 pv_mean = as.numeric(NA))

for(RI in 1:length(fileNames)) {

  site.location <- fileNames[RI]
  ausplots.info.i.index <- which(veg.info$site.info$site_location_name == site.location)
  site.path <- file.path(directory,paste0(site.location,".csv"))
  dea.data <- read.csv(site.path)
  dea.data <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.data, sites.query)
  dea.data <- subset(dea.data, subset = (time >= "1987-07-01" & time <= "2022-06-30"))
  bs.mean <- mean(dea.data$bs, na.rm = T)
  pv.mean <- mean(dea.data$pv, na.rm = T)
  npv.mean <- mean(dea.data$npv, na.rm = T) 
  
  annual.fc.data.i <- data.frame(site_location_name = site.location, 
                               bs_mean = bs.mean,
                               npv_mean = npv.mean,
                               pv_mean = pv.mean)
  annual.fc.data <- rbind(annual.fc.data, annual.fc.data.i)
  print(RI)
}




