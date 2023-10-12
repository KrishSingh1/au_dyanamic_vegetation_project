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
  print(site.location.name)
  
  nc.path <- file.path(precip.path, files[RI])
  nc <- nc_open(filename = nc.path)
  time.var <- ncvar_get(nc, "time")
  dates <- as.Date(time.var, origin = "1850-01-01")
  var <- ncvar_get(nc, 'precip')
  daily.precip <- data.frame(precip = var, time = dates)
  
  # So its in the range of July-June
  daily.precip <- subset(daily.precip, subset = (time >= "1987-07-01" & time <= "2022-07-01"))
  rownames(daily.precip) <- 1:nrow(daily.precip)
  daily.precip$group.col <- rep(NA, nrow(daily.precip)) 
  bound <- '-07-01'
  for(year in 1987:2021) {
    lower.b <- as.Date(paste0(year, bound))
    upper.b <- as.Date(paste0(year+1, bound))
    daily.precip[daily.precip$time >= lower.b &  daily.precip$time < upper.b,]$group.col <- paste(year,year+1, sep = "-")
  }
  
  single.annual.mean <- aggregate(daily.precip[,c('precip')], by = list(daily.precip$group.col), FUN = sum)
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
                             bs_sd = as.numeric(NA),
                             bs_cv = as.numeric(NA),
                             npv_mean = as.numeric(NA),
                             npv_sd = as.numeric(NA),
                             npv_cv = as.numeric(NA),
                             pv_mean = as.numeric(NA),
                             pv_sd = as.numeric(NA),
                             pv_cv = as.numeric(NA),
                             missing_periods = NA,
                             missing_periods_count = as.numeric(NA))

missing.data <- c()
for(RI in 1:length(fileNames)) {
  print(RI)
  site.location <- fileNames[RI]
  ausplots.info.i.index <- which(veg.info$site.info$site_location_name == site.location)
  site.path <- file.path(directory,paste0(site.location,".csv"))
  dea.data <- read.csv(site.path)
  
  
  print(site.location)
  #print(dea.data)
    #view(dea.data)
  dea.data <- subset(dea.data, subset = (ue < 27)) # reduce uncertainity
  #print(dea.data)
  dea.data <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.data, sites.query)
  dea.data$time <- as.Date(dea.data$time)
  dea.data <- subset(dea.data, subset = (time >= "1987-07-01" & time <= "2022-07-01"))
  
  if(nrow(dea.data) > 0) {
    #print(dea.data)
    
    bs.mean <- mean(dea.data$bs, na.rm = T)
    pv.mean <- mean(dea.data$pv, na.rm = T)
    npv.mean <- mean(dea.data$npv, na.rm = T) 
    
    
    rownames(dea.data) <- 1:nrow(dea.data)
    dea.data$group.col <- rep(NA, nrow(dea.data)) 
    #print(dea.data)
    
    #dea.data$group.col <- cut(dea.data$time, breaks = '365 days', labels = FALSE)
    missing_periods <- c()
    bound <- '-07-01'
    for(year in 1987:2021) {
      lower.b <- as.Date(paste0(year, bound))
      upper.b <- as.Date(paste0(year+1, bound))
      affected.rows <- nrow(dea.data[dea.data$time >= lower.b &  dea.data$time < upper.b,])
      if(affected.rows > 0) {
        dea.data[dea.data$time >= lower.b &  dea.data$time < upper.b,]$group.col <- paste(year,year+1, sep = "-") 
      } else {
        missing_periods <- c(missing_periods, paste(year,year+1, sep = "-"))
      }
    }
    
    #print(dea.data)
    single.annual.mean <- aggregate(dea.data[,c('bs', 'npv', 'pv')], by = list(dea.data$group.col),
                                    FUN = mean, na.rm = T)
    
    # standard deviation
    annual.sd <- lapply(single.annual.mean[,c('bs','npv','pv')], sd, na.rm = T)
    bs_sd <- annual.sd$bs
    npv_sd <- annual.sd$npv
    pv_sd <- annual.sd$pv
    
    ## mean
    annual.mean <- lapply(single.annual.mean[,c('bs','npv', 'pv')], mean, na.rm = T)
    bs_mean <- annual.mean$bs
    npv_mean <- annual.mean$npv
    pv_mean <- annual.mean$pv
    
    ## Coeficient of variation
    
    bs_cv = annual.sd$bs/annual.mean$bs
    npv_cv = annual.sd$npv/annual.mean$npv
    pv_cv = annual.sd$pv/annual.mean$pv
    
    # Count missing period 
    missing_periods_str <- paste(missing_periods, collapse = ',')
    missing_periods_count <- length(missing_periods)
    
    
    annual.fc.data.i <- data.frame(site_location_name = site.location, 
                                 bs_mean = bs_mean,
                                 bs_sd = bs_sd,
                                 bs_cv = bs_cv,
                                 npv_mean = npv_mean,
                                 npv_sd = npv_sd,
                                 npv_cv = npv_cv,
                                 pv_mean = pv_mean,
                                 pv_sd = pv_sd,
                                 pv_cv = pv_cv,
                                 missing_periods = missing_periods_str,
                                 missing_periods_count = missing_periods_count)
    annual.fc.data <- rbind(annual.fc.data, annual.fc.data.i)
  } else {
    missing.data <- c(missing.data, site.location)
  }
}
annual.fc.data <- annual.fc.data[-1,]
save(... = annual.fc.data, file = 'annual.fc.data.RData')
load('annual.fc.data.RData')





############## Visualise the Data ##################

load('annual.precip.data.RData')
load('annual.fc.data.RData')

precip.fc.data <- merge(annual.fc.data, annual.precip.data, by = 'site_location_name')

library(ggplot2)
library(cowplot)

pl.prec.pv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = pv_mean)) + geom_point() + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Daily Green Cover (%)')
pl.prec.bs <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = bs_mean)) + geom_point() + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Daily Bare Cover (%)')  
pl.prec.npv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = npv_mean)) + geom_point() + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Daily Brown Cover (%)') 

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)

# cover_mean_vs_precip_mean


pl.prec.pv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = pv_mean)) + labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Daily Green Cover (%)')
pl.prec.bs <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = bs_mean)) + labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Daily Bare Cover (%)')
pl.prec.npv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = npv_mean)) + labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Daily Brown Cover (%)')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)

# cover_cv_vs_precip_cv

pl.prec.pv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = pv_cv)) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')
pl.prec.bs <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = bs_cv)) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Bare Cover (%)')
pl.prec.npv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = npv_cv)) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Brown Cover (%)')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)


## cover_mean_vs_precip_sd

pl.prec.pv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_sd, y = pv_mean)) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')
pl.prec.bs <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_sd, y = bs_mean)) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Bare Cover (%)')
pl.prec.npv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_sd, y = npv_mean)) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Brown Cover (%)')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)






