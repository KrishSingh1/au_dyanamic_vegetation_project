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
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")


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


trim_to_nearest_coord <- function(ausplots.info.i.index, veg.info, dea.fc.i, reference.query ) {
  
  reference.query.index <- which(reference.query$site_location_name == veg.info$site.info$site_location_name[ausplots.info.i.index][1])
  print(reference.query.index)
  
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



plot_site_markings <- function(easting.site, northing.site, dea.fc.i) {
  
  MASS::eqscplot(dea.fc.i$x, dea.fc.i$y,tol = .5, xlab = "easting", ylab = "northing")
  
  points(x = easting.site,y = northing.site, pch = 2, col = 'red')
  points(x = easting.site+100,y = northing.site, pch = 2, col= 'red')
  points(x = easting.site,y = northing.site+100, pch = 2, col= 'red')
  points(x = easting.site+100,y = northing.site+100, pch = 2, col= 'red')
  
}




######### Obtain Dataset for Exploration ###########

dea.fc.sites.nearest <- data.frame("site_unique" = NA,
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
  dea.fc.i <- subset(dea.fc.i, subset = (ue < 27))
  
  site.info.index <- which(veg.info$site.info$site_location_name == file.i)
  dea.fc.i <- trim_to_nearest_coord(site.info.index, veg.info, dea.fc.i, sites.query)
  
  for (i in ausplots.fc.i$site_unique) {
    print(dea.fc.i)
    
    ausplots.info.i.index <- grep(i, veg.info$site.info$site_unique)
    
    ausplots.date.i <- as.Date(veg.info$site.info$visit_start_date[ausplots.info.i.index])
    
    
    times.forwards <- seq(ausplots.date.i, by='1 days', length = 31)
    times.backwards <- seq(ausplots.date.i, by='-1 days', length = 31)
    closest.times <- rbind(dea.fc.i[dea.fc.i$time %in%times.forwards,],
                           dea.fc.i[dea.fc.i$time %in%times.backwards,])
    print(closest.times)
    
    dea.fc.agg.nearest <- data.frame("site_unique" = i,
                                     lapply(closest.times[,c("bs","npv","pv", "ue")],FUN = mean, na.rm = T), 
                                     "npixels" = nrow(unique(closest.times[,c('x','y')])))
    
    print(dea.fc.agg.nearest)
    dea.fc.sites.nearest <- rbind(dea.fc.sites.nearest, dea.fc.agg.nearest)

  }

  no.files.processed <- no.files.processed + 1
  print(paste(no.files - no.files.processed, "left"))
}

#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest.csv")
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_pixel_inc.csv")
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_median.csv")
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_no_spatial.csv")
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_adjusted_spatial.csv")

dea.fc.sites.nearest <- dea.fc.sites.nearest[-1,]
#write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_filtered_ue.csv")
write.csv(dea.fc.sites.nearest, "dea_fc_sites_nearest_new_aggregation.csv")

if(debug) {
  
  test.trim <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.fc.i)
  
  plot_site_markings(veg.info$site.info$pit_marker_easting[ausplots.info.i.index],
                     veg.info$site.info$pit_marker_northing[ausplots.info.i.index],
                     dea.fc.nearest.test)
  
  plot_site_markings(veg.info$site.info$pit_marker_easting[ausplots.info.i.index],
                     veg.info$site.info$pit_marker_northing[ausplots.info.i.index],
                     dea.fc.nearest)
  
}



dea.fc.sites.nearest

####### Generating plots ########

library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(tune)

#dea.fc.sites.nearest <- read.csv(file = "dea_fc_sites_nearest.csv")

#opaque.fc <- fractional_cover(veg.PI = veg.info$veg.PI, in_canopy_sky = "TRUE") 
#dea.fc.sites.plotting <- merge(dea.fc.sites.nearest, opaque.fc, by = 'site_unique')

#dea.fc.sites.nearest <- read.csv("dea_fc_sites_nearest_pixel_inc.csv")
#dea.fc.sites.nearest <- read.csv("dea_fc_sites_nearest_filtered_ue.csv")
dea.fc.sites.nearest <- read.csv("dea_fc_sites_nearest_new_aggregation.csv")

# Test subsetting for sites not in proper oriention or marked 
#valid_observations <- veg.info$site.info$site_unique[which(veg.info$site.info$plot_is_aligned_to_grid & veg.info$site.info$plot_is_permanently_marked)]
#dea.fc.sites.nearest <- subset(dea.fc.sites.nearest, subset = (site_unique %in% valid_observations))

#insitu.fractional.cover <- readRDS("../STEP2_VEG_EXTRACTION/insitu_fractional_cover_canopy_2-0-3rds")


insitu.fractional.cover <- subset(insitu.fractional.cover, (NA. <= 10))
dea.fc.sites.plotting <- merge(dea.fc.sites.nearest, insitu.fractional.cover, by = 'site_unique')
dea.fc.sites.plotting <- subset(dea.fc.sites.plotting, subset = (npixels >= 100 & npixels <= 121))



# Greenness 

pv.stats <- lm(pv~green,dea.fc.sites.plotting)
cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + geom_abline(slope = pv.stats$coefficients[["green"]], 
                                                                                                        intercept = pv.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
cal.green



Metrics::rmse(actual = dea.fc.sites.plotting$green, 
              predicted = dea.fc.sites.plotting$pv)


# Bare
bs.stats <- lm(bs~bare,dea.fc.sites.plotting)
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + geom_abline(slope = bs.stats$coefficients[["bare"]], 
                                                                                  intercept = bs.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
cal.bare

Metrics::rmse(actual = dea.fc.sites.plotting$bare, 
              predicted = dea.fc.sites.plotting$bs)

# Brown
npv.stats <- lm(npv~brown,dea.fc.sites.plotting)
cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + geom_abline(slope = npv.stats$coefficients[["brown"]], 
                                                                                  intercept = npv.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

Metrics::rmse(actual = dea.fc.sites.plotting$brown, 
              predicted = dea.fc.sites.plotting$npv)


# By vegetation type ------------------------------------------------------


growth.form <- readRDS("growth_form_matrix.rds")

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

growth.form$site_location_name <- unlist(lapply(rownames(growth.form), get_location_name))
growth.form.agg <- aggregate(growth.form, by = list(growth.form$site_location_name), FUN = mean, na.rm = T)
colnames(growth.form.agg)[which(colnames(growth.form.agg) == 'Group.1')] <- 'site_location_name'

# Sum Growth Forms by Classification --------------------------------------

growth.form.classification <- read.csv("Growth_Type_Classification.csv",header = F)
growth.form.classification <- na.omit(growth.form.classification)

grass.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Grass']
shrub.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Shrub']
tree.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Tree']

growth.form.agg$grass <- rowSums(growth.form.agg[,grass.names], na.rm = T)
growth.form.agg$shrub <- rowSums(growth.form.agg[,shrub.names], na.rm = T)
growth.form.agg$tree <- rowSums(growth.form.agg[,tree.names], na.rm = T)


# Begin classification ----------------------------------------------------

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("grass","shrub","tree")])))
}

growth.form.agg$vegetation_type <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = classify))



# Combine with Fractional Data -----------------------------------------

growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]

dea.fc.sites.plotting$site_location_name <- unlist(lapply(dea.fc.sites.plotting$site_unique, get_location_name))
dea.fc.sites.plotting <- merge(dea.fc.sites.plotting, growth.form.essen, by = 'site_location_name')

cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  facet_wrap(~vegetation_type) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = T)

cal.green

tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$green, 
              predicted = tree$pv)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$green, 
              predicted = shrub$pv)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$green, 
              predicted = grass$pv)

# Bare --------------------------------------------------------------------


# Bare
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) + coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = T)
  
cal.bare

Metrics::rmse(actual = dea.fc.sites.plotting$bare, 
              predicted = dea.fc.sites.plotting$bs)


tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$bare, 
              predicted = tree$bs)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$bare, 
              predicted = shrub$bs)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$bare, 
              predicted = grass$bs)






# Brown
cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + facet_wrap(~vegetation_type) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = T)

cal.brown

Metrics::rmse(actual = dea.fc.sites.plotting$brown, 
              predicted = dea.fc.sites.plotting$npv)


tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$brown, 
              predicted = tree$npv)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$brown, 
              predicted = shrub$npv)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$brown, 
              predicted = grass$npv)



library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

gridExtra::grid.arrange(cal.green, cal.brown, cal.bare)

plot_space <- par(mfrow=c(3,1))
plot_space <- c(cal.green, cal.bare, cal.brown)
plot_space <- cal.bare
plot_space <- cal.brown
plot_space

########## Convert to long #######
dea.fc.sites.plotting.long <- reshape2::melt(dea.fc.sites.plotting[, c('site_unique','bs','npv','pv')], id.vars = c('site_unique'), value.name ="remote.cover")
site.fc.df.long <- reshape2::melt(dea.fc.sites.plotting[,c('site_unique','bare','brown','green')], id.vars = c('site_unique'), value.name = "insitu.cover")

dea.fc.sites.plotting.long$variable <- as.character(dea.fc.sites.plotting.long$variable)
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'pv')] <- 'green'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'npv')] <- 'brown'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'bs')] <- 'bare'

both.plotting.df.long <- merge(dea.fc.sites.plotting.long, site.fc.df.long, by = c("site_unique", 'variable'))
all.stats <- lm(remote.cover~insitu.cover,both.plotting.df.long)


all.pl.validate <- ggplot(data = both.plotting.df.long, aes(x = insitu.cover, y = remote.cover, colour = variable)) + geom_point() + labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) + geom_abline(slope = all.stats$coefficients[["insitu.cover"]], 
                                                                                                                           intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

cal.all <- ggplot(dea.fc.sites.plotting) + geom_point(aes(x = bare, y = bs, colour = 'bare'), alpha = 0.5) + geom_point(aes(x = green, y = pv, colour = 'green'), alpha = 0.5) + geom_point(aes(x = brown, y = npv, colour = 'brown'), alpha = 0.5) + geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) +
  xlim(0,100) + ylim(0,100) + labs(x = "cover (in-situ)", y = "cover (remote)") + scale_colour_manual(name = 'Cover', values = c('brown' = '#0072B2', 'green' = '#009E73', 'bare' = 'red')) +
  geom_abline(slope = all.stats$coefficients[["insitu.cover"]],intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(data = both.plotting.df.long, 
                                                                                                                                     mapping = use_label(c("eq", "R2", 'p'), aes(x = insitu.cover, y =remote.cover)))


cowplot::plot_grid(cal.green, cal.brown, cal.bare,all.pl.validate)
cowplot::plot_grid(cal.green, cal.brown, cal.bare,cal.all)





### Try with ausplots' other fc calcs. ###

fc <- fractional_cover(veg.PI = veg.info$veg.PI, in_canopy_sky = T) 
fc <- subset(fc, (other <= 10))
dea.fc.sites.plotting <- merge(dea.fc.sites.nearest, fc, by = 'site_unique')

dea.fc.sites.plotting <- subset(dea.fc.sites.plotting, subset = (npixels > 100 & npixels <= 121))

# Greenness 
ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) 

Metrics::rmse(actual = dea.fc.sites.plotting$green, 
              predicted = dea.fc.sites.plotting$pv)

ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point() + geom_abline() +
  xlim(0,100) + ylim(0,100) 

Metrics::rmse(actual = dea.fc.sites.plotting$bare, 
              predicted = dea.fc.sites.plotting$bs)

# Brown
ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point() + geom_abline() +
  xlim(0,100) + ylim(0,100) 


Metrics::rmse(actual = dea.fc.sites.plotting$brown, 
              predicted = dea.fc.sites.plotting$npv)
