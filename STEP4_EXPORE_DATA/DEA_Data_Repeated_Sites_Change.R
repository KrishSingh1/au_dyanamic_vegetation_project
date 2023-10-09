###### DEA_Data_Repeated_Sites_Visualisation ######
# By Krish Singh
# 20230929
# To see the change in fractional cover from site visit to another with DEA data. 

library(plotly)
library(ggplot2)
library(xts)
library(forecast)
library(seasonal)
library(dplyr)
library(caret)
library(cowplot)
library(data.table)
library(tune)



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
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")


# Get Site names
site.names <- unique(veg.info$site.info$site_location_name)

# Count the number of observations of each site
counts.df <- as.data.frame(table(veg.info$site.info$site_location_name))
sites.revisit.2.df <- subset(counts.df, Freq >= 2) # For sites that were visited two or more 

# Merge sites with counts == 2 with current veg information
site.info.df <- as.data.frame(veg.info$site.info)
site.info.df <- merge(site.info.df, insitu.fractional.cover, by = "site_unique")

# Remove in-situ fractional cover of sites with an NA. of above 10%.
site.info.df <- subset(site.info.df, subset = (NA. <= 10))

# Remeasure Counts 
counts.df <- as.data.frame(table(site.info.df$site_location_name))
sites.revisit.2.df <- subset(counts.df, Freq >= 2) # For sites that were visited two or more

# Subset based on counts 
site.info.df.revisit <- subset(site.info.df, subset = site_location_name %in% sites.revisit.2.df$Var1)
site.info.df.revisit$visit_start_date <- as.Date(site.info.df.revisit$visit_start_date)

# Subset based on the avaliable dea information
site.info.df.revisit <- subset(site.info.df.revisit, subset = site_location_name %in% fileNames)


table(site.info.df.revisit$site_location_name)
## Some basic visualisations

ggplot(data = site.info.df.revisit, aes(x = visit_start_date, y = green)) +
  geom_point() + facet_grid(~state)
## A lot of the repeated measures were in south australia 


## Now to get the change in FC in-situ
site.location.names <- unique(site.info.df.revisit$site_location_name)

site.fc.change.df <- data.frame(site_location_name = NA,
                                visit_start_date_a = as.Date(NA),
                                visit_start_date_b = as.Date(NA),
                                green = as.numeric(NA),
                                brown = as.numeric(NA),
                                bare = as.numeric(NA))

for (name in site.location.names) {

  visit.data <- subset(site.info.df.revisit, subset = (site_location_name == name))
  visit.data <- visit.data[order(visit.data$visit_start_date, decreasing =  T),]
  
  samples <- nrow(visit.data)
  
  change <- data.frame(site_location_name = NA,
                       visit_start_date_a = as.Date(NA),
                       visit_start_date_b = as.Date(NA),
                       green = as.numeric(NA),
                       brown = as.numeric(NA),
                       bare = as.numeric(NA))
  
  for(i in 1:(samples - 1)) {
    b <- visit.data[i, c("green", "brown", "bare")]
    a <- visit.data[i+1, c("green", "brown", "bare")]
    print(visit.data[i:(i+1), c("site_location_name", "green", "brown", "bare", "visit_start_date")])
    change.i <- b - a
    change.i$visit_start_date_a <- visit.data$visit_start_date[i+1]
    change.i$visit_start_date_b <- visit.data$visit_start_date[i]
    change.i$site_location_name <- name
    print(change.i)
    #print(change.i)
    change <- rbind(change, change.i)
  }  
  
  if(samples > 2) {
    print("From beginning to end")
    b <- visit.data[1, c("green", "brown", "bare")]
    a <- visit.data[samples, c("green", "brown", "bare")]
    print(visit.data[1:samples, c("site_location_name", "green", "brown", "bare", "visit_start_date")])
    change.i <- b - a
    change.i$visit_start_date_a <- visit.data$visit_start_date[samples]
    change.i$visit_start_date_b <- visit.data$visit_start_date[1]
    change.i$site_location_name <- name
    print(change.i)
    #print(change.i)
    change <- rbind(change, change.i)
    
  }
  
  
  change <- change[-1,]
  #print(change)
  site.fc.change.df <- rbind(site.fc.change.df, change)
}
site.fc.change.df <- site.fc.change.df[-1,]

bare.pl <- ggplot(data = site.fc.change.df, mapping = aes(x = bare)) + geom_histogram()
green.pl <- ggplot(data = site.fc.change.df, mapping = aes(x = green)) + geom_histogram()
brown.pl <- ggplot(data = site.fc.change.df, mapping = aes(x = brown)) + geom_histogram()

plot_grid(bare.pl, green.pl, brown.pl)


## Now get dea revisit measurements 
# Algorithm:
# 1. Get the datasets: queryfile, Dea.fc files, ausplots fc
# 2. Check which of the sites have repeated visits (2 visits for now)
# 3. (In DEA FC) get data-points that are month before and after from the site visits (from Ausplots FC)
# 4. (IN DEA FC) take the average of data points defined in 3.
# 5. Export that data

use.saved.data <- T
if(!use.saved.data) {
  dea.fc.means.df <- data.frame(site_location_name = NA,
                                time = as.Date(NA),
                                pv = as.numeric(NA),
                                npv = as.numeric(NA),
                                bs = as.numeric(NA),
                                spatial_ref = NA)
  
  missing.data <- c()
  
  site.fc.change.df.names <- unique(site.fc.change.df$site_location_name)
  for (site.location in site.fc.change.df.names) {
    
    print(site.location)
    site.path <- file.path(directory,paste0(site.location,".csv"))
  
    ausplots.info.i.index <- which(veg.info$site.info$site_location_name == site.location)
    dea.data <- read.csv(site.path)
    dea.data <- trim_to_nearest_coord(ausplots.info.i.index, veg.info, dea.data, sites.query)
    
    if(nrow(dea.data) > 0){
      dea.data.agg <- aggregate(dea.data, by = list(dea.data$time),
                                FUN = mean, na.rm = T)
      dea.data.agg$Group.1 <- as.Date(dea.data.agg$Group.1)
      dea.data.agg <- dea.data.agg[,c("Group.1", "pv", "npv", "bs", "spatial_ref")]
      
      visit.times <- subset(site.info.df.revisit,
                            subset = (site_location_name == site.location))
      
      means <- data.frame(Group.1 = as.Date(NA), bs = NA, npv = NA, pv = NA, spatial_ref = NA)
      
      for(i in 1:nrow(visit.times)){ 
        date <- visit.times$visit_start_date[i]
        #print(date)
        times.forwards <- seq(date, by='1 days', length = 31)
        times.backwards <- seq(date, by='-1 days', length = 31)
        closest.times <- rbind(dea.data.agg[dea.data.agg$Group.1 %in%times.forwards,],
                               dea.data.agg[dea.data.agg$Group.1 %in%times.backwards,])
        #print(closest.times)
        means <- rbind(means,data.frame(Group.1 = date, lapply(closest.times[,c("bs","npv","pv", "spatial_ref")],
                                                               FUN = mean, na.rm = T)))
        #print(means)
      }
      #print(means)
      means <- means[-1,]
      means$site_location_name <- rep(site.location, nrow(means))
      #print(means)
      colnames(means)[which(colnames(means) == "Group.1")] = "time"
      
      #print(means)
      dea.fc.means.df <- rbind(dea.fc.means.df, means)
    } else{
      missing.data <- c(missing.data, site.location)
    }
  } 
  dea.fc.means.df <- dea.fc.means.df[-1,] 
  #save(dea.fc.means.df, file =  "dea.fc.means.df_23108.RData") 
} else {
  load('dea.fc.means.df_23108.RData')
}


# Now get the change in fc

site.location.names <- unique(dea.fc.means.df$site_location_name)
dea.fc.change.df <- data.frame(site_location_name = NA,
                                visit_start_date_a = as.Date(NA),
                                visit_start_date_b = as.Date(NA),
                                pv = as.numeric(NA),
                                npv = as.numeric(NA),
                                bs = as.numeric(NA))

for (name in site.location.names) {
  
  
  visit.data <- subset(dea.fc.means.df, subset = (site_location_name == name))
  visit.data <- visit.data[order(visit.data$time, decreasing =  T),]
  
  change <- data.frame(site_location_name = NA,
                       visit_start_date_a = as.Date(NA),
                       visit_start_date_b = as.Date(NA),
                       pv = as.numeric(NA),
                       npv = as.numeric(NA),
                       bs = as.numeric(NA))
  samples <- nrow(visit.data)
  
  for(i in 1:(samples - 1)) {
    b <- visit.data[i, c("pv", "npv", "bs")]
    a <- visit.data[i+1, c("pv", "npv", "bs")]
    print(visit.data[i:(i+1), c("site_location_name", "pv", "npv", "bs", "time")])
    change.i <- b - a
    change.i$visit_start_date_a <- visit.data$time[i+1]
    change.i$visit_start_date_b <- visit.data$time[i]
    change.i$site_location_name <- name
    print(change.i)
    #print(change.i)
    change <- rbind(change, change.i)
  }
  
  if(samples > 2) {
    print("From beginning to end")
    b <- visit.data[1, c("pv", "npv", "bs")]
    a <- visit.data[samples, c("pv", "npv", "bs")]
    print(visit.data[1:samples, c("site_location_name", "pv", "npv", "bs", "time")])
    change.i <- b - a
    change.i$visit_start_date_a <- visit.data$time[samples]
    change.i$visit_start_date_b <- visit.data$time[1]
    change.i$site_location_name <- name
    print(change.i)
    #print(change.i)
    change <- rbind(change, change.i)
    
  }
  
  change <- change[-1,]
  #print(change)
  dea.fc.change.df <- rbind(dea.fc.change.df, change)
}
dea.fc.change.df <- dea.fc.change.df[-1,]


### This visualisation is not aggregated i.e. 3 visits will count as two timestamps between visit dates (A,B) and (B,C)##

both.changs.df <- merge(dea.fc.change.df,site.fc.change.df, by = c("site_location_name", "visit_start_date_a",
                                                 "visit_start_date_b"))

bs.bare.pl <- ggplot(data = both.changs.df, aes(x = bare, y = bs)) + labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))

pv.green.pl <- ggplot(data = both.changs.df, aes(x = green, y = pv)) + labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") + 
  geom_point() + geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))
npv.brown.pl <- ggplot(data = both.changs.df, aes(x = brown, y = npv), colour = 'blue') + geom_point() + labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") + 
  geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))


all.pl <- ggplot(data = both.changs.df) + geom_point(aes(x = brown, y = npv, colour = 'brown')) + geom_point(aes(x = green, y = pv, colour = 'green')) + 
  geom_point(aes(x = bare, y = bs, colour = 'bare')) +labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + scale_colour_manual(name = 'Cover', values = c('brown' = 'blue', 'green' = 'green', 'bare' = 'red')) + 
  xlim(c(-100,100)) + ylim(c(-100,100))


plot_grid(bs.bare.pl,npv.brown.pl,pv.green.pl, all.pl) 


### Averaging the change in cover over time in sites with 3 visits ###

both.changes.agg <- aggregate(both.changs.df[,c("pv","npv","bs", "green", "brown", "bare")],
          list(both.changs.df$site_location_name), FUN = mean, na.rm = T)


bs.bare.pl <- ggplot(data = both.changes.agg, aes(x = bare, y = bs)) + labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))

pv.green.pl <- ggplot(data = both.changes.agg, aes(x = green, y = pv)) + labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") + 
  geom_point() + geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))
npv.brown.pl <- ggplot(data = both.changs.df, aes(x = brown, y = npv), colour = 'blue') + geom_point() + labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") + 
  geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))


all.pl <- ggplot(data = both.changes.agg) + geom_point(aes(x = brown, y = npv, colour = 'brown')) + geom_point(aes(x = green, y = pv, colour = 'green')) + 
  geom_point(aes(x = bare, y = bs, colour = 'bare')) +labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + scale_colour_manual(name = 'Cover', values = c('brown' = 'blue', 'green' = 'green', 'bare' = 'red')) + 
  xlim(c(-100,100)) + ylim(c(-100,100))



plot_grid(bs.bare.pl,npv.brown.pl,pv.green.pl, all.pl) 
