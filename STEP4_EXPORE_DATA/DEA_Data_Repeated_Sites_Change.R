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



directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")

insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")

site.names <- unique(veg.info$site.info$site_location_name)
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")

# Count the number of observations of each site

counts.df <- as.data.frame(table(veg.info$site.info$site_location_name))
sites.revisit.2.df <- subset(counts.df, Freq == 2) # For sites that were only visited twice 

site.info.df <- as.data.frame(veg.info$site.info)
site.info.df <- merge(site.info.df, insitu.fractional.cover, by = "site_unique")

counts.df <- as.data.frame(table(site.info.df$site_location_name))
sites.revisit.2.df <- subset(counts.df, Freq == 2) # For sites that were only visited twice 

site.info.df.revisit <- subset(site.info.df, subset = site_location_name %in% sites.revisit.2.df$Var1)
site.info.df.revisit$visit_start_date <- as.Date(site.info.df.revisit$visit_start_date)

site.info.df.revisit <- subset(site.info.df.revisit, subset = site_location_name %in% fileNames)

table(site.info.df.revisit$site_location_name)
## Some basic visualisations

ggplot(data = site.info.df.revisit, aes(x = visit_start_date, y = green)) +
  geom_point() + facet_grid(~state)
## A lot of the repeated measures were in south australia 

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
  
  #print(visit.data[, c("visit_start_date","green", "brown", "bare")])
  change <- visit.data[1, c("green", "brown", "bare")] - visit.data[2, c("green", "brown", "bare")]
  #print(change)
  
  change$visit_start_date_a <- visit.data$visit_start_date[2]
  change$visit_start_date_b <- visit.data$visit_start_date[1]
  change$site_location_name <- name
  
  #print(change)
  site.fc.change.df <- rbind(site.fc.change.df, change)
}
site.fc.change.df <- site.fc.change.df[-1,]


# Algorithm:
# 1. Get the datasets: queryfile, Dea.fc files, ausplots fc
# 2. Check which of the sites have repeated visits (2 visits for now)
# 3. (In DEA FC) get data-points that are month before and after from the site visits (from Ausplots FC)
# 4. (IN DEA FC) take the average of each site visit

dea.fc.means.df <- data.frame(site_location_name = NA,
                              time = as.Date(NA),
                              pv = as.numeric(NA),
                              npv = as.numeric(NA),
                              bs = as.numeric(NA),
                              spatial_ref = NA)

missing.data <- c()
browser()
for (RI in 1:nrow(site.info.df.revisit)) {

  
  site.location <- site.fc.change.df$site_location_name[RI]
  print(site.location)
  site.path <- file.path(directory,paste0(site.location,".csv"))
  
  dea.data <- read.csv(site.path)
  
  if(nrow(dea.data) > 0){
    dea.data.agg <- aggregate(dea.data, by = list(dea.data$time),
                              FUN = mean, na.rm = T)
    dea.data.agg$Group.1 <- as.Date(dea.data.agg$Group.1)
    dea.data.agg <- dea.data.agg[,c("Group.1", "pv", "npv", "bs", "spatial_ref")]
    
    visit.times <- subset(site.info.df.revisit,
                          subset = (site_location_name == site.location))
    
    means <- data.frame(Group.1 = as.Date(NA), bs = NA, npv = NA, pv = NA, ue = NA)
    
    for(i in 1:nrow(visit.times)){ 
      date <- visit.times$visit_start_date[i]
      print(date)
      times.forwards <- seq(date, by='1 days', length = 31)
      times.backwards <- seq(date, by='-1 days', length = 31)
      closest.times <- rbind(dea.data.agg[dea.data.agg$Group.1 %in%times.forwards,],
                             dea.data.agg[dea.data.agg$Group.1 %in%times.backwards,])
      print(closest.times)
      means <- rbind(means,data.frame(Group.1 = date, lapply(closest.times[,c("bs","npv","pv")],
                                                             FUN = mean, na.rm = T)))
      print(means)
    }
    print(means)
    means <- means[-1,]
    means$site_location_name <- rep(site.location, nrow(means))
    print(means)
    colnames(means)[which(colnames(means)) == "Group.1"] = "time"
    print(means)
    #dea.fc.means.df <- rbind(dea.fc.means.df, means)
  } else{
    missing.data <- missing.data(site.location)
  }
} 


RI = which(sites.revisit.df$site.names == 'NTAFIN0001') # A good triple one

which(fileNames == 'NTAFIN0001')

site.location <- sites.revisit.df$site.names[RI]
site.path <- paste(directory,paste0(site.location,".csv"), sep = "/")

dea.data <- read.csv(site.path)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")

site.info.data <- veg.info$site.info
site.info.index <- which(site.info.data$site_location_name == site.location)
site.info.data <- site.info.data[site.info.index,]

insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")


ausplots.fc <- insitu.fractional.cover[grep(site.location, insitu.fractional.cover$site_unique),]
site.info.data <- merge(ausplots.fc, site.info.data, by = 'site_unique')



# Visual validation
# plot_site_markings <- function(easting.site, northing.site, dea.fc.i) {
#   
#   MASS::eqscplot(dea.fc.i$x, dea.fc.i$y,tol = .5, xlab = "easting", ylab = "northing")
#   
#   points(x = easting.site,y = northing.site, pch = 2, col = 'red')
#   points(x = easting.site+100,y = northing.site, pch = 2, col= 'red')
#   points(x = easting.site,y = northing.site+100, pch = 2, col= 'red')
#   points(x = easting.site+100,y = northing.site+100, pch = 2, col= 'red')
#   
# }
# 
# plot_site_markings(veg.info$site.info$pit_marker_easting[site.info.index][2],
#                    veg.info$site.info$pit_marker_northing[site.info.index][2],
#                    dea.data)
# 
# dea.data <- trim_to_nearest_coord(site.info.index, veg.info, dea.data, sites.query)
# 
# plot_site_markings(veg.info$site.info$pit_marker_easting[site.info.index][2],
#                    veg.info$site.info$pit_marker_northing[site.info.index][2],
#                    dea.data)

###### Visualise The Data ######


dea.data.agg <- aggregate(dea.data, by = list(dea.data$time),
                          FUN = mean, na.rm = T)

posit.date <- as.POSIXlt(dea.data.agg$Group.1) # for use for a later section: smoothing time series 
dea.data.agg$Group.1 <- as.Date(dea.data.agg$Group.1)

## Get essential data from Ausplots 
site.info.data.essen <- site.info.data[,c("visit_start_date", "bare",
                                          "brown", "green" )]

site.info.data.essen$visit_start_date <- as.Date(site.info.data.essen$visit_start_date)


colnames(site.info.data.essen) <- colnames(
  dea.data.agg[,c("Group.1","bs","npv","pv")]
)

site.info.data.essen$Group.1 <- as.Date(site.info.data.essen$Group.1)
dea.data.agg.essen <- dea.data.agg[,c("Group.1","bs","npv","pv", "ue")]


means <- data.frame(Group.1 = as.Date(NA), bs = NA, npv = NA, pv = NA, ue = NA)
for(i in 1:length(site.info.data.essen$Group.1)){ 
  date <- site.info.data.essen$Group.1[i]
  times.forwards <- seq(date, by='1 days', length = 31)
  times.backwards <- seq(date, by='-1 days', length = 31)
  closest.times <- rbind(dea.data.agg.essen[dea.data.agg.essen$Group.1 %in%times.forwards,],
                         dea.data.agg.essen[dea.data.agg.essen$Group.1 %in%times.backwards,])
  means <- rbind(means,data.frame(Group.1 = date, lapply(closest.times[,c("bs","npv","pv","ue")], FUN = mean, na.rm = T)))
}
means <- means[-1,]

#time.sequence <- data.frame(Group.1 =  seq(from = min(dea.data.agg.essen$Group.1), to = max(dea.data.agg.essen$Group.1), by='8 days'))
#merged.data <- dea.data.agg.essen %>% full_join(time.sequence)
#merged.data$Group.1 <- as.Date(merged.data$Group.1)
#merged.data <- merged.data %>% arrange(Group.1)

## Add dates of obs to the dea datasets to visually see the na, if there is one
for(i in 1:length(means$Group.1) ) {
  date <- means$Group.1[i]
  if (sum(dea.data.agg.essen$Group.1 %in% date) == 0){
    dea.data.agg.essen <- rbind(dea.data.agg.essen, data.frame(Group.1 = date, bs = NA, npv = NA, pv = NA, ue = NA))
  }
  
}

