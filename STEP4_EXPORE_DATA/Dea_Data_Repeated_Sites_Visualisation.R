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

directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")

insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")

site.names <- unique(veg.info$site.info$site_location_name)
site.observations <- unique(veg.info$site.info$site_unique)
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")

# Count the number of observations of each site


df <- as.data.frame(t(data.frame(purrr::flatten(lapply(site.observations, strsplit, '-')))))


counts <- c()
for (n in site.names){
  counts <- c(counts, sum(df$V1 == n))
}
counts.df <- data.frame(site.names, counts)

sites.revisit.df <- subset(counts.df, counts > 1) # For all revisited data
#sites.revisit.2.df <- subset(counts.df, counts == 2) # For sites that were only visited twice 


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


###### Load Dataset ######

# record index of the sites with multiple visits
#RI = 81# SATFLB0006 (2)
#RI = 82 # NTAMAC0002
#RI = 83 # WAAGES0003
#RI = 35 # NTABRT0002 (2)
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



plot_site_markings <- function(easting.site, northing.site, dea.fc.i) {
  
  MASS::eqscplot(dea.fc.i$x, dea.fc.i$y,tol = .5, xlab = "easting", ylab = "northing")
  
  points(x = easting.site,y = northing.site, pch = 2, col = 'red')
  points(x = easting.site+100,y = northing.site, pch = 2, col= 'red')
  points(x = easting.site,y = northing.site+100, pch = 2, col= 'red')
  points(x = easting.site+100,y = northing.site+100, pch = 2, col= 'red')
  
}

plot_site_markings(veg.info$site.info$pit_marker_easting[site.info.index][2],
                   veg.info$site.info$pit_marker_northing[site.info.index][2],
                   dea.data)

dea.data <- trim_to_nearest_coord(site.info.index, veg.info, dea.data, sites.query)
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






## Using ggplotly
pl <- ggplot(dea.data.agg.essen, aes(x = Group.1)) +
  geom_line(mapping = aes(y = bs, colour = "bs")) +
  geom_line(mapping = aes(y = npv, colour = "npv")) +
  geom_line(mapping = aes(y = pv, colour = "pv")) +
  geom_line(mapping = aes(y = ue, colour = "ue")) +
  geom_line(data = means, aes(x = Group.1, y = bs, colour = "bs.mean"))+
  geom_line(data = means, aes(x = Group.1, y = npv, colour = "npv.mean"))+
  geom_line(data = means, aes(x = Group.1, y = pv, colour = "pv.mean"))+
  xlab("Time") +
  ylab("Colour Intensity") +
  labs(title = site.location) +
  geom_point(data = site.info.data.essen,
             mapping = aes(x = Group.1, y = bs, colour = "bs.obs")) +
  geom_point(data = site.info.data.essen,
             mapping = aes(x = Group.1, y = npv, colour = "npv.obs")) +
  geom_point(data = site.info.data.essen,
             mapping = aes(x = Group.1, y = pv, colour = "pv.obs")) +
  scale_color_manual(
    name = "Colour Bands", 
    values = c("bs" = "red", "npv" = "blue", "pv" = "green", "ue" = "yellow",
               "bs.obs" = "darkred", "npv.obs" = "darkblue", 
               "pv.obs" = "darkgreen", "bs.mean" = "darkred", 
               "npv.mean" = "darkblue", "pv.mean" = "darkgreen")) +
  theme_minimal() +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y %b",
               date_minor_breaks = "2 month") +
  theme(axis.text.x=element_text(angle=60, hjust = 1)) +
  geom_hline(yintercept = 0)


p <- ggplotly(pl) %>% 
  rangeslider()
p


ggplot()+ geom_line(data = means, aes(x = Group.1, y = bs, colour = "bs.mean"))
ggplot()+ geom_line(data = means, aes(x = Group.1, y = pv, colour = "pv.mean"))
ggplot()+ geom_line(data = means, aes(x = Group.1, y = npv, colour = "npv.mean"))

