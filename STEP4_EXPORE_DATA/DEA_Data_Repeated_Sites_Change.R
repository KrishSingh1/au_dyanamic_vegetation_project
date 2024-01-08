###### DEA_Data_Repeated_Sites_Visualisation ######
# By Krish Singh
# 20230929
# To see the change in fractional cover from site visit to another with DEA data. 


# Libraries ---------------------------------------------------------------

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
library(ggpubr)
library(ggpmisc)
library(Matrix)


# Functions ---------------------------------------------------------------


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


# Main --------------------------------------------------------------------

# Get DEA file names 
directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
sites.query <- read.csv("../DATASETS/sites_info_query.csv")

# Load AusPlots data 

veg.info <- readRDS("../DATASETS/site_veg_2-0-6.rds")
site.names <- unique(veg.info$site.info$site_location_name)

insitu.fractional.cover <- read.csv('../DATASETS/AusPlots_FC_Iter_2_0_6.csv')
insitu.fractional.cover$other[is.na(insitu.fractional.cover$other)] <- 0 # set NA to 0 for 'other'
insitu.fractional.cover <- subset(insitu.fractional.cover, (other <= 10)) # remove observations with 'other' at 10% or less
insitu.fractional.cover[insitu.fractional.cover$site_unique == 'TCATCH0010-58826',]$bare = 0

# Count the number of observations of each site
counts.df <- as.data.frame(table(veg.info$site.info$site_location_name))
round(table(counts.df$Freq)/sum(counts.df$Freq) * 100,1)
sites.revisit.2.df <- subset(counts.df, Freq >= 2) # For sites that were visited two or more 

# Merge sites with counts == 2 with current veg information
site.info.df <- as.data.frame(veg.info$site.info)
site.info.df <- merge(site.info.df, insitu.fractional.cover, by = "site_unique")

# Remove in-situ fractional cover of sites with an NA. of above 10%.
site.info.df <- subset(site.info.df, subset = (other <= 10))

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
vers <- packageVersion('ausplotsR')
write.csv(site.fc.change.df, paste0("../DATASETS/AusPlots_FC_Change_", vers, ".csv"))

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

use.saved.data <- F
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
    dea.data <- subset(dea.data, subset = (ue < 27))
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
  write.csv(dea.fc.means.df, file =  "../DATASETS/DEA_fc_means.csv") 
} else {
  dea.fc.means.df <- read.csv("../DATASETS/DEA_fc_means.csv")
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
write.csv(dea.fc.change.df, file =  "../DATASETS/DEA_fc_change.csv") 


# Junk Script (Don't Run) -------------------------------------------------

dea.fc.change.df <- read.csv("../DATASETS/DEA_fc_change.csv", row.names =  1)
site.fc.change.df <- read.csv("../DATASETS/AusPlots_FC_Change_2.0.6.csv", row.names = 1)

## Testing why obs of site.fc.change.df is different to dea.fc.change.df

setdiff(site.fc.change.df$site_location_name, 
        dea.fc.change.df$site_location_name)
# [1] "SAAKAN0011" "SATFLB0007" "SATFLB0012" "SATFLB0014"

setdiff(dea.fc.change.df$site_location_name, 
        site.fc.change.df$site_location_name)
#  character(0)

t <- get_ausplots(c("SAAKAN0011", "SATFLB0007", "SATFLB0012", "SATFLB0014"),
                  veg.PI =T)
fractional_cover(t$veg.PI) # looks fine

#                       site_unique bare brown green other
# SAAKAN0011-58628 SAAKAN0011-58628  1.0  11.2  87.0   0.8
# SAAKAN0011-58879 SAAKAN0011-58879  2.4  21.7  75.9   0.0
# SATFLB0007-53709 SATFLB0007-53709 36.6  47.9  15.4   0.0
# SATFLB0007-58657 SATFLB0007-58657 30.5  30.7  38.8   0.0
# SATFLB0012-53699 SATFLB0012-53699  9.2  34.0  56.8   0.0
# SATFLB0012-58677 SATFLB0012-58677 13.3  21.6  65.1   0.0
# SATFLB0014-53702 SATFLB0014-53702 16.6  31.0  52.4   0.0
# SATFLB0014-58667 SATFLB0014-58667 13.2  26.5  60.1   0.2

dea.SAAKAN0011 <- read.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/')
# File is empty

# SATFLB0007
dea.fc.means <- read.csv("../DATASETS/DEA_fc_means.csv")
dea.fc.means[dea.fc.means$site_location_name == 'SATFLB0007',]

 site.location.names %in% setdiff(site.fc.change.df$site_location_name,
        dea.fc.change.df$site_location_name)

# FALSE FALSE FALSE FALSE

 sites.revisit.2.df[sites.revisit.2.df$Var1 %in% setdiff(site.fc.change.df$site_location_name,
                                     dea.fc.change.df$site_location_name),]
 #           Var1 Freq
 # 411 SAAKAN0011    2
 # 482 SATFLB0007    2
 # 487 SATFLB0012    2
 # 489 SATFLB0014    2
 


insitu.fractional.cover[insitu.fractional.cover$site_unique == 'SAAKAN0011-58879',]
insitu.fractional.cover[insitu.fractional.cover$site_unique == 'SAAKAN0011-58628',]

site.path <- file.path(directory,paste0("SATFLB0007",".csv"))
dea.data <- read.csv(site.path)
mean(is.na(dea.data$ue))

site.path <- file.path(directory,paste0("SATFLB0012",".csv"))
dea.data <- read.csv(site.path)
mean(is.na(dea.data$ue))

site.path <- file.path(directory,paste0("SATFLB0014",".csv"))
dea.data <- read.csv(site.path)
mean(is.na(dea.data$ue))

## What went wrong:
# the DEA data for these sites were not retrieved properly, or the ue were not retrieved 