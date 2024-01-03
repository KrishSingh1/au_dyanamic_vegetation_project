#### Visualising Time Series of RS Products ###
## Author: Krish Singh
## Date: 20231209
## Purpose: To check for consistency of the fractional_cover calculations 


# Algorithm ---------------------------------------------------------------

# Output: visual plots 
# - all paired combinations of (NDVI, FC, SFC)
# - combined time series plot of NDVI, FC, SFC, and in-situ FC
# Input:
# - RS data for RS at site 
# - In-situ data of FC of the recorded sites 
# Process:
# - Retrieve RS data
# - Retrieve in-situ fc
# - Plot the NDVI, FC, and SFC



# Libraries ---------------------------------------------------------------

library(TSstudio)
library(readr) # needed to resolve delimiter issue in sfc csv files 
library(ausplotsR)
library(dplyr)
library(zoo)
library(plotly)

# Main --------------------------------------------------------------------

site.subset.names <- read.csv('../STEP2_NDVI_EXTRACTION/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.subset.names <- unique(site.subset.names$site_location_name)

# Select main site  
focus.site.name <- site.subset.names[which(site.subset.names == 'NSANAN0002')]

# Load NDVI
NDVIs.path <- '../STEP2_NDVI_EXTRACTION/EarthEngine/Output/NDVI_Extraction_2/'
focus.site.file.name <- paste0(focus.site.name, '_NDVI', '.csv')
focus.site.ndvi <- read.csv(paste0(NDVIs.path,focus.site.file.name))

## Need to extract the dates from system index (left of the last '_' seperator)
focus.site.ndvi$date <- unlist(lapply(focus.site.ndvi$system.index,
                                      FUN = function(string){
                                        splitted.str <- strsplit(string, '_')
                                        return(splitted.str[[1]][[length(unlist(splitted.str)) - 1]])
                                      }
))


focus.site.ndvi$date <- as.Date(focus.site.ndvi$date, format = '%Y%m%d')
focus.site.ndvi <- focus.site.ndvi[order(focus.site.ndvi$date),]
rownames(focus.site.ndvi) <- 1:nrow(focus.site.ndvi)

# Load SFC
SFC.path <- '../STEP2_FC_EXTRACTION/VegMachine/SFC/'
focus.site.file.name <- paste0(focus.site.name, '_SFC', '.csv')
focus.site.sfc <- readr::read_csv(paste0(SFC.path,focus.site.file.name))

# To convert to a date, I needed to append a day ('01') while in actuality
# there are no specific dates 
focus.site.sfc$date <- as.Date(unlist(lapply(focus.site.sfc$date,
                                             function(date) 
                                             {return(paste0(date, '-01'))}
)))



focus.site.sfc <- as.data.frame(focus.site.sfc) # note parsing issue with the last column

# Load DEA FC

dea.path <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
focus.site.file.name <- paste0(focus.site.name, '.csv')
focus.site.dea <- read.csv(paste0(dea.path,focus.site.file.name))
focus.site.dea <- aggregate(focus.site.dea, by = list(focus.site.dea$time), FUN = mean, na.rm = T)
focus.site.dea$date <- as.Date(focus.site.dea$Group.1)

# Combine the datasets 
focus.site.ndvi <- focus.site.ndvi[,c('date', 'NDVI')]
focus.site.sfc <- focus.site.sfc[,c('date', 'green_mean')]
focus.site.dea <- focus.site.dea[,c('date', 'pv')]

test <- focus.site.ndvi %>% 
  full_join(focus.site.sfc, by = 'date') %>%
  full_join(focus.site.dea, by = 'date') 

test <- test[order(test$date),]
rownames(test) <- 1:nrow(test)

gapfill.green_mean <- na.locf(test[,c("date", "green_mean")])
test <- test[, c("date", "NDVI", "pv")]

test <- test %>%
  full_join(gapfill.green_mean, by = 'date')

test$NDVI <- test$NDVI*100

# Append the in-situ dataset 

veg.info <- get_ausplots(focus.site.name, veg.PI = T, site_info = T)
insitu.fc <- fractional_cover(veg.info$veg.PI)
veg.info <- merge(as.data.frame(veg.info$site.info), insitu.fc, by = "site_unique")
veg.info.fc <- veg.info[,c("visit_start_date", "green")]
colnames(veg.info.fc)[1] <- 'date'
veg.info.fc$date <- as.Date(veg.info.fc$date)

test <- test %>% full_join(veg.info.fc, by = 'date')
test <- test[order(test$date),]
rownames(test) <- 1:nrow(test)

test.2 <- aggregate(test[,c("NDVI", "pv", "green_mean", "green")],
                    by = list(test$date), 
                    FUN = mean, na.rm = T)
test.2 <- test.2[order(test.2$Group.1),]
rownames(test.2) <- 1:nrow(test.2)
colnames(test.2)[1] <- 'date'

# Visualisation 

## Using ggplotly
pl <- ggplot(test.2, aes(x = date)) +
  geom_line(mapping = aes(y = NDVI, colour = "Landsat_NDVI")) +
  geom_line(mapping = aes(y = pv, colour = "DEA_FC")) +
  geom_line(mapping = aes(y = green_mean, colour = "VegMachine_SFC")) +
  xlab("Time") +
  ylab("Fractional Cover (%)") +
  labs(focus.site.name) +
  geom_point(mapping = aes(y = green, colour = "insitu_green")) +
  scale_color_manual(
    name = "Cover Types", 
    values = c("Landsat_NDVI" = "red", "DEA_FC" = "blue", "VegMachine_SFC" = "green",
               "insitu_green" = "darkgreen")) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b",
               date_minor_breaks = "3 month") +
  theme(axis.text.x=element_text(angle=60, hjust = 1)) +
  geom_hline(yintercept = 0)


p <- ggplotly(pl) %>% 
  rangeslider()
p


# Write Visualisation Data ------------------------------------------------

write.csv(test.2, paste0('combined_data_', focus.site.name ,'.csv'),row.names = F)


# Other:

ts_plot(test.2, slider = T)
ts_plot(na.locf(test.2), slider = T)








I <- ausplotsR::get_ausplots("NSANSS0001", site_info = T)








