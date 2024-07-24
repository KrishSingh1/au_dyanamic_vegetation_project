#### Combine Historical Burn with Modis
# Author Krish SIngh
# D: 05-07-2024
# Objective: Combine historical burn with modis such that they can be used
#   in model training collectively


# Libraries ---------------------------------------------------------------

library(dplyr)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

historical <- read.csv('../DATASETS/AusPlotsBurnData/Historical_Bush_Fires/AusPlots_Historical_BurnDates.csv')
historical$Data_Source <- rep('Historical_BushFires_Boundaries', nrow(historical))
historical$ignition_d <- as.Date(historical$ignition_d,)

modis <- read.csv('../DATASETS/AusPlotsBurnData/MODIS_Burn/AusPlots_BurnDates_2024_Corrected_ColNames.csv')
colnames(modis)[3] <- c('ignition_d')
modis$Data_Source <- rep('MODIS/061/MCD64A1', nrow(modis))
modis$ignition_d <-  as.Date(modis$ignition_d, format = '%Y-%m-%d')


common <- c('st_lct_', 'ignition_d')
AusPlots_burn <- rbind(historical[,common], modis[, common])
AusPlots_burn <- unique(AusPlots_burn)

AusPlots_burn <- AusPlots_burn %>% 
  left_join(historical, by = common) %>% 
  left_join(modis, by = common)

colnames(AusPlots_burn)[1] <- 'site_location_name'
AusPlots_burn <- AusPlots_burn[, -which(colnames(AusPlots_burn) == 'X')]

write.csv(AusPlots_burn, '../DATASETS/AusPlotsBurnData/Combined_Data/AusPlots_Combined_Fire_Dataset.csv')
write.csv(AusPlots_burn, '../DATASETS_TO_SHARE/AusPlots_Combined_Fire_Dataset.csv')


