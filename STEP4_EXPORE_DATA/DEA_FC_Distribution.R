####### DEA Evaluation Nearest Point  #######
# Author: Krish Singh
# Date: 2024-07-11
# Objective: To obtain the quantile of the PV, NPV, BS which the AusPlots FC survery
#   was taken 


# Libraries ---------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)


# Functions ---------------------------------------------------------------

plot_distribution <- function(site, evaluation_set) {
  
  dea_fc <- fread(paste0(directory, 'Input_DataSet_', site, '.csv')) %>%
    mutate(time =  as.Date(time))
  
  evaluation_set <- evaluation_set %>% 
    subset(site_location_name == site)
  
  ecdf_green <- ggplot(data = dea_fc) +
    stat_ecdf(mapping = aes(x = pv_filter), color = 'darkgreen') +
    geom_point(data = evaluation_set,
               mapping = aes(x = pv_filter, y = pv_filter_percentile, color = time),
               size = 2) + labs(title = paste0(site)) 
  
  ecdf_brown <- ggplot(data = dea_fc) + 
    stat_ecdf(mapping = aes(x = npv_filter), color = 'blue') +
    geom_point(data = evaluation_set,
             mapping = aes(x = npv_filter, y = npv_filter_percentile, color = time),
             size = 2) +labs(title = paste0(' ')) 
  
  ecdf_bare <-  ggplot(data = dea_fc) +
    stat_ecdf(mapping = aes(x = bs_filter), color = 'red') +
    geom_point(data = evaluation_set,
               mapping = aes(x = bs_filter, y = bs_filter_percentile, color = time),
                           size = 2) +
    labs(title = paste0(' ')) 
  
  plot(cowplot::plot_grid(ecdf_green, ecdf_brown, ecdf_bare))
  
}

# Main --------------------------------------------------------------------


# Read and Process Datasets 

# We Perform the time point matching for each site with avaliable DEA FC
# To do that, we check our directory for downloaded DEA FC
directory <- "../DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
fileNames <- unlist(lapply(strsplit(fileNames, '_'), FUN = function(x){
  unlist(x)[3]
}))

evaluation_df <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv')

# Obtain the csv file of the quantile, and add it to the evaluation set 
evaluation_df <- evaluation_df %>%
  mutate(pv_filter_percentile = NA,
         npv_filter_percentile = NA,
         bs_filter_percentile = NA)
  

for (site in fileNames) {
  
  dea_fc <- read.csv(paste0(directory, 'Input_DataSet_', site, '.csv')) %>%
    mutate(time =  as.Date(time))
  
  site_index <- which(evaluation_df$site_location_name == site)
  
  for(i in site_index) {
    
    evaluation_set <- evaluation_df[i,] %>%
      mutate(pv_filter_percentile = ecdf(dea_fc$pv_filter)(pv_filter),
             npv_filter_percentile = ecdf(dea_fc$npv_filter)(npv_filter),
             bs_filter_percentile = ecdf(dea_fc$bs_filter)(bs_filter))
    
    evaluation_df[i, c('pv_filter_percentile', 
                       'npv_filter_percentile',
                       'bs_filter_percentile')] <- evaluation_set[1, c('pv_filter_percentile', 
                                                                      'npv_filter_percentile',
                                                                      'bs_filter_percentile')]
    
  }
}


write.csv(evaluation_df,'../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_with_percentiles.csv')
write.csv(evaluation_df,'../DATASETS_TO_SHARE/DEA_FC_Ground_Truth_Evaluation_File/DEA_FC_Ground_Truth_Evaluation_with_percentiles.csv')


plot_distribution(fileNames[90],evaluation_df)
