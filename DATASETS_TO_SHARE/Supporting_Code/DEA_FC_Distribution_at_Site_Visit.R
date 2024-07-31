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

plot_distribution <- function(site, evaluation_set, dea_fc_all) {
  
  dea_fc <- dea_fc_all %>%
    subset(site_location_name == site) %>%
    arrange(time)
  #print(dea_fc)
  
  evaluation_set <- evaluation_set %>% 
    subset(site_location_name == site) %>%
    mutate(time = as.factor(time))
  
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


dea_fc_all <- read.csv('../DEA_FC_Smoothed_1987_2022/DEA_FC_Combined_1987_2022.csv') %>%
  mutate(time = as.Date(time))

evaluation_df <- read.csv('../DEA_FC_Ground_Truth_Evaluation_File/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  mutate(pv_filter_percentile = NA,
         npv_filter_percentile = NA,
         bs_filter_percentile = NA)

fileNames <- unique(evaluation_df$site_location_name)

# Obtain the csv file of the quantile, and add it to the evaluation set 

counter <- 1
for (site in fileNames) {
  
  # Keep track of the progress 
  print(paste('Reading ', site, ' {',counter,'/',length(fileNames),'}'))
  
  # So its site-specific 
  dea_fc <- dea_fc_all %>%
    subset(site_location_name == site) %>%
    arrange(time)
  
  # get the index numbers of the site under the site evaluation 
  site_index <- which(evaluation_df$site_location_name == site) 
  
  # Calculate the percentile from the ecdf distribution and
  #   update the percentile column from the evaluation df 
  evaluation_df[site_index,] <- evaluation_df[site_index,] %>%
    mutate(pv_filter_percentile = ecdf(dea_fc$pv_filter)(pv_filter),
           npv_filter_percentile = ecdf(dea_fc$npv_filter)(npv_filter),
           bs_filter_percentile = ecdf(dea_fc$bs_filter)(bs_filter))

  counter <- counter + 1
}

write.csv(evaluation_df,'../DEA_FC_Ground_Truth_Evaluation_File/DEA_FC_Ground_Truth_Evaluation_with_percentiles.csv')

# Plot a site with more than one visit; WAAVIB0003
plot_distribution('WAAVIB0003', evaluation_df, dea_fc_all)
