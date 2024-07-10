###### Deriving UE from DEA FC ######
# Author: Krish Singh 
# Date: 08-07-2024
# Objective: To derive UE for filtering across all sites
#   - to analyse how much data is being lost per site 
#   - to analyse gains/drops in standard deviation in comparison
#       to the baseline standard deviation 
#   - What is the 'baseline standard deviation?'
#   - I can derive the ue based on the major vegetation type 
#   - Should the standard deviation be based only 1 fraction, or a multiple?


# Libaries ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(dplyr)


# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

cols <- c('site_location_name', 'ue_filter', 'sd_dev', 'mean', 'n_datapoints', 'n_outliers')
ue_filter_diag <- t(data.frame(row.names = cols))
ue_filter_interval <- seq(from = 10, to = 127, length.out = 10)

counter_current <- 1
counter_max <- length(file.names)

cols <- c('site_location_name', 'ue_min', 'ue_mean', 'ue_max', 'ue_med')
ue_filter_min_max <- t(data.frame(row.names = cols))

for (query in file.names) {
  # Get the progress bar
  print(paste0(
    'START: ', counter_current, '/',counter_max, ' {', query  ,'}'
  ))
  site.fc <- fread(paste0(directory, "/", query, ".csv")) # use data.table for faster processing'
  ue_filter_min_max <- rbind(ue_filter_min_max, c(site_location_name = query,
                                                  ue_min = min(as.numeric(site.fc$ue), na.rm = TRUE),
                                                  ue_mean = mean(as.numeric(site.fc$ue), na.rm = TRUE),
                                                  ue_max = max(as.numeric(site.fc$ue), na.rm = TRUE),
                                                  ue_med = median(as.numeric(site.fc$ue), na.rm = TRUE)))
  
  for(u in ue_filter_interval) {
    sub_fc <- subset(site.fc, ue <= u)
    sub_fc <- sub_fc[, list(pv = mean(pv, na.rm = TRUE)), by = list(time)]
    pv <- sub_fc$pv
    sd_pv <- sd(pv,na.rm = TRUE)
    mean_pv <- mean(pv, na.rm = TRUE)
    datapoints <- nrow(sub_fc)
    n_outliers <- length(boxplot.stats(pv)$out)
    row_fc <- c(site_location_name = query, 
                ue_filter = u,
                sd_dev = sd_pv,
                mean = mean_pv, 
                n_datapoints = datapoints,
                n_outliers = n_outliers)
    
    ue_filter_diag <- rbind(ue_filter_diag, row_fc)
    
  }
  #write.csv(ue_filter_diag, 'ue_filter_diagonistics.csv')
  counter_current <- counter_current + 1
}

# Plot The Results:

ue_filter_diag_df <- as.data.frame(ue_filter_diag)
ue_filter_diag_df$ue_filter <- as.numeric(ue_filter_diag_df$ue_filter)
ue_filter_diag_df$sd_dev <- as.numeric(ue_filter_diag_df$sd_dev)
ue_filter_diag_df$mean <- as.numeric(ue_filter_diag_df$mean)
ue_filter_diag_df$n_datapoints <- as.numeric(ue_filter_diag_df$n_datapoints)
ue_filter_diag_df$n_outliers <-as.numeric(ue_filter_diag_df$n_outliers)


ue_filter_min_max <- as.data.frame(ue_filter_min_max)

for (columns_to_numeric in cols[-1]) {
  ue_filter_min_max[columns_to_numeric] <- as.numeric(ue_filter_min_max[[columns_to_numeric]])
}

ggplot(data = ue_filter_diag_df) +
  geom_line(mapping = aes(x = ue_filter, y = sd_dev, group = site_location_name))

dom_species_group <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv')
dom_species_group <- dom_species_group[,c('site_location_name', 'vegetation_type')]

ue_filter_diag_df_veg <- ue_filter_diag_df %>%
  left_join(dom_species_group)

ggplot(data = ue_filter_diag_df_veg) +
  geom_point(mapping = aes(x = ue_filter, y = sd_dev, color = n_datapoints)) +
  geom_line(mapping = aes(x = ue_filter, y = sd_dev, color = n_datapoints, 
                          group = site_location_name)) +
  facet_wrap(~vegetation_type, ncol = 3)


site_info <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv')
site_info <- site_info[,c('site.info.site_location_name', 'site.info.state')]
site_info <- unique(site_info)
colnames(site_info) <- c('site_location_name', 'state')

ue_filter_diag_df_veg <- ue_filter_diag_df_veg %>%
  left_join(site_info)

ggplot(data = ue_filter_diag_df_veg) +
  geom_point(mapping = aes(x = ue_filter, y = sd_dev, color = n_datapoints)) +
  geom_line(mapping = aes(x = ue_filter, y = sd_dev, color = n_datapoints, 
                          group = site_location_name)) +
  facet_wrap(~state, ncol = 3)


ggplot(data = ue_filter_diag_df_veg) +
  geom_point(mapping = aes(x = ue_filter, y = n_datapoints, color = sd_dev)) +
  geom_line(mapping = aes(x = ue_filter, y = n_datapoints, color = sd_dev, 
                          group = site_location_name)) +
  facet_wrap(~state, ncol = 3)

ggplot(data = ue_filter_diag_df_veg) +
  geom_point(mapping = aes(x = ue_filter, y = n_outliers, color = n_datapoints)) +
  geom_line(mapping = aes(x = ue_filter, y = n_outliers, color = n_datapoints, group = site_location_name)) +
  facet_wrap(~vegetation_type, ncol = 3)


ggplot(data = ue_filter_diag_df_veg) +
  geom_point(mapping = aes(x = ue_filter, y =mean, colour = state)) +
  facet_wrap(~vegetation_type, ncol = 3)
  


