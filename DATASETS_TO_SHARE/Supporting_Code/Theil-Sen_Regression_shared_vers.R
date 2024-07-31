##### Theil-sen Regression ####
# Date: 24-07-24
# Author: Krish Singh
# Objective: determine a linear trend for each site FC using Theil-Sen Regression


# Libraries ---------------------------------------------------------------

library('RobustLinearReg')
library('ggplot2')
library('data.table')
library('tidyverse')
library('tidyr')

# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

dea_fc_all <- read.csv('../DATASETS_TO_SHARE/DEA_FC_Smoothed_1987_2022/DEA_FC_Combined_1987_2022.csv') %>%
  mutate(time = as.Date(time))
file.names <- unique(dea_fc_all$site_location_name)

# Create temp df 
temp <- data.frame('site_location_name' = NA)
fractions <- c('pv_filter', 'npv_filter', 'bs_filter')
fractions_col_names <- c()
for (f in fractions) {
  fractions_col_names <- c(fractions_col_names, 
                           paste0(f, '_', 'slope'), 
                           paste0(f, '_', 'intercept'))
} # create the columns based on fraction, mainly the slope and intercept names 
temp[fractions_col_names] <- NA


counter <- 1
for (f in file.names) { # Iterate through each site 
  
  # Keep track of the progress 
  print(paste('Reading ', f, ' {',counter,'/',length(file.names),'}'))
  
  # Subset so its site-specific 
  dea_fc <- dea_fc_all %>%
    subset(site_location_name == f) %>%
    arrange(time)
    
  # This vector contains the slope/intercept params for each fraction
  fraction_slopes <- c()
  for (fraction in fractions) { # calc slope/intercept via theil-sen reg for each fraction
    
    formula <- as.formula(paste0(fraction, ' ~ ', 'time'))
    t_s <- theil_sen_regression(formula, data = dea_fc)
    fraction_slopes <- c(fraction_slopes, 
                         t_s$coefficients[2], # slope 
                         t_s$coefficients[1]) # intercept 
    
  }
  site_location_name <- f # get site_name 
  names(fraction_slopes) <- fractions_col_names # name the slope/intercept
  temp <- rbind(temp, c('site_location_name' = site_location_name, fraction_slopes)) # bind the slope/intercept info into the temp
  counter <- counter + 1
}

temp <- na.omit(temp)
rownames(temp) <- 1:nrow(temp)
temp <- temp %>% 
  mutate(pv_filter_slope_yr = as.numeric(pv_filter_slope) * 365,
         npv_filter_slope_yr = as.numeric(npv_filter_slope) * 365,
         bs_filter_slope_yr = as.numeric(bs_filter_slope) * 365)

# This gives the change in fraction per yr 
# Note we also could do this: (predict(t_s, data.frame(time = as.Date('2022-01-01'))) - predict(t_s, data.frame(time = as.Date('2021-01-01'))))
# slightly different results, but negligible

write.csv(temp,'../DATASETS_TO_SHARE/DEA_FC_Linear_Trends_File/AusPlots_Theil_Sen_Regression_Stats.csv', row.names=FALSE)

# Examine the data  -------------------------------------------------------


theil_sen_reg <- read.csv('../DATASETS_TO_SHARE/DEA_FC_Linear_Trends_File/AusPlots_Theil_Sen_Regression_Stats.csv')

# plot the highest three slopes 
n = 3
theil_sen_reg_copy <- theil_sen_reg
for(i in 1:n){
  
  max_index <- which.max(as.numeric(theil_sen_reg_copy$pv_filter_slope))[1]
  max_slope <- theil_sen_reg_copy[max_index,]
  
  dea_fc <- dea_fc_all %>%
    subset(site_location_name == max_slope$site_location_name) %>%
    arrange(time) %>%
    select(time, pv_filter, npv_filter, bs_filter)
  
  breaks = 5
  title_size = 10
  pv_plot <- ggplot(data = dea_fc) + geom_line(mapping = aes(x = time, y = pv_filter)) +
    geom_abline(intercept = max_slope$pv_filter_intercept[1], slope = max_slope$pv_filter_slope[1], color = 'red') +
    scale_y_continuous(limits = c(0, 100), breaks = scales::pretty_breaks(n = breaks)) + 
    ggtitle(paste0(max_slope$site_location_name, ' slope = ', round(max_slope$pv_filter_slope_yr[1],7), ' pv/yr')) +
    theme(plot.title = element_text(size=title_size))
  
  npv_plot <- ggplot(data = dea_fc) + geom_line(mapping = aes(x = time, y = npv_filter)) +
    geom_abline(intercept = max_slope$npv_filter_intercept[1], slope = max_slope$npv_filter_slope[1], color = 'red') +
    scale_y_continuous(limits = c(0, 100),breaks = scales::pretty_breaks(n = breaks)) + 
    ggtitle(paste0(max_slope$site_location_name, ' slope = ', round(max_slope$npv_filter_slope_yr[1],7), ' npv/yr')) +
    theme(plot.title = element_text(size=title_size))
  
  bs_plot <- ggplot(data = dea_fc) + geom_line(mapping = aes(x = time, y = bs_filter)) +
    geom_abline(intercept = max_slope$bs_filter_intercept[1], slope = max_slope$bs_filter_slope[1], color = 'red') +
    scale_y_continuous(limits = c(0, 100),breaks = scales::pretty_breaks(n = breaks)) + 
    ggtitle(paste0(max_slope$site_location_name, ' slope = ', round(max_slope$bs_filter_slope_yr[1],7), ' bs/yr')) +
    theme(plot.title = element_text(size=title_size))
  
  
  plot(cowplot::plot_grid(pv_plot, npv_plot, bs_plot,nrow = 3))
  
  theil_sen_reg_copy <- theil_sen_reg_copy[-max_index,]
}



