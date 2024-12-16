######## Supplementary For Chapter 2 #############
# Date: 31-10-24
# Author: Krish K Singh 


# Libraries ---------------------------------------------------------------



library(plotly)
library(ggplot2)
library(dplyr)
library(caret)
library(cowplot)
library(data.table)
library(tune)
library(ggpubr)
library(ggpmisc)
library(Matrix)
library(Metrics)
library(grid)
library(tidyr)
library(lubridate)

# Functions ---------------------------------------------------------------
plot_distribution <- function(site, evaluation_set, 
                              directory = "../DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/") {
  
  dea_fc <- fread(paste0(directory, 'Input_DataSet_', site, '.csv')) %>%
    mutate(time =  as.factor(time))
  
  evaluation_set <- evaluation_set %>% 
    subset(site_location_name == site)
  
  ecdf_green <- ggplot(data = dea_fc) +
    stat_ecdf(mapping = aes(x = pv_filter), color = 'darkgreen') +
    geom_point(data = evaluation_set,
               mapping = aes(x = pv_filter, y = pv_filter_percentile, color = time),
               size = 2) + labs(title = paste0(site))  + theme_bw() +
    ylab('PV Percentile')
  
  ecdf_brown <- ggplot(data = dea_fc) + 
    stat_ecdf(mapping = aes(x = npv_filter), color = 'blue') +
    geom_point(data = evaluation_set,
               mapping = aes(x = npv_filter, y = npv_filter_percentile, color = time),
               size = 2) +labs(title = paste0(' '))  + theme_bw() +
    ylab('NPV Percentile')
  
  ecdf_bare <-  ggplot(data = dea_fc) +
    stat_ecdf(mapping = aes(x = bs_filter), color = 'red') +
    geom_point(data = evaluation_set,
               mapping = aes(x = bs_filter, y = bs_filter_percentile, color = time),
               size = 2) +
    labs(title = paste0(' ')) + theme_bw() +
    ylab('BS Percentile')
  
  p1 <- ecdf_green + theme(legend.position = "none",
                      axis.text.x = element_text(angle = 45, hjust = 1))
  p2 <- ecdf_brown + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 45, hjust = 1))
  p3 <- ecdf_bare 
  
  combined_plot <- plot_grid(p1, p2, p3, ncol = 2, labels = c("a)", "b)", "c)"))
  combined_plot
  
}

# Main --------------------------------------------------------------------



# Set-up ------------------------------------------------------------------

# Read our FC dataset
example_site <- 'SAABHC0004'
dea_fc_path <- '../DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'
dea_fc_path <- paste0(dea_fc_path, 'Input_DataSet_', example_site, '.csv')
dea_fc <- read.csv(dea_fc_path)

# Read our evaluation dataset, which has the closest DEA FC timestamp to sites visited 
evaluation.data.2 <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_with_percentiles_.csv') %>%
  subset(site_location_name == example_site) 

plot_distribution(site = example_site, evaluation_set = evaluation.data.2)

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
theil_sen_reg_subset <- subset(theil_sen_reg, site_location_name == example_site)

# Plot time series per fractional cover 
for (fraction in c('pv', 'npv', 'bs')) {
  
  if (fraction == 'pv'){
    colour <- 'darkgreen'
    og_name <- 'green'
  } else if (fraction == 'npv') {
    colour <- 'steelblue'
    og_name <- 'brown'
  } else if (fraction == 'bs') {
    colour <- 'darkred'
    og_name <- 'bare'
  }
  

  t <- ggplot(data = dea_fc, aes(x = as.Date(time)))  +
    # the main time series 
    geom_line(aes(y = .data[[paste0(fraction, '_filter')]], ), size = 0.4, color = colour) + 
    scale_y_continuous(limits = c(0, 60), breaks = scales::pretty_breaks(n = 5)) +
    scale_x_date(date_labels = "%Y", minor_breaks = ('1 year'),
                date_breaks = ('5 years')) +
    xlab('time') +
    ylab(fraction) +
    # The on ground fractional cover measurements 
    geom_point(data = evaluation.data.2, aes(x =  as.Date(visit_start_date), 
                                             y = .data[[paste0(og_name)]]), 
               shape = 4, color = 'black', size = 1.6) +
    geom_smooth(data = evaluation.data.2,
                aes(x =  as.Date(visit_start_date), 
                    y = .data[[paste0(og_name)]]),
                method = 'lm',se =F,
                color = 'black') +
    # The associated timestamp of the Landsat imagery 
    geom_point(data = evaluation.data.2, aes(x =  as.Date(time), 
                                             y = .data[[paste0(fraction, '_filter')]]), 
               shape = 4, color = 'blue', size = 1.6) +
    geom_smooth(data = evaluation.data.2,
                aes(x =  as.Date(time), 
                    y = .data[[paste0(fraction, '_filter')]]),
                method = 'lm',se =F) +
    # Thiel-sen Regression
    geom_abline(intercept = theil_sen_reg_subset[[paste0(fraction, '_filter','_intercept')]], 
                slope = theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope')]], color = 'red') +
    ggtitle(paste0('slope = ', round(theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope_yr')]],7),
                   paste0(' ', fraction, '/yr'))) + theme_bw() 
  plot(t)
}




## Since for the mean time I just want to show PV:


og_model <- lm(formula = green~as.Date(time), data = evaluation.data.2)
rs_model <- lm(formula = pv_filter~as.Date(time), data = evaluation.data.2)

fraction <- 'pv'

if (fraction == 'pv'){
  colour <- 'darkgreen'
  og_name <- 'green'
} else if (fraction == 'npv') {
  colour <- 'steelblue'
  og_name <- 'brown'
} else if (fraction == 'bs') {
  colour <- 'darkred'
  og_name <- 'bare'
}

t <- ggplot(data = dea_fc, aes(x = as.Date(time)))  +
  # the main time series 
  geom_line(aes(y = .data[[paste0('pv', '_filter')]], ), size = 0.4, color = colour) + 
  scale_y_continuous(limits = c(0, 60), breaks = scales::pretty_breaks(n = 5)) +
  scale_x_date(date_labels = "%Y", minor_breaks = ('1 year'),
               date_breaks = ('5 years')) +
  xlab('Time') +
  ylab('PV (%)') +
  # The on ground fractional cover measurements 
  geom_point(data = evaluation.data.2, aes(x =  as.Date(visit_start_date), 
                                           y = .data[[paste0(og_name)]]), 
             shape = 4, color = 'black', size = 1.6) +
  geom_smooth(data = evaluation.data.2,
              aes(x =  as.Date(visit_start_date), 
                  y = .data[[paste0(og_name)]]),
              method = 'lm',se =F,
              color = 'black') +
  # The associated timestamp of the Landsat imagery 
  geom_point(data = evaluation.data.2, aes(x =  as.Date(time), 
                                           y = .data[[paste0(fraction, '_filter')]]), 
             shape = 4, color = 'blue', size = 1.6) +
  geom_smooth(data = evaluation.data.2,
              aes(x =  as.Date(time), 
                  y = .data[[paste0(fraction, '_filter')]]),
              method = 'lm',se =F) +
  # Thiel-sen Regression
  geom_abline(intercept = theil_sen_reg_subset[[paste0(fraction, '_filter','_intercept')]], 
              slope = theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope')]], color = 'red') +
  theme_bw() +
  annotate('text', x = as.Date('2020-01-01'), 
           y = 8, 
           label = paste0("Long-term Slope (DEA FC)", ' = ',round(theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope_yr')]],4), ' %PV/yr'), 
           color = "red", size = 2.5,
           hjust = 1 ) +
  annotate('text', x = as.Date('2020-01-01'), 
           y = 7, 
            label = paste0('\nShort-term (AusPlots FC)', ' = ', round(coef(og_model)[[2]] * 365,4), ' %PV/yr'),
           color = "black", size = 2.5,
           hjust = 1 ) +
  annotate('text', x = as.Date('2020-01-01'),
           y = 2, 
           label = paste0('Short-term (DEA FC)',' = ', round(coef(rs_model)[[2]] * 365,4), ' %PV/yr'),
           color = "blue", size = 2.5, hjust = 1)
  
  
plot(t)

rainfall <- ggplot(data = dea_fc, aes(x = as.Date(time))) +
  geom_line(mapping = aes(y = precip_90), color = 'steelblue') + theme_bw() +
  scale_x_date(date_labels = "%Y", minor_breaks = ('1 year'),
               date_breaks = ('5 years')) +
  ylab('PPT (31-90 days, mm)') +
  xlab('Time')
rainfall


dea_fc <- fread(paste0("../DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/", 'Input_DataSet_', example_site, '.csv')) %>%
  mutate(time =  as.factor(time))
evaluation_set <- evaluation.data.2 
ecdf_green <- ggplot(data = dea_fc) +
  stat_ecdf(mapping = aes(x = pv_filter), color = 'darkgreen') +
  geom_point(data = evaluation_set,
             mapping = aes(x = pv_filter, y = pv_filter_percentile, color = time),
             size = 2)  + theme_bw() +
  ylab('PV Percentile') +
  xlab('PV (%)') +
  guides(color=guide_legend(title="Survey Visit Date (Nearest DEA FC Timestamp)", size = 0.9))
plot(ecdf_green)

combined_plot <- plot_grid(t, rainfall, ecdf_green, ncol = 1, labels = c("a)", "b)", "c)"))
plot(combined_plot)


plot(dea_fc$precip_30)






# Junk script

unique_times <- unique(evaluation.data.2$time)
temp <- data.frame('fraction_type' = NA,
                   'time' =  NA,
                   'percentile' = NA)

for (i in unique_times){
  for (f in c('green', 'brown', 'bare')) {
    
    fc_idx <- which((evaluation.data.2$time == i) & 
                      (evaluation.data.2$fraction_type == f))
    fc <- evaluation.data.2$remote_value[[fc_idx]]
    
    if (f == 'green') {
      remote_fc_name <- 'pv_filter'
    } else if (f == 'brown') {
      remote_fc_name <- 'npv_filter'
    } else if (f == 'bare') {
      remote_fc_name <- 'bs_filter'
    } else {
      print('Error in the how evaluation dataset names fractional cover type')
    }
    
    percent <- ecdf(dea_fc[[remote_fc_name]])(fc)
    
    row <- c('fraction_type' = f,
             'time' = evaluation.data.2$time[fc_idx],
             'percentile' = percent)
    
    temp <- rbind(temp, row)
  }
}
temp <- temp[-1,] # remove the first row

# Since evaluation dataset is in a long-format, I will do a table join with
# Key = (timestamp, cover type)
# after calculating the percentiles 

# Now the table join:
evaluation.data.2 <- evaluation.data.2 %>%
  left_join(temp)

d <- subset(evaluation.data.2, fraction_type == 'green')

ggplot(data = dea_fc) +
  stat_ecdf(mapping = aes(x = pv_filter), color = 'darkgreen') +
  geom_point(data = d, 
             mapping = aes(x = remote_value, y = percentile))
