###### DEA_Data_Repeated_Sites_Visualisation ######
# By Krish Singh
# 2024-07-30
# To see the change in fractional cover from site visit to another with DEA data. 


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



# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

# Get DEA file names 

# Load AusPlots data 
evaluation_fc <- fread('../DEA_FC_Ground_Truth_Evaluation_File/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  mutate(time = as.Date(time)) %>%
  arrange(time)
  
fileNames <- unique(evaluation_fc$site_location_name)
# Looking at the varying number of NAs between in-situ for green, brown, bs
# Its best to calculate the change differently
# Additionally, we are not considering the length of time into our change calcs 

evaluation_list <- list(green_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'green', 'pv_filter', 'time')]),
     brown_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'brown', 'npv_filter', 'time')]),
     bs_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'bare', 'bs_filter', 'time')]))

evaluation_list <- lapply(evaluation_list, FUN = function(x) {
  counts.df <- as.data.frame(table(x$site_location_name)) %>%
    subset(Freq >= 2)  # Remove sites that were not revisited
  x <- subset(x, site_location_name %in% unique(counts.df$Var1)) # now subset the dataframe by sites with more than 1 visit
  return(x)
})

change_list <- lapply(evaluation_list, FUN = function(one_fraction) {
  
  one_fraction <- as.data.frame(one_fraction)
  site_list <- unique(one_fraction$site_location_name)
  temp <- c()
  
  for(s in site_list) {
    site_fc_information <- subset(one_fraction, site_location_name == s) %>%
      arrange(time)
    #print(site_fc_information)
    n_samples <- nrow(site_fc_information)
    
    for (sample in 1:(n_samples-1)) {
      # Sample 'a' and 'b' the first and second  time point, with the time points 
      
      essential_cols_names <- c('green', 'brown', 'bare', 'pv_filter', 'bs_filter', 'npv_filter', 'time')
      essential_cols <- colnames(site_fc_information)[which(colnames(site_fc_information) %in% essential_cols_names)] # Extract by fraction type and 'time'

      a <- site_fc_information[sample, essential_cols]
      b <- site_fc_information[sample + 1, essential_cols]
      
      time_a <- site_fc_information[['time']][sample]
      time_b <- site_fc_information[['time']][sample + 1]
      
      change <- b - a 
      change <- change %>%
        mutate(time_a = time_a,
               time_b = time_b,
               site_location_name = s) %>%
        rename(days_difference = time)
      #print(change)
      temp <- rbind(temp, change)
    }
  }
  temp <- as.data.frame(temp)
  rownames(temp) <- 1:nrow(temp)
    
  return(temp)
})

green_change <- change_list$green_fc

bs.stats <- lm(bs_filter~bare, change_list$bs_fc)
bs.bare.pl <- ggplot(data = change_list$bs_fc, aes(x = bare, y = bs_filter)) + 
  labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_obs_pred() + xlim(c(-100,100)) +
  geom_abline(slope = bs.stats$coefficients[["bare"]],
              intercept = bs.stats$coefficients[["(Intercept)"]]) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  annotation_custom(textGrob(paste0('RMSE: ',
                                    round(rmse(actual = change_list$bs_fc$bare,
                                               predicted = change_list$bs_fc$bs_filter),2)),
                                   0.16, 0.8))

plot(bs.bare.pl)    

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_BARE_CHANGE.png',
       bs.bare.pl, width = 13, height = 13, units = "in")


green.stats <- lm(pv_filter~green, change_list$green_fc)
pv.green.pl <- ggplot(data = change_list$green_fc, aes(x = green, y = pv_filter)) + 
  labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_obs_pred() + xlim(c(-100,100)) +
  geom_abline(slope = green.stats$coefficients[["green"]],
              intercept = green.stats$coefficients[["(Intercept)"]]) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))  +
  annotation_custom(textGrob(paste0('RMSE: ',
                                    round(rmse(actual = change_list$green_fc$green,
                                               predicted = change_list$green_fc$pv_filter),2)),
                             0.16, 0.8)) 

plot(pv.green.pl)       

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_GREEN_CHANGE.png',
       pv.green.pl, width = 13, height = 13, units = "in")

brown.stats <- lm(npv_filter~brown, change_list$brown_fc)
npv.brown.pl <- ggplot(data = change_list$brown_fc, aes(x = brown, y = npv_filter)) + 
  labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_obs_pred() + xlim(c(-100,100)) +
  geom_abline(slope = brown.stats$coefficients[["brown"]],
              intercept = brown.stats$coefficients[["(Intercept)"]]) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  annotation_custom(textGrob(paste0('RMSE: ',
                                    round(rmse(actual = change_list$brown_fc$brown,
                                               predicted = change_list$brown_fc$npv_filter),2)),
                             0.16, 0.8)) 

plot(npv.brown.pl)   

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_BROWN_CHANGE.png',
       npv.brown.pl, width = 13, height = 13, units = "in")


plot(cowplot::plot_grid(pv.green.pl, npv.brown.pl, bs.bare.pl))


# Plot by Vegetation Growth Form ------------------------------------------


growth.form.agg <- read.csv('../AusPlots_Dominant_Growth_Form/growth_forms_classification_by_dom_species_final_2-0-6.csv')
growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]


# Now plot by vegetation type 

## Green 

dea.fc.sites.plotting <- merge(change_list$green_fc, growth.form.essen, by = 'site_location_name')
cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv_filter, x = green)) + geom_point(alpha = 0.5) + 
  xlim(-100,100) + ylim(-100,100) + labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") +
  facet_wrap(~vegetation_type) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)
plot(cal.green)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_CHANGE_GREEN_PER_GROWTHFORM.png',
       cal.green, width = 13, height = 13, units = "in")


## Bare
dea.fc.sites.plotting <- merge(change_list$bs_fc, growth.form.essen, by = 'site_location_name')

cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs_filter, x = bare)) + geom_point(alpha = 0.5) +
  xlim(-100,100) + ylim(-100,100)  + labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) +
  coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)
plot(cal.bare)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_CHANGE_BARE_PER_GROWTHFORM.png',
       cal.bare, width = 13, height = 13, units = "in")

## Brown
dea.fc.sites.plotting <- merge(change_list$brown_fc, growth.form.essen, by = 'site_location_name')

cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv_filter, x = brown)) + geom_point(alpha = 0.5) +
  xlim(-100,100) + ylim(-100,100)  + labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) +
  coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)
plot(cal.brown)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_CHANGE_BROWN_PER_GROWTHFORM.png',
       cal.brown, width = 13, height = 13, units = "in")

