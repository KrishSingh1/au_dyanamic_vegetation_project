####### DEA Evaluation Nearest Point shared vers  #######
# Author: Krish Singh
# Date: 2024-07-11
# Objective: To evaluate DEA FC against AusPlots FC at the nearest time point
#   of survey 
# Note: this is the current method we decided to use to evaluate DEA FC
#   because we smoothed the DEA FC via savitzky-golay filter
#   
#  
# Inputs:
# - DEA FC
# - AusPlots FC
# Process:
# 1. For each survey:
# 2.  Read in DEA FC + AusPlots FC + site_info
# 3.  Get time of survey from site
# 4.  From this time point, find the closest time point from DEA FC
# 5.  Record the DEA time point, the AusPlots time point, the DEA pv_filter, npv_filter, and bs_filter, and the AusPlots pv_filter, npv_filter, bs_filter
# Output:
# - A dataset with columns:
# - site_unique, ausplots timepoint, dea time point, DEA FC, AusPlots FC 
# Then, we plot some evaluation plots 

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tune)
library(ggpmisc)
library(reshape2)
library(data.table) # for fast csv reading and processing

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

# Read and Process Datasets 

# We Perform the time point matching for each site with avaliable DEA FC

dea_fc_all <- fread('../DEA_FC_Smoothed_1987_2022/DEA_FC_Combined_1987_2022.csv')
AusPlots_fc <- read.csv('../AusPlots_FC/AusPlots_FC_Iter_2_0_6.csv')[,-1]
site_info <- read.csv('../Site_Info/extracted_Final_site_info_2-0-6.csv')[,-1]

site_names <- unique(dea_fc_all$site_location_name)

colnames(site_info) <- unlist(lapply(strsplit(colnames(site_info), '\\.'), FUN = function(x){
  unlist(x)[3]
}))

AusPlots_fc <- AusPlots_fc %>%
  left_join(site_info[,c('site_unique', 'site_location_name', 'visit_start_date')], by = 'site_unique') %>%
  subset(site_location_name %in%  site_names) %>% 
  mutate(visit_start_date = as.Date(visit_start_date),
         time = NA,
         pv_filter = NA,
         npv_filter = NA,
         bs_filter = NA) 

for (site in site_names) {
  
  # Subset dea_fc to the site specifically
  dea_fc <- dea_fc_all %>% 
    subset(site_location_name == site) %>%
    mutate(time =  as.Date(time))
  
  # Get indices of the site 
  site_ground_indices <- which(AusPlots_fc$site_location_name == site)
  
  # Now for each visit within a site, find the nearest timepoint 
  for (i in site_ground_indices){
    on_ground_date <- AusPlots_fc$visit_start_date[i]
    
    closest_time_point_index <- dea_fc$time %>% 
      difftime(on_ground_date, units = 'days') %>%
      as.numeric() %>% abs() %>% which.min()
    
    # Update the dataset accordingly
    AusPlots_fc[i, c('time', 'pv_filter', 'npv_filter', 'bs_filter')] <- dea_fc[closest_time_point_index, c('time', 'pv_filter', 'npv_filter', 'bs_filter')]
  }
}

# Final Fix for the dea date
AusPlots_fc <- AusPlots_fc %>% mutate(time = as.Date(time))
write.csv(AusPlots_fc, '../DEA_FC_Ground_Truth_Evaluation_File/DEA_FC_Ground_Truth_Evaluation.csv')


# Plot by Fraction -----------------------------------------------------------

# Greenness


pv_filter.stats <- lm(pv_filter~green, AusPlots_fc)
cal.green <- ggplot(AusPlots_fc, aes(y = pv_filter, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + 
  geom_abline(slope = pv_filter.stats$coefficients[["green"]], 
              intercept = pv_filter.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

plot(cal.green)
ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_GREEN.png',
       cal.green, width = 13, height = 13, units = "in")


# Brownness 
npv_filter.stats <- lm(npv_filter~brown,AusPlots_fc)
cal.brown <- ggplot(AusPlots_fc, aes(y = npv_filter, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +
  geom_abline(slope = npv_filter.stats$coefficients[["brown"]], 
              intercept = npv_filter.stats$coefficients[["(Intercept)"]], size = 0.9) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
plot(cal.brown)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_BROWN.png',
       cal.brown, width = 13, height = 13, units = "in")

# Bareness
bs_filter.stats <- lm(bs_filter~bare,AusPlots_fc)
cal.bare <- ggplot(AusPlots_fc, aes(y = bs_filter, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + 
  geom_abline(slope = bs_filter.stats$coefficients[["bare"]], 
              intercept = bs_filter.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
plot(cal.bare)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_BARE.png',
       cal.bare, width = 13, height = 13, units = "in")


# Combine Plots 
dea.fc.sites.plotting.long <- reshape2::melt(AusPlots_fc[, c('site_unique','bs_filter','npv_filter','pv_filter')], 
                                             id.vars = c('site_unique'), value.name ="remote.cover")
site.fc.df.long <- reshape2::melt(AusPlots_fc[,c('site_unique','bare','brown','green')], 
                                  id.vars = c('site_unique'), value.name = "insitu.cover")

dea.fc.sites.plotting.long$variable <- as.character(dea.fc.sites.plotting.long$variable)
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'pv_filter')] <- 'green'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'npv_filter')] <- 'brown'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'bs_filter')] <- 'bare'

both.plotting.df.long <- merge(dea.fc.sites.plotting.long, site.fc.df.long, by = c("site_unique", 'variable'))
all.stats <- lm(remote.cover~insitu.cover,both.plotting.df.long)

all.pl.validate <- ggplot(data = both.plotting.df.long, aes(x = insitu.cover, y = remote.cover, colour = variable)) +
  geom_point() + labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) + 
  geom_abline(slope = all.stats$coefficients[["insitu.cover"]],
              intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

cal.all <- ggplot(AusPlots_fc) + geom_point(aes(x = bare, y = bs_filter, colour = 'bare'), alpha = 0.5) +
  geom_point(aes(x = green, y = pv_filter, colour = 'green'), alpha = 0.5) + 
  geom_point(aes(x = brown, y = npv_filter, colour = 'brown'), alpha = 0.5) + geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) +
  xlim(0,100) + ylim(0,100) + labs(x = "cover (in-situ)", y = "cover (remote)") + 
  scale_colour_manual(name = 'Cover', values = c('brown' = '#0072B2', 'green' = '#009E73', 'bare' = 'red')) +
  geom_abline(slope = all.stats$coefficients[["insitu.cover"]],intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) +
  stat_poly_eq(data = both.plotting.df.long, mapping = use_label(c("eq", "R2", 'p'), aes(x = insitu.cover, y =remote.cover)))


cowplot::plot_grid(cal.green, cal.brown, cal.bare, all.pl.validate) # check if all.pl.validate give same statistics as validation
t <- cowplot::plot_grid(cal.green, cal.brown, cal.bare, cal.all) # Gives same statistics 
plot(t)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_ALL.png',
       t, width = 13, height = 13, units = "in")

# Plot by vegetation type -------------------------------------------------

growth.form.agg <- read.csv('../AusPlots_Dominant_Growth_Form/growth_forms_classification_by_dom_species_final_2-0-6.csv')
growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]

dea.fc.sites.plotting <- merge(AusPlots_fc, growth.form.essen, by = 'site_location_name')

# Now plot by vegetation type 

## Green 

cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv_filter, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  facet_wrap(~vegetation_type) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)
plot(cal.green)

ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_GREEN_PER_GROWTHFORM.png',
       cal.green, width = 13, height = 13, units = "in")

## Bare
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs_filter, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) +
  coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)
plot(cal.bare)


ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_BARE_PER_GROWTHFORM.png',
       cal.bare, width = 13, height = 13, units = "in")

## Brown
cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv_filter, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +
  facet_wrap(~vegetation_type) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = F)
plot(cal.brown)
ggsave('../DEA_Evaluation_Plots/DEA_VS_AUSPLOTS_FC_BROWN_PER_GROWTHFORM.png',
       cal.brown, width = 13, height = 13, units = "in")





