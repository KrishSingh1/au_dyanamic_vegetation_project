####### DEA Evaluation Nearest Point  #######
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
library(tidyr)
library(MASS)
library(viridis)

# Functions ---------------------------------------------------------------


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

AusPlots_fc <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_FC_Iter_2_0_6.csv')[,-1]
site_info <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv')[,-1]

colnames(site_info) <- unlist(lapply(strsplit(colnames(site_info), '\\.'), FUN = function(x){
  unlist(x)[3]
}))

AusPlots_fc <- AusPlots_fc %>%
  left_join(site_info[,c('site_unique', 'site_location_name', 'visit_start_date')], by = 'site_unique') %>%
  subset(site_location_name %in%  fileNames) %>% 
  mutate(visit_start_date = as.Date(visit_start_date),
         time = NA,
         pv_filter = NA,
         npv_filter = NA,
         bs_filter = NA) 

for (site in fileNames) {
  
  # Read site dea_fc 
  dea_fc <- read.csv(paste0(directory, 'Input_DataSet_', site, '.csv')) %>%
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
write.csv(AusPlots_fc, '../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv')
write.csv(AusPlots_fc, '../DATASETS_TO_SHARE/DEA_FC_Ground_Truth_Evaluation.csv')

# Group by vegetation, growth form, and bioclimatic regions 
AusPlots_fc <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv')


remote_long <- AusPlots_fc %>% select(!c("green", "brown", "bare")) %>%
  pivot_longer(cols = c("pv_filter", "npv_filter", "bs_filter", 'error', 'X'), 
               names_to = "fraction_type",
               values_to = "remote_value") %>%
  mutate(fraction_type = case_when(
    fraction_type == 'pv_filter' ~ 'green',                  
    fraction_type == 'npv_filter' ~ 'brown',
    fraction_type == 'bs_filter' ~ 'bare'))


obs_long <- AusPlots_fc %>%
  select(!c("pv_filter", "npv_filter", "bs_filter")) %>%
  pivot_longer(cols = c("green", "brown", "bare", 'error','X'), 
               names_to = "fraction_type",
               values_to = "on_ground_value")

growth.form.agg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv') %>%
  select(c("site_location_name", "vegetation_type")) %>% 
  rename(growth_form = "vegetation_type")
veg.type.agg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv') %>%
  select(c("site_location_name", "vegetation_type"))

bioclimatic <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/sites_super_classified.csv')
Broader_Classifications = c('Tropical/Savanna', 
                            'Tropical/Savanna', 'Temp/Med', 'Temp/Med','Temp/Med','Desert')
bioclimatic <- read.csv('../DATASETS/Australian Bioclimatic Regions/AusPlots_BioclimaticRegion_Classified.csv')
bioclimatic$bioclimatic_region <- unlist(lapply(bioclimatic$Group_Code, FUN = function(x){
  return(Broader_Classifications[x])
}))
colnames(bioclimatic)[1] <- 'site_location_name'

combined <- remote_long %>% 
  left_join(obs_long,
            by = c('site_unique', 'fraction_type', 'time', 'site_location_name',
                   'visit_start_date', 'other'))  %>% 
  left_join(growth.form.agg) %>%
  left_join(veg.type.agg) %>%
  left_join(bioclimatic) %>%
  filter(!is.na(fraction_type)) %>%
  filter(!is.na(on_ground_value))
  



write.csv(combined, '../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete.csv')

# Plot by Fraction -----------------------------------------------------------

combined <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete.csv')


# New version:



stats_fc <- combined %>%
  group_by(fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = sqrt(mean(residuals(model)^2)),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

#

combined_change <- ggplot(data = combined, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(0, 110)) + geom_smooth(method = 'lm') +
  geom_text(data = stats_fc, aes(x = 50 , y = 100, 
                                 label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                                                "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                                 )),
            color = "black", hjust = 1, size = 3) + facet_wrap(~fraction_type) + 
  geom_density2d(color = "green", alpha = 0.7) +
  theme_bw()
combined_change

## Now by vegetation type 

stats_fc <- combined %>%
  group_by(vegetation_type, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = sqrt(mean(residuals(model)^2)),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2],
      n_samples = nrow(model$model)
    )
  })

combined_change_veg <- ggplot(data = combined, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(0,110)) + geom_smooth(method = 'lm') + facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc, size = 2.9,
            aes(x = 50, y = 100,
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)))) +
  theme_bw() + geom_density2d(color = "green", alpha = 0.7)

combined_change_veg
## Now by Vegetation growth form


stats_form <- combined %>%
  group_by(fraction_type, growth_form) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = sqrt(mean(residuals(model)^2)),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2],
      n_samples = nrow(model$model)
    )
  })

included_growth_forms <- subset(stats_form, subset = (n_samples >= 10))
combined_filtered <- subset(combined, subset = (growth_form %in% unique(included_growth_forms$growth_form)))


combined_change_veg <- ggplot(data = combined_filtered, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point(size = 1) + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(0,110)) + geom_smooth(method = 'lm') + facet_grid(fraction_type ~ growth_form) +
  geom_text(data = included_growth_forms, size = 2.5,
            aes(x = 50, y = 100,
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                ))) +
  theme_bw()


combined_change_veg


## Now by bioclimatic regions 




stats_bio <- combined %>%
  group_by(bioclimatic_region, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = sqrt(mean(residuals(model)^2)),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change_veg <- ggplot(data = combined, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(0,110)) + geom_smooth(method = 'lm') + facet_grid(bioclimatic_region  ~ fraction_type) +
  geom_text(data = stats_bio, size = 3,
            aes(x = 50, y = 100, 
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                ))) + theme_bw()



combined_change_veg








# Greenness


pv_filter.stats <- lm(pv_filter~green, AusPlots_fc)
cal.green <- ggplot(AusPlots_fc, aes(y = pv_filter, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + 
  geom_abline(slope = pv_filter.stats$coefficients[["green"]], 
              intercept = pv_filter.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

plot(cal.green)


# Brownness 
npv_filter.stats <- lm(npv_filter~brown,AusPlots_fc)
cal.brown <- ggplot(AusPlots_fc, aes(y = npv_filter, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +
  geom_abline(slope = npv_filter.stats$coefficients[["brown"]], 
              intercept = npv_filter.stats$coefficients[["(Intercept)"]], size = 0.9) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
plot(cal.brown)

# Bareness
bs_filter.stats <- lm(bs_filter~bare,AusPlots_fc)
cal.bare <- ggplot(AusPlots_fc, aes(y = bs_filter, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + 
  geom_abline(slope = bs_filter.stats$coefficients[["bare"]], 
              intercept = bs_filter.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

plot(cal.bare)


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
cowplot::plot_grid(cal.green, cal.brown, cal.bare, cal.all) # Gives same statistics 



# Plot by vegetation type -------------------------------------------------

growth.form.agg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv')
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

## Bare
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs_filter, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) +
  coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)

plot(cal.bare)


## Brown
cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv_filter, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +
  facet_wrap(~vegetation_type) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = F)

plot(cal.brown)




