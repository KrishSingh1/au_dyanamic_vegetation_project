###### DEA_Data_Repeated_Sites_Visualisation ######
# By Krish Singh
# 20240929
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
library(tidyr)



# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

# Get DEA file names 
directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
sites.query <- read.csv("../DATASETS/sites_info_query.csv")

# Load AusPlots data 
evaluation_fc <- fread('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  mutate(time = as.Date(time)) %>%
  arrange(time)
  
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
      
      temp <- rbind(temp, change)
    }
  }
  temp <- as.data.frame(temp)
  rownames(temp) <- 1:nrow(temp)
    
  return(temp)
})

write.csv(change_list$green_fc, '../DATASETS/AusPlots_Extracted_Data/Final/green_change.csv')
write.csv(change_list$brown_fc, '../DATASETS/AusPlots_Extracted_Data/Final/brown_change.csv')
write.csv(change_list$bs_fc, '../DATASETS/AusPlots_Extracted_Data/Final/bare_change.csv')

## Combine the fractions into one  

remote_long <- change_list$green_fc %>%
  left_join(change_list$brown_fc) %>%
  left_join(change_list$bs_fc)  %>% 
  select(!c("green", "brown", "bare")) %>%
  pivot_longer(cols = c("pv_filter", "npv_filter", "bs_filter"), 
               names_to = "fraction_type",
               values_to = "remote_value") %>%
  mutate(fraction_type = case_when(
    fraction_type == 'pv_filter' ~ 'green',                  
    fraction_type == 'npv_filter' ~ 'brown',
    fraction_type == 'bs_filter' ~ 'bare'))
  

obs_long <- change_list$green_fc %>%
  left_join(change_list$brown_fc) %>%
  left_join(change_list$bs_fc) %>%
  select(!c("pv_filter", "npv_filter", "bs_filter")) %>%
  pivot_longer(cols = c("green", "brown", "bare"), 
               names_to = "fraction_type",
               values_to = "on_ground_value")
  
growth.form.agg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv') %>%
  select(c("site_location_name", "vegetation_type")) %>% 
  rename(growth_form = "vegetation_type")
veg.type.agg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv') %>%
  select(c("site_location_name", "vegetation_type"))

Broader_Classifications = c('Tropical/Savanna', 
                            'Tropical/Savanna', 'Temp/Med', 'Temp/Med','Temp/Med','Desert')
bioclimatic <- read.csv('../DATASETS/Australian Bioclimatic Regions/AusPlots_BioclimaticRegion_Classified.csv')
bioclimatic$bioclimatic_region <- unlist(lapply(bioclimatic$Group_Code, FUN = function(x){
  return(Broader_Classifications[x])
}))
colnames(bioclimatic)[1] <- 'site_location_name'

bioclimatic <- bioclimatic %>% select(c('site_location_name', 'bioclimatic_region'))
  
combined <- remote_long %>% 
  left_join(obs_long,
            by = c('site_location_name','time_a', 
                   'time_b', 'fraction_type', 'days_difference'))  %>%
  left_join(growth.form.agg) %>%
  left_join(veg.type.agg) %>%
  left_join(bioclimatic)


write.csv(combined, '../DATASETS/AusPlots_Extracted_Data/Final/Fractional_Cover_Change_Evaluation.csv')

# Visualise the results 

combined <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/Fractional_Cover_Change_Evaluation.csv')


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



combined_change <- ggplot(data = combined, mapping = aes(x = on_ground_value, y = remote_value, group = fraction_type)) + 
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') +
  geom_text(data = stats_fc, aes(x = 50, y = 90, 
                              label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                                             "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                                            )),
            color = "black", hjust = 1, size = 3) + facet_wrap(~fraction_type) + theme_bw() + geom_density2d(alpha = 1)
  
combined_change

## By vegetation type 

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
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

combined_change_veg <- ggplot(data = combined, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') + facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc, size = 3, 
            aes(x = 0, y = 90,
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                                                "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)))) +
  theme_bw()

combined_change_veg

# Visualise by the vegetation type 

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
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') + facet_grid(fraction_type ~ growth_form) +
  geom_text(data = included_growth_forms, size = 2.5,
            aes(x = 0, y = 90,
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                ))) +
  theme_bw()


combined_change_veg



# Bioclimatic 

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
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') + facet_grid(bioclimatic_region  ~ fraction_type) +
  geom_text(data = stats_bio, size = 3,
            aes(x = 0, y = 90, 
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                ))) + theme_bw()



combined_change_veg



