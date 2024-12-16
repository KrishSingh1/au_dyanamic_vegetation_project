#### Chapter 2 Group 2 Figures ####
## Krish Singh
# Date: 27-09-2024


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

one_point <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete.csv')
change <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/Fractional_Cover_Change_Evaluation.csv')

sum(table(subset(change, fraction_type == 'green')$site_location_name) == 1)

length(unique(change$site_location_name)) == 1

# Evaluation Plot of RS versus INSITU -------------------------------------


# Calc the R2 of the linear model
# Calc the RMSE of the two data points (not of the linear model)
stats_fc <- one_point %>%
  group_by(fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      bias = mean(frac_values[['remote_value']] - frac_values[['on_ground_value']]),
      r = cor(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

#

combined_change <- ggplot(data = one_point, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point() + ylim(c(0, 110))  + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred()  + geom_smooth(method = 'lm') +
  geom_text(data = stats_fc, aes(x = 80 , y = 100, 
                                 label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                                                "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                                 )),
            color = "black", hjust = 1, size = 3) + facet_wrap(~fraction_type) + 
  geom_density2d(color = "green", alpha = 0.7) +
  theme_bw()
combined_change



# By Vegetation Type ------------------------------------------------------

# Calc the R2 of the linear model
# Calc the RMSE of the two data points (not of the linear model)
stats_fc <- one_point %>%
  group_by(vegetation_type, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2],
      n_samples = nrow(model$model)
    )
  })

combined_change_veg <- ggplot(data = one_point, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(0,110)) + geom_smooth(method = 'lm') + facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc,  hjust = 1, size = 2.9,
            aes(x = 80, y = 100,
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)))) +
  theme_bw() + geom_density2d(color = "green", alpha = 0.7)

combined_change_veg




# By Bioclimatic Regions --------------------------------------------------


stats_bio <- one_point %>%
  group_by(bioclimatic_region, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change_veg <- ggplot(data = one_point, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)", y = "fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(0,110)) + geom_smooth(method = 'lm') + facet_grid(bioclimatic_region  ~ fraction_type) +
  geom_text(data = stats_bio, size = 3,
            aes(x = 50, y = 100, 
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                ))) + theme_bw() + geom_density2d(color = "green", alpha = 0.7)



combined_change_veg




# Evaluation Plot of the change of RS versus INSITU -----------------------



stats_fc <- change %>%
  group_by(fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change <- ggplot(data = change, mapping = aes(x = on_ground_value, y = remote_value, group = fraction_type)) + 
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') +
  geom_text(data = stats_fc, aes(x = 50, y = 90, 
                                 label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                                                "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                                 )),
            color = "black", hjust = 1, size = 3) + facet_wrap(~fraction_type) + theme_bw() + geom_density2d(alpha = 1, color = 'green')

combined_change

# Now by Vegetation type


stats_fc <- change %>%
  group_by(vegetation_type, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

combined_change_veg <- ggplot(data = change, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') + facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc, size = 3, hjust = 1,
            aes(x = 80, y = 90,
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)))) +
  theme_bw() 

combined_change_veg

# By Bioclimatic Regions



stats_bio <- change %>%
  group_by(bioclimatic_region, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change_veg <- ggplot(data = change, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "\u0394 fractional cover (in-situ)", y = "\u0394 fractional cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red', size = 1) +
  coord_obs_pred() + xlim(c(-100,110)) + geom_smooth(method = 'lm') + facet_grid(bioclimatic_region  ~ fraction_type) +
  geom_text(data = stats_bio, size = 3,
            aes(x = 0, y = 90, 
                label = paste0("y = ", round(intercept, 3), " + ", round(slope, 3), "x", ", p = ", round(p_value_slope, 3),
                               "\nR² = ", round(r2, 3), "\nRMSE = ", round(rmse, 3)
                ))) + theme_bw()



combined_change_veg




# For Suplementary Info ---------------------------------------------------

one_point <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete.csv')
one_point <- one_point %>%
  mutate(visit_start_date = as.Date(visit_start_date),
         time = as.Date(time)) %>%
  subset(fraction_type == 'green')

one_point$time_diff <- abs(as.numeric(one_point$visit_start_date - one_point$time))
one_point$fractional_error <- abs((one_point$remote_value - one_point$on_ground_value))


model <- lm(data = one_point[which(one_point$fraction_type == 'green'),], fractional_error ~ time_diff)
summary(model)
plot(model)



ggplot(data = one_point, mapping = aes(x = time_diff, y = fractional_error)) + geom_point() +
  labs(x = 'Absolute Time Difference (days)', y = 'Absolute Error') +
  theme_bw() + facet_wrap(~fraction_type) + geom_smooth(method = 'lm') + facet_grid(~bioclimatic_region ~ vegetation_type)


print(mean(time_diff > 6))
print(sd(time_diff))


