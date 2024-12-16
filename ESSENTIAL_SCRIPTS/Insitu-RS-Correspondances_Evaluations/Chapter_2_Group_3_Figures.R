########## Chapter 2 Group 3 Figures ############
#### Krish Singh
#### Date: 17-10-24



# Library -----------------------------------------------------------------

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


# Main --------------------------------------------------------------------



# Set up ------------------------------------------------------------------


# Set up the thiel-sen_regs 

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
theil_sen_reg_copy <- theil_sen_reg

dom_veg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv') %>%
  select(c('site_location_name', 'vegetation_type'))

combin <- theil_sen_reg_copy %>%
  left_join(dom_veg, by = 'site_location_name')

# Set up the short-term slopes 

# Load AusPlots data 
evaluation_fc <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv') %>%
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


# Greenness 
stats_fc_pv <- evaluation_list[['green_fc']] %>%
  group_by(site_location_name) %>%
  arrange('time') %>%
  do({
    model <- lm(pv_filter ~ time, data = .) # rs
    model2 <- lm(green ~ time, data = .) # og
    data.frame(
      intercept_rs_pv = coef(model)[1],
      slope_rs_pv = coef(model)[2],
      intercept_og_pv = coef(model2)[1],
      slope_og_pv = coef(model2)[2],
      n_samples = nrow(model$model)
    )
  })

# Brownness
stats_fc_npv <- evaluation_list[['brown_fc']] %>%
  group_by(site_location_name) %>%
  arrange('time') %>%
  do({
    model <- lm(npv_filter ~ time, data = .) # rs 
    model2 <- lm(brown ~ time, data = .) # og
    data.frame(
      intercept_rs_npv = coef(model)[1],
      slope_rs_npv = coef(model)[2],
      intercept_og_npv = coef(model2)[1],
      slope_og_npv = coef(model2)[2],
      n_samples = nrow(model$model)
    )
  })

# Bare soil
stats_fc_bs <- evaluation_list[['bs_fc']] %>%
  group_by(site_location_name) %>%
  arrange('time') %>%
  do({
    model <- lm(bs_filter ~ time, data = .) # rs
    model2 <- lm(bare ~ time, data = .) # og
    data.frame(
      intercept_rs_bs = coef(model)[1],
      slope_rs_bs = coef(model)[2],
      intercept_og_bs = coef(model2)[1],
      slope_og_bs = coef(model2)[2],
      n_samples = nrow(model$model)
    )
  })

# Set up Part 3 - Combining theils and short term trend -------------------

test <- stats_fc_pv %>%
  left_join(stats_fc_npv, by = c('site_location_name', 'n_samples') ) %>%
  left_join(stats_fc_bs, by = c('site_location_name', 'n_samples')) %>%
  left_join(combin, by = 'site_location_name')




  
# Graphic -----------------------------------------------------------------

lims <- c(-25, 25)
# Greenness
pv <- ggplot(data = test, mapping = aes(y = pv_filter_slope_yr, x = slope_rs_pv *365)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (PV/yr)') + xlab('Short Term Trend (PV/yr)') +  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

# Brownness
npv <- ggplot(data = test, mapping = aes(y = npv_filter_slope_yr, x = slope_rs_npv * 365)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (NPV/yr)') + xlab('Short Term Trend (NPV/yr)') + coord_fixed(ratio= 1, xlim = lims, ylim = lims)

# Bare Soil
bs <- ggplot(data = test, mapping = aes(y = bs_filter_slope_yr, x = slope_rs_bs * 365)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (BS/yr)') + xlab('Short Term Trend (BS/yr)')  + coord_fixed(ratio= 1, xlim = lims, ylim = lims)


combined_plot <- plot_grid(pv, npv, bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot

range(test$pv_filter_slope_yr)

range(test$npv_filter_slope_yr)

range(test$bs_filter_slope_yr)

range(test$slope_rs_pv) * 365
range(test$slope_rs_npv) * 365
range(test$slope_rs_bs) * 365

# Group by veg type:

pv <- ggplot(data = test, mapping = aes(y = pv_filter_slope, x = slope_rs_pv)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (PV/day)') + xlab('Short Term Trend (PV/day)') + facet_grid(~vegetation_type)
pv

npv <- ggplot(data = test, mapping = aes(y = npv_filter_slope, x = slope_rs_npv)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (NPV/day)') + xlab('Short Term Trend (NPV/day)')+ facet_grid(~vegetation_type)
npv

bs <- ggplot(data = test, mapping = aes(y = bs_filter_slope, x = slope_rs_bs)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (BS/day)') + xlab('Short Term Trend (BS/day)') + facet_grid(~vegetation_type)
bs






# What if I remove the non-signficant slopes 
test_sign <- subset(test, subset = (pv_filter_signf == T &
                                    npv_filter_signf == T &
                                    bs_filter_signf == T))

pv <- ggplot(data = test_sign, mapping = aes(y = pv_filter_slope, x = slope_rs_pv)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (PV/day)') + xlab('Short Term Trend (PV/day)')

# Brownness
npv <- ggplot(data = test_sign, mapping = aes(y = npv_filter_slope, x = slope_rs_npv)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (NPV/day)') + xlab('Short Term Trend (NPV/day)')

# Bare Soil
bs <- ggplot(data = test_sign, mapping = aes(y = bs_filter_slope, x = slope_rs_bs)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (BS/day)') + xlab('Short Term Trend (BS/day)')


combined_plot <- plot_grid(pv, npv, bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot

# On ground and RS slope --------------------------------------------------

# Greenness
pv <- ggplot(data = test, mapping = aes(y = slope_og_pv, x = slope_rs_pv)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (PV/day)') + xlab('Short Term Trend (PV/day)')

# Brownness
npv <- ggplot(data = test, mapping = aes(y = slope_og_npv, x = slope_rs_npv)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (NPV/day)') + xlab('Short Term Trend (NPV/day)')

# Bare Soil
bs <- ggplot(data = test, mapping = aes(y = slope_og_bs, x = slope_rs_bs)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1)) +
  ylab('Long Term Trend (BS/day)') + xlab('Short Term Trend (BS/day)')


combined_plot <- plot_grid(pv, npv, bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot




# Checking if I take out outliers -----------------------------------------

test['difference_pv'] <-  (test['pv_filter_slope'] - test['slope_rs_pv'])^2

lower_bounds <- quantile(test[['difference_pv']], 0.25) - IQR(test[['difference_pv']])
upper_bounds <- quantile(test[['difference_pv']], 0.75) + IQR(test[['difference_pv']])

test_outlier_pv <- test %>%
  subset( (difference_pv >= lower_bounds) &
          (difference_pv <= upper_bounds))

ggplot(data = test_outlier_pv, mapping = aes(y = pv_filter_slope, x = slope_rs_pv)) +
  geom_point() + geom_smooth(method =  'lm') + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_abline(color = 'red', lty = 2, size = 1) + coord_fixed(ratio=1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1))



# For the appendix --------------------------------------------------------

# Here, I want to look into the timings of site visits 
evaluation_fc <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  mutate(time = as.Date(time)) %>%
  arrange(time) 

length(na.omit(evaluation_fc$green))
length(na.omit(evaluation_fc$brown))
length(na.omit(evaluation_fc$bare))

counts.df <- as.data.frame(table(evaluation_fc$site_location_name)) %>% 
  subset(Freq >= 2)  

evaluation_fc_revisit <- subset(evaluation_fc, site_location_name %in% unique(counts.df$Var1))
evaluation_fc_revisit$day_of_year <- yday(evaluation_fc_revisit$time)




site_visit_sd <- aggregate(x = day_of_year ~ site_location_name,
          data = evaluation_fc_revisit, FUN = sd, na.rm = F)


pl_pv <- ggplot(data = site_visit_sd, 
                mapping = aes(x = day_of_year)) +  
  theme_bw() + 
  labs(x = 'Day of Year of Visit (Standard Deviation)', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(day_of_year)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(day_of_year) + sd(day_of_year)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(day_of_year) - sd(day_of_year)), 
             linetype = "dashed", size = 1, color = 'red') +
  annotate('text', x = max(site_visit_sd$day_of_year) * 0.9, 
           y = 30, 
           label = paste0("x\u0305", ' = ',round(mean(site_visit_sd$day_of_year),4), 
                          '\ns = ', round(sd(site_visit_sd$day_of_year),4)), 
           color = "black", size = 3)


# Try different method, from consectative visits

change <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/Fractional_Cover_Change_Evaluation.csv')

change1 <- unique(change[, c('time_a', 'time_b', 'site_location_name')]) %>%
  mutate(time_a = as.Date(time_a),
         time_b = as.Date(time_b)) %>%
  mutate(day_of_year1 = yday(time_a),
         day_of_year2 = yday(time_b)) %>%
  mutate(days_diff = abs(day_of_year1 - day_of_year2))

corrected_doy <- c()
for (i in (change1$days_diff)){
  if(i > 183) {
    corrected_doy <- c(corrected_doy, 365 - i )
  } else {
    corrected_doy <- c(corrected_doy, i)
  }
}


change1$corrected_doy <- corrected_doy

mean(corrected_doy >= 100)

pl_pv <- ggplot(data = change1, 
                mapping = aes(x = corrected_doy)) +  
  theme_bw() + 
  labs(x = 'Day of Year Since Last Visit (Absolute difference)', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9, bins = 30) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(corrected_doy)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(corrected_doy) + sd(corrected_doy)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(corrected_doy) - sd(corrected_doy)), 
             linetype = "dashed", size = 1, color = 'red') +
  annotate('text', x = max(change1$corrected_doy) * 0.9, 
           y = 30, 
           label = paste0("x\u0305", ' = ',round(mean(change1$corrected_doy),4), 
                          '\ns = ', round(sd(change1$corrected_doy),4)), 
           color = "black", size = 3)

pl_pv
