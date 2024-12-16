##### Theil-sen Regression ####
# Date: 24-07-24
# Author: Krish Singh
# Objective: determine a linear trend for each site FC using Theil-Sen Regression


# Libraries ---------------------------------------------------------------

library('ggplot2')
library('data.table')
library('tidyverse')
library('tidyr')
#library('trend')
library('deming')

library(ozmaps)   
library(sf)
library(ggplot2)

library(sp)
library(raster)
library(viridis)
library(rasterVis)

library(tidyverse)
library(gridExtra)
library(envalysis)



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

library(patchwork)
# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

# Create temp df 
temp <- data.frame('site_location_name' = NA)
fractions <- c('pv_filter', 'npv_filter', 'bs_filter')
fractions_col_names <- c()
for (f in fractions) {
  fractions_col_names <- c(fractions_col_names, 
                           paste0(f, '_', 'slope'), 
                           paste0(f, '_', 'intercept'),
                           paste0(f, '_', 'slope_lower'),
                           paste0(f, '_', 'slope_higher'))
} # create the columns based on fraction, mainly the slope and intercept names 
temp[fractions_col_names] <- NA

counter <- 1
for (f in file.names) { # Iterate through each site 
  
  # Keep track of the progress 
  print(paste('Reading ', f, ' {',counter,'/',length(file.names),'}'))
  
  # This vector contains the slope/intercept params for each fraction
  fraction_slopes <- c()
  for (fraction in fractions) { # calc slope/intercept via thei-sen reg for each fraction
    dea_fc <- read.csv(paste0(directory, f, '.csv')) %>%
      mutate(time = as.Date(time)) %>%
      filter(time >= '2000-01-01')
  
    ## Time periods : time < '2000-01-01'
    ##              : time >= '2000-01-01'

    formula <- as.formula(paste0(fraction, ' ~ ', 'time'))
    t_s <- theilsen(formula, data = dea_fc, x = T, y = T, model = TRUE)
    fraction_slopes <- c(fraction_slopes, 
                         t_s$coefficients[2], # slope 
                         t_s$coefficients[1], # intercept 
                         t_s[["ci"]][2],
                         t_s[["ci"]][4]) 
    
  
  }
  site_location_name <- unlist(strsplit(f, '_'))[3] # get site_name 
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


# Now add lat-long coordinates to these sites:

site.info <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv')

site.info.unique <- unique(site.info[,c('site.info.site_location_name', 
                                        'site.info.latitude', 'site.info.longitude')])

colnames(site.info.unique) <- c('site_location_name', 'latitude', 'longitude')

temp <- temp %>% left_join(site.info.unique, by = 'site_location_name')


#write.csv(temp,'../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Post2000.csv', row.names=FALSE)
#write.csv(temp,'../DATASETS_TO_SHARE/DEA_FC_Linear_Trends_File/AusPlots_Theil_Sen_Regression_Stats.csv', row.names=FALSE)


# Checking for Slopes significance  -------------------------------------------------------

# We simply just check whether or not the lower and higher conf interval crosses 0

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats.csv')
theil_sen_reg_copy <- theil_sen_reg

# get the sign of conf lower and higher 

for (i in c('pv_filter', 'npv_filter', 'bs_filter')){
  lower_sign <- sign(theil_sen_reg_copy[[paste0(i, '_slope_lower')]])
  higher_sign <- sign(theil_sen_reg_copy[[paste0(i, '_slope_higher')]])
  theil_sen_reg_copy[paste0(i, '_signf')] <- lower_sign == higher_sign
}


#write.csv(theil_sen_reg_copy,'../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv', row.names=FALSE)

# Examine the data  -------------------------------------------------------


# Plots 

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
theil_sen_reg_copy <- theil_sen_reg
# Perform classifications for each fraction based on their significance and slope
# Green if signficant AND slope is positive
# Yellow if NOT Significant
# Red is signficiant AND slope is negative 

# If sigificant 
#   if slope > 0
#        color = green, code 3
#   else
#         color = red, code 1
# else
#    color = yellow, code 2 

# For each fraction
for (i in c('pv_filter', 'npv_filter', 'bs_filter')) {
  signf_name <- paste0(i,'_signf')
  slope_name <- paste0(i, '_slope')
  slope_coding <- paste0(i, '_code')
  
  coding <- apply(theil_sen_reg_copy, MARGIN = 1, FUN = function(row){

    slope_sign <- sign(as.numeric(row[[slope_name]]))
    if (row[[signf_name]] == T) {
      
      if(slope_sign == 1){
        code <- 3
      } else if (slope_sign == -1){
        code <- 1
      } else {
        code <- 0
      }
    } else {
      code <- 2
    }
    return(code)
  })
  theil_sen_reg_copy[slope_coding] <- as.factor(coding)
}


sf_oz <- ozmap("states")
pl_pv <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_copy, 
             mapping = aes(x = longitude, y = latitude, color = pv_filter_code),
             size= 1.2, alpha = 0.7) + 
  scale_colour_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) +
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) +
  coord_sf(xlim = c(114, 152), ylim = c(-10, -43) )
pl_pv

## For NPV

sf_oz <- ozmap("states")
pl_npv <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_copy, 
             mapping = aes(x = longitude, y = latitude, color = npv_filter_code),
             size= 1.2, alpha = 0.7) + 
  scale_colour_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) +
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) +
  coord_sf(xlim = c(114, 152), ylim = c(-10, -43) )
pl_npv

## For BS
sf_oz <- ozmap("states")
pl_bs <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_copy, 
             mapping = aes(x = longitude, y = latitude, color = bs_filter_code),
             size= 1.2, alpha = 0.6) +  
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) +
  scale_colour_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) + 
  coord_sf(xlim = c(114, 152), ylim = c(-10, -43) )
pl_bs

# Set up the plots for a gridded plot 
p1 <- pl_pv + theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
p2 <- pl_npv + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1))
p3 <- pl_bs + theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <-  p1 + p2 + p3 + plot_layout(ncol = 2,  guides = 'collect') +
  plot_annotation(tag_levels = 'a')
combined_plot


combined_plot <- plot_grid(p1, p2, p3, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot

### Histograms

## PV filter 
pl_pv <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'PV/yr')
pl_pv


## NPV filter 
pl_npv <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1,color = 'red') +
  labs(x = 'NPV/yr')
pl_npv



## BS filter 

pl_bs <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr')
pl_bs

# Combined plots 


p1 <- pl_pv + theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
p2 <- pl_npv + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1))
p3 <- pl_bs + theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <-  p1 + p2 + p3 + plot_layout(ncol = 2,  guides = 'collect') +
  plot_annotation(tag_levels = 'a')
combined_plot


combined_plot <- plot_grid(p1, p2, p3, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot


### ANOVA test

aov_test <- theil_sen_reg_copy %>% 
  pivot_longer(cols = c("pv_filter_slope", "npv_filter_slope", "bs_filter_slope"), 
             names_to = "fraction_type",
             values_to = "slope_value")

res.aov <- aov(slope_value ~ fraction_type, data = aov_test)
summary(res.aov)
TukeyHSD(res.aov)
perm <- RVAideMemoire::perm.anova(slope_value ~ fraction_type, data = aov_test, nperm = 1000)
summary(perm)

## Grouping the histogram by vegetation type 


dom_veg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv')

theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(dom_veg, by = 'site_location_name') 

means <- theil_sen_reg_copy %>%
  group_by(vegetation_type) %>%
  summarize(mean_value = mean(pv_filter_slope_yr), .groups = 'drop')

## PV

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.05) +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 0.9, color = 'red') +
  labs(x = 'PV/yr') + facet_grid(~vegetation_type) +
  geom_vline(data = means, aes(xintercept = mean_value), linetype = "dashed", size = 0.9)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'PV/yr') + facet_grid(~vegetation_type) +
  geom_vline(data = means, aes(xintercept = mean_value), linetype = "dashed", size = 0.9)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = pv_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_violin(fill = '#42f58a', alpha = 0.5) +
  labs(y = 'PV/yr', x = 'Vegetation type') +
  geom_boxplot(width = 0.3, fill = "#42f58a")
pl


res.aov <- aov(pv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg_copy)
summary(res.aov)

TukeyHSD(res.aov)


## NPV

means <- theil_sen_reg_copy %>%
  group_by(vegetation_type) %>%
  summarize(mean_value = mean(npv_filter_slope_yr), .groups = 'drop')


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'NPV/yr') + facet_grid(~vegetation_type) + 
  geom_vline(data = means, aes(xintercept = mean_value), linetype = "dashed", size = 0.9)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = npv_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_violin(fill = '#2f5fad', alpha = 0.5) +
  labs(y = 'NPV/yr', x = 'Vegetation type') +
  geom_boxplot(width = 0.3, fill = "#2f5fad")
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'NPV/yr') + facet_grid(~vegetation_type) +
  geom_vline(data = means, aes(xintercept = mean_value), linetype = "dashed", size = 0.9)
pl


res.aov <- aov(npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg_copy)
summary(res.aov)
TukeyHSD(res.aov)

## BS

means <- theil_sen_reg_copy %>%
  group_by(vegetation_type) %>%
  summarize(mean_value = mean(bs_filter_slope_yr), .groups = 'drop')


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr') + facet_grid(~vegetation_type) + 
  geom_vline(data = means, aes(xintercept = mean_value), linetype = "dashed", size = 0.9)
pl


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr') + facet_grid(~vegetation_type) +
  geom_vline(data = means, aes(xintercept = mean_value), linetype = "dashed", size = 0.9)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = bs_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_violin(fill = '#ad2f37', alpha = 0.5) +
  labs(y = 'BS/yr', x = 'Vegetation type') +
  geom_boxplot(width = 0.2, fill = "#ad2f37")
pl

res.aov <- aov(bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg_copy)
summary(res.aov)
TukeyHSD(res.aov)


## By grouping by growth-form 

theil_sen_reg_copy <- subset(theil_sen_reg, subset = (pv_filter_signf == T & npv_filter_signf == T & bs_filter_signf == T))

dom_veg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv')

common_sites <- intersect(dom_veg$site_location_name,
                          theil_sen_reg_copy$site_location_name)

## PV

theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(dom_veg, by = 'site_location_name') 


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.05) +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'PV/yr') + facet_wrap(~vegetation_type)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray", binwidth = 0.05) +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'PV/yr') + facet_wrap(~vegetation_type)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = pv_filter_slope_yr, x =vegetation_type, group = vegetation_type)) +  
  theme_bw() + 
  geom_boxplot() +
  labs(y = 'PV/yr', x = 'Plant growth form') 
pl



# NPV

theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(dom_veg, by = 'site_location_name') %>%
  subset(subset = (npv_filter_signf == T))


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'NPV/yr') + facet_wrap(~vegetation_type)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'NPV/yr') + facet_wrap(~vegetation_type)
pl


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = npv_filter_slope_yr, x =vegetation_type, group = vegetation_type)) +  
  theme_bw() + 
  geom_boxplot() +
  labs(y = 'NPV/yr', x = 'Plant growth form') 
pl


# BS

theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(dom_veg, by = 'site_location_name') %>%
  subset(subset = (bs_filter_signf == T))

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr') + facet_wrap(~vegetation_type)
pl


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray", binwidth = 0.05) +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr') + facet_wrap(~vegetation_type)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = bs_filter_slope_yr, x =vegetation_type, group = vegetation_type)) +  
  theme_bw() + 
  geom_boxplot() +
  labs(y = 'BS/yr', x = 'Plant growth form') 
pl

# Bioclimatic Regions

bioclimatic <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/sites_super_classified.csv')
bioclimatic$bioclimatic_region <- unlist(lapply(bioclimatic$super_group, FUN = function(x){
  unlist(strsplit(x, split = ' '))[1]
}))
colnames(bioclimatic)[1] <- 'site_location_name'



theil_sen_reg_copy <- subset(theil_sen_reg)

common_sites <- intersect(bioclimatic$site_location_name,
                          theil_sen_reg_copy$site_location_name)


# PV

theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(bioclimatic, by = 'site_location_name')

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'PV/yr') + facet_wrap(~bioclimatic_region)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'PV/yr') + facet_wrap(~bioclimatic_region)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = pv_filter_slope_yr, x =bioclimatic_region, group = bioclimatic_region)) +  
  theme_bw() + 
  geom_boxplot() +
  labs(y = 'PV/yr', x = 'Bioclimatic region') 
pl

res.aov <- aov(pv_filter_slope_yr ~ bioclimatic_region, data = theil_sen_reg_copy)
summary(res.aov)
TukeyHSD(res.aov)


# NPV

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'NPV/yr') + facet_wrap(~bioclimatic_region)
pl


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'NPV/yr') + facet_wrap(~bioclimatic_region)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = npv_filter_slope_yr, x =bioclimatic_region, group = bioclimatic_region)) +  
  theme_bw() + 
  geom_boxplot() +
  labs(y = 'NPV/yr', x = 'Bioclimatic region') 
pl


res.aov <- aov(npv_filter_slope_yr ~ bioclimatic_region, data = theil_sen_reg_copy)
summary(res.aov)
TukeyHSD(res.aov)

# BS

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_histogram(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr') + facet_wrap(~bioclimatic_region)
pl


pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  geom_density(color = "black", fill = "gray", binwidth = 0.07) +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  labs(x = 'BS/yr') + facet_wrap(~bioclimatic_region)
pl

pl <- ggplot(data = theil_sen_reg_copy, 
             mapping = aes(y = bs_filter_slope_yr, x =bioclimatic_region, group = bioclimatic_region)) +  
  theme_bw() + 
  geom_boxplot() +
  labs(y = 'BS/yr', x = 'Bioclimatic region') 
pl

res.aov <- aov(bs_filter_slope_yr ~ bioclimatic_region, data = theil_sen_reg_copy)
summary(res.aov)
TukeyHSD(res.aov)

## Now comparing this with the long-term trend


# PV

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
#theil_sen_reg_copy <- subset(theil_sen_reg, subset = (pv_filter_signf ==  T))
theil_sen_reg_copy <- theil_sen_reg

change <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/green_change.csv')

dom_veg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv')

common_sites <- intersect(dom_veg$site_location_name,
                          theil_sen_reg_copy$site_location_name)
combin <- change %>%
  left_join(theil_sen_reg_copy, by = 'site_location_name') %>%
  subset(subset = !is.na(pv_filter_slope)) %>%
  mutate(time_a = as.Date(time_a),
         time_b = as.Date(time_b)) %>%
  mutate(modelled_a = (pv_filter_intercept + as.numeric(time_a)*pv_filter_slope), 
         modelled_b = (pv_filter_intercept + as.numeric(time_b)*pv_filter_slope)) %>%
  mutate(modelled_diff = modelled_b - modelled_a,
         observed_slope = pv_filter/days_difference) %>%
  left_join(dom_veg, by = 'site_location_name')


ggplot(data = combin, mapping = aes(y = pv_filter, x = modelled_diff)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1) 

+  facet_wrap(~vegetation_type)



ggplot(data = combin, mapping = aes(y = pv_filter, x = modelled_diff)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1) +  facet_wrap(~vegetation_type)

ggplot(data = combin, mapping = aes(y = observed_slope, x = pv_filter_slope)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1)  +  facet_wrap(~vegetation_type)






# NPV

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
theil_sen_reg_copy <- subset(theil_sen_reg, subset = (npv_filter_signf ==  T))

change <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/brown_change.csv')

dom_veg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv')

common_sites <- intersect(dom_veg$site_location_name,
                          theil_sen_reg_copy$site_location_name)
combin <- change %>%
  left_join(theil_sen_reg_copy, by = 'site_location_name') %>%
  subset(subset = !is.na(npv_filter_slope)) %>%
  mutate(time_a = as.Date(time_a),
         time_b = as.Date(time_b)) %>%
  mutate(modelled_a = (npv_filter_intercept + as.numeric(time_a)*npv_filter_slope), 
         modelled_b = (npv_filter_intercept + as.numeric(time_b)*npv_filter_slope)) %>%
  mutate(modelled_diff = modelled_b - modelled_a,
         observed_slope = npv_filter/days_difference) %>%
  left_join(dom_veg, by = 'site_location_name')

ggplot(data = combin, mapping = aes(y = npv_filter, x = modelled_diff)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1)

ggplot(data = combin, mapping = aes(y = observed_slope, x = npv_filter_slope)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1)




# BS

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
theil_sen_reg_copy <- subset(theil_sen_reg, subset = (bs_filter_signf ==  T))

change <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/bare_change.csv')

dom_veg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv')

common_sites <- intersect(dom_veg$site_location_name,
                          theil_sen_reg_copy$site_location_name)
combin <- change %>%
  left_join(theil_sen_reg_copy, by = 'site_location_name') %>%
  subset(subset = !is.na(bs_filter_slope)) %>%
  mutate(time_a = as.Date(time_a),
         time_b = as.Date(time_b)) %>%
  mutate(modelled_a = (bs_filter_intercept + as.numeric(time_a)*bs_filter_slope), 
         modelled_b = (bs_filter_intercept + as.numeric(time_b)*bs_filter_slope)) %>%
  mutate(modelled_diff = modelled_b - modelled_a,
         observed_slope = bs_filter/days_difference) %>%
  left_join(dom_veg, by = 'site_location_name')

ggplot(data = combin, mapping = aes(y = bs_filter, x = modelled_diff)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1) + facet_wrap(~vegetation_type)

ggplot(data = combin, mapping = aes(y = observed_slope, x = bs_filter_slope, color = vegetation_type)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  geom_vhlines(xintercept = 0, yintercept = 0, color = 'red', lty = 2, size = 1)






# Now seperate by veg type







## For the time series:

for(i in 1:10){

  max_index <- which.max(as.numeric(theil_sen_reg_copy$pv_filter_slope))[1]
  max_slope <- theil_sen_reg_copy[max_index,]
  
  dea_fc <- read.csv(paste0(directory, 'Input_DataSet_', max_slope$site_location_name, '.csv')) %>%
    mutate(time = as.Date(time)) %>%
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

# This also gives the change in pv per yr 
# (predict(t_s, data.frame(time = as.Date('2022-01-01'))) - predict(t_s, data.frame(time = as.Date('2021-01-01'))))
# slightly different results, but negiliable


theil_sen_reg_copy <- theil_sen_reg
for(i in 1:20){
  
  max_index <- which.min(theil_sen_reg_copy$slope)[1]
  max_slope <- theil_sen_reg_copy[max_index,]
  
  dea_fc <- read.csv(paste0(directory, 'Input_DataSet_', max_slope$site_location_name, '.csv'))
  dea_fc$time <- as.Date(dea_fc$time)
  
  p <- ggplot(data = dea_fc) + geom_line(mapping = aes(x = time, y = pv_filter)) +
    geom_abline(intercept = max_slope$intercept[1], slope = max_slope$slope[1], color = 'red') +
    ylim(c(0,100)) + ggtitle(paste0(max_slope$site_location_name, ' slope = ', round(max_slope$slope[1],7), '%/u'))
  
  
  
  theil_sen_reg_copy <- theil_sen_reg_copy[-max_index,]
}


boxplot(theil_sen_reg$slope)
hist(theil_sen_reg$slope)




