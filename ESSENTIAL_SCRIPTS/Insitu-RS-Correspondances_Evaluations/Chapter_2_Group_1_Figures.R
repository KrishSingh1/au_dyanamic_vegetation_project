#### Chapter 2 Group 1 Figures ####
## KRISH sINGH
## 26-04-2024


# Libraries ----------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(cowplot)
library(dplyr)
library(ozmaps)
library(ggstatsplot)
library(RColorBrewer)
library(ggsignif)
library(grDevices)


# Functions ----------------------------------------------------------------

# Main --------------------------------------------------------------------

time_period <- ' '

if(time_period == '-2000'){
  print('Pre 2000')
  theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Pre2000.csv')
} else if (time_period == '+2000') {
  print('Post 2000')
  theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Post2000.csv')
} else if (time_period == '+2010') {
  print('Post 2010')
  theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Post2010.csv')
} else {
  print('Whole time series')
  theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
}



veg_type <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv')
Broader_Classifications = c('Tropical/Savanna', 
'Tropical/Savanna', 'Temp/Med', 'Temp/Med','Temp/Med','Desert')
bioclimatic <- read.csv('../DATASETS/Australian Bioclimatic Regions/AusPlots_BioclimaticRegion_Classified.csv')
bioclimatic$bioclimatic_region <- unlist(lapply(bioclimatic$Group_Code, FUN = function(x){
  return(Broader_Classifications[x])
}))
colnames(bioclimatic)[1] <- 'site_location_name'


# Combine the datasets
theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(veg_type, by = 'site_location_name') %>%
  left_join(bioclimatic, by = 'site_location_name')

for (i in c('pv_filter', 'npv_filter', 'bs_filter')){
  lower_sign <- sign(theil_sen_reg_copy[[paste0(i, '_slope_lower')]])
  higher_sign <- sign(theil_sen_reg_copy[[paste0(i, '_slope_higher')]])
  # Basically we want to test lower_sign == higher_sign, and if one value is not equal to 0
  theil_sen_reg_copy[paste0(i, '_signf')] <- ((lower_sign == higher_sign) &
                                                (lower_sign != 0))
}

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
    #print(slope_sign)
    
    if (row[[signf_name]] == T){
      
      if(slope_sign == 1) {
        code <- 3
      } else if (slope_sign == -1){
        code <- 1
      }
    } else {
      code <- 2
    }
    return(code)
  })
  theil_sen_reg_copy[slope_coding] <- as.factor(coding)
}

# Maps --------------------------------------------------------------------

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
p3 <- pl_bs + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none")

combined_plot <- plot_grid(p1, p2, p3, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot

title <- ggdraw() + 
  draw_label('1987-2023', fontface = 'bold', size = 14, hjust = 0.5)
final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
final_plot

# Histograms for Overall fractional cover 

pl_pv <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  labs(x = 'PV/yr', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9, bins = 40) +
  #scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2), limits = c(-0.8, 0.9)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr) + sd(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr) - sd(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  scale_y_continuous(breaks = seq(0, 150, 20), limits = c(0, 150)) +
  annotate('text', x = max(theil_sen_reg_copy$pv_filter_slope_yr) * 0.9, 
           y = 140, 
           label = paste0("x\u0305", ' = ',round(mean(theil_sen_reg_copy$pv_filter_slope_yr),4), 
                          '\ns = ', round(sd(theil_sen_reg_copy$pv_filter_slope_yr),4)), 
           color = "black", size = 3)

pl_pv


pl_npv <- ggplot(data = theil_sen_reg_copy, 
                 mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  labs(x = 'NPV/yr', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9) +
  #scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2), limits = c(-0.8, 0.9)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr) + sd(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr) - sd(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  scale_y_continuous(breaks = seq(0, 150, 20), limits = c(0, 150)) +
  annotate('text', x = max(theil_sen_reg_copy$npv_filter_slope_yr) * 0.9, 
           y = 140, 
           label = paste0("x\u0305", ' = ',round(mean(theil_sen_reg_copy$npv_filter_slope_yr),4), 
                          '\ns = ', round(sd(theil_sen_reg_copy$npv_filter_slope_yr),4)), 
           color = "black", size = 3)

pl_npv


pl_bs <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  labs(x = 'BS/yr', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9) +
  #scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2), limits = c(-0.8, 0.9)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr) + sd(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr) - sd(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  scale_y_continuous(breaks = seq(0, 150, 20), limits = c(0, 150)) +
  annotate('text', x = max(theil_sen_reg_copy$bs_filter_slope_yr) * 0.9, 
           y = 140, 
           label = paste0("x\u0305", ' = ',round(mean(theil_sen_reg_copy$bs_filter_slope_yr),4), 
                          '\ns = ', round(sd(theil_sen_reg_copy$bs_filter_slope_yr),4)), 
           color = "black", size = 3)

pl_bs

combined_plot <- plot_grid(pl_pv, pl_npv, pl_bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot

title <- ggdraw() + 
  draw_label('1987-2000', fontface = 'bold', size = 14, hjust = 0.5)
final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
final_plot

#
paste0()
d <- aggregate(pv_filter_slope_yr ~ vegetation_type,
          data = theil_sen_reg_copy, FUN = mean)

for (i in d) {
  g <- paste0(names(i, i)
} 

d$vegetation_type



# Box Plots for Vegetation Types ------------------------------------------

generate_mean_string <- function(d, group, slope) {
  if (slope == 'pv') {
    slope_name = 'pv_filter_slope_yr'
  } else if (slope == 'npv') {
    slope_name = 'npv_filter_slope_yr'
  } else if (slope == 'bs') {
    slope_name = 'bs_filter_slope_yr'
  }
  
  if (group == 'veg') {
    group_name = 'vegetation_type'
  } else if (group == 'bioclimatic') {
    group_name = 'bioclimatic_region'
  }
  
  agg <- aggregate(x = theil_sen_reg_copy[[slope_name]],
                   by = list(theil_sen_reg_copy[[group_name]]),
                   FUN = mean)
  print(agg)
  mean_str <- ''
  if (group_name == 'bioclimatic_region') {
    abrev <- substr(group_name, start = 1, stop = 3) # Abbreviate
    mean_str <- ''
    for (i in 1:nrow(agg)) {
      mean_str <- paste0(mean_str, paste0('\nx\u0305(', abrev, i, ') = ' , round(agg[i, 'x'], 4)))
      print(mean_str)
    }
  } else if (group_name == 'vegetation_type') {
    print('VEG')
    for (i in 1:nrow(agg)) {
      mean_str <- paste0(mean_str, paste0('\nx\u0305(', agg[['Group.1']][i], ') = ' , round(agg[i, 'x'], 4)))
      print(mean_str)
    }
  }
  
  return(mean_str)
}


pv_mean_str <- generate_mean_string(theil_sen_reg_copy, 'veg', 'pv')

pl_pv <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(y = pv_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'PV/yr', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, 0.2), limits = c(-0.8, 1.2)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "ab") +
  annotate("text", x = 3, y = 1, label = "b") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  annotate("text", x = 0.8, y = -0.6, label = pv_mean_str,
           hjust = 1, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")
  
pl_pv


npv_mean_str <- generate_mean_string(theil_sen_reg_copy, 'veg', 'npv')
pl_npv <- ggplot(data = theil_sen_reg_copy, 
                 mapping = aes(y = npv_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'NPV/yr', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, 0.2), limits = c(-0.8, 1.2)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "b") +
  annotate("text", x = 3, y = 1, label = "c")  +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  annotate("text", x = 0.8, y = -0.6, label = npv_mean_str,
           hjust = 1, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red") 


pl_npv

bs_mean_str <- generate_mean_string(theil_sen_reg_copy, 'veg', 'bs')
pl_bs <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(y = bs_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'BS/yr', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, 0.2), limits = c(-0.8, 1.2)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "ab") +
  annotate("text", x = 3, y = 1, label = "b") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  annotate("text", x = 0.8, y = -0.6, label = bs_mean_str,
         hjust = 1, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")

pl_bs


combined_plot <- plot_grid(pl_pv, pl_npv, pl_bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot
ggsave(combined_plot)



# Box Plots for Bioclimatic Regions ---------------------------------------

pv_mean_str <- generate_mean_string(theil_sen_reg_copy, 'bioclimatic', 'pv')
pl_pv <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(y = pv_filter_slope_yr, x =bioclimatic_region)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'PV/yr', x = 'Bioclimatic region') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2), limits = c(-1, 1.0)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "a") +
  annotate("text", x = 3, y = 1, label = "b") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  annotate("text", x = 3.5, y = -0.8, label = pv_mean_str,
           hjust = 1, size = 3) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")
  

pl_pv

npv_mean_str <- generate_mean_string(theil_sen_reg_copy, 'bioclimatic', 'npv')
pl_npv <- ggplot(data = theil_sen_reg_copy, 
                 mapping = aes(y = npv_filter_slope_yr, x =bioclimatic_region)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'NPV/yr', x = 'Bioclimatic region') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2), limits = c(-1, 1.0)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "a") +
  annotate("text", x = 3, y = 1, label = "a") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  annotate("text", x = 3.5, y = -0.8, label = npv_mean_str,
           hjust = 1, size = 3) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")

pl_npv

bs_mean_str <- generate_mean_string(theil_sen_reg_copy, 'bioclimatic', 'bs')
pl_bs <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(y = bs_filter_slope_yr, x =bioclimatic_region)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'BS/yr', x = 'Bioclimatic region') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2), limits = c(-1, 1.0)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "a") +
  annotate("text", x = 3, y = 1, label = "b") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  annotate("text", x = 3.5, y = -0.8, label = bs_mean_str,
           hjust = 1, size = 3) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")
pl_bs


combined_plot <- plot_grid(pl_pv, pl_npv, pl_bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot


# When I split the Time series into prior and after 2000 ------------------

add_slope_codes <- function(theil_sen_reg_copy) {
  
  for (i in c('pv_filter', 'npv_filter', 'bs_filter')) {
    signf_name <- paste0(i,'_signf')
    slope_name <- paste0(i, '_slope')
    slope_coding <- paste0(i, '_code')
    
    coding <- apply(theil_sen_reg_copy, MARGIN = 1, FUN = function(row){
      
      slope_sign <- sign(as.numeric(row[[slope_name]]))
      #print(slope_sign)
      
      if (row[[signf_name]] == T){
        
        if(slope_sign == 1) {
          code <- 3
        } else if (slope_sign == -1){
          code <- 1
        }
      } else {
        code <- 2
      }
      return(code)
    })
    theil_sen_reg_copy[slope_coding] <- as.factor(coding)
  }
  return(theil_sen_reg_copy)
}

get_significance <- function(theil_sen_reg_copy) {
  
  for (i in c('pv_filter', 'npv_filter', 'bs_filter')){
    lower_sign <- sign(theil_sen_reg_copy[[paste0(i, '_slope_lower')]])
    higher_sign <- sign(theil_sen_reg_copy[[paste0(i, '_slope_higher')]])
    # Basically we want to test lower_sign == higher_sign, and if one value is not equal to 0
    theil_sen_reg_copy[paste0(i, '_signf')] <- ((lower_sign == higher_sign) &
                                                  (lower_sign != 0))
  }
  return(theil_sen_reg_copy)
}


theil_sen_reg_pre <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Pre2000.csv')
theil_sen_reg_pre <- theil_sen_reg_pre %>% 
  get_significance() %>% add_slope_codes()


theil_sen_reg_post <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Post2000.csv')
theil_sen_reg_post <- theil_sen_reg_post %>%
  get_significance() %>% add_slope_codes()






# Pre 2000

sf_oz <- ozmap("states")
pl_pv_pre <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_pre, 
             mapping = aes(x = longitude, y = latitude, color = pv_filter_code),
             size= 1.2, alpha = 0.7) + 
  scale_colour_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) +
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) + coord_sf(xlim = c(114, 152), ylim = c(-10, -43)) + title(main = '1987-2000')
pl_pv_pre

# Post 2000

pl_pv_post <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_post, 
             mapping = aes(x = longitude, y = latitude, color = pv_filter_code),
             size= 1.2, alpha = 0.7) + 
  scale_colour_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) +
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) + coord_sf(xlim = c(114, 152), ylim = c(-10, -43)) + title(main = '1987-2000')
pl_pv_post
legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1)

plt_1 <- pl_pv_pre + theme(legend.position = "none", 
                           axis.text.x = element_text(angle = 45, hjust = 1))
plt_2 <- pl_pv_post + theme(legend.position = "none", 
                            axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot_grid(plt_1, plt_2, ncol = 2, labels = c("a) 1987-2000", "b) 2000-2023"))
combined_plot

plot_title <- 'Pre_Post_2000_PV.png'
dir <- "C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/Thesis/Plots_For_Thesis/Chapter 2/"
ggsave(paste0(dir, plot_title),
       width = 15, height = 10)

