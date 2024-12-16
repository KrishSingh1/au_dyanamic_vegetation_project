##### Statistical Analysises #####
## 25-09-2024
## Krish Singh




# Libraries ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(flextable)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------


# Load the main datasets 
theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')

# Vegetation type 
veg_type <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv')



# Bioclimatic regions
Broader_Classifications = c('Tropical/Savanna', 
                            'Tropical/Savanna', 'Temp/Med', 'Temp/Med','Temp/Med','Desert')
bioclimatic <- read.csv('../DATASETS/Australian Bioclimatic Regions/AusPlots_BioclimaticRegion_Classified.csv')
bioclimatic$bioclimatic_region <- unlist(lapply(bioclimatic$Group_Code, FUN = function(x){
  return(Broader_Classifications[x])
}))
colnames(bioclimatic)[1] <- 'site_location_name'


# Combine the datasets
theil_sen_reg <- theil_sen_reg %>%
  left_join(veg_type, by = 'site_location_name') %>%
  left_join(bioclimatic, by = 'site_location_name')


# Test 1 ------------------------------------------------------------------
# Test for a difference in mean between each fraction 

aov_test <- theil_sen_reg %>% 
  pivot_longer(cols = c("pv_filter_slope", "npv_filter_slope", "bs_filter_slope"), 
               names_to = "fraction_type",
               values_to = "slope_value")

res.aov <- aov(slope_value ~ fraction_type, data = aov_test)
summary(res.aov)

word <- tidy(res.aov)
df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")
writeClipboard(df_str)



## Test which ones are different
TukeyHSD(res.aov)

perm <- RVAideMemoire::perm.anova(slope_value ~ fraction_type, data = aov_test, nperm = 1000)
perm
summary(perm)

word <- tidy(TukeyHSD(res.aov))
df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")
writeClipboard(df_str)

to_clipboard <- function(table){
  word <- tidy(table)
  df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")
  writeClipboard(df_str)
}

# Test 2 ------------------------------------------------------------------
# Test for a difference in mean for each vegetation type in their respective fractional type

# For PV:

res.aov <- aov(pv_filter_slope ~ vegetation_type, data = theil_sen_reg)
summary(res.aov)

word <- tidy(res.aov)
word
df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")

writeClipboard(df_str)


to_clipboard(TukeyHSD(res.aov))
TukeyHSD(res.aov)


# For NPV:

res.aov <- aov(npv_filter_slope ~ vegetation_type, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# BS:

res.aov <- aov(bs_filter_slope ~ vegetation_type, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# Grab the means:

means_pv <- theil_sen_reg %>%
  group_by(vegetation_type) %>%
  summarize(pv_mean = mean(pv_filter_slope_yr), .groups = 'drop')

means_npv <- theil_sen_reg %>%
  group_by(vegetation_type) %>%
  summarize(npv_mean = mean(npv_filter_slope_yr), .groups = 'drop')

means_bs <- theil_sen_reg %>%
  group_by(vegetation_type) %>%
  summarize(bs_mean = mean(bs_filter_slope_yr), .groups = 'drop')

mean_combined <- means_pv %>%
  left_join(means_npv, by = 'vegetation_type') %>%
  left_join(means_bs, by = 'vegetation_type')



# Test 3 ------------------------------------------------------------------
# Test for a difference in means between aggregated bioclimatic groups


# For PV:

res.aov <- aov(pv_filter_slope ~ bioclimatic_region, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# For NPV:

res.aov <- aov(npv_filter_slope ~ bioclimatic_region, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# BS:

res.aov <- aov(bs_filter_slope ~ bioclimatic_region, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# Grab the means:

means_pv <- theil_sen_reg %>%
  group_by(bioclimatic_region) %>%
  summarize(pv_mean = mean(pv_filter_slope_yr), .groups = 'drop')

means_npv <- theil_sen_reg %>%
  group_by(bioclimatic_region) %>%
  summarize(npv_mean = mean(npv_filter_slope_yr), .groups = 'drop')

means_bs <- theil_sen_reg %>%
  group_by(bioclimatic_region) %>%
  summarize(bs_mean = mean(bs_filter_slope_yr), .groups = 'drop')

mean_combined <- means_pv %>%
  left_join(means_npv, by = 'bioclimatic_region') %>%
  left_join(means_bs, by = 'bioclimatic_region')









