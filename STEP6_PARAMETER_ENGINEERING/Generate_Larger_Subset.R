##### Generate Larger Subset #####
## Krish Singh
## 20240124


# Library -----------------------------------------------------------------

library(data.table)
library(dplyr)

# Functions ---------------------------------------------------------------

# To get random sample 
get_subset <- function(seed, site.classify, size = 30) {
  set.seed(seed)
  site.subset <- site.classify %>% slice_sample(by = 'vegetation_type', n = size, replace = F)
}


# Main --------------------------------------------------------------------

seed <- 2024
site.classify <- fread('../DATASETS/AusPlots_Sites_Classified_2-0-6.csv')
site.subset <- get_subset(seed, site.classify) # get random sample 
write.csv(site.subset, '../DATASETS/Sites_Bigger_Subset_20240124/ausplots_bigger_subset.csv')


