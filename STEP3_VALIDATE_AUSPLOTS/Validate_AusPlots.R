#### Validate AusPlots ###
## Author: Krish Singh
## Date: 20231208
## Purpose: To check for consistency of the fractional_cover calculations 
##           between batch and iterative retrieval 


# Libraries ---------------------------------------------------------------

library(ausplotsR)

# Algorithm ---------------------------------------------------------------

# Output: Sets of fractional cover of various site observations 
# Input: Name of site locations 
# Process:
# - Have two different lists of site location names 
#   - One with all sites of interest (n = 713)
#   - One with subset of the sites of interest (n = 12)
# - Retrieve site information for each of these list 
#  - The full data set with all sites of interest to be retrieved by batch
#  - The subset to be retrieved by batch and iteration 
# - Calculate FC for set of site information  
# - Compare the retrieved FC for these sites 



# Functions ---------------------------------------------------------------


# Test by iterating fractional cover on each site individually

get_individual_fcs <- function(site.names){
  
  one.veg.info <- get_ausplots(site.names[1], veg.PI = T, site_info = T)
  one.veg.fcs <- fractional_cover(one.veg.info$veg.PI)
  one.veg.fcs <- one.veg.fcs[,c("site_unique", "green", "brown", "bare")]
  empty.locations <- c()
  
  for(name in site.names[-1]) {
    one.veg.info <- get_ausplots(name, veg.PI = T, site_info = T)
    temp <- fractional_cover(one.veg.info$veg.PI)
    if(is.null(nrow(temp))) {
      empty.locations <- c(empty.locations, name)
    } else {
      temp <- temp[,c("site_unique", "green", "brown", "bare")]
      one.veg.fcs <- rbind(one.veg.fcs, temp)
    }
  }
  
  return(list(fcs = one.veg.fcs, missing_fc = empty.locations))
}

#
get_fc_retrieval_methods <- function(subset.names, all.veg.fc, one.veg.fcs){
  
  subset.veg.info <- get_ausplots(subset.names,
                                  veg.PI = T, site_info = T)
  subset.veg.fc <- fractional_cover(subset.veg.info$veg.PI)
  one.veg.fcs <- get_individual_fcs(subset.names)
  one.veg.fcs <- one.veg.fcs$fcs
  
  all.veg.fc.test <- subset(all.veg.fc, subset = site_unique %in% subset.veg.fc$site_unique)
  all.veg.fc.test <- merge(all.veg.fc.test[,c('site_unique', 'green')],
                           subset.veg.fc[,c('site_unique', 'green')], by = 'site_unique')
  all.veg.fc.test <- merge(all.veg.fc.test,
                           one.veg.fcs[,c('site_unique', 'green')], by = 'site_unique')
  
  return(list(fcs = all.veg.fc.test, missing_fc = one.veg.fcs$missing_fc))
}


# Main --------------------------------------------------------------------

# Obtain site names 

all.sites <- read.csv("../STEP1_INFO_EXRACTION/sites_info_query.csv")
all.names <- all.sites$site_location_name

subset.sites <- read.csv("../STEP4_EXPORE_DATA/sites_subset.csv")
subset.names <- unique(subset.sites$site_location_name)

# Retrieve AusPlots Data

# I saved the data to save time uncomment below to regenerate dataset
#all.veg.info <- get_ausplots(all.names, veg.PI = T, site_info = T)
#saveRDS(all.veg.info, 'all_veg_info.RDS')
all.veg.info <- readRDS('../STEP3_VALIDATE_AUSPLOTS/all_veg_info.RDS')

subset.veg.info <- get_ausplots(subset.names, veg.PI = T, site_info = T)
saveRDS(subset.veg.info, 'subset_veg_info.RDS')
subset.veg.info <- readRDS('subset_veg_info.RDS')

# Calculate Fractional Cover 

# I saved the data to save time uncomment below to regenerate dataset
#all.veg.fc <- fractional_cover(all.veg.info$veg.PI)
#saveRDS(all.veg.fc, 'all_veg_fc.RDS')
all.veg.fc <- readRDS('../STEP3_VALIDATE_AUSPLOTS/all_veg_fc.RDS')
subset.veg.fc <- fractional_cover(subset.veg.info$veg.PI)
one.veg.fcs <- get_individual_fcs(subset.names)
one.veg.fcs <- one.veg.fcs$fcs


# Test for differences 

all.veg.fc.test <- subset(all.veg.fc, subset = site_unique %in% subset.veg.fc$site_unique)
all.veg.fc.test <- merge(all.veg.fc.test[,c('site_unique', 'green')],
                         subset.veg.fc[,c('site_unique', 'green')], by = 'site_unique')
all.veg.fc.test <- merge(all.veg.fc.test,
                         one.veg.fcs[,c('site_unique', 'green')], by = 'site_unique')



# Test with other possible subsets 

subset.names.2 <- sample(all.names, size = 20)
test.2 <- get_fc_retrieval_methods(subset.names.2, all.veg.fc)


# Sites with potential bugs  

# NSABHC0016
#  [1] "QDAMUL0002" "NSABHC0014" "QDAMUL0001" "QDAMGD0017" "NTASSD0018" "QDAMGD0008"
# [7] "QDAMGD0009" "QDAMGD0014" "QDAMGD0024" "QDAMGD0003"
# SASMDD0010


one.veg.info <- get_ausplots("NSABHC0016", veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# NSABHC0016 returns no fractional cover 
# But was returbed for all.veg.fc

one.veg.info <- get_ausplots('QDAMUL0002', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMUL0002 returns no fractional cover 
# But was returned for all.veg.fc 

one.veg.info <- get_ausplots('NSABHC0014', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# NSABHC0014 returns no fractional cover 
# But was returned for all.veg.fc 

one.veg.info <- get_ausplots('QDAMUL0001', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMUL0001 returns no fractional cover 
# But was returned for all.veg.fc 

one.veg.info <- get_ausplots('QDAMGD0017', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMGD0017 returns no fractional cover 
# but was returned for all.veg.fc

one.veg.info <- get_ausplots('NTASSD0018', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# NTASSD0018 returns no fractional cover 
# but was returned for all.veg.fc

one.veg.info <- get_ausplots('QDAMGD0008', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMGD0008 returns no fractional cover 
# but was returned for all.veg.fc

one.veg.info <- get_ausplots('QDAMGD0009', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMGD0009 returns no fractional cover 
# but was returned for all.veg.fc

one.veg.info <- get_ausplots('QDAMGD0024', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMGD0024 returns no fractional cover 
# but was returned for all.veg.fc

one.veg.info <- get_ausplots('QDAMGD0003', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# QDAMGD0003 returns no fractional cover 
# but was returned in all.veg.fc

one.veg.info <- get_ausplots('SASMDD0010', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# no veg.pi was returned 
# Error in if (is.na(temp$growth_form) | temp$in_canopy_sky %in% TRUE &&  : 
# argument is of length zero


one.veg.info <- get_ausplots(c('SASMDD0010',
                              'NSABHC0016',
                              'NSABHC0014',
                              'QDAMUL0001',
                              'QDAMGD0017',
                              'NTASSD0018',
                              'QDAMGD0008',
                              'QDAMGD0009',
                              'QDAMGD0024',
                              'QDAMGD0003'), veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)


#### Unique site observations is missing from after fractional_cover()

setdiff(all.veg.info$site.info$site_unique, all.veg.fc$site_unique)
#  [1] "QDASEQ0001-58722" "QDASSD0010-57630" "SAAFLB0003-58636" "SAAFLB0004-58611" "SAAFLB0030-57075"
#  [6] "SAAFLB0031-57076" "SASMDD0003-57065" "SASMDD0008-57062" "SASMDD0009-57063" "SASMDD0010-57064"
#  [11] "SASMDD0012-58020"[]

# test what output querying these ^ sites ^ does 

one.veg.info <- get_ausplots('QDASEQ0001', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# produced QDASEQ0001-58960 but not QDASEQ0001-58722

one.veg.info <- get_ausplots('QDASSD0010', veg.PI = T, site_info = T)
fractional_cover(one.veg.info$veg.PI)
# produced QDASEQ0001-58960 but not QDASEQ0001-58722

