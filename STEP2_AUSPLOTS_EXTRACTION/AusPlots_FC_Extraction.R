### AusPlots FC Queries ###
## Krish Singh
## 20231215
## To obtain ausplots fractional cover iteratively. 


# Libraries ---------------------------------------------------------------

library(ausplotsR) # 2.0.6


# Functions ---------------------------------------------------------------

create_error_fc_data <- function(site.info, error){
  
  one.veg.fc <- data.frame(site_unique = site.info$site_unique,
                           green = rep(NA, nrow(site.info)), 
                           brown = rep(NA, nrow(site.info)),
                           bare = rep(NA, nrow(site.info)),
                           other = rep(NA, nrow(site.info)),
                           error = rep(error, nrow(site.info)))
  return(one.veg.fc)
}

# iterating fractional cover on each site individually
get_individual_fcs <- function(site.names){
  
  fc <- data.frame(site_unique = NA, green = NA,
                   brown = NA, bare = NA,other = NA,
                   error = NA)

  for(name in site.names) {
    error <- 0 # Error 0: No error
    one.veg.info <- get_ausplots(name, veg.PI = T, site_info = T)
    if(nrow(one.veg.info$veg.PI) == 0) {
      error <- 1 # Error 1: missing veg.PI 
      one.veg.fc <- create_error_fc_data(one.veg.info$site.info, error)
    } else {
      one.veg.fc <- fractional_cover(one.veg.info$veg.PI) # retrieve fc data
      if(is.character(one.veg.fc)){ # empty fc dataset is in the form of an empty character
        error <- 2 # Error 2: fractional_cover returns empty character 
        one.veg.fc <- create_error_fc_data(one.veg.info$site.info, error)
      } else {
        one.veg.fc$error <- rep(error, nrow(one.veg.fc)) 
        
        # Check if all columns (exclude 'other') are present; 'site_unique' 'bare', 'green', 'brown' 
        columns.diff <- setdiff(c("site_unique", "bare", "green", "brown", "other", "error"), colnames(one.veg.fc))
        # When we get partially missing data - either one or more of bare, green, or brown is missing 
        if(length(columns.diff) > 0){
          error <- 4 # Error 4: Site obtains partially missing fractional cover data column (eg. missing one of or more between bare, green, brown)
          for(i in columns.diff) {
            one.veg.fc[i] <- rep(NA, nrow(one.veg.fc))
          }
          one.veg.fc$error <- rep(error, nrow(one.veg.fc)) # change error to 4 for entire subset
        } 
        
        one.veg.fc <- one.veg.fc[,c("site_unique", "green", "brown", "bare", "other", "error")]
        # check for missing FC for site observations 
        missing.obs <- setdiff(one.veg.info$site.info$site_unique, one.veg.fc$site_unique)
        if(length(missing.obs) > 0){
          error <- 3 # Error 3: site obtains partially missing fractional cover data row-wise (missing observations entirely)
          temp <- create_error_fc_data(
            subset(one.veg.info$site.info,subset = (site_unique %in% missing.obs)), error)
          one.veg.fc <- rbind(one.veg.fc, temp)
        }
      }
    }
    fc <- rbind(fc, one.veg.fc)
  }
  fc <- fc[-1,]
  rownames(fc) <- 1:nrow(fc)
  return(fc)
}


# Main --------------------------------------------------------------------


sites <- read.csv("../DATASETS/sites_info_query.csv")
site.names <- sites$site_location_name
fcs <- get_individual_fcs(site.names)

write.csv(fcs,'../DATASETS/AusPlots_FC_Iter_2_0_6.csv')

# Test --------------------------------------------------------------------

sites.sub <- read.csv("../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv")
site.names.sub <- unique(sites.sub$site_location_name)
fcs.sub <- get_individual_fcs(site.names.sub)

write.csv(fcs.sub,'../DATASETS/AusPlots_Sub_FC_Iter_2_0_6.csv')


