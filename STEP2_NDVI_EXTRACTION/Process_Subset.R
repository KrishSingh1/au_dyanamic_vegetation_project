library(dplyr)

site.sub <- read.csv('Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
site.locations <- site.sub[,c('site_location_name', 'longitude', 'latitude')]
site.sub.distinct <- site.locations %>% distinct() # remove duplicate
length(unique(site.sub.distinct$site_location_name)) == nrow(site.sub.distinct) # check if the number of rows equate to the number of unique sites 


write.csv(site.sub.distinct, file = "site_subset_lat_lon.csv", row.names = F)

 