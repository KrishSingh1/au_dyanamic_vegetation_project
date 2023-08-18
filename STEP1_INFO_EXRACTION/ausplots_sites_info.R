###### ausplots site coords extraction ######
## Author: Krish Karan Singh 
## Date: 230624
## Purpose: To export the easting/northing coords of each site in AusPlots as a csv file 

write = T # if you want to write a new file set to 'T' or 'TRUE'

library(ausplotsR)

my.data <- get_ausplots(veg.vouchers=FALSE,veg.PI=FALSE)
site.info <- as.data.frame(my.data$site.info)

subsetted <- !(is.na(site.info$pit_marker_easting) | 
  is.na(site.info$pit_marker_northing) |
    is.na(site.info$pit_marker_mga_zones))
  
site.info.coorded <- site.info[subsetted,]

site.info.coorded.100x100 <- subset(site.info.coorded, subset = (plot_is_100m_by_100m == T))

site.info.coorded.essen <- site.info.coorded.100x100[,c('site_location_name','pit_marker_easting',
                                                'pit_marker_northing', 'pit_marker_mga_zones', 'latitude',
                                                'longitude')]

site.info.coorded.essen <- site.info.coorded.essen[!duplicated(
  site.info.coorded.essen$site_location_name),]

con = length(unique(site.info.coorded.100x100$site_location_name)) == length(site.info.coorded.essen$site_location_name)


if (write & con) {
  write.csv(site.info.coorded.essen, "sites_info_query2_latlon.csv")
  print('File Written')
} else if (!con) {
  print('Error writing file')
} else {
  print('File not written')
}



###### END ######

## There appears to be a discrepency with the number of sites in the original query and 
##  a newer one -> merge the newer lat/lon to the original query
## The extra site is NTAPCK0003


original <- read.csv('sites_info_query.csv')
original <- original[,-1]
original.latlong <- merge(original, site.info.coorded.essen)

# t <- table(c(original$site_location_name, site.info.coorded.essen$site_location_name))
# site.extra <- t[t == 1]
#any(is.element(original$site_location_name, 'NTAPCK0003')) # 0
#any(is.element(site.info.coorded.essen$site_location_name, 'NTAPCK0003')) # 1
##


if (write & con) {
  write.csv(original.latlong, "sites_info_query_latlon.csv")
  print('File Written')
} else if (!con) {
  print('Error writing file')
} else {
  print('File not written')
}

#################



