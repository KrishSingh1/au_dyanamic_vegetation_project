## ExtractBoundariesFromAusPlots
## By Krish Singh
## 2024-06-15

# IMPORTS -----------------------------------------------------------------

library(sf)
library(sfheaders)


# FUNCTIONS ---------------------------------------------------------------

calculateEastingNorthing <- function(site_row, epsg_initial = '283'){

  
  siteEPSG <- as.numeric(paste0(epsg_initial, as.character(site_row$estimated_mga)))
  # print(siteEPSG)
  # check if the easting/northing is avaliable first, 
  #   if na then use the SW latlon point to convert to easting/northing 
  
  point <- st_sfc(st_point(c(site_row$site.info.longitude[1],site_row$site.info.latitude[1])), crs = 4326)
  point <- st_transform(point, siteEPSG, allow_ballpark = FALSE)
  
  eastings <- st_coordinates(point)
  
  site_row$pit_marker_easting_calc <- eastings[,1]
  site_row$pit_marker_northing_calc <- eastings[,2]

  return(site_row)
}


# MAIN --------------------------------------------------------------------

# The grid starts from 108 degree E to 156 degree E in increments of 6 degrees approximately
lower <- seq(108,150,6)
upper <- seq(114,156,6)
mga_zone <- 49:56
mga_zone_ref <- data.frame('mga' = mga_zone, 'lower' = lower, 'upper' = upper)

site_info <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv',
                      header = TRUE, row.names = 1)

essentials_attributes <- c('site.info.site_location_name',
                           'site.info.latitude',
                           'site.info.longitude',
                           'site.info.point',
                           'site.info.pit_marker_easting',
                           'site.info.pit_marker_northing',
                           'site.info.pit_marker_datum',
                           'site.info.pit_marker_mga_zones')

site_info_essential <- site_info[,essentials_attributes]


# MGA differences ---------------------------------------------------------

estimated_mga <- c()
# Loop through all longitudes and assign the associated sites into their MGA zone
for(lon in site_info_essential$site.info.longitude) {
  fits <- FALSE
  i <- 1
  while(fits == FALSE){
    if(lon > mga_zone_ref$lower[[i]] & lon < mga_zone_ref$upper[[i]]) {
      fits <- TRUE
      estimated_mga <- c(estimated_mga, mga_zone_ref$mga[[i]])
    }
    i <- i + 1
  }
}

site_info_essential['estimated_mga'] <- estimated_mga

# test if there are contradictions 
test_validity <- site_info_essential[!is.na(site_info_essential$site.info.pit_marker_mga_zones),]
test_validity['difference'] <- test_validity$site.info.pit_marker_mga_zones - test_validity$estimated_mga

print(table(test_validity['difference'])) # quick scan of the differences 

test_validity_contradictions <- test_validity[which(test_validity['difference'] != 0),]

### Some MGA differences 
# 	WAACEK0003 -> 51 not 49
#   QDABBS0008 -> 56 not 55
#   SATFLB0002 -> 54 not 53
#   SATFLB0003 -> 54 not 53
#   SATFLB0004 -> 54 not 53 
#   SATFLB0006 -> 54 not 53
#   ...
#   SATFLB0009 -> 53 not 54


print(table(site_info_essential['site.info.point'])) # important: check if all points are in SW


# Check differences between AusPlots info easting/northing coordinates --------

# Test AusPlots against given plot coordinates (All_Plot_Points_May_2023):
ausplots_shape_data <- st_read("../DATASETS/AusPlots_Location/All_Plot_Points_May_2023_Shapefile/All_Plot_Points_May_2023_R_Readable.shp", quiet = TRUE)
ausplots_shape_data_sw <- subset(ausplots_shape_data, subset = (point == 'SW')) # subset for all 'SW' points
essential_features_sw <- c('Name', 'point', 'easting', 'northing', 'latitude', 'longitude', 'zone')
test_consistency <- merge(ausplots_shape_data_sw[,essential_features_sw], site_info_essential, by.x = 'Name', by.y = 'site.info.site_location_name')
test_consistency <- test_consistency[!is.na(site_info_essential['site.info.pit_marker_easting']),]

test_consistency['easting_error_consistency'] <- abs(test_consistency[['site.info.pit_marker_easting']] -
                                                    test_consistency[['easting']])
test_consistency['northing_error_consistency'] <- abs(test_consistency[['site.info.pit_marker_northing']] -
                                                       test_consistency[['northing']])
test_consistency['distance_error'] <- sqrt(test_consistency[['easting_error_consistency']]^2 + 
                                             test_consistency[['northing_error_consistency']])

# SAAKAN0011's recorded easting and northing is actually a lon/lat

# project the coordinates from lon/lat to easting/northing using AusPlots site info --------
# And calculating the error from AusPlots easting/northing, and from the given all_plot_points dataset 

# Perform projection
site_info_essential_calc <- t(data.frame(row.names = colnames(site_info_essential)))
for(i in 1:nrow(site_info_essential)){
  site_info_essential_calc <- rbind(site_info_essential_calc,
                                    calculateEastingNorthing(site_info_essential[i,]))
}
test_consistency_2 <- merge(ausplots_shape_data_sw[,essential_features_sw], site_info_essential_calc, by.x = 'Name', by.y = 'site.info.site_location_name')
test_consistency_2 <- test_consistency_2[!is.na(site_info_essential_calc['site.info.pit_marker_easting']),]
check_north_east_error <- test_consistency_2[!is.na(site_info_essential_calc['site.info.pit_marker_easting']),]

# Calc difference from the file coordinates versus the performed projection
check_north_east_error['east_difference'] <- round(abs(
  check_north_east_error[['easting']] - 
    check_north_east_error[['pit_marker_easting_calc']]))

check_north_east_error['north_difference'] <- round(abs(
  check_north_east_error[['northing']] - 
    check_north_east_error[['pit_marker_northing_calc']]))

check_north_east_error['distance_error'] <- sqrt((check_north_east_error[['east_difference']]^2 +
                                                   check_north_east_error[['north_difference']]^2))

# Calc difference from the ausplots site location versus the performed projection
check_north_east_error['east_difference_from_pit'] <- round(abs(
  check_north_east_error[['site.info.pit_marker_easting']] - 
    check_north_east_error[['pit_marker_easting_calc']]))

check_north_east_error['north_difference_from_pit'] <- round(abs(
  check_north_east_error[['site.info.pit_marker_northing']] - 
    check_north_east_error[['pit_marker_northing_calc']]))

check_north_east_error['distance_error_from_pit'] <- sqrt((check_north_east_error[['east_difference_from_pit']]^2 +
                                                    check_north_east_error[['north_difference_from_pit']]^2))

check_north_east_error <- unique(check_north_east_error) # remove duplicate entries 

# Print the summary for both methods
print(summary(round(check_north_east_error[['distance_error']])))
print(summary(round(check_north_east_error[['distance_error_from_pit']])))

print(nrow(unique(check_north_east_error[which(check_north_east_error[['distance_error_from_pit']] > 20), 'Name'])))
print(nrow(unique(check_north_east_error[which(check_north_east_error[['distance_error']] > 20), 'Name'])))

## Select Error-likely Sites 

error_likely <- unique(check_north_east_error[which(check_north_east_error[['distance_error_from_pit']] > 20), 'Name'])
error_likely_name <- error_likely[['Name']]
old_site_list <- read.csv('../STEP2_FC_EXTRACTION/query/sites_info_query.csv', header = T, row.names = 1)

# From this, there are 

# Calc difference from the ausplots site location versus the performed projection
check_north_east_error['east_difference_from_pit_query'] <- round(abs(
  old_site_list[['site.info.pit_marker_easting']] - 
    check_north_east_error[['pit_marker_easting_calc']]))

check_north_east_error['north_difference_from_pit_query'] <- round(abs(
  old_site_list[['site.info.pit_marker_northing']] - 
    check_north_east_error[['pit_marker_northing_calc']]))

check_north_east_error['distance_error_from_pit_query'] <- sqrt((check_north_east_error[['east_difference_from_pit_query']]^2 +
                                                             check_north_east_error[['north_difference_from_pit_query']]^2))




length(intersect(error_likely_name, unique(old_site_list[['site.info.site_location_name']]))) ==  length(intersect(unique(old_site_list[['site.info.site_location_name']]), error_likely_name))

setequal(intersect(error_likely_name, unique(old_site_list[['site.info.site_location_name']])), 
         intersect(unique(old_site_list[['site.info.site_location_name']]), error_likely_name))

error_likely_name

