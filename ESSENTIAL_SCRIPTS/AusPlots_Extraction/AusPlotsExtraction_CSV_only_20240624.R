## Extract Boundaries From AusPlots
## By Krish Singh
## 2024-06-18


# Import ------------------------------------------------------------------

library(sf)
library(sfheaders)

# FUNCTION ----------------------------------------------------------------

calculateEastingNorthing <- function(site_row, epsg_initial = '283'){
  
  siteEPSG <- as.numeric(paste0(epsg_initial, as.character(site_row$estimated_mga)))
  
  point <- st_sfc(st_point(c(site_row$site.info.longitude[1],site_row$site.info.latitude[1])), crs = 4326)
  point <- st_transform(point, siteEPSG, allow_ballpark = FALSE)
  
  eastings <- st_coordinates(point)
  
  site_row$SW_easting_calc <- eastings[,1]
  site_row$SW_northing_calc <- eastings[,2]
  
  lower_bounds <- c(site_row$SW_easting_calc, site_row$SW_northing_calc)
  upper_bounds <- lower_bounds + 100
  
  return(site_row)
}



# Main --------------------------------------------------------------------

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


# Create Boundary Boxes ---------------------------------------------------

site_info_essential_polygon <- unique(site_info_essential[,c('site.info.site_location_name',
                                                             'site.info.latitude',
                                                             'site.info.longitude',
                                                             'estimated_mga')])
site_info_essential_calc <- c()
for(i in 1:nrow(site_info_essential_polygon)){
  site_info_essential_calc <- rbind(site_info_essential_calc,
                                    calculateEastingNorthing(site_info_essential_polygon[i,]))
}

site_completed <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/completed_AusPlots_dataset_2-0-6.csv')
site_sw_points_completed <- subset(site_info_essential_calc, subset = (site.info.site_location_name %in% site_completed$site_location_name))
colnames(site_sw_points_completed) <- c('site_location_name', 'latitude', 'longitude', 'mga', 'SW_easting', 'SW_northing') 
rownames(site_sw_points_completed) <- 1:nrow(site_sw_points_completed)
write.csv(site_sw_points_completed, '../DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Merged_Polygons/AusPlots_Merged_Completed.csv')



