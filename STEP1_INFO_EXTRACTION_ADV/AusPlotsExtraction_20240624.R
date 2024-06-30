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
  
  box <- st_as_sfc(st_bbox(st_sfc(st_point(lower_bounds),
                                  st_point(upper_bounds),
                                  crs = siteEPSG)))
  box <- st_transform(box, 4326, allow_ballpark = FALSE)
  site_row_sf <- st_as_sf(site_row, geometry = box)
  
  return(site_row_sf)
}


# Get corner points for each site from a df with site names and a list of coordinates 
get_corner_points <- function(site_coords) {
  
  # perform operations to get corners 
  Coordinates <- as.data.frame(st_coordinates(site_coords))

  Coordinates$sum_sw_ne <- with(Coordinates,X + Y)
  Coordinates$sum_se_nw <- with(Coordinates, X - Y)
    
  SW <- which.min(Coordinates$sum_sw_ne)
  NE <- which.max(Coordinates$sum_sw_ne)
  NW <- which.min(Coordinates$sum_se_nw)
  SE <- which.max(Coordinates$sum_se_nw)
  print(Coordinates)
    
  corner <- site_coords[c(SW,SE,NE,NW),] # get corner rows 
  corner['point'] <- c('SW', 'SE', 'NE', 'NW')
  print(corner)
  
  return(corner)
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

site_info_aligned <- site_info[which(site_info['site.info.plot_is_aligned_to_grid'] == TRUE),]
aligned_sites_list <- unique(site_info_aligned[['site.info.site_location_name']])

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

site_info_essential_polygon_aligned <- subset(site_info_essential_polygon, 
                                              subset = (site.info.site_location_name %in%
                                                          aligned_sites_list))

# Perform Projection ------------------------------------------------------
site_info_essential_calc <- c()
for(i in 1:nrow(site_info_essential_polygon_aligned)){
  site_info_essential_calc <- rbind(site_info_essential_calc,
                                    calculateEastingNorthing(site_info_essential_polygon_aligned[i,]))
}


# Export Projection -------------------------------------------------------

#st_write(site_info_essential_calc, "../DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Grid_Aligned/AusPlots_Grid_Aligned_Polygons.shp")

# Merge With kmz file -----------------------------------------------------

# For sites that are not aligned to grid or not a 100m x 100m (i.e. a 50x200, 200x50) dimension, I will rely on
# the given .kmz file provided by the AusPlots team 

site_info_not_aligned <- site_info[which(site_info['site.info.plot_is_aligned_to_grid'] == FALSE),]
not_aligned_sites_list <- unique(site_info_not_aligned[['site.info.site_location_name']])

site_info_shp <- st_read('../DATASETS/AusPlots_Location/All_Plot_Points_May_2023_Shapefile/All_Plot_Points_May_2023_R_Readable.shp')
site_info_shp_not_aligned <- subset(site_info_shp, subset = (Name %in% not_aligned_sites_list))

# Check if there are at least the corner points per site
print(table(site_info_shp_not_aligned[['Name']]))

# NSABHC0018 only has one point

sufficient_points <- names(table(site_info_shp_not_aligned[['Name']])[table(site_info_shp_not_aligned[['Name']]) >= 4])

site_info_shp_not_aligned_sufficient <- subset(site_info_shp_not_aligned, subset = (Name %in% sufficient_points))
essential_points <- c('SW', 'SE', 'NE', 'NW')

# There is one incorrectly labelled site (sw, nw, etc.) in the kmz file, 'QDASEQ0001', deal separately
incorrect_site_label <- c('QDASEQ0001')
not_aligned_polygons <- c()

for(site in sufficient_points){
  
  if(site %in% incorrect_site_label) { # deal all incorrectly labelled sites by relabelling the corner points
    site_points <- subset(site_info_shp_not_aligned_sufficient,subset = (Name == site))
    site_4_points <- get_corner_points(site_points)
    
    print(get_corner_points(site_points))
  } else {
    site_4_points <- subset(site_info_shp_not_aligned_sufficient,
                            subset = (
                              (Name == site) & 
                              (point %in% essential_points)
                            ))
  }
  
  SW <- st_coordinates(subset(site_4_points, subset = (point == 'SW')))[,c('X','Y')]
  SE <- st_coordinates(subset(site_4_points, subset = (point == 'SE')))[,c('X','Y')]
  NE <- st_coordinates(subset(site_4_points, subset = (point == 'NE')))[,c('X','Y')]
  NW <- st_coordinates(subset(site_4_points, subset = (point == 'NW')))[,c('X','Y')]
  
  polygon <- st_sfc(st_polygon(list(rbind(SW, SE, NE, NW, SW))), crs = 4326)  # extra SW to close the polygon
  SW_row <- subset(site_4_points, subset = (point == 'SW'))[c('Name', 'longitude', 'latitude', 'point')]
  site_row_sf <- st_as_sf(data.frame(SW_row),
                          geometry = polygon)
  
  not_aligned_polygons <- rbind(not_aligned_polygons, site_row_sf)
}


# Export Non-aligned Polygons ---------------------------------------------

#st_write(not_aligned_polygons, "../DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Nongrid_Aligned/AusPlots_Nongrid_Aligned_Polygons.shp")

# Merge the two created polygons ------------------------------------------

# Rename column names of both polygons to allow for merging
adjust_cols_not_aligned <- not_aligned_polygons
colnames(adjust_cols_not_aligned)[1] <- 'site_location_name'

site_info_essential_calc_to_merge <- site_info_essential_calc[,1:3]
site_info_essential_calc_to_merge['point'] <- rep('SW', nrow(site_info_essential_calc_to_merge))
site_info_essential_calc_to_merge <- site_info_essential_calc_to_merge[
  ,c('site.info.site_location_name','site.info.longitude',
     'site.info.latitude','point', 'geometry')]
colnames(site_info_essential_calc_to_merge) <- colnames(adjust_cols_not_aligned)

merged_ausplots_polygons <- rbind(site_info_essential_calc_to_merge, adjust_cols_not_aligned)

# Add on the mgas again

#n = 10
#chunks <- cut(1:nrow(merged_ausplots_polygons), breaks = n, labels = F)
#geolist <- split(merged_ausplots_polygons, chunks)

#counter = 1
#for (i in geolist){
#  fileName <- paste0("AusPlots_Merged_Polygons_", counter, ".geojson")
#  st_write(i, paste0("../DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Merged_Polygons/AusPlots_Merged_Polygons_Split/",fileName))
#  counter = counter + 1
#}


# Export Polygons as merged 

st_write(merged_ausplots_polygons, "../DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Merged_Polygons/AusPlots_Merged_Polygons_20240624.geojson")


## Now only for SW points, note: it is possible to use the polygons as an alternative
locations <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv')
site_sw_points <- unique(locations[,c('site.info.site_location_name', 'site.info.longitude', 'site.info.latitude')])
site_completed <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/completed_AusPlots_dataset_2-0-6.csv')

site_sw_points_completed <- subset(site_sw_points, subset = (site.info.site_location_name %in% site_completed$site_location_name))
colnames(site_sw_points_completed) <- c('site_location_name', 'longitude', 'latitude')
write.csv(site_sw_points_completed, '../DATASETS/AusPlots_Location/site_sw_points_completed.csv')
