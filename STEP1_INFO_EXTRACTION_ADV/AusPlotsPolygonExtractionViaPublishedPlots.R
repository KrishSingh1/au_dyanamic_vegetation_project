## Extract Boundaries From AusPlots
## By Krish Singh
## 2024-06-18


# Import ------------------------------------------------------------------

library(sf)
library(sfheaders)
library(dplyr)

# FUNCTION ----------------------------------------------------------------


# main --------------------------------------------------------------------


site.corners.data <- read.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv')
site.corners.data.cleaned <- site.corners.data %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(3577) %>%
  st_transform(4326)

site.corners.data.cleaned$point <- toupper(site.corners.data.cleaned$point)

not_aligned_polygons <- c()
incorrect_site_label <- c()

essential_points <- c('SW', 'SE', 'NE', 'NW')
unique_sites <- unique(site.corners.data.cleaned$site_location_name)

for(site in unique_sites){
  
  site_4_points <- site.corners.data.cleaned %>%
    subset((site_location_name == site) & (point %in% essential_points))
  
  
  SW <- st_coordinates(subset(site_4_points, subset = (point == 'SW')))[,c('X','Y')]
  SE <- st_coordinates(subset(site_4_points, subset = (point == 'SE')))[,c('X','Y')]
  NE <- st_coordinates(subset(site_4_points, subset = (point == 'NE')))[,c('X','Y')]
  NW <- st_coordinates(subset(site_4_points, subset = (point == 'NW')))[,c('X','Y')]
  
  polygon <- st_sfc(st_polygon(list(rbind(SW, SE, NE, NW, SW))), crs = 4326)  # extra SW to close the polygon
  SW_row <- subset(site_4_points, subset = (point == 'SW'))[c('site_location_name')]
  site_row_sf <- st_as_sf(data.frame(SW_row),
                          geometry = polygon)
  
  not_aligned_polygons <- rbind(not_aligned_polygons, site_row_sf)
}

# Now filter the polygons by the dea fc data we currently have
directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

not_aligned_polygons_filtered <- not_aligned_polygons %>%
  subset(site_location_name %in% file.names)

st_write(not_aligned_polygons_filtered, '../DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Derived_Boundary_Polygons/AusPlots_Polygons_from_Published_Plots.shp')


#not_aligned_polygons_filtered$area <- st_area(not_aligned_polygons_filtered)
#st_is_valid(not_aligned_polygons_filtered)
#site.corners.data.cleaned[site.corners.data.cleaned$site_location_name == 'WAAPIL0019',]
#summary(st_area(not_aligned_polygons_filtered[which(st_is_valid(not_aligned_polygons_filtered)),] ))




