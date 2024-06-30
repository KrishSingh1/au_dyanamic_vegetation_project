


# Libraries ---------------------------------------------------------------


library(sf)
library(sfheaders)


# Functions ---------------------------------------------------------------

get_corner_points <- function(site_coords) {
  
  # perform operations to get corners 
  site_coords$sum_sw_ne <- with(site_coords,x + y)
  site_coords$sum_se_nw <- with(site_coords, x - y)
  
  SW <- which.min(site_coords$sum_sw_ne)
  NE <- which.max(site_coords$sum_sw_ne)
  NW <- which.min(site_coords$sum_se_nw)
  SE <- which.max(site_coords$sum_se_nw)
  print(site_coords)
  
  corner <- site_coords[c(SW,SE,NE,NW),] # get corner rows 
  corner['point'] <- c('SW', 'SE', 'NE', 'NW')
  print(corner)
  
  return(corner)
}




# Main --------------------------------------------------------------------

files <- list.files('../DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent', pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

grouped <- c()
for(i in files) {

  site_dea_fc <- read.csv(paste0('../DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent/', i))
  
  coordinates <- unique(site_dea_fc[,c('x','y')])
  four_corners <- get_corner_points(unique(site_dea_fc[,c('x','y')]))
  SW <- as.numeric(four_corners[four_corners['point'] == 'SW', c('x', 'y')])
  SE <- as.numeric(four_corners[four_corners['point'] == 'SE', c('x', 'y')])
  NE <- as.numeric(four_corners[four_corners['point'] == 'NE', c('x', 'y')])
  NW <- as.numeric(four_corners[four_corners['point'] == 'NW', c('x', 'y')])
  
  polygon <- st_sfc(st_polygon(list(rbind(SW, SE, NE, NW, SW))), crs = 3577)
  site_row_sf <- st_as_sf(data.frame(site_location_name = i,polygon),
                          geometry = polygon)
  site_row_sf <- st_transform(site_row_sf, crs = 4326)
  grouped <- rbind(grouped, site_row_sf)
}


st_write(grouped, "../STEP3_VALIDATE_DEA_DATA/New_Batch_Shapefile/DEA_Returned_Boundaries_New.shp")


# For pointed datasets:

grouped_point <- c()
for(i in files) {
  
  site_dea_fc <- read.csv(paste0('../DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent/', i))
  
  coordinates <- unique(site_dea_fc[,c('x','y')])
  coordinates['x'] <- as.numeric(coordinates$x)
  coordinates['y'] <-  as.numeric(coordinates$y)
 
  site_row_sf <- st_as_sf(coordinates, coords = c("x","y"), crs = 3577)
  site_row_sf <- st_transform(site_row_sf, crs = 4326)
  grouped_point <- rbind(grouped_point, site_row_sf)
}


st_write(grouped_point, "../STEP3_VALIDATE_DEA_DATA/New_Batch_Shapefile/DEA_Returned_Point_New.shp")


