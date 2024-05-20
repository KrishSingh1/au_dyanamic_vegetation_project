
# IMPORTS -----------------------------------------------------------------


library(sf)
library(sfheaders)


# FUNCTIONS ---------------------------------------------------------------


# Get corner points for each site from a df with site names and a list of coordinates 
get_corner_points <- function(coordinates) {
  
  # template df to return
  corner.points <- data.frame(Name = as.character(NA),
                              X = as.numeric(NA),
                              Y = as.numeric(NA),
                              Z = as.numeric(NA))
  
  coordinates$X <- as.numeric(coordinates$X)
  coordinates$Y <- as.numeric(coordinates$Y)
  
  # iterate through list of site names 
  site.names <- unique(coordinates$V1)
  for(n in site.names) {
    site.coords <- subset(coordinates, V1 == n) # subset by site name 
    
    # perform operations to get corners 
    site.coords$sum_sw_ne <- with(site.coords,X+Y)
    site.coords$sum_se_nw <- with(site.coords,X-Y)
    
    SW <- which.min(site.coords$sum_sw_ne)
    NE <- which.max(site.coords$sum_sw_ne)
    NW <- which.min(site.coords$sum_se_nw)
    SE <- which.max(site.coords$sum_se_nw)
    
    corner <- site.coords[c(SW,SE,NE, NW),] # get corner rows 
    corner <- corner[,c('V1', 'X', 'Y', 'Z')]
    colnames(corner)[1] <- 'Name'
    
    corner.points <- rbind(corner.points, corner) # bind to template df 
  }
  corner.points <- corner.points[-1,]
  rownames(corner.points) <- 1:nrow(corner.points)
  
  return(corner.points)
}


# MAIN --------------------------------------------------------------------


kmz_data <- st_read("../STEP1_INFO_EXTRACTION_ADV/AusPlots_Locations/doc.kml", quiet = TRUE)
# print(kmz_data)

## Sites with only one point
one.point <- table(kmz_data$Name)[table(kmz_data$Name) < 4]
single.points <- subset(kmz_data,(Name %in% names(one.point)))

veg.info <- read.csv("../DATASETS/AusPlots_Extracted_Data/extracted_site_info_2-0-6.csv")
single_corrected_polygons <- c()
missing_single_point_sites <- c()
colnames(veg.info) <- unlist(lapply(colnames(veg.info), FUN = function(x){str_split(x, '\\.')[[1]][3]}))

for (site_name in single.points$Name) {
  
  site <- subset(veg.info, subset = (site_location_name == site_name))
  if(nrow(site) == 0) {
    site <- tryCatch({
      get_ausplots(site_name)$site.info[1,]
    }, error = function(cond){
      site
    })
  }
  
  if(nrow(site) == 0){
    missing_single_point_sites <- c(missing_single_point_sites, site_name)
  } else{
    siteEPSG <- as.numeric(paste0('283',site$pit_marker_mga_zones))
    if(is.na(site$pit_marker_easting[1])) {
      point <- st_sfc(st_point(c(site$longitude,site$latitude)), crs = "WGS84")
      point <- st_transform(point, siteEPSG)
      eastings <- st_coordinates(point)
      site$pit_marker_easting <- eastings[,1]
      site$pit_marker_northing <- eastings[,2]
    }
    
    lower_bounds <- c(site$pit_marker_easting, site$pit_marker_northing)
    upper_bounds <- lower_bounds + 100
    
    box <- st_as_sfc(st_bbox(st_sfc(st_point(lower_bounds),
                                    st_point(upper_bounds),
                                    crs = siteEPSG)))
    box <- st_transform(box, crs = "WGS84")
    single_corrected_polygons <- rbind(single_corrected_polygons, st_sf(data.frame(Name = site_name), geometry = box))
  }
}


single.points <- subset(single.points, (Name %in% missing_single_point_sites)) # see all sites missing 

# Check results 
#st_write(single_corrected_polygons[,c("Name", "geometry")], '../DATASETS/AusPlots_Location/Test_Geometries_20240508.kml', append = FALSE)


plot(st_geometry(single_corrected_polygons[3,]))

## Sites with 4 points 
four.point <- table(kmz_data$Name)[table(kmz_data$Name) == 4]
four.point.data <- subset(kmz_data, (Name %in% names(four.point)))

# Extract sites with more than 4 coordinates by removing sites with 1 and 4 coordinate points 
multiple.points <- subset(kmz_data,!(Name %in% names(one.point)))
multiple.points <- subset(multiple.points,!(Name %in% names(four.point)))

df.sites.coords <- as.data.frame(cbind(multiple.points$Name, st_coordinates(multiple.points)))

# Fix the df.sites.coords for SAAEYB0006 in particular, remove the outlier point
df.sites.coords <- df.sites.coords[-which((df.sites.coords$V1 == 'SAAEYB0006') & 
         (df.sites.coords$Y == -25.28746249)),] 

# Fix the df.sites.coords for SAAEYB0006 in particular, remove the outlier point
df.sites.coords <- df.sites.coords[-which((df.sites.coords$V1 == 'SASMDD0003') & 
                                            (df.sites.coords$X == 134.586722)),] 

# Fix the df.sites.coords for WAACOO0008 in particular, it seems one point is way off, but not in a systematic way, so it was removed
df.sites.coords <- df.sites.coords[-which((df.sites.coords$V1 == 'WAACOO0008') & 
         (df.sites.coords$Y == -31.59637778)), ] 


corner.points <- get_corner_points(df.sites.coords)
corner.points.sf <- st_as_sf(corner.points, coords = c('X', 'Y', 'Z'),
                             crs =  st_crs(kmz_data))
corner.points.sf <- rbind(corner.points.sf, four.point.data[,c('Name', 'geometry')]) # combine corner data with the four point data again

grouped.points <- aggregate(corner.points.sf$geometry, 
                            by = list(corner.points.sf$Name), 
                            FUN = function(x) {
                              st_cast(st_combine(x),"POLYGON")
                            })

colnames(grouped.points)[1] <- 'Name'

# Combine Single points with polygons 
grouped.points <- st_as_sf(grouped.points ,crs =  st_crs(kmz_data))
combined <- rbind(single_corrected_polygons[,c("Name", "geometry")], grouped.points)
row.names(combined) <- 1:nrow(combined) # reset row index
combined <- st_zm(combined)

# Check Validity of Resultant coords and fix self-intersecting polygons by calling concave_hull 
combined[!st_is_valid(combined),] <- st_concave_hull(combined[!st_is_valid(combined),], ratio = 1)
st_write(combined, '../DATASETS/AusPlots_Location/AusPlots_Geometries_20240508.kml', append = FALSE)
st_write(combined, '../DATASETS/AusPlots_Location/AusPlots_Geometries_20240508.shp', append = FALSE)


# Create subset 
site.subset <- read.csv('../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv')
subset.points <- subset(combined, Name %in% site.subset$site_location_name)
row.names(subset.points) <- 1:nrow(subset.points) # reset row index
st_write(subset.points, '../DATASETS/AusPlots_Location/site_subset.kml', append = FALSE)

# Convert to geojson ------------------------------------------------------

st_write(subset.points, "site_subset.geojson")

# Convert to csv file
write.csv(corner.points[,c('Name','X','Y')], file = 'sites_corner_coords.csv', row.names = F)

subset.corner <- subset(corner.points, Name %in% site.subset$site_location_name)

write.csv(subset.corner[,c('Name','X','Y')], file = 'subset_sites_corner_coords.csv', row.names = F)

# TESTING -----------------------------------------------------------------

df.sites.coords.test <- subset(df.sites.coords,V1 == 'NSABBS0001')
df.sites.coords.test$X <- as.numeric(df.sites.coords.test$X)
df.sites.coords.test$Y <- as.numeric(df.sites.coords.test$Y)

with(df.sites.coords.test, plot(X,Y))

df.sites.coords.test$sum_sw_ne <- with(df.sites.coords.test,X+Y)
df.sites.coords.test$sum_se_nw <- with(df.sites.coords.test,X-Y)

SW <- which.min(df.sites.coords.test$sum_sw_ne)
NE <- which.max(df.sites.coords.test$sum_sw_ne)
NW <- which.min(df.sites.coords.test$sum_se_nw)
SE <- which.max(df.sites.coords.test$sum_se_nw)

with(df.sites.coords.test[c(SW,NE,SE,NW),], plot(X,Y))

corner.test <- df.sites.coords.test[c(SW,NE,SE,NW),]



# Site --------------------------------------------------------------------


df.sites.coords.test <- subset(df.sites.coords, subset = (V1 == 'NSFNNC0006'))
df.sites.coords.test$X <- as.numeric(df.sites.coords.test$X)
df.sites.coords.test$Y <- as.numeric(df.sites.coords.test$Y)

with(df.sites.coords.test, plot(X,Y))


four.point.data <- subset(kmz_data, (Name %in% names(four.point)))

df.sites.coords.test$sum_sw_ne <- with(df.sites.coords.test,X+Y)
df.sites.coords.test$sum_se_nw <- with(df.sites.coords.test,X-Y)

SW <- which.min(df.sites.coords.test$sum_sw_ne)
NE <- which.max(df.sites.coords.test$sum_sw_ne)
NW <- which.min(df.sites.coords.test$sum_se_nw)
SE <- which.max(df.sites.coords.test$sum_se_nw)

with(df.sites.coords.test[c(SW,NE,SE,NW),], plot(X,Y))

corner.test <- df.sites.coords.test[c(SW,NE,SE,NW),]

plot(st_make_valid(combined[!st_is_valid(combined),]))

plot(combined[!st_is_valid(combined),] %>% st_concave_hull(ratio = 1))

combined[!st_is_valid(combined),]

