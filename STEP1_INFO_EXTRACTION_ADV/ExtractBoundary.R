
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
combined <- rbind(single.points[,c("Name", "geometry")], grouped.points)
row.names(combined) <- 1:nrow(combined) # reset row index

# Check Validity of Resultant coords and fix self-intersecting polygons by calling concave_hull 
combined[!st_is_valid(combined),] <- st_concave_hull(combined[!st_is_valid(combined),], ratio = 1)
st_write(combined, '../DATASETS/AusPlots_Location/AusPlots_Geometries_20240415.kml', append = FALSE)

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

