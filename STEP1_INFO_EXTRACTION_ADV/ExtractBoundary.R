
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

kmz_data <- st_read("../STEP1_INFO_EXTRACTION_ADV/AusPlots_Locations/All Plot points May 2023/doc.kml", quiet = TRUE)
# print(kmz_data)

## Sites with only one point:
one.point <- table(kmz_data$Name)[table(kmz_data$Name) < 4]
print(one.point)

table(kmz_data$Name)[table(kmz_data$Name) == 4]

##
names(one.point)
multiple.points <- subset(kmz_data,!(Name %in% names(one.point)))

# grouped_points <- aggregate(multiple.points$geometry, 
#                             by = list(multiple.points$Name), 
#                             FUN = function(x) {
#                               st_cast(st_combine(x),"POLYGON")
#                               })

#colnames(grouped_points)[1] <- 'Name'
df.sites.coords <- as.data.frame(cbind(multiple.points$Name, st_coordinates(multiple.points)))

corner.points <- get_corner_points(df.sites.coords)

corner.points.sf <- st_as_sf(corner.points, coords = c('X', 'Y', 'Z'),
                             crs =  4326)

grouped.points <- aggregate(corner.points.sf$geometry, 
                            by = list(corner.points.sf$Name), 
                            FUN = function(x) {
                              st_cast(st_combine(x),"POLYGON")
                            })

colnames(grouped.points)[1] <- 'Name'

st_write(grouped.points, 'doc.kml', append = FALSE)

# Create subset 
site.subset <- read.csv('site_subset_lat_lon.csv')
subset.points <- subset(grouped.points, Name %in% site.subset$site_location_name)

st_write(subset.points, 'site_subset.kml', append = FALSE)
# NOTE: polygon NSABHC0023 could not be created because it had only one point 

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




