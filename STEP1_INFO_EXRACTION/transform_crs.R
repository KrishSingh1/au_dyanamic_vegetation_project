library(ausplotsR)
library(sp)

veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")

info <- veg.info$site.info[436,c("site_location_name","pit_marker_mga_zones","pit_marker_easting", 
                                 "pit_marker_northing", "longitude", "latitude", "pit_marker_datum")]


location <- SpatialPointsDataFrame(coords=info[,c("pit_marker_easting","pit_marker_northing")],
                                   data=info)

# crs.string <- paste0("+init=epsg:283",info$pit_marker_mga_zones)
# crs.datum <- paste0("+datum=",info$pit_marker_datum)
# config <- paste(crs.string, crs.datum)

# proj4string(location) <- CRS(config)
# project.string <- paste0("+proj=longlat +datum=", crs.datum)
# spTransform(location,CRS("+proj=longlat +datum=WGS84"))


proj4string(location) <- CRS("+init=epsg:28354 +datum=WGS84")
d <- spTransform(location,CRS("+proj=longlat +datum=WGS84"))
print(d)

location.2 <- SpatialPointsDataFrame(coords=info[,c("pit_marker_easting","pit_marker_northing")],
                                   data=info)

proj4string(location.2) <- CRS("+init=epsg:7854 +datum=WGS84")
d.2 <- spTransform(location.2,CRS("+proj=longlat +datum=WGS84"))
print(d.2)

# links:
# https://gis.stackexchange.com/questions/231903/choosing-projected-coordinate-system-for-western-australia
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf



location.3 <- SpatialPointsDataFrame(coords=info[,c("latitude","longitude")],
                                   data=info)
proj4string(location.3) <- CRS("+init=epsg:28349 +datum=WGS84")
d.3 <- spTransform(location,CRS("+proj=easting +datum=WGS84"))



