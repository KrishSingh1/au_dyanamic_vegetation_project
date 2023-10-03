library(ausplotsR)
library(sp)

plot <- get_ausplots(my.Plot_IDs = "SATFLB0006",site_info=TRUE)
info <- plot$site.info[1,]


location <- SpatialPointsDataFrame(coords=info[,c("pit_marker_easting","pit_marker_northing")],
                                   data=info)
info$pit_marker_datum

# crs.string <- paste0("+init=epsg:283",info$pit_marker_mga_zones)
# crs.datum <- paste0("+datum=",info$pit_marker_datum)
# config <- paste(crs.string, crs.datum)

# proj4string(location) <- CRS(config)
# project.string <- paste0("+proj=longlat +datum=", crs.datum)
# spTransform(location,CRS("+proj=longlat +datum=WGS84"))


proj4string(location) <- CRS("+init=epsg:28353 +datum=WGS84")
spTransform(location,CRS("+proj=longlat +datum=WGS84"))


# links:
# https://gis.stackexchange.com/questions/231903/choosing-projected-coordinate-system-for-western-australia
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf