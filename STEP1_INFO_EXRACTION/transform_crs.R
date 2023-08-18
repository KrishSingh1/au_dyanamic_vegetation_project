library(ausplotsR)

plot <- get_ausplots(my.Plot_IDs = "SATFLB0004",site_info=TRUE)
info <- plot$site.info[1,]


location <- SpatialPointsDataFrame(coords=info[,c("pit_marker_easting","pit_marker_northing")],
                                  data=info)


plot(location)

proj4string(location) <- CRS("+init=epsg:28354 +datum=WGS84")
spTransform(location,CRS("+proj=longlat +datum=WGS84") )


# links:
# https://gis.stackexchange.com/questions/231903/choosing-projected-coordinate-system-for-western-australia
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf