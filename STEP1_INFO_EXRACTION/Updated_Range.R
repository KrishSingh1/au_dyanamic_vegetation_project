
library(ausplotsR)
library(sp)
library(rgdal)

my.data <- get_ausplots(veg.vouchers=FALSE,veg.PI=FALSE)
site.info <- as.data.frame(my.data$site.info)


x.min <- min(site.info$pit_marker_easting,na.rm = T) - 100
y.min <- min(site.info$pit_marker_northing,na.rm = T) - 100

x.max <- max(site.info$pit_marker_easting,na.rm = T) + 100
y.max <- max(site.info$pit_marker_northing,na.rm = T) + 100




site.info$pit_marker_mga_zones

# try zone by zone 
site.info.55 = subset(site.info, subset = (pit_marker_mga_zones == 55))

x.min <- min(site.info.55$pit_marker_easting,na.rm = T) - 100
y.min <- min(site.info.55$pit_marker_northing,na.rm = T) - 100

x.max <- max(site.info.55$pit_marker_easting,na.rm = T) + 100
y.max <- max(site.info.55$pit_marker_northing,na.rm = T) + 100   

print(paste("x:", x.min, ",", x.max, "\ny:",
            y.min, ",", y.max))
