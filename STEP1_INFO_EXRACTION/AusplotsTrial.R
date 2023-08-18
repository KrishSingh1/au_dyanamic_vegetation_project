#install.packages("ausplotsR")
#library(ausplotsR)
#get_ausplots(herbarium_determination_search="Eucalyptus moderata")

#my.data <- get_ausplots(veg.vouchers=FALSE)

#Set maximum number of plots to all available in the veg.PI data:
#ausplots_visual(my.data, map = FALSE, map.attribute = TRUE, 
#                fraction.pie = FALSE, growthform.pie = FALSE, cumulative.cover = FALSE, 
#                whittaker = FALSE, outfile = "AusPlots_treeCover.pdf", 
#                max.plots=length(unique(my.data$veg.PI$site_location_name))) 


### Obtaining Associated Sites ###

## Algorithm for data retrieval
## [From R]
## 1) get site 
## 2) get site's coordinates, and time range 
## [From jupyter notebook sandbox]
## 3) retrieve data from the coordinates retrieved from 2)
## 4) save retrieved data as csv, download it
## ... 



# <- get _ ausplots(bounding _ box=c(120, 140, −30, −10))

library(ausplotsR)
library(sp)
library(rgdal)
## Sandbox retriving small amount of data for SATFLB0004


#my.data <- get_ausplots(bounding_box=c(120, 140, -30, -10),site_info=TRUE)
my.data <- get_ausplots(my.Plot_IDs = c("NSTSYB0002"),site_info=TRUE)
info <- as.data.frame(my.data$site.info)
info <- cbind(info, fractional_cover(my.data$veg.PI)[-1]) # get fractional data

get.latlong <- function(df, pit.name.1, pit.name.2, init, datum.proj){
  
  df.sp <- SpatialPointsDataFrame(
    coords=info.record[c(pit.name.1,pit.name.2)],
    data=df)
  
  #proj.command.str <- paste("+init=", init, zone, " +datum=", datum.proj, sep = "")
  proj.command.str <- paste("+init=", init, zone, sep = "")
  print(proj.command.str)
  
  proj4string(df.sp) <- CRS(proj.command.str)
  
  
  proj.command.str <- paste("+proj=longlat", 
                            " ","+datum=", datum.proj, sep = "")
  print(proj.command.str)
  
  df.sp.latlong <- spTransform(df.sp,CRS(proj.command.str))
  df.latlong <- as.data.frame(df.sp.latlong@coords)
  
  return(df.latlong)
} 



for (i in 1:nrow(info)) {
  
  info.record <- info[i,]
  print(info.record)
  zone <- as.character(info.record$pit_marker_mga_zones) # get MGA zone 
  datum.recorded <- as.character(info.record$pit_marker_datum)
  init <- "epsg:283"
  datum.proj <- "WGS84" # The desired datum projection
  
  
  ## Loop through the dataset to project easting northing into latlong form 
  if ( !(is.na(info.record$pit_marker_easting) || 
         is.na(info.record$pit_marker_northing))) {
    
    
    ## get the relative N and E coordinates from the Northing-Eastern coordinate system 
    info.record$pit_marker_easting_E = info.record$pit_marker_easting + 100
    info.record$pit_marker_northing_N = info.record$pit_marker_northing + 100
    
    east_north <- get.latlong(info.record, "pit_marker_easting_E", 
                             "pit_marker_northing_N",
                             init,datum.proj)
    
    west_south <- get.latlong(info.record, "pit_marker_easting", 
                             "pit_marker_northing",
                             init,datum.proj)
  
    info$lon_east[i] <-  east_north$pit_marker_easting_E
    info$lat_north[i] <- east_north$pit_marker_northing_N
    
    ## extract easting and northing lat-long to find the error
    info$lon_west[i] <-  west_south$pit_marker_easting
    info$lat_south[i] <- west_south$pit_marker_northing
    }
}

write.csv(info, "230510_site_info_list_NSTSYB0002.csv")



EPSG <- make_EPSG() # list of EPSG codes 

# method 1 with datum: 138.567316940566, -31.3277509948955


# method 2: 138.567316940554, -31.3277509957817



