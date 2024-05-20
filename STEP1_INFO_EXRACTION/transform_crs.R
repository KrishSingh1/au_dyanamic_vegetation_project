library(ausplotsR)
library(sf)

veg.info <- read.csv("../DATASETS/AusPlots_Extracted_Data/extracted_site_info_2-0-6.csv")
# Note: NSAAUA0001 is not avaliable at this stage
# fix column names 
colnames(veg.info) <- unlist(lapply(colnames(site), FUN = function(x){str_split(x, '\\.')[[1]][3]}))

site_location_name <- 'NSABHC0015'
site <- subset(veg.info, subset = (site.info.site_location_name == site_location_name))
if(nrow(site) == 0) {
  site <- get_ausplots(site_location_name)$site.info
  site <- site[1,]
}


## if Pit marking easting are unknown 

if(is.na(site$pit_marker_easting)){
  point <- st_sfc(st_point(c(site$longitude,site$latitude)), crs = "WGS84")
  siteEPSG <- as.numeric(paste0('283',site$pit_marker_mga_zones))
  point <- st_transform(point, siteEPSG)
  eastings <- st_coordinates(point)
  site$pit_marker_easting <- eastings[,1]
  site$pit_marker_northing <- eastings[,2]
}

lower_bounds <- c(site$pit_marker_northing, site$pit_marker_easting)
upper_bounds <- lower_bounds + 100

box <- st_as_sfc(st_bbox(st_sfc(st_point(lower_bounds),
       st_point(upper_bounds),
       crs = siteEPSG)))

box <- st_transform(box, crs = "WGS84")
st_coordinates(box)



single_points_corrected <- data.frame(Name = 'NA', Description = 'NA')
missing_single_point_sites <- c()
polygons <- c()

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
    
    lower_bounds <- c(site$pit_marker_easting,site$pit_marker_northing)
    upper_bounds <- lower_bounds + 100
    
    box <- st_as_sfc(st_bbox(st_sfc(st_point(lower_bounds),
                                    st_point(upper_bounds),
                                    crs = siteEPSG)))
    box <- st_transform(box, crs = "WGS84")
    polygons <- rbind(polygons, st_sf(data.frame(Name = site_name), geometry = box))
  }
}



site <- subset(veg.info, subset = (site_location_name == single.points$Name[1]))

