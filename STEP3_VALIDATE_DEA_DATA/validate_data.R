


# Test if FC data maps correctly to ausplots coordinates ------------------


# check if the satellite imaging captured the right coordinates
# algorithm:
#   1. get coords (x,y of both site and sat)
#   2. get mid.coords for site 
#   2. get largest values of sat x and y
#   3. condition checks:
#       a) site.mid.x > sat.x_min & site.mid.x < sat.X
#       b) site.mid.y > sat.y_min & site.mid.y < sat.y
#   4. If both are true, then the mid-point of the site lies within the site image
#   5. Calculate the offsets of the SW and NE borders 
#   6. Return (True/false for midpoint, offset.x, offset.y)

check.sat.position <- function(site.x.coord, site.y.coord, sat.x.coord, sat.y.coord){
  
  # Get mid coordinates from the site itself 
  site.mid.x <- site.x.coord+50
  site.mid.y <- site.y.coord+50
  
  # get the minimum and maximum of the satelite pixel coordinates 
  sat.min.max.x.y.coords <- c("x.min" = min(sat.x.coord), "x.max" = max(sat.x.coord),
                              "y.min" = min(sat.y.coord), "y.max" = max(sat.y.coord))
  #print(sat.min.max.x.y.coords)
  
  
  # Check if the mid coordinates from the site lies within the boundaries of the sattelite coordinates 
  is.witin.x <- site.mid.x > sat.min.max.x.y.coords["x.min"] & site.mid.x < sat.min.max.x.y.coords["x.max"]
  is.witin.y <- site.mid.y > sat.min.max.x.y.coords["y.min"] & site.mid.y < sat.min.max.x.y.coords["y.max"]
  is.within.coords <- is.witin.x & is.witin.y
 
  
  ## Get offsets for the corner points (SW, NE)
  offsets.SW.components <- abs(c(site.x.coord - sat.min.max.x.y.coords[["x.min"]],
                             site.y.coord - sat.min.max.x.y.coords[["y.min"]]))
  # +100 to SW to get NE
  offset.NE.components <- abs(c(site.x.coord + 100 - sat.min.max.x.y.coords[["x.max"]],
                            site.y.coord + 100 - sat.min.max.x.y.coords[["y.max"]]))
  
  
  # Calculate the distance of the offsets
  offset.SW <- sqrt(sum(offsets.SW.components^2))
  offset.NE <- sqrt(sum(offset.NE.components^2))
  
  return(cbind("is.within.coords" = is.within.coords, 
           "offset.SW" = offset.SW,
           "offset.NE" = offset.NE))
  
}




directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")

offset.data <- data.frame(site_location_name = as.character(NA),
                           is.within.coords = as.numeric(NA),
                           offset.SW = as.numeric(NA),
                           offset.NE = as.numeric(NA))

for(RI in 1:nrow(sites.query)){
  print(RI)
  site.location <- sites.query$site_location_name[RI]
  site.x.coord <- sites.query$pit_marker_easting[RI]
  site.y.coord <- sites.query$pit_marker_northing[RI]
  
  # Get DEA dataset
  site.path <- paste(directory,paste0(site.location,".csv"), sep = "/")
  dea.data <- read.csv(site.path)
  
  # Check if the dataset is empty
  if(nrow(dea.data) > 0) {
    offsets <- check.sat.position(site.x.coord,site.y.coord,dea.data$x, dea.data$y)
    offset.data.row <- data.frame(site_location_name = site.location,offsets) 
  } else {
    offset.data.row <- data.frame(site_location_name = site.location,
               is.within.coords = as.numeric(NA),
               offset.SW = as.numeric(NA),
               offset.NE = as.numeric(NA))
  }
  
  offset.data <- rbind(offset.data, offset.data.row)
}
offset.data <- offset.data[-1,]
save(... = offset.data,file =  'dea_validation.RData')


offset.data[offset.data$is.within.coords == 0,]
# Check between methods ---------------------------------------------------



original_method <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/SASMDD0002.csv")
new_method <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/new_script_test_c/SASMDD0002.csv")


## Test 0: Check for consistent x-y coord ranges between the two datasets

# TRUE = methods exhibit consistent spatial extent 
# FALSE = methods exhibit inconsistent spatial extent 

if (all(range(new_method$x) == range(original_method$x)) & 
    all(range(new_method$y) == range(original_method$y))) {
  print(T)
} else {
  print(F)
}

## Results for Test 0:
# WAANUL0007: TRUE
# NTAFIN0030: TRUE
# QDAMUL0002: TRUE
# SASMDD0016: TRUE 
# SASMDD0002: TRUE

## Test 1: Check if the intersect is equal 

# TRUE = The methods produced identical results for the common time slices
# FALSE = The methods did not produce identical results for the common time slices 

common.timeslices <- intersect(unique(original_method$time),(unique(new_method$time)))

new.method.common <- subset(new_method,subset = (time %in% common.timeslices))
origin.method.common <- subset(original_method, subset = (time %in% common.timeslices))

for(col in colnames(new.method.common)){
  print(all(new.method.common[[col]] == origin.method.common[[col]],na.rm = T))
}

## Test 1 Results:
# WAANUL0007: TRUE
# NTAFIN0030: TRUE
# QDAMUL0002: TRUE
# SASMDD0016: TRUE 
# SASMDD0002: TRUE


## Test 2: Check if the set difference is NA 

# TRUE = The uncommon time silces does not have missing data across the methods 
# FALSE =  The uncommon time silces did have missing data across the methods

diff.timeslices <- setdiff(unique(original_method$time),(unique(new_method$time)))
origin.method.diff <- subset(original_method, subset = (time %in% diff.timeslices))
new.method.diff <- subset(new_method, subset = (time %in% diff.timeslices))


check.na <- function(df){
  
  all.na <- T
  len <- nrow(df)
  
  if(len > 0){
    
    for(col in c('bs','pv','npv','ue')){
      
      na.count <- sum(is.na(df[[col]]))
      if(na.count != len){
        all.na <- F
      }
      
    }
    
  }
  return(all.na)
}

check.na(new.method.diff) & check.na(origin.method.diff)

## Test 2 Results:
# WAANUL0007: TRUE
# NTAFIN0030: TRUE
# QDAMUL0002: TRUE 
# SASMDD0016: TRUE
# SASMDD0002: TRUE

## CONCLUSION:
# The methods does not pose inconsistencies with the data it retrieves. 


## Others (not recorded):

## Check for duplication between the two datasets

original_method[duplicated(original_method),]
new_method[duplicated(new_method),]


