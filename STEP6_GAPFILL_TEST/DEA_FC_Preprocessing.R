### Combine Precip and DEA FC ###
# Krish Singh
# 20240122

# Library -----------------------------------------------------------------

library(data.table)
library(ncdf4)
library(dplyr)
library(sf)
library(ggplot2)
library(ausplotsR)
library(sfheaders)


# Functions ---------------------------------------------------------------

get_preprocessed_dea_fc <- function(query, 
                                    directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files',
                                    site.corners.data){
  dea.fc <- tryCatch({
    temp <- fread(paste0(directory, "/", query, ".csv")) # use data.table for faster processing
    temp <- trim_to_nearest_coord(site.corners.data, temp, query) # trim spatially
    temp <- subset(temp, subset = (ue <= 25.5)) # filter based on unmixing error (25.5)
    write.csv(temp, paste0('../DATASETS/DEA_FC_PROCESSED/SPATIAL/', query, '.csv')) # Save Separately for debugging purposes
    temp <- aggregate(temp[,-1], 
                          by = list(temp$time), FUN = mean, na.rm = T) # aggregate
    colnames(temp)[1] = 'time'
    return(temp)
  }, error = function(e) {
    print(paste0(conditionMessage(e), " in ", query))
    return(NA)
  })
  return(dea.fc)
}



trim_to_nearest_coord <- function(site.corners.data, dea.fc.i, query) {
  
  # Subset the site corners data by the query 
  site.specifc.corner <- site.corners.data %>%
    subset(subset = (site_location_name == query)) %>%  
    st_coordinates() %>%
    as.data.frame()
  
  print(site.specifc.corner)
 
  # Remote End Points: 
  E.remote.incre <- unique(dea.fc.i$x)
  N.remote.incre <- unique(dea.fc.i$y)

  # Grab the exterior easting and northing 
  S.site <- site.specifc.corner$Y %>% min()
  W.site <- site.specifc.corner$X %>% min()
  N.site <- site.specifc.corner$Y %>% max()
  E.site <- site.specifc.corner$X %>% max()
  
  # Find Closest Points which fits this boundary:
  W.closest <- E.remote.incre[which.min(abs(E.remote.incre - W.site))]
  E.closest <- E.remote.incre[which.min(abs(E.remote.incre - E.site))]
  N.closest <- N.remote.incre[which.min(abs(N.remote.incre - N.site))]
  S.closest <- N.remote.incre[which.min(abs(N.remote.incre - S.site))]
  
  # Trim dataset based on the boundary:
  trimmed <- subset(dea.fc.i, subset = (x >= W.closest & x <= E.closest &
                                          y >= S.closest & y <= N.closest))
  
  return(trimmed)
}


# Pre Main ----------------------------------------------------------------

# Preprocess the corners data so it has all corner points

directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

site.corners.data <- read.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024.csv')
site.corners.data.cleaned <- site.corners.data[, c('site_location_name', 'point', 'latitude', 'longitude')]
site.corners.data.cleaned$missing <- rep('FALSE', nrow(site.corners.data.cleaned))

# There is a problem site: WAAPIL0019, where SE ~= NW, simply get rid of the SE point, and estimate it 
site.corners.data.cleaned <-site.corners.data.cleaned[-which(
  site.corners.data.cleaned$site_location_name == 'WAAPIL0019' &  site.corners.data.cleaned$point == 'SE'),]

missing.sites <- setdiff(file.names, site.corners.data.cleaned$site_location_name)
missing.sites.data <- get_ausplots(missing.sites)
missing.sites.data <- missing.sites.data$site.info[,c('site_location_name', 'point', 'longitude', 'latitude')]
missing.sites.data$missing <- rep('True', nrow(missing.sites.data))
missing.sites.data <- unique(missing.sites.data)

site.corners.data.cleaned <- rbind(site.corners.data.cleaned, missing.sites.data)
rownames(site.corners.data.cleaned) <- 1:nrow(site.corners.data.cleaned)

site.corners.data.cleaned <- site.corners.data.cleaned %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326) %>% # Set crs to the original crs
  st_transform(3577, allow_ballpark = F ) # Convert to 3577, same as the DEA FC default
site.corners.data.cleaned$estimation_2 <- rep('False', nrow(site.corners.data.cleaned))
site.corners.data.cleaned$point <- toupper(site.corners.data.cleaned$point)

# Subset the data to only include 'SW', 'SE', 'NE', and 'NW'
unique_points <- site.corners.data.cleaned %>%
  subset(subset = (point == 'SW' | point ==  'SE'  | point == 'NE' | point == 'NW') )

# # Check for sites with missing corner points
result <- unique_points %>%
  group_by(site_location_name) %>%
  summarize(count = n())
results2 <- result[which(result$count < 4),]$site_location_name
results2.site.data <- get_ausplots(results2)
results2.site.data <- results2.site.data$site.info

# Include Exception, where WAAPIL0019's points lead to self-intercepting points

# Estimate the corner points
for(sites in results2) {
  
  # Subset based on the site 
  site.specifc.corner <- site.corners.data.cleaned %>%
    subset(site_location_name == sites)
  
  # In cases where more than 1 point, but less than 4 points are availiable, just extract the SW point
  if(nrow(site.specifc.corner) > 1) {
    site.specifc.corner <- site.specifc.corner %>%
      subset(point == 'SW')
  }

  # Get coordinates 
  site.specifc.corner <- site.specifc.corner %>%
    st_coordinates() %>%
    as.data.frame()
  rownames(site.specifc.corner) <- 'SW'

  site.info.specific <- results2.site.data %>%
    subset(site_location_name == sites)

  # Estimate the other three points by the 100mx100m dimension 
  if(site.info.specific$plot_is_100m_by_100m[1] == TRUE){
    NE <- c('X' = site.specifc.corner$X[1] + 100, 'Y' = site.specifc.corner$Y[1] + 100)
    SE <- c('X' = site.specifc.corner$X[1] + 100, 'Y' = site.specifc.corner$Y[1])
    NW <- c('X' = site.specifc.corner$X[1], 'Y' = site.specifc.corner$Y[1] + 100)
    spare <- as.data.frame(rbind(NE, SE, NW))

    site.specifc.corner <- rbind(site.specifc.corner,spare )
    site.specifc.corner$point <- rownames(site.specifc.corner)
    site.specifc.corner$site_location_name <- rep(sites, nrow(site.specifc.corner))
    rownames(site.specifc.corner) <- 1:nrow(site.specifc.corner)

    site.specifc.corner <- site.specifc.corner %>%
      st_as_sf(coords = c('X','Y')) %>%
      st_set_crs(3577)
    site.specifc.corner$estimation_2 <- rep('True', nrow(site.specifc.corner))
    site.specifc.corner$missing <- rep('NA', nrow(site.specifc.corner))

    site.corners.data.cleaned <- rbind(site.corners.data.cleaned,site.specifc.corner)
  } else{
    print(paste0('WARNING, CHECK DIMENSION OF ', sites))
  }
}

site.corners.data.cleaned %>%
  sf_to_df(fill = T) %>%
  select(c(site_location_name, point, estimation_2, missing, x, y)) %>%
  write.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv')


# The above approach can lead to duplicated corner points if they had to be estimated due to the absence of other points
# We remove the duplicated corner points by removing corner points marked by estimation_2 == 'True' and keeping the original, as marked by 'False'
site_unique <- unique(site.corners.data.cleaned$site_location_name)
clean_set <- c()
for(site in site_unique){
  
  # Get the four corner points 
  temp <- site.corners.data.cleaned %>%
    subset(site_location_name == site) %>%
    subset((point == 'SW' | point ==  'SE'  | point == 'NE' | point == 'NW'))
  
  # Get Counts of each corner point 
  freq_table <- table(temp$point)
  dup_checker <- which(freq_table > 1)
  
  # Check for any duplicates 
  if(any(dup_checker) == TRUE) {
    duplicated_points <- names(freq_table[which(freq_table > 1)]) # get names of duplicates 
    for(dp in duplicated_points){ # Remove the duplicate  
      temp = temp[-which(temp$point == dp & temp$estimation_2 == 'True'),] 
    } 
  }
  clean_set <- rbind(clean_set, temp)
}

clean_set %>%
  sf_to_df(fill = T) %>%
  select(c(site_location_name, point, estimation_2, missing, x, y)) %>%
  write.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv')

# Main --------------------------------------------------------------------


# Alg:
# 1. Obtain all coordinate points for each sites via the published corner points 
# 2. Convert the coordinates from the corner points into EPSG 3577
# 3. Read in the DEA FC from the site 
# 4. Using the corner points from the published corner points, subset the DEA FC
#    --> such that all internal points are kept 
# 5. Filter the DEA FC to include all points under ue <= 25.5

directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

site.corners.data <- read.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv')
site.corners.data.cleaned <- site.corners.data[, c('site_location_name', 'point', 'x', 'y')]
site.corners.data.cleaned <- site.corners.data.cleaned %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(3577) # Set crs to the original crs

plot(st_geometry(site.corners.data.cleaned)) # Plot to check if this roughly makes an Australian shape

error.messages <- c('')
counter_max <- length(file.names)
counter_current <- 1
for (query in file.names) {
  # Get the progress bar
  print(paste0(
    'START: ', counter_current, '/',counter_max, ' {', query  ,'}'
  ))
  site.fc <- get_preprocessed_dea_fc(query, site.corners.data = site.corners.data.cleaned, directory = directory)
  if(class(site.fc) != 'data.frame') {
    error.messages <- c(error.messages, paste0('Error in processing ', query, '.csv'))
  } else {
    write.csv(site.fc, paste0('../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/', query, '.csv')) 
  }
  counter_current <- counter_current + 1
}
writeLines(error.messages, '../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/log.txt')

# TESTING -----------------------------------------------------------------

# Trouble sites:
# "no rows to aggregate in SATFLB0007"
# "no rows to aggregate in SATFLB0012"
# "no rows to aggregate in SATFLB0014"

WAGCOO0001
QDASEQ0001
test.site <- 'QDASEQ0001'
directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent'
test.dea <- read.csv(paste0(directory, "/", test.site, ".csv")) # use data.table for faster processing

test.dea.trimed <- trim_to_nearest_coord(site.corners.data = site.corners.data.cleaned,
                      dea.fc.i = test.dea,
                      query = test.site)

test.dea.trimed <- test.dea.trimed %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(3577)

ggplot() + geom_sf(data = test.dea.trimed, colour = 'red') +
  geom_sf(data = site.corners.data.cleaned[which(site.corners.data.cleaned$site_location_name == test.site),],
          color = 'blue')




