#### Cleaning the AusPlots Published Corner Points 
# Author: Krish Singh
# Date 16-07-2024
# Objective: To address missing corner points some of the recorded Ausplots sites


# Libraries ---------------------------------------------------------------

library(dplyr)
library(sf)
library(sfheaders)
library(data.table)
library(ausplotsR)


# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------


# Preprocess the corners data so it has all corner points

site.names <- fread('../DEA_FC_Smoothed_1987_2022/DEA_FC_Combined_1987_2022.csv') %>%
  select(site_location_name) %>% unique() 
site.names <- site.names$site_location_name

site.corners.data <- read.csv('../AusPlots_Location/Published Plot Corners_extract26062024.csv')
site.corners.data.cleaned <- site.corners.data[, c('site_location_name', 'point', 'latitude', 'longitude')]
site.corners.data.cleaned$missing <- rep('FALSE', nrow(site.corners.data.cleaned))

# # There is a problem site: WAAPIL0019, where SE ~= NW, simply get rid of the SE point, and estimate it
site.corners.data.cleaned <-site.corners.data.cleaned[-which(
  site.corners.data.cleaned$site_location_name == 'WAAPIL0019' &  site.corners.data.cleaned$point == 'SE'),]

missing.sites <- setdiff(site.names, site.corners.data.cleaned$site_location_name)
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


# Check for sites with missing corner points
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

#   # Get coordinates
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
  write.csv('../AusPlots_Location/Published Plot Corners_extract26062024_cleaned.csv')


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
  write.csv('../AusPlots_Location/Published Plot Corners_extract26062024_cleaned.csv')
