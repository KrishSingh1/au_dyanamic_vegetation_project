### Extract Historical Bushfires ###
# Krish Singh
# 20240118


# Library -----------------------------------------------------------------

library(sf)
library(ggplot2)
library(foreach)
library(doParallel)
library(bench)
library(dplyr)
library(sfheaders)

# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

# Use parralelisation to speed up dataset queries 
num_cores <- 6 # adjust according to available CPU cores of your computer (I did n/2 - 1, where n is number of my CPU cores)
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Download historical bushfires and enter the directory beforehand 
hist.shp <- st_read("../DATASETS/Historical_Bushfire_Boundaries/Historical_Bushfire_Boundaries.dbf") # get polygons from historical bushfires
kmz_data <- st_read("../DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Derived_Boundary_Polygons/AusPlots_Polygons_from_Published_Plots.shp", quiet = TRUE) # get polygons from ausplots sites 

hist.shp <- st_transform(hist.shp, st_crs(kmz_data))
hist.shp <- hist.shp[-126888, ]# remove record 126888 as it was not considered to be a valid geometry: see 'Extra'

num_chunks <- 200 # Number of chunks, to further speed up process 

hist_chunks <- split(hist.shp, 1:num_chunks)

timing_result <- system.time({
  result_list <- foreach(chunk = hist_chunks, .combine = rbind, .packages = c("sf")) %dopar% {
    result_chunk <- st_join(x = kmz_data, y = chunk, left = FALSE) # have each worker query based on a chunk of the historical bushfire dataset
    return(result_chunk)
  }
})

stopCluster(cl)
View(result_list) # view dataset, note some records do not have an ignition/extinguished dates 

# Output dataset
result_list_csv <- result_list %>%
  sf_to_df(fill = T) %>% 
  filter(is.na(ignition_d) == FALSE) %>%
  select(!c('sfg_id', 'polygon_id', 'linestring_id', 'x', 'y')) %>%
  unique()
  
write.csv(result_list_csv, '../DATASETS/AusPlotsBurnData/Historical_Bush_Fires/AusPlots_Historical_BurnDates.csv')
st_write(result_list,'../DATASETS/AusPlotsBurnData/Historical_Bush_Fires/AusPlots_Historical_BurnDates.kml', append = FALSE) # kml file
st_write(result_list,'../DATASETS/AusPlotsBurnData/Historical_Bush_Fires/AusPlots_Historical_BurnDates.geojson', append = FALSE) # geojson file



# Extra -------------------------------------------------------------------

hist.shp <- st_read("../DATASETS/Historical_Bushfire_Boundaries/Historical_Bushfire_Boundaries.dbf") # get polygons from historical bushfires
print(st_is_valid(hist.shp[126888, ]))
# FALSE -> not valid 

#plot(hist.shp[126888, ])
# plot shape 


#plot(st_geometry(kmz_data[which(st_is_valid(kmz_data) == FALSE),]))
