### Explore Historica Bushfires ###
# Krish Singh
# 20240118


# Library -----------------------------------------------------------------


# Functions ---------------------------------------------------------------

library(sf)
library(ggplot2)
library(foreach)
library(doParallel)
library(bench)

# Main --------------------------------------------------------------------

num_cores <- 6
cl <- makeCluster(num_cores)
registerDoParallel(cl)

hist.shp <- st_read("../DATASETS/Historical_Bushfire_Boundaries/Historical_Bushfire_Boundaries.dbf")
kmz_data <- st_read("../DATASETS/AusPlots_Location/site_subset.kml", quiet = TRUE)
#kmz_data <- st_centroid(kmz_data)[1:2, ]
# 113.25 for centroid 

hist.shp <- st_transform(hist.shp, st_crs(kmz_data))
hist.shp <- hist.shp[-126888, ]

num_chunks <- 200  # Adjust as needed
## 100 -> ~ 93sec for 2 
## 200 -> 93.52 sec for 2

hist_chunks <- split(hist.shp, 1:num_chunks)

timing_result <- system.time({
  result_list <- foreach(chunk = hist_chunks, .combine = rbind, .packages = c("sf")) %dopar% {
    # Perform intersection for each chunk
    result_chunk <- st_intersection(chunk, kmz_data)
    return(result_chunk)
  }
})

stopCluster(cl)

# timing_result['elapsed']

st_write(result_list,'../DATASETS/AusPlots_Historical_BurnDates.kml')
st_write(result_list,'../DATASETS/AusPlots_Historical_BurnDates.shp')

# Junk Script (Don't Run) -------------------------------------------------

# hist.shp <- st_read("../DATASETS/Historical_Bushfire_Boundaries/Historical_Bushfire_Boundaries.shp")
# 
# # Query through my plots 
# kmz_data <- st_read("../DATASETS/AusPlots_Location/site_subset.kml", quiet = TRUE)
# 
# #Convert crs of historical data into crs of kmz data
# hist.shp <- st_transform(hist.shp, st_crs(kmz_data))
# NSANAN0002 <- subset(kmz_data, subset = (Name == 'NSANAN0002'))
# 
# midpoint <- st_centroid(NSANAN0002)
# is.valid.hist <- st_is_valid(hist.shp)
# 
# print(which(!is.valid.hist))
# #  126888 -> one point is invalid at this index
# hist.shp <- st_make_valid(hist.shp) # fix the invalid geometry
# st_is_valid(hist.shp[126888,]) # still not fixed
# plot(hist.shp[126888,])
# 
# hist.shp <- hist.shp[-126888,] # drop the anomoly 
# 
# # Query the dataset with the site's polygons 
# result <- st_intersection(hist.shp, NSANAN0002)


####
# 
# num_cores <- 6
# 
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
# 
# hist.shp <- st_read("../DATASETS/Historical_Bushfire_Boundaries/Historical_Bushfire_Boundaries.shp")
# kmz_data <- st_read("../DATASETS/AusPlots_Location/site_subset.kml", quiet = TRUE)
# kmz_data <- st_centroid(kmz_data) # use centroid to speed up query
# 
# hist.shp <- st_transform(hist.shp, st_crs(kmz_data))
# hist.shp <- hist.shp[-126888, ]
# 
# timing_result <- system.time({
#   st_intersection(hist.shp,kmz_data[2,])
# })
# print(timing_result)
# 
# #    user  system elapsed 
# #    249.50    5.72  493.14 
# 
# result_list <- foreach(i = 1:nrow(kmz_data), .combine = rbind, .packages = c("sf")) %dopar% {
#   return(st_intersection(hist.shp,kmz_data[i,]))
# }
# 
# stopImplicitCluster()
# stopCluster(cl)
# 
# 
# result <- do.call(rbind, result_list)


##

# num_cores <- 2
# 
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
# 
# hist.shp <- st_read("../DATASETS/Historical_Bushfire_Boundaries/Historical_Bushfire_Boundaries.shp")
# kmz_data <- st_read("../DATASETS/AusPlots_Location/site_subset.kml", quiet = TRUE)
# #kmz_data <- st_centroid(kmz_data) # use centroid to speed up query
# # speed is 8:40 with centroid 
# kmz_data <- kmz_data[1:2,]
# 
# hist.shp <- st_transform(hist.shp, st_crs(kmz_data))
# hist.shp <- hist.shp[-126888, ]
# 
# 
# results <- bench::mark({
#   result_list <- foreach(i = 1:nrow(kmz_data), .combine = rbind, .packages = c("sf")) %dopar% {
#     return(st_intersection(hist.shp,kmz_data[i,]))
#   }
# },check_memory = TRUE)
# 
# stopCluster(cl)

