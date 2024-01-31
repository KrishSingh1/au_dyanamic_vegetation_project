### Combine Precip and DEA FC ###
# Krish Singh
# 20240122



# Library -----------------------------------------------------------------

library(data.table)
library(ncdf4)
library(dplyr)

# Functions ---------------------------------------------------------------

get_preprocessed_dea_fc <- function(query, 
                                    directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files',
                                    veg.info){
  dea.fc <- tryCatch({
    temp <- fread(paste0(directory, "/", query, ".csv")) # use data.table for faster processing
    temp <- trim_to_nearest_coord(data.table(veg.info$site.info), temp, query) # trim spatially
    temp <- subset(temp, subset = (ue <= 25.5)) # filter based on unmixing error (25.5 ~ 10%)
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



trim_to_nearest_coord <- function(veg.info, dea.fc.i, query) {
  
  reference.query.index <- which(query == veg.info$site_location_name)[1]
  print(reference.query.index)
  print(veg.info$site_location_name[reference.query.index])
  
  # Site End Points:   
  #W.site <- veg.info$site.info$pit_marker_easting[ausplots.info.i.index][2]
  #S.site <- veg.info$site.info$pit_marker_northing[ausplots.info.i.index][2]
  W.site <- veg.info$pit_marker_easting[reference.query.index]
  S.site <- veg.info$pit_marker_northing[reference.query.index]
  
  N.site <- S.site + 100
  E.site <- W.site + 100
  
  #print(W.site)
  
  # Remote End Points: 
  E.remote.incre <- unique(dea.fc.i$x)
  N.remote.incre <- unique(dea.fc.i$y)
  
  # Find Closest Points:
  W.closest <- E.remote.incre[which.min(abs(E.remote.incre - W.site))]
  E.closest <- E.remote.incre[which.min(abs(E.remote.incre - E.site))]
  N.closest <- N.remote.incre[which.min(abs(N.remote.incre - N.site))]
  S.closest <- N.remote.incre[which.min(abs(N.remote.incre - S.site))]
  
  #print(W.closest)
  #print(E.closest)
  #print(N.closest)
  #print(S.closest)
  # Trim dataset:
  trimmed <- subset(dea.fc.i, subset = (x >= W.closest & x <= E.closest &
                                          y >= S.closest & y <= N.closest))
  
  #print(unique(trimmed$x))
  #print(unique(trimmed$y))
  return(trimmed)
}

# Main --------------------------------------------------------------------


files <- list.files('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files', pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)
veg.info <- readRDS('../DATASETS/site_veg_2-0-6.rds')

error.messages <- c()

for (query in file.names) {
  site.fc <- get_preprocessed_dea_fc(query, veg.info = veg.info)
  if(class(site.fc) != 'data.frame') {
    error.messages <- c(error.messages, paste0('Error in processing ', query, '.csv'))
  } else {
    write.csv(site.fc, paste0('../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/', query, '.csv')) 
  }
}
writeLines(error.messages, '../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/log.txt')








