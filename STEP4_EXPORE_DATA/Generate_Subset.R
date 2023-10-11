###### DEA_Data_Repeated_Sites_Visualisation ######
# By Krish Singh
# 20230929
# To extract a subset of the dataset

trim_to_nearest_coord <- function(ausplots.info.i.index, veg.info, dea.fc.i, reference.query ) {
  
  reference.query.index <- which(reference.query$site_location_name == veg.info$site.info$site_location_name[ausplots.info.i.index][1])
  print(reference.query.index)
  
  # Site End Points:   
  #W.site <- veg.info$site.info$pit_marker_easting[ausplots.info.i.index][2]
  #S.site <- veg.info$site.info$pit_marker_northing[ausplots.info.i.index][2]
  W.site <- reference.query$pit_marker_easting[reference.query.index]
  S.site <- reference.query$pit_marker_northing[reference.query.index]
  
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



# The DEA FC data files on my directory
directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)

veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds") # the ausplots dataset
# The original query file 
sites.query <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/query/sites_info_query.csv")

# Count the number of observations of each site


###### Load Dataset ######


# Sites to be extracted:

sites <- c('WAAPIL0003', 'NSABHC0023', 'TCATCH0006', 
           'WAAGAS0002', 'NSAMDD0014', 'NTAGFU0021', 
           'NSANSS0001', 'SATSTP0005','SATSTP0005',
           'QDASSD0015', 'QDASSD0015', 'QDASSD0015',
           'NTAFIN0002', 'NTAFIN0002', 'NTAFIN0002', 
           'NSANAN0002', 'QDAEIU0010')

site.unique <- unique(sites) # remove duplicates in my list 

# Process each dea fc dataset in the list
for(site.location in site.unique) {

  site.path <- paste(directory,paste0(site.location,".csv"), sep = "/")

  veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
  site.info.data <- veg.info$site.info
  site.info.index <- which(site.info.data$site_location_name == site.location)

  # Preprocess data
  dea.data <- read.csv(site.path)
  dea.data <- subset(dea.data, subset = (ue < 27)) # Filter out for ue less than 27 
  dea.data <- trim_to_nearest_coord(site.info.index, veg.info, dea.data, sites.query) # spatial trimming

  dea.data.agg <- aggregate(dea.data, by = list(dea.data$time),
                          FUN = mean, na.rm = T) # aggregate each timeslice by the mean

  dea.data.agg$Group.1 <- as.Date(dea.data.agg$Group.1)
  dea.data.agg$time <- dea.data.agg$Group.1
  dea.data.agg <- dea.data.agg[,-1] # Remove the #group.1 column

  write.csv(x = dea.data.agg,file = paste0(site.location,".csv"))
}
