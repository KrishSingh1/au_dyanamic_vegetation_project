### DEA FC Burn Visualisations ### 
#  Krish Singh
#  240121


# Library -----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(sf)

# Function ----------------------------------------------------------------

get_preprocessed_dea_fc <- function(query, 
                                    directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files',
                                    veg.info){
  
  files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
  file.names <- tools::file_path_sans_ext(files)
  
  if(all(query %in% file.names) == F) {
    print(paste0("Error: Cannot find ", query, " in given directory"))
    return(NULL)
  }
  
  dea.fc <- fread(paste0(directory, "/", query, ".csv")) # use data.table for faster processing
  dea.fc <- trim_to_nearest_coord(data.table(veg.info$site.info), dea.fc, query)
  dea.fc <- subset(dea.fc, subset = (ue <= 25.5))
  
  dea.fc <- aggregate(dea.fc[,c('bs', 'pv', 'npv')], 
                      by = list(dea.fc$time), FUN = mean, na.rm = T)
  colnames(dea.fc)[1] = 'time'
  
  
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


plot_Burn_gg <- function(dea.fc, query, burn.date, f_type = 'pv') {
  print(query)
  
  burn.date <- subset(burn.date, subset = (site_location_name ==  query))
  burn.date$date <- as.Date(burn.date$date)
  
  
  g <- ggplot(data = dea.fc, mapping = aes(x = time, y = .data[[f_type]])) +
    geom_line() + labs(title = paste0(query, " ", f_type, " and Burn Dates")) +
    geom_hline(yintercept = 0, linetype = 4, color = 'green')
  
  for (d in 1:nrow(burn.date)) {
    print(burn.date[d,]$date)
    g <- g + geom_vline(xintercept = as.numeric(burn.date[d,]$date), 
                        linetype = 4, color = 'red')
  }
  
  return(g)
}

main <- function(query, directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files'
                 , veg.info){
  
  dea.fc <- get_preprocessed_dea_fc(query, veg.info = veg.info)
  dea.fc$time <- as.Date(dea.fc$time)
  
  burn.date <- read.csv('../DATASETS/AusPlots_BurnDate.csv')
  burn.reflectances <- read.csv('../DATASETS/AusPlots_BurnReflectances.csv')
  hist.shp <- st_read("../DATASETS/AusPlots_Historical_BurnDates.shp")
  hist.shp <- hist.shp[!is.na(hist.shp$igntn_d)  | !is.na(hist.shp$captr_d),]
  dates <- data.frame("site_location_name" = c(burn.date$site_location_name, hist.shp$Name),
                      "date" = c(burn.date$date, as.character(hist.shp$igntn_d)))
  dates$date <- as.Date(dates$date)
  
  
  return(plot_Burn_gg(dea.fc, query, dates))
}


# Main --------------------------------------------------------------------

directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files'
#files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
#file.names <- tools::file_path_sans_ext(files)

veg.info <- readRDS('../DATASETS/site_veg_2-0-6.rds')
query <- 'NSANAN0002'
dea.fc <- get_preprocessed_dea_fc('NSANAN0002', veg.info = veg.info)
dea.fc$time <- as.Date(dea.fc$time)


# Gather burn dates data 
burn.date <- read.csv('../DATASETS/AusPlots_BurnDate.csv')
burn.reflectances <- read.csv('../DATASETS/AusPlots_BurnReflectances.csv')
hist.shp <- st_read("../DATASETS/AusPlots_Historical_BurnDates.shp")
hist.shp <- hist.shp[!is.na(hist.shp$igntn_d)  | !is.na(hist.shp$captr_d),]
dates <- data.frame("site_location_name" = c(burn.date$site_location_name, hist.shp$Name),
                    "date" = c(burn.date$date, as.character(hist.shp$igntn_d)))
dates$date <- as.Date(dates$date)

# Plot burn date 
plot_Burn_gg(dea.fc, query, dates)

# Streamline the process 
main(query, veg.info =veg.info)
main('NSANSS0001', veg.info =veg.info)
main('NTAGFU0021', veg.info =veg.info)
main('QDASSD0015', veg.info =veg.info)
main('WAAPIL0003', veg.info =veg.info)
main('NSANAN0002', veg.info =veg.info)
main('WAAGAS0002', veg.info =veg.info)









