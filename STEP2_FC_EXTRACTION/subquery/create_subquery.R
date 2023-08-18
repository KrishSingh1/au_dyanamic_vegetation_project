directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
length(unique(fileNames))

query <- read.csv('sites_info_query.csv')

difference <- setdiff(query$site_location_name, fileNames)

subquery <- query[query$site_location_name %in% difference,]
subquery <- subquery[, c('site_location_name', 
                         'pit_marker_easting',
                         'pit_marker_northing',
                         'pit_marker_mga_zones')]
write.csv(subquery,'sites_info_subquery_c.csv')
