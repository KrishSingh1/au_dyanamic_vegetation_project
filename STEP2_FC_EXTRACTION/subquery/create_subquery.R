directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/New_Batch/"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
length(unique(fileNames))

query <- read.csv('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Polygons_20240619/Merged_Polygons/AusPlots_Merged_Completed.csv')

difference <- setdiff(query$site_location_name, fileNames)

subquery <- query[query$site_location_name %in% difference,]

write.csv(subquery,'AusPlots_Merged_Completed_d.csv')
