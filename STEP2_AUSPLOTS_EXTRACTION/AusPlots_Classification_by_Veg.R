#### AusPlots Site Classification by Veg Type #### 
# By Krish Singh
# Date: 240105
# Purpose: To classify ausplots sites by veg type


# Libraries ---------------------------------------------------------------

library(ausplotsR)

# Functions ---------------------------------------------------------------

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}


# Main --------------------------------------------------------------------

growth.form <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_pc_final_2-0-6.csv')
  
# Get Site location names from unique_site_name
growth.form$site_location_name <- unlist(lapply(growth.form$X, get_location_name))

# Aggregate the percent coverage by site location name, by the mean 
growth.form.agg <- aggregate(growth.form, by = list(growth.form$site_location_name), FUN = mean, na.rm = T)


# Remove columns 'X' and 'site_location_name'
growth.form.agg <- growth.form.agg[,!(names(growth.form.agg) %in% c("X","site_location_name"))]
# Rename group.1, the actual aggregate as site_location_name 
colnames(growth.form.agg)[which(colnames(growth.form.agg) == 'Group.1')] <- 'site_location_name'

# Sum Growth Forms by Classification --------------------------------------

# Load classification scheme
growth.form.classification <- read.csv("../DATASETS/AusPlots_Extracted_Data/Growth_Type_Classification.csv", header = F)
growth.form.classification <- na.omit(growth.form.classification)

grass.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Grass']
shrub.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Shrub']
tree.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Tree']

# Give row sum based on the groupings 
growth.form.agg$grass <- rowSums(growth.form.agg[,grass.names], na.rm = T)
growth.form.agg$shrub <- rowSums(growth.form.agg[,shrub.names], na.rm = T)
growth.form.agg$tree <- rowSums(growth.form.agg[,tree.names], na.rm = T)


# Begin classification ----------------------------------------------------

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("grass","shrub","tree")])))
}

growth.form.agg$vegetation_type <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = classify))


version <- gsub('\\.', '-', packageVersion("ausplotsR"))
file.pathd <- paste0('../DATASETS/AusPlots_Extracted_Data/Final','AusPlots_Sites_Classified_', version, '.csv')
write.csv(growth.form.agg,file.pathd)


# Junk Script (Don't run) -------------------------------------------------

# Check if the aggregation gives the correct number of site_location_name s
length(unique(growth.form$site_location_name)) == nrow(growth.form.agg)


