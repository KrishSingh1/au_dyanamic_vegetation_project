#### AusPlots Site Classification by Veg Type #### 
# By Krish Singh
# Date: 2024-04-22
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


classify <- function(dataset.row) {
  return(names(which.max(dataset.row[colnames(growth.form.agg)[which(colnames(growth.form.agg) != 'site_location_name')]])))
}
growth.form.agg$vegetation_type <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = classify))

write.csv(growth.form.agg,'../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv')
write.csv(growth.form.agg,'../DATASETS_TO_SHARE/AusPlots_Dominant_Growth_Form/growth_forms_classification_by_dom_species_final_2-0-6.csv')


# Junk Script (Don't run) -------------------------------------------------

# Check if the aggregation gives the correct number of site_location_name s
length(unique(growth.form$site_location_name)) == nrow(growth.form.agg)


