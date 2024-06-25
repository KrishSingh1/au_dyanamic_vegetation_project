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

# Now calculate the relative dominance: percent of dominant species/total sum of percentage of species
calc.relative.dominance <- function(dataset.row) {
  # Get and covert species percent into numeric 
  site.species.percentages <- as.numeric(dataset.row[which(!(names(dataset.row) %in% c('site_location_name', 'vegetation_type')))])
  site.sum <- sum(site.species.percentages, na.rm = T) # calc the sum
  dominant.species.percent <- as.numeric(dataset.row[dataset.row['vegetation_type'][[1]]])
  rel_dom <- dominant.species.percent/site.sum # get the relative dominance
  return(rel_dom)
}

growth.form.agg$relative_dominance <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = calc.relative.dominance))



# Now calculate the relative dominance: percent of dominant species/total sum of percentage of species
calc.scaled.dominance <- function(dataset.row) {
  # Get and covert species percent into numeric 
  dominant.species.percent <- as.numeric(dataset.row[dataset.row['vegetation_type'][[1]]])
  rel_dom <- as.numeric(dataset.row['relative_dominance'])
  
  scaled_dom <- dominant.species.percent * rel_dom # get the scaled dominance
  return(scaled_dom)
}

growth.form.agg$scaled_dom <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = calc.scaled.dominance))

write.csv(growth.form.agg,'../DATASETS/AusPlots_Extracted_Data/Final/growth_forms_classification_by_dom_species_final_2-0-6.csv')

# Junk Script (Don't run) -------------------------------------------------

# Check if the aggregation gives the correct number of site_location_name s
length(unique(growth.form$site_location_name)) == nrow(growth.form.agg)


