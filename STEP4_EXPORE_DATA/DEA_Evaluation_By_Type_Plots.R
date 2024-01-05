#### DEA EVALUATION BY TYPE PLOTS #### 
# By Krish Singh
# Date: 240105
# Purpose: To visually explore the alignment of AusPlots and DEA data. 

growth.form <- readRDS("growth_form_matrix.rds")

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

growth.form$site_location_name <- unlist(lapply(rownames(growth.form), get_location_name))
growth.form.agg <- aggregate(growth.form, by = list(growth.form$site_location_name), FUN = mean, na.rm = T)
colnames(growth.form.agg)[which(colnames(growth.form.agg) == 'Group.1')] <- 'site_location_name'

# Sum Growth Forms by Classification --------------------------------------

growth.form.classification <- read.csv("../DATASETS/Growth_Type_Classification.csv",header = F)
growth.form.classification <- na.omit(growth.form.classification)

grass.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Grass']
shrub.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Shrub']
tree.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Tree']

growth.form.agg$grass <- rowSums(growth.form.agg[,grass.names], na.rm = T)
growth.form.agg$shrub <- rowSums(growth.form.agg[,shrub.names], na.rm = T)
growth.form.agg$tree <- rowSums(growth.form.agg[,tree.names], na.rm = T)


# Begin classification ----------------------------------------------------

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("grass","shrub","tree")])))
}

growth.form.agg$vegetation_type <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = classify))
