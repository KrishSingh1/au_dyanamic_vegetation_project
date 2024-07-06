########## AusPlots Grass Cover by Tribe Extractor ############
# Krish Singh
# 20240630
# Purpose: Use the reference grass species list to aggregate
#          grass cover by their tribe 

# Libraries ---------------------------------------------------------------
library(ausplotsR)
library(dplyr)

# Functions ---------------------------------------------------------------

# Pre-main ----------------------------------------------------------------

# Preprocess and clean the grass tribes data

# grass_pp_list <- read.csv('Input/Grass_Tribes.csv')
# grass_pp_list <- grass_pp_list[-c(136,137,143),] # Clean the list
# colnames(grass_pp_list) <- c('species', 'pp', 'tribe', 'endemic', 'age_class', 'distribution_genus')
# grass_pp_list$genus <- unlist(lapply(grass_pp_list$species, FUN = function(x){
#   return(unlist(strsplit(x, split = ' '))[1])}))
# grass_pp_list <- grass_pp_list[, c('species', 'genus','pp', 'tribe', 'endemic', 'age_class')]
# 
# 
# write.csv(grass_pp_list, 'Input/Grass_Tribes_Cleaned.csv')


# Main --------------------------------------------------------------------

# Algorithm:
# 1. Get species reference and AusPlots veg pi data
# 2. Calculate the cover of genus species with veg pi
# 3. Derive the genus of each species in the cover data
# 4. For each genus, search up the associated species in the species reference data to determine the grass tribe
# 5. Allocate the tribe of each genus
# 6. Aggregate the cover data by the tribe name

# Using Veg PI already Collected  -----------------------------------------

site_info <- readRDS('Input/site_veg_Final2-0-6.rds')
grass_pp_list <- read.csv('Input/Grass_Tribes_Cleaned.csv')

# This function basically counts the frequency of each species of each hit against the total number of point intercepts (1010)
# See documentation '?ausplotsR::species_table'
species_pc <- species_table(site_info$veg.PI, m_kind=c( "percent_cover"),
                            cover_type=c("OCC"), species_name=c("GS"), 
                            strip_bryophytes=FALSE)


test <- as.data.frame(t(species_pc)) # Transpose the df so the species names are on the row and visit id as columns --> enables tribe-level aggregation

# Get tribe names 
tribe_names <- unlist(lapply(rownames(test), species_reference = grass_pp_list,
                               FUN = function(x, species_reference){
                        
    # Preprocess Species Name                             
    genus_name <- unlist(strsplit(x, split = '\\.'))[1] # remove the '.'s

    # Now Search from the species_reference, and allocate 'tribe'
    search <- species_reference[which(species_reference$genus == genus_name),]
    if(nrow(search) > 0) {
      tribe_name = search$tribe[[1]]
    } else{
      tribe_name = 'non_applicable/no_tribe' 
    }
    return(tribe_name)
}))

test$tribe_name <- tribe_names

print(table(tribe_names)) # print the number of species that were labelled Andropogoneae or Chloridoideae

# Now aggregate (sum up) cover by tribe
grass_cover <- aggregate(test[, -ncol(test)],
                         by = list(test$tribe_name), 
                         FUN = 'sum')
rownames(grass_cover) <- grass_cover$Group.1

# Transpose the cover data back to the original configuration
grass_cover <- as.data.frame(t(grass_cover[,-which(colnames(grass_cover) == 'Group.1')]))

# Add extra information, such as lat-lon coords, and other potentially useful information
# Use table joins with site information

site_info_df <- site_info$site.info 
grass_cover$site_unique <- rownames(grass_cover)
grass_cover <- grass_cover %>% 
  left_join(site_info_df)

# Subset to features that are mainly of interest, include more if desired 
cols_of_interest <- c('site_unique', 'Andropogoneae', 'Chloridoideae',
                      'non_applicable/no_tribe', 'latitude', 'longitude')

grass_cover <- grass_cover[,cols_of_interest]

# Output Results ----------------------------------------------------------


write.csv(test, 'Output/ausplots_species_cover_tribe_labelled.csv')
write.csv(grass_cover, 'Output/ausplots_grass_cover_by_tribe.csv')


# TESTING -----------------------------------------------------------------
# library(ausplotsR)
# 
# # Confirmation that this manual process is equivalent to the one used in ausplots
# # Note: mine does not take account to sites visited multiple times 
# 
# sites_list <- get_ausplots() # get a list of all sites 
# site_location_name <- unique(sites_list$site.info$site_location_name)
# test_site_name <- site_location_name[4] # pick any number with one visit 
# site_data <- get_ausplots(test_site_name, veg.PI = T)
# site_data_veg_pi <- site_data$veg.PI
# 
# total_points <- length(unique(site_data_veg_pi$hits_unique))
# species_list <- unique(site_data_veg_pi$genus_species)
# species_list <- na.omit(unique(site_data_veg_pi$genus_species))
# 
# species_fraction = c()
# for(s in species_list){
#   hits <- nrow(unique(subset(site_data_veg_pi,
#                              subset = (genus_species == s)))) # have each species count only once per hit
#   percent_cover <- round(hits/total_points * 100, 1)
#   species_fraction <- c(species_fraction, percent_cover)
# }
# 
# 
# names(species_fraction) = species_list
# species_fraction <- as.data.frame(t(as.data.frame(species_fraction)))
# rownames(species_fraction) <- site_data_veg_pi$site_unique[1]
# species_fraction <- species_fraction[,order(colnames(species_fraction)) ]
# 
# # Now Test against Ausplots 
# ausplots_calc_species_table <- species_table(site_data_veg_pi, m_kind=c( "percent_cover"),
#                                              cover_type=c("OFC"), species_name=c("GS"),
#                                              strip_bryophytes=FALSE)
# 
# # Check the row sums 
# print(rowSums(ausplots_calc_species_table) == rowSums(species_fraction))
# 
# # Check cover value of each species
# print(sum(ausplots_calc_species_table == species_fraction) == ncol(species_fraction))
