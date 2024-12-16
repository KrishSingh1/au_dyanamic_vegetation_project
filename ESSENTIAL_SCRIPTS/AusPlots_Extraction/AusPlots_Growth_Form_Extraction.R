#### AusPlots Growth Form Extraction #### 
# By Krish Singh
# Date: 240105
# Purpose: To extract the growth form of AusPlots sites 

# Libraries ---------------------------------------------------------------

library(ausplotsR)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------

veg_info <- readRDS("../DATASETS/AusPlots_Extracted_Data/Final/site_veg_Final2-0-6.rds")
version <- gsub('\\.', '-', packageVersion("ausplotsR"))

growth.forms <- growth_form_table(veg_info$veg.PI, m_kind = "percent_cover",
                                  cover_type = "PFC", species_name = "SN", cumulative = F)

write.csv(growth.forms, paste0('../DATASETS/AusPlots_Extracted_Data/Final/','growth_forms_pc_final_', version, '.csv'))

# Junk Code (don't run) ---------------------------------------------------

# Testing why fc iter gives different number of obs than fc obtained via batch

fc <- read.csv('../DATASETS/AusPlots_FC_Iter_2_0_6.csv')
fc.pub <- subset(fc, subset = (fc$error != 3 & fc$error != 1)) # fc.pub - fc with current published data 

fc.batch <- readRDS('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/AusPlots_Query/Output/all_veg_fc.RDS')

# Between fc and fc.batch
setdiff(fc.batch$site_unique, fc$site_unique)
# character(0)

setdiff(fc$site_unique, fc.batch$site_unique)
# [1] "SASMDD0010-57064" "SASMDD0012-58020" "SAAFLB0004-58611" "SAAFLB0030-57075"
# [5] "SASMDD0003-57065" "SASMDD0009-57063" "SAAFLB0003-58636" "SAAFLB0031-57076"
# [9] "SASMDD0008-57062" "QDASEQ0001-58722" "QDASSD0010-57630"

# Between fc.pub and fc.batch

setdiff(fc.pub$site_unique, fc$site_unique)
# character(0)

setdiff(fc$site_unique, fc.pub$site_unique)
# [1] "SASMDD0012-58020" "SAAFLB0004-58611" "SAAFLB0030-57075" "SASMDD0003-57065"
# [5] "SASMDD0009-57063" "SAAFLB0003-58636" "SAAFLB0031-57076" "SASMDD0008-57062"
# [9] "QDASEQ0001-58722" "QDASSD0010-57630"

# now check the difference between the two differences 
setdiff(setdiff(fc$site_unique, fc.batch$site_unique), 
        setdiff(fc$site_unique, fc.pub$site_unique))

# [1] "SASMDD0010-57064"

setdiff(setdiff(fc$site_unique, fc.pub$site_unique),
        setdiff(fc$site_unique, fc.batch$site_unique))
# character(0)

veg_info$site.info[veg_info$site.info$site_unique == 'SASMDD0010-57064',]

fc[fc$site_unique == 'SASMDD0010-57064',] # this visit is the reason 

# Testing why growth forms has 854 obs, while fc.pub has 856 visits 

setdiff(rownames(growth.forms), fc$site_unique)
# character(0)

setdiff(fc.pub$site_unique, rownames(growth.forms))

# [1] "QDADEU0002-58962" "QDADEU0003-58898"

subset(fc.batch, subset = (site_unique %in%
                             setdiff(fc.pub$site_unique, rownames(growth.forms))))

#                       site_unique bare brown green other
# QDADEU0002-58962 QDADEU0002-58962 19.4  16.6  61.7   2.3
# QDADEU0003-58898 QDADEU0003-58898  4.4  35.4  59.3   0.9

subset(veg_info$veg.PI, subset = (site_unique == 'QDADEU0002-58962'))
# provides high number of NA

subset(veg_info$veg.PI, subset = (site_unique == 'QDADEU0003-58898'))
# provides high number of NA

# Testing QDADEU0002 and QDADEU0003 individually

QDADEU0003 <- get_ausplots('QDADEU0003', veg.PI = T)

growth.forms.QDADEU0003 <- growth_form_table(QDADEU0003$veg.PI, m_kind = "percent_cover",
                                  cover_type = "PFC", species_name = "SN")
# Returns nothing 

QDADEU0002 <- get_ausplots('QDADEU0002', veg.PI = T)

growth.forms.QDADEU0002 <- growth_form_table(QDADEU0002$veg.PI, m_kind = "percent_cover",
                                             cover_type = "PFC", species_name = "SN")


