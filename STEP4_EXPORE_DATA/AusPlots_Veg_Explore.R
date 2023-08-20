###### AusPlots Vegetation Data exploration ######
## Author: Krish Singh
## Date: 230808
## Purpose: To explore the data in AusPlots, particularly in the vegetation information



library(ausplotsR)

veg.info <- readRDS("site_veg.rds")
map_ausplots(veg.info)
ausplots_visual(veg.info,max.plots=length(unique(veg.info$veg.PI$site_location_name)) )

growth.form2 <- as.data.frame(growth_form_table(veg.info$veg.PI, m_kind = "percent_cover",
                                               cover_type = "OCC", cumulative = F, species_name = 'HD'))
#saveRDS(growth.form, file = "growth_form_matrix.rds")
growth.form <- readRDS("growth_form_matrix.rds")

insitu.fractional.cover <- fractional_cover(veg.info$veg.PI)
#insitu.fractional.cover2 <- fractional_cover(veg.info$veg.PI, 
#                                            ground_fractional = T, in_canopy_sky = F)
#saveRDS(insitu.fractional.cover, file = "AusPlots_fractional_cover.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")



###
my.fractional <- merge(insitu.fractional.cover, veg.info$site.info, by="site_unique")[,c("site_unique", "bare", "brown", "green", "other", "longitude", "latitude")]
my.fractional <- na.omit(insitu.fractional.cover)

head(my.fractional)

insitu.fractional.cover.g <- fractional_cover(veg.info$veg.PI,ground_fractional = T)
