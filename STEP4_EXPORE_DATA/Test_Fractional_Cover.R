veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")

# Test Default ------------------------------------------------------------
insitu.fractional.cover.test <- fractional_cover(veg.info$veg.PI)
saveRDS(insitu.fractional.cover.test, file = "../STEP2_VEG_EXTRACTION/insitu_fractional_cover_default_2-0-3rds")

# Test Ground_Cover -------------------------------------------------------

insitu.fractional.cover.ground <- fractional_cover(veg.info$veg.PI, ground_fractional = TRUE)
saveRDS(insitu.fractional.cover.ground, file = "../STEP2_VEG_EXTRACTION/insitu_fractional_cover_ground_2-0-3rds")


# Test Canopy_Cover -------------------------------------------------------

insitu.fractional.cover.canopy <- fractional_cover(veg.info$veg.PI, in_canopy_sky = TRUE)
saveRDS(insitu.fractional.cover.canopy, file = "../STEP2_VEG_EXTRACTION/insitu_fractional_cover_canopy_2-0-3rds")

