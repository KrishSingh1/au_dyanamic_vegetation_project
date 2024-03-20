library('geosphere')


doy = 1:365
# Test 1: lat 33.8688
lat = 33.8688
photoperiods_test_R = geosphere::daylength(lat = lat, doy = doy)
photoperiod_test_1 =  data.frame("PhotoPeriod" = photoperiods_test_R, dayOfYear = doy, lat = rep(lat, length(doy)) )
write.csv(photoperiod_test_1, "photoperiod_test_1.csv")

# Test 2: Now test a site WAAPIL0003

site_subset <- read.csv("../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv")
lat <- subset(site_subset, subset = (site_location_name == 'WAAPIL0003'))['latitude'][[1]]
photoperiods_test_2_R = geosphere::daylength(lat = lat, doy = doy)
photoperiod_test_2 =  data.frame("PhotoPeriod" = photoperiods_test_2_R, dayOfYear = doy, lat = rep(lat, length(doy)) )
write.csv(photoperiod_test_2, "photoperiod_test_2.csv")