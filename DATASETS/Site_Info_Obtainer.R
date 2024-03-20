site_veg <- readRDS('../DATASETS/site_veg_2-0-6.rds')
site.info <- site_veg$site.info
write.csv(site.info, '../DATASETS/site_info_2-0-6.csv')