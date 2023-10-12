library(leaflet)

veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
site.info <- as.data.frame(veg.info$site.info)


aus.map.var <- leaflet(site.info) %>% addTiles %>%
  addCircleMarkers(data = site.info,
                   lat = ~latitude,
                   lng = ~longitude,
                   radius = 0.5,
                   fillOpacity = 1, color = 'green') %>% 
aus.map.var
