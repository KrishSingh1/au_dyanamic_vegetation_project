library(leaflet)

veg.info <- readRDS("../DATASETS/AusPlots_Extracted_Data/site_veg_2-0-6.rds")
site.info <- as.data.frame(veg.info$site.info)
focus_sites <- site.info[site.info[['site_location_name']] %in%  c('NSANAN0002', 'NTAGFU0021', 'WAACOO0024'),]


aus.map.var <- leaflet() %>% addTiles %>%
  addCircleMarkers(data = site.info,
                   lat = ~latitude,
                   lng = ~longitude,
                   radius = 0.5,
                   fillOpacity = 1, color = 'green') %>%
  addMarkers(data = focus_sites,
    lng = ~longitude, lat = ~latitude,
    label = ~site_location_name,
    labelOptions = labelOptions(noHide = TRUE, direction = "right",
                                style = list("font-size" = "8px", "opacity"= "0.5"))) %>%
  addCircleMarkers(data = focus_sites,
                   lat = ~latitude,
                   lng = ~longitude,
                   radius = 0.5,
                   fillOpacity = 1, color = 'red')
  

aus.map.var
