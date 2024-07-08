########## AusPlots Grass Cover by Tribe Extractor ############
# Krish Singh
# 20240630
# Objective: To visualize sites dominated by Chloridoideae and Andropogonae 

# Libraries  --------------------------------------------------------------
library(dplyr)
library(leaflet)
library(BAMMtools)


# Functions ---------------------------------------------------------------
plot_aus_map <- function(df, variable){
  
  df["var"] <- df[variable]
  col.var <- colorNumeric(palette = c("red", 'green'), domain = df$var)
  aus.map.var <- leaflet(df) %>% addTiles %>%
    addCircleMarkers(data = df,
                     lat = ~latitude,
                     lng = ~longitude,
                     color = ~col.var(var),
                     radius = 1,
                     fillOpacity = 1, 
                     popup= ~paste0(variable, ": ", var, "<br>", 
                                    "site: ", site_location_name, "<br>",
                                    "longitude: ", longitude, "<br>",
                                    "latitude: ", latitude)
    )  %>%
    addLegend("bottomright", pal = col.var, values = ~var,
              title = paste0(variable, ' (%, hits/1010)'), opacity = 1)
  
  
  return(aus.map.var)
}



# Main --------------------------------------------------------------------

grass_tribes <- read.csv('Output/ausplots_grass_cover_by_tribe.csv')

# For the Purposes of visualising this on a map, I will need to aggregate the percent cover, FUN = mean
grass_tribes_agg <- aggregate(grass_tribes[,c('Andropogoneae', 'Chloridoideae')],
                              by = list(grass_tribes$site_location_name), FUN = mean)
colnames(grass_tribes_agg)[1] <- 'site_location_name'




grass_tribes_agg <- grass_tribes_agg %>%
  left_join(grass_tribes[,c('site_location_name', 'latitude', 'longitude')])


# Plot Andropogoneae
plot_aus_map(grass_tribes_agg[which(grass_tribes_agg$Andropogoneae > 0),], colnames(grass_tribes_agg)[2])

# Plot Chloridoideae
plot_aus_map(grass_tribes_agg[which(grass_tribes_agg$Chloridoideae > 0),], colnames(grass_tribes_agg)[3])







