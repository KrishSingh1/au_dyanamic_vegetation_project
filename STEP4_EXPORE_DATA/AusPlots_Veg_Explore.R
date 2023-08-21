###### AusPlots Vegetation Data exploration ######
## Author: Krish Singh
## Date: 230808
## Purpose: To explore the data in AusPlots, particularly in the vegetation information



library(ausplotsR)

veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")


## Get overall visuals built in 
map_ausplots(veg.info)
ausplots_visual(veg.info,max.plots=length(unique(veg.info$veg.PI$site_location_name)))
## 


## Inspect growth form table
growth.form <- as.data.frame(growth_form_table(veg.info$veg.PI, m_kind = "percent_cover", cumulative = F))
# cumulative - F means absolute cover i.e. percent of points covered by the species

# saveRDS(growth.form, file = "growth_form_matrix.rds")
growth.form <- readRDS("growth_form_matrix.rds")




## Inspect fractional cover ## 
#insitu.fractional.cover <- fractional_cover(veg.info$veg.PI)
#saveRDS(insitu.fractional.cover, file = "AusPlots_fractional_cover.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")



fc.vio <- as.list(insitu.fractional.cover)

fractional_type <- c()
fractional_percent <- c()

for(n in names(fc.vio[-1])){
  names(fc.vio[[n]]) <- rep(n, length(fc.vio[[n]]))
  fractional_type <- c(fractional_type, names(fc.vio[[n]]))
  fractional_percent <- c(fractional_percent, fc.vio[[n]])
}

fc.vio.df <- data.frame(fractional_type,fractional_percent)


library(ggplot2)

# IN LOG10
violin <- ggplot(fc.vio.df, aes(x=fractional_type, y=fractional_percent)) + 
  geom_violin(trim = F) + scale_y_log10() + geom_boxplot(width=0.1) + theme_minimal()
violin

# No Scale
bxplt <- ggplot(fc.vio.df, aes(x=fractional_type, y=fractional_percent)) + 
  geom_boxplot() + theme_minimal()
bxplt


hs <- ggplot(fc.vio.df, aes(x=fractional_percent)) + 
  geom_histogram(bins = 8) + facet_grid(. ~fractional_type)
hs



brplt <- ggplot(fc.vio.df, aes(y = fractional_percent, x = fractional_type)) + 
  geom_bar(width = 0.3, stat = "identity") + coord_flip() + theme_minimal()
brplt



### 
my.fractional <- merge(insitu.fractional.cover, veg.info$site.info, by="site_unique")[,c("site_unique", "bare", "brown", "green", "NA.", "longitude", "latitude")]


library(dplyr)

hs <- ggplot(my.fractional, aes(x=latitude)) +
  geom_histogram(binwidth = 4)
hs


my.fractional.interv <- my.fractional %>% mutate(lat.bin = cut(latitude, breaks=9))



fractional_type.interv <- rep(x = c("bare", "brown", "green", "NA."),
                              each = nrow(my.fractional.interv))

fractional_percent.interv <- c(my.fractional.interv$bare,
                               my.fractional.interv$brown,
                               my.fractional.interv$green,
                               my.fractional.interv$NA.)


latitudinal_bin <- rep(my.fractional.interv$lat.bin, 
                       times = 4 )


my.fractional.interv.df <- data.frame(fractional_type.interv,
                                      fractional_percent.interv,
                                      latitudinal_bin)



brplt <- ggplot(my.fractional.interv.df, aes(y = fractional_percent.interv, x = fractional_type.interv)) + 
  geom_bar(width = 0.3, stat = "identity") + coord_flip() + theme_minimal() +
  facet_wrap(latitudinal_bin)
brplt

# No Scale
bxplt <- ggplot(my.fractional.interv.df, aes(x=fractional_type, y=fractional_percent)) + 
  geom_boxplot() + facet_wrap(latitudinal_bin)
bxplt








