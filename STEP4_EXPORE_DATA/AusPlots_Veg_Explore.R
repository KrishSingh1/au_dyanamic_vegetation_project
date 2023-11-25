###### AusPlots Vegetation Data exploration ######
## Author: Krish Singh
## Date: 230808
## Purpose: To explore the data in AusPlots, particularly in the vegetation information

library(ausplotsR)
library(reshape2)
library(ggplot2)
library(dplyr)



veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")


# data <- sample(veg.info$site.info$site_location_name, size = 12)
# veg.info.df <- as.data.frame(veg.info$site.info)
# subset(veg.info.df, subset = (site_location_name %in% data))
# veg.subset <- subset(veg.info.df, subset = (site_location_name %in% data))
# sites.subset <- write.csv(veg.subset, "sites_subset.csv")

## Get overall visuals built in 
map_ausplots(veg.info)
#ausplots_visual(veg.info,max.plots=length(unique(veg.info$veg.PI$site_location_name)))
## 


## Inspect growth form table
# growth.form <- as.data.frame(growth_form_table(veg.info$veg.PI, m_kind = "percent_cover", cumulative = F))
# cumulative - F means absolute cover i.e. percent of points covered by the species

# saveRDS(growth.form, file = "growth_form_matrix.rds")
growth.form <- readRDS("growth_form_matrix.rds")



## Inspect fractional cover ## 
#insitu.fractional.cover.test <- fractional_cover(veg.info$veg.PI)
#saveRDS(insitu.fractional.cover, file = "AusPlots_fractional_cover.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")


fc.vio.df <- melt(insitu.fractional.cover, variable.name = "fractional_type",
                                value.name = "fractional_percent", 
                                measure.vars = c("bare","brown","green","NA."))





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
my.fractional <- merge(insitu.fractional.cover,
                       veg.info$site.info, by="site_unique")[,c("site_unique",
                                                                "bare", "brown", "green",
                                                                "NA.", "longitude", "latitude",
                                                                "visit_end_date")]
my.fractional$visit_end_date <- as.Date(my.fractional$visit_end_date)



hs <- ggplot(my.fractional, aes(x=latitude)) +
  geom_histogram(binwidth = 4)
hs


my.fractional.interv <- my.fractional%>% mutate(lat.bin = cut(latitude, breaks=9))

table(my.fractional.interv$lat.bin)



my.fractional.interv.df <- melt(my.fractional.interv, variable.name = "fractional_type",
                                value.name = "fractional_percent", 
                                measure.vars = c("bare","brown","green","NA."))



scplt <- ggplot(my.fractional.interv.df, aes(x = latitude, y = fractional_percent)) +
  geom_point() + facet_wrap(.~fractional_type,ncol = 2) + stat_smooth()
scplt



brplt <- ggplot(my.fractional.interv.df, aes(y = fractional_percent, x = fractional_type)) + 
  geom_bar(width = 0.3, stat = "identity") + coord_flip() + facet_wrap(~lat.bin)
brplt

# No Scale
bxplt <- ggplot(my.fractional.interv.df, aes(x=fractional_type, y=fractional_percent)) + 
  geom_boxplot() + facet_wrap(~lat.bin)
bxplt



timeseries <- ggplot(my.fractional.interv.df, aes(x = visit_end_date, y = fractional_percent)) +
  geom_point() + facet_wrap(.~fractional_type,ncol = 2) + geom_line() + stat_smooth()
timeseries

timeseries <- ggplot(my.fractional.interv.df, aes(x = visit_end_date, y = fractional_percent,
                                                  colour = fractional_type)) +
  geom_point() + facet_wrap(.~lat.bin,ncol = 3)  + geom_line() + stat_smooth() + ylim(0,100)
timeseries


## Visualise spatial variance of growth forms 

growth.form$site_unique <- rownames(growth.form)
growth.form.df <- melt(growth.form, id = "site_unique", 
                       variable.name = "growth.form", value.name = "occurance")


my.fractional.interv.grwth <- merge(my.fractional.interv,
                                    growth.form.df, 
                                    by = "site_unique")



scplt <- ggplot(my.fractional.interv.grwth, aes(x = latitude, y = occurance)) +
  geom_point() + facet_wrap(~growth.form) + stat_smooth()
scplt


brplt <- ggplot(my.fractional.interv.grwth, aes(y = occurance, x = growth.form)) + 
  geom_bar(width = 0.3, stat = "identity") + coord_flip() + facet_wrap(~lat.bin)
brplt

bxplt <- ggplot(my.fractional.interv.grwth, aes(x = growth.form, y=occurance, fill = growth.form)) + 
  geom_boxplot() + facet_wrap(~lat.bin)
bxplt


timeseries <- ggplot(my.fractional.interv.grwth, aes(x = visit_end_date, y = occurance)) +
  geom_point() + facet_wrap(~growth.form,ncol = 4) + geom_line() + stat_smooth()
timeseries


timeseries <- ggplot(my.fractional.interv.grwth, aes(x = visit_end_date, y = occurance, colour = growth.form)) +
  geom_point() + facet_wrap(~lat.bin,ncol = 3) + geom_line() + stat_smooth() + ylim(0,100)
timeseries

