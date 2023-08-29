###### AusPlots Vegetation Data exploration advanced ######
## Author: Krish Singh
## Date: 230824
## Purpose: To explore the data in AusPlots focusing on temporal variation

library(ausplotsR)
library(reshape2)
library(ggplot2)
library(dplyr)
library(plotly)



veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
growth.form <- readRDS("growth_form_matrix.rds")
growth.form.strata <- growth_form_table(veg.info$veg.PI, m_kind = "percent_cover", 
                                        cumulative = FALSE, by_strata = TRUE)
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")


site.names <- unique(veg.info$site.info$site_location_name)
site.observations <- unique(veg.info$site.info$site_unique)

# Count the number of observations of each site


df <- as.data.frame(t(data.frame(purrr::flatten(lapply(site.observations, strsplit, '-')))))


counts <- c()
for (n in site.names){
  counts <- c(counts, sum(df$V1 == n))
}
counts.df <- data.frame(site.names, counts)

sites.revisit.df <- subset(counts.df, counts > 1) # For all revisited data
sites.revisit.2.df <- subset(counts.df, counts == 2) # For sites that were only visited twice 

info <- as.data.frame(veg.info$site.info)[,c("site_location_name", "visit_start_date", "site_unique")]
info.revisit.2 <- subset(info, subset = (site_location_name %in% sites.revisit.2.df$site.names))
info.revisit.2$visit_start_date <- as.Date(info.revisit.2$visit_start_date)

info.revisit.2.sorted <- info.revisit.2 %>% arrange(+visit_start_date)

indexes <- c()
for (n in sites.revisit.2.df$site.names){
  indexes <- c(indexes, which(info.revisit.2.sorted$site_location_name == n)[1])
}
  
info.revisit.2.t1 <- info.revisit.2.sorted[indexes,]
info.revisit.2.t2 <- info.revisit.2.sorted[-indexes,]


growth.form$site_unique <- rownames(growth.form)
growth.form.df <- melt(growth.form, id = "site_unique", 
                       variable.name = "growth.form", value.name = "occurance")


growth.form.df.t1 <- merge(growth.form.df, info.revisit.2.t1, by = 'site_unique')

growth.form.df.t2 <- merge(growth.form.df, info.revisit.2.t2, by = 'site_unique')

length(unique(growth.form.df.t1$site_location_name))
length(unique(growth.form.df.t2$site_location_name))

missing_location.t1 <- setdiff(unique(growth.form.df.t1$site_location_name),
                               unique(growth.form.df.t2$site_location_name))
missing_location.t2 <- setdiff(unique(growth.form.df.t2$site_location_name),
                               unique(growth.form.df.t1$site_location_name))


growth.form.df.t1.filtered <-subset(growth.form.df.t1, 
                                    subset = !(site_location_name %in% missing_location.t1)) %>% 
  arrange(site_location_name, growth.form)

growth.form.df.t2.filtered <-subset(growth.form.df.t2,
                                    subset = !(site_location_name %in% missing_location.t2)) %>% 
  arrange(site_location_name, growth.form) 

# Check if the column values for growth form and site_location_name are identitcal
all(growth.form.df.t1.filtered$growth.form == growth.form.df.t2.filtered$growth.form) == 
  all(growth.form.df.t1.filtered$site_location_name == growth.form.df.t2.filtered$site_location_name)


growth.form.change.df <- merge(growth.form.df.t1.filtered, growth.form.df.t2.filtered, by = c("site_location_name", "growth.form"))



### Plot 1: Individual growth forms 


change.p <- ggplot(growth.form.change.df ,map = aes(x = occurance.x, y = occurance.y,
           colour =  growth.form)) + geom_point() + geom_abline() + geom_smooth()
ggplotly(change.p, tooltip = c("occurance.x", "occurance.y", 
                               "growth.form", "visit_start_date.x",
                               "visit_start_date.y"))  


### Now looking at strata 

growth.form.strata$site_unique <- rownames(growth.form.strata)
growth.form.starta.df <- melt(growth.form.strata, id = "site_unique", 
                              variable.name = "strata", value.name = "percentage_cover")
growth.form.strata.t1 <- merge(growth.form.starta.df, 
                               info.revisit.2.t1, by = 'site_unique')
growth.form.strata.t2 <- merge(growth.form.starta.df,
                               info.revisit.2.t2, by = 'site_unique')

missing_location.t1 <- setdiff(unique(growth.form.strata.t1$site_location_name), unique(growth.form.strata.t2$site_location_name))
missing_location.t2 <- setdiff(unique(growth.form.strata.t2$site_location_name), unique(growth.form.strata.t1$site_location_name))


growth.form.strata.t1.filtered <-subset(growth.form.strata.t1, subset = !(site_location_name %in% missing_location.t1)) %>% 
  arrange(site_location_name, strata)
growth.form.strata.t2.filtered <-subset(growth.form.strata.t2, subset = !(site_location_name %in% missing_location.t2)) %>% 
  arrange(site_location_name, strata) 

growth.form.strata.change <- merge(growth.form.strata.t1, growth.form.strata.t2, by = c("site_location_name", "strata"))



### Plot 2 - Strata together 

change.p <- ggplot(growth.form.strata.change, 
                   map = aes(x = percentage_cover.x, y = percentage_cover.y,
                                                    colour =  strata)) + geom_point() + geom_abline() 
ggplotly(change.p) 

change.p <- ggplot(growth.form.strata.change ,map = aes(x = percentage_cover.x, y = percentage_cover.y,
                                                        colour =  strata)) + geom_point() + geom_abline() +
  facet_grid(~strata)


### Plot 3 - Strata seperated 

ggplotly(change.p) 


Metrics::rmse(actual = growth.form.strata.change$percentage_cover.x, 
              predicted = growth.form.strata.change$percentage_cover.y)

Metrics::bias(actual = growth.form.strata.change$percentage_cover.x, 
              predicted = growth.form.strata.change$percentage_cover.y)


Metrics::mae(actual = growth.form.strata.change$percentage_cover.x, 
              predicted = growth.form.strata.change$percentage_cover.y)

