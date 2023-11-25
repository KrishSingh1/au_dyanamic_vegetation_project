##### Explore the differences of Precipitation effects in Vegetation Types #####


# libraries ---------------------------------------------------------------

library(ggplot2)
library(cowplot)



# Start -------------------------------------------------------------------

growth.form <- readRDS("growth_form_matrix.rds")

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

growth.form$site_location_name <- unlist(lapply(rownames(growth.form), get_location_name))
growth.form.agg <- aggregate(growth.form, by = list(growth.form$site_location_name), FUN = mean, na.rm = T)
colnames(growth.form.agg)[which(colnames(growth.form.agg) == 'Group.1')] <- 'site_location_name'

# Sum Growth Forms by Classification --------------------------------------

growth.form.classification <- read.csv("Growth_Type_Classification.csv",header = F)
growth.form.classification <- na.omit(growth.form.classification)

grass.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Grass']
shrub.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Shrub']
tree.names <- growth.form.classification$V1[growth.form.classification$V2 == 'Tree']

growth.form.agg$grass <- rowSums(growth.form.agg[,grass.names], na.rm = T)
growth.form.agg$shrub <- rowSums(growth.form.agg[,shrub.names], na.rm = T)
growth.form.agg$tree <- rowSums(growth.form.agg[,tree.names], na.rm = T)


# Begin classification ----------------------------------------------------

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("grass","shrub","tree")])))
}

growth.form.agg$vegetation_type <- unlist(apply(growth.form.agg, MARGIN = 1, FUN = classify))


# Combine with Precipitation Data -----------------------------------------

growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]

load('annual.precip.data.RData')
load('annual.fc.data.RData')
load('annual.tmax.data.RData')

precip.fc.data <- merge(annual.fc.data, annual.precip.data, by = 'site_location_name')
precip.fc.data <- merge(precip.fc.data, annual.tmax.data, by = 'site_location_name')
precip.fc.data <- merge(precip.fc.data, growth.form.essen, by = 'site_location_name')


# Visualise the data ------------------------------------------------------


# cover_mean_vs_precip_mean -----------------------------------------------

pl.prec.pv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = pv_mean)) + 
  geom_point() + facet_wrap(~vegetation_type) +
  labs(x = 'Mean Annual Precipitation (mm/yr)', y = 'Mean Green Cover (%)') + 
  geom_smooth(method = 'gam')

pl.prec.bs <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = bs_mean)) + 
  geom_point() + facet_wrap(~vegetation_type) +
  labs(x = 'Mean Annual Precipitation (mm/yr)', y = 'Mean Bare Cover (%)') +
    geom_smooth(method = 'gam')


pl.prec.npv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = npv_mean)) +
  geom_point() + facet_wrap(~vegetation_type) + 
  labs(x = 'Mean Annual Precipitation (mm/yr)', y = 'Mean Brown Cover (%)') +
  geom_smooth(method = 'gam')

pl.prec.npv

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)


pl.prec.pv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = pv_mean, colour = vegetation_type)) + 
  geom_point() + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Green Cover (%)')

pl.prec.bs <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = bs_mean, colour = vegetation_type)) +
  geom_point()  + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Bare Cover (%)')  

pl.prec.npv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = npv_mean, colour = vegetation_type)) +
  geom_point()  + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Brown Cover (%)') 

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)





# Try with temperature max colouring 


pl.prec.pv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = pv_mean, colour = tmax_mean)) + 
  geom_point() +  labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Green Cover (%)') + facet_wrap(~vegetation_type)

pl.prec.bs <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = bs_mean, colour = tmax_mean)) +
  geom_point() + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Bare Cover (%)')  + facet_wrap(~vegetation_type)

pl.prec.npv <- ggplot(data = precip.fc.data, aes(x = precip_mean, y = npv_mean, colour = tmax_mean)) +
  geom_point() + labs(x = 'Mean Annual Precipitation (mm/y)', y = 'Mean Brown Cover (%)') + facet_wrap(~vegetation_type)


plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)




# cover_mean_vs_precip_cv ---------------------------------------------------


pl.prec.pv <- ggplot(data = precip.fc.data,aes(x = precip_cv, y = pv_mean)) + geom_point() + 
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Green Cover (%)') + facet_wrap(~vegetation_type) +
  geom_smooth(method = 'gam')

pl.prec.bs <- ggplot(data = precip.fc.data, aes(x = precip_cv, y = bs_mean)) + geom_point() + 
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Bare Cover (%)') + facet_wrap(~vegetation_type) +
  geom_smooth(method = 'gam')

pl.prec.npv <- ggplot(data = precip.fc.data, aes(x = precip_cv, y = npv_mean)) + geom_point() +
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Brown Cover (%)') + facet_wrap(~vegetation_type) +
  geom_smooth(method = 'gam')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)


# Try with temperature max colouring 


pl.prec.pv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = pv_mean, colour = tmax_mean)) + 
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Green Cover (%)') + facet_wrap(~vegetation_type)

pl.prec.bs <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = bs_mean, colour = tmax_mean)) + 
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Bare Cover (%)') + facet_wrap(~vegetation_type)

pl.prec.npv <- ggplot(data = precip.fc.data) + geom_point(aes(x = precip_cv, y = npv_mean, colour = tmax_mean)) +
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Brown Cover (%)') + facet_wrap(~vegetation_type)

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)



# cover_cv_vs_precip_cv ---------------------------------------------------


pl.prec.pv <- ggplot(data = precip.fc.data, aes(x = precip_cv, y = pv_cv)) +
  geom_point() + 
  facet_wrap(~vegetation_type) + 
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Green Cover (CV)')

pl.prec.bs <- ggplot(data = precip.fc.data, aes(x = precip_cv, y = bs_cv)) + 
  geom_point() + facet_wrap(~vegetation_type) +
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Bare Cover (CV)')

pl.prec.npv <- ggplot(data = precip.fc.data,aes(x = precip_cv, y = npv_cv)) + 
  geom_point() + facet_wrap(~vegetation_type) +
  labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Brown Cover (CV)') 

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)


####


# Try with temperature max colouring 

pl.prec.pv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x = precip_cv, y = pv_cv, colour = tmax_mean)) + facet_wrap(~vegetation_type) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

pl.prec.bs <- ggplot(data = precip.fc.data) + 
  geom_point(aes(x = precip_cv, y = bs_cv, colour = tmax_mean)) + facet_wrap(~vegetation_type) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Bare Cover (%)')

pl.prec.npv <- ggplot(data = precip.fc.data) + 
  geom_point(aes(x = precip_cv, y = npv_cv, colour = tmax_mean)) + facet_wrap(~vegetation_type) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Brown Cover (%)')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)


# tmax_mean_vs_precp_mean -------------------------------------------------


pl.prec.pv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_mean , y = precip_mean, colour = pv_mean)) + facet_wrap(~vegetation_type) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

pl.prec.bs <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_mean , y = precip_mean, colour = bs_mean)) + facet_wrap(~vegetation_type)#+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')


pl.prec.npv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_mean , y = precip_mean, colour = npv_mean)) + facet_wrap(~vegetation_type)#+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')


plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)




# tmax_cv_precip_mean -----------------------------------------------------


pl.prec.pv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_cv , y = precip_mean, colour = pv_mean)) + facet_wrap(~vegetation_type) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

pl.prec.bs <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_cv , y = precip_mean, colour = bs_mean)) + facet_wrap(~vegetation_type)#+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

pl.prec.npv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_cv , y = precip_mean, colour = npv_mean)) + facet_wrap(~vegetation_type) #+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)


# tmax_cv_precip_cv -------------------------------------------------------


pl.prec.pv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_cv , y = precip_cv, colour = pv_mean)) + facet_wrap(~vegetation_type)#+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

pl.prec.bs <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_cv , y = precip_cv, colour = bs_mean)) + facet_wrap(~vegetation_type)#+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

pl.prec.npv <- ggplot(data = precip.fc.data) +
  geom_point(aes(x =tmax_cv , y = precip_cv, colour = npv_mean)) + facet_wrap(~vegetation_type)#+ labs(x = 'Mean Annual Precipitation (CV)', y = 'Mean Annual Green Cover (%)')

plot_grid(pl.prec.bs, pl.prec.npv, pl.prec.pv)




table(growth.form.agg$vegetation_type)

