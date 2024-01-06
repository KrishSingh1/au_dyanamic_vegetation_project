#### DEA EVALUATION PLOTS #### 
# By Krish Singh
# Date: 240105
# Purpose: To visually explore the alignment of AusPlots and DEA data. 


# Libraries ---------------------------------------------------------------

library(ausplotsR)
library(Metrics)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(tune)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


# Functions ---------------------------------------------------------------

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

# Main --------------------------------------------------------------------

# Load AusPlots data 
insitu.fractional.cover <- read.csv('../DATASETS/AusPlots_FC_Iter_2_0_6.csv')
insitu.fractional.cover$other[is.na(insitu.fractional.cover$other)] <- 0 # set NA to 0 for 'other'
insitu.fractional.cover <- subset(insitu.fractional.cover, (other <= 10)) # remove observations with 'other' at 10% or less
insitu.fractional.cover[insitu.fractional.cover$site_unique == 'TCATCH0010-58826',]$bare = 0

# Load Preprocessed DEA Data
dea.fc.sites.nearest <- read.csv("../STEP4_EXPORE_DATA/dea_fc_sites_nearest_new_aggregation.csv")

# Merge the data 
dea.fc.sites.plotting <- merge(dea.fc.sites.nearest, insitu.fractional.cover, by = 'site_unique')
dea.fc.sites.plotting <- subset(dea.fc.sites.plotting, subset = (npixels >= 100 & npixels <= 121))

# Generate Plots 

## Evaluate green fractions 

pv.stats <- lm(pv~green,dea.fc.sites.plotting)
cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + 
  geom_abline(slope = pv.stats$coefficients[["green"]], 
              intercept = pv.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

plot(cal.green)

print(paste0("Green Fraction RMSE: ", rmse(actual = dea.fc.sites.plotting$green, 
              predicted = dea.fc.sites.plotting$pv)))

## Evaluate bare fractions 

bs.stats <- lm(bs~bare,dea.fc.sites.plotting)
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + 
  geom_abline(slope = bs.stats$coefficients[["bare"]], 
              intercept = bs.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

plot(cal.bare)


print(paste0("Bare Fraction RMSE: ", rmse(actual = dea.fc.sites.plotting$bare, 
                                                    predicted = dea.fc.sites.plotting$bs)))

## Evaluate brown fractions 

npv.stats <- lm(npv~brown,dea.fc.sites.plotting)

cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +
  geom_abline(slope = npv.stats$coefficients[["brown"]], 
              intercept = npv.stats$coefficients[["(Intercept)"]], size = 0.9) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

plot(cal.brown)

print(paste0("Bare Fraction RMSE: ", rmse(actual = dea.fc.sites.plotting$brown, 
                                          predicted = dea.fc.sites.plotting$npv)))

## Combine plots as a grid 


dea.fc.sites.plotting.long <- reshape2::melt(dea.fc.sites.plotting[, c('site_unique','bs','npv','pv')], 
                                             id.vars = c('site_unique'), value.name ="remote.cover")
site.fc.df.long <- reshape2::melt(dea.fc.sites.plotting[,c('site_unique','bare','brown','green')], 
                                  id.vars = c('site_unique'), value.name = "insitu.cover")

dea.fc.sites.plotting.long$variable <- as.character(dea.fc.sites.plotting.long$variable)
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'pv')] <- 'green'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'npv')] <- 'brown'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'bs')] <- 'bare'

both.plotting.df.long <- merge(dea.fc.sites.plotting.long, site.fc.df.long, by = c("site_unique", 'variable'))
all.stats <- lm(remote.cover~insitu.cover,both.plotting.df.long)

all.pl.validate <- ggplot(data = both.plotting.df.long, aes(x = insitu.cover, y = remote.cover, colour = variable)) +
  geom_point() + labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) + 
  geom_abline(slope = all.stats$coefficients[["insitu.cover"]],
              intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

cal.all <- ggplot(dea.fc.sites.plotting) + geom_point(aes(x = bare, y = bs, colour = 'bare'), alpha = 0.5) +
  geom_point(aes(x = green, y = pv, colour = 'green'), alpha = 0.5) + 
  geom_point(aes(x = brown, y = npv, colour = 'brown'), alpha = 0.5) + geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) +
  xlim(0,100) + ylim(0,100) + labs(x = "cover (in-situ)", y = "cover (remote)") + 
  scale_colour_manual(name = 'Cover', values = c('brown' = '#0072B2', 'green' = '#009E73', 'bare' = 'red')) +
  geom_abline(slope = all.stats$coefficients[["insitu.cover"]],intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) +
  stat_poly_eq(data = both.plotting.df.long, mapping = use_label(c("eq", "R2", 'p'), aes(x = insitu.cover, y =remote.cover)))


cowplot::plot_grid(cal.green, cal.brown, cal.bare, all.pl.validate) # check if all.pl.validate give same statistics as validation
cowplot::plot_grid(cal.green, cal.brown, cal.bare, cal.all) # Gives same statistics 

# Combine vegetation type with plot data 

growth.form.agg <- read.csv('../DATASETS/AusPlots_Sites_Classified_2-0-6.csv')
growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]

dea.fc.sites.plotting$site_location_name <- unlist(lapply(dea.fc.sites.plotting$site_unique, get_location_name))
dea.fc.sites.plotting <- merge(dea.fc.sites.plotting, growth.form.essen, by = 'site_location_name')

# Now plot by vegetation type 

## Green 

cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  facet_wrap(~vegetation_type) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)

plot(cal.green)

tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$green, 
              predicted = tree$pv)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$green, 
              predicted = shrub$pv)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$green, 
              predicted = grass$pv)

## Bare
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) +
  coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm',fullrange = F)

plot(cal.bare)

Metrics::rmse(actual = dea.fc.sites.plotting$bare, 
              predicted = dea.fc.sites.plotting$bs)


tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$bare, 
              predicted = tree$bs)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$bare, 
              predicted = shrub$bs)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$bare, 
              predicted = grass$bs)


## Brown
cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +
  facet_wrap(~vegetation_type) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = F)

plot(cal.brown)

Metrics::rmse(actual = dea.fc.sites.plotting$brown, 
              predicted = dea.fc.sites.plotting$npv)

tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$brown, 
              predicted = tree$npv)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$brown, 
              predicted = shrub$npv)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$brown, 
              predicted = grass$npv)

## Combine plots as a grid 

gridExtra::grid.arrange(cal.green, cal.brown, cal.bare)


cal.green
cal.brown
cal.bare

cowplot::plot_grid(cal.green, cal.brown,cal.bare, nrow = 3, ncol = 1)


# Junk Script (Don't Run) -------------------------------------------------
