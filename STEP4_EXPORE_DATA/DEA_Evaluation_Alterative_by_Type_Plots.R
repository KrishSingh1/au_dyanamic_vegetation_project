##### DEA Evaluation Alternative by Type Plots #####
## Krish Singh
# 240105
# Purpose Plot DEA against fractional cover by site vegetation tyoe 



# Libraries ---------------------------------------------------------------

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(Metrics)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------



# Combine with Fractional Data -----------------------------------------

growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]

dea.fc.sites.plotting$site_location_name <- unlist(lapply(dea.fc.sites.plotting$site_unique, get_location_name))
dea.fc.sites.plotting <- merge(dea.fc.sites.plotting, growth.form.essen, by = 'site_location_name')

cal.green <- ggplot(dea.fc.sites.plotting, aes(y = pv, x = green)) + geom_point(alpha = 0.5) + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (in-situ)", y = "green cover (remote)") +
  facet_wrap(~vegetation_type) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() +  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = T)

cal.green

tree <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$green, 
              predicted = tree$pv)

shrub <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$green, 
              predicted = shrub$pv)

grass <- dea.fc.sites.plotting[dea.fc.sites.plotting$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$green, 
              predicted = grass$pv)

# Bare --------------------------------------------------------------------


# Bare
cal.bare <- ggplot(dea.fc.sites.plotting, aes(y = bs, x = bare)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100)  + labs(x = "bare cover (in-situ)", y = "bare cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + facet_wrap(~vegetation_type) + coord_obs_pred() + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = T)

cal.bare

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


# Brown
cal.brown <- ggplot(dea.fc.sites.plotting, aes(y = npv, x = brown)) + geom_point(alpha = 0.5) +
  xlim(0,100) + ylim(0,100) + labs(x = "brown cover (in-situ)", y = "brown cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) + coord_obs_pred() + facet_wrap(~vegetation_type) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) +
  stat_smooth(method = 'lm', fullrange = T)

cal.brown

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





gridExtra::grid.arrange(cal.green, cal.brown, cal.bare)

plot_space <- par(mfrow=c(3,1))
plot_space <- c(cal.green, cal.bare, cal.brown)
plot_space <- cal.bare
plot_space <- cal.brown
plot_space

########## Convert to long #######
dea.fc.sites.plotting.long <- reshape2::melt(dea.fc.sites.plotting[, c('site_unique','bs','npv','pv')], id.vars = c('site_unique'), value.name ="remote.cover")
site.fc.df.long <- reshape2::melt(dea.fc.sites.plotting[,c('site_unique','bare','brown','green')], id.vars = c('site_unique'), value.name = "insitu.cover")

dea.fc.sites.plotting.long$variable <- as.character(dea.fc.sites.plotting.long$variable)
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'pv')] <- 'green'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'npv')] <- 'brown'
dea.fc.sites.plotting.long$variable[which(dea.fc.sites.plotting.long$variable == 'bs')] <- 'bare'

both.plotting.df.long <- merge(dea.fc.sites.plotting.long, site.fc.df.long, by = c("site_unique", 'variable'))
all.stats <- lm(remote.cover~insitu.cover,both.plotting.df.long)


all.pl.validate <- ggplot(data = both.plotting.df.long, aes(x = insitu.cover, y = remote.cover, colour = variable)) + geom_point() + labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred()  + xlim(c(0,100)) + ylim(c(0,100)) + geom_abline(slope = all.stats$coefficients[["insitu.cover"]], 
                                                                                                                     intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

cal.all <- ggplot(dea.fc.sites.plotting) + geom_point(aes(x = bare, y = bs, colour = 'bare'), alpha = 0.5) + geom_point(aes(x = green, y = pv, colour = 'green'), alpha = 0.5) + geom_point(aes(x = brown, y = npv, colour = 'brown'), alpha = 0.5) + geom_abline(slope = 1, intercept = 0, lty = 2, size = 0.9) +
  xlim(0,100) + ylim(0,100) + labs(x = "cover (in-situ)", y = "cover (remote)") + scale_colour_manual(name = 'Cover', values = c('brown' = '#0072B2', 'green' = '#009E73', 'bare' = 'red')) +
  geom_abline(slope = all.stats$coefficients[["insitu.cover"]],intercept = all.stats$coefficients[["(Intercept)"]], size = 0.9) + stat_poly_eq(data = both.plotting.df.long, 
                                                                                                                                               mapping = use_label(c("eq", "R2", 'p'), aes(x = insitu.cover, y =remote.cover)))


cowplot::plot_grid(cal.green, cal.brown, cal.bare,all.pl.validate)
cowplot::plot_grid(cal.green, cal.brown, cal.bare,cal.all)