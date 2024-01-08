#### DEA Evaluation Change Plots 
## Krish Singh
## 240108
## To evaluate the change in FC in ausplots site from DEA and in-situ


# Libraries ---------------------------------------------------------------

library(plotly)
library(ggplot2)
library(xts)
library(forecast)
library(seasonal)
library(dplyr)
library(caret)
library(cowplot)
library(data.table)
library(tune)
library(ggpubr)
library(ggpmisc)
library(Matrix)
library(ausplotsR)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

# Load Datasets 

dea.fc.change.df <- read.csv("../DATASETS/DEA_fc_change.csv", row.names =  1)
site.fc.change.df <- read.csv("../DATASETS/AusPlots_FC_Change_2.0.6.csv", row.names = 1)


# This visualisation is not aggregated i.e. 3 visits will count as two timestamps between visit dates (A,B) and (B,C)##

both.changs.df <- merge(dea.fc.change.df,site.fc.change.df, by = c("site_location_name", "visit_start_date_a",
                                                                   "visit_start_date_b"))

bs.bare.pl <- ggplot(data = both.changs.df, aes(x = bare, y = bs)) + labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))

pv.green.pl <- ggplot(data = both.changs.df, aes(x = green, y = pv)) + labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") + 
  geom_point() + geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))

npv.brown.pl <- ggplot(data = both.changs.df, aes(x = brown, y = npv), colour = 'blue') + geom_point() + labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") + 
  geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100))


all.pl <- ggplot(data = both.changs.df) + geom_point(aes(x = brown, y = npv, colour = 'brown')) + geom_point(aes(x = green, y = pv, colour = 'green')) + 
  geom_point(aes(x = bare, y = bs, colour = 'bare')) +labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0) + coord_obs_pred() + scale_colour_manual(name = 'Cover', values = c('brown' = 'blue', 'green' = 'green', 'bare' = 'red')) + 
  xlim(c(-100,100)) + ylim(c(-100,100))


plot_grid(bs.bare.pl,npv.brown.pl,pv.green.pl, all.pl) 


# Averaging the change in cover over time in sites with 3 visits ###

both.changs.df <- merge(dea.fc.change.df,site.fc.change.df, by = c("site_location_name", "visit_start_date_a",
                                                                   "visit_start_date_b"))

both.changes.agg <- aggregate(both.changs.df[,c("pv","npv","bs", "green", "brown", "bare")],
                              list(both.changs.df$site_location_name), FUN = mean, na.rm = T)

bs.stats <- lm(bs~bare,both.changes.agg)
bs.bare.pl <- ggplot(data = both.changes.agg, aes(x = bare, y = bs)) + labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred() + xlim(c(-100,100)) + geom_abline(slope = bs.stats$coefficients[["bare"]], 
                                                                                                                     intercept = bs.stats$coefficients[["(Intercept)"]]) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
bs.bare.pl

overall.filtered <- na.omit(both.changes.agg)

Metrics::rmse(actual = overall.filtered$bare, 
              predicted = overall.filtered$bs)

pv.stats <- lm(pv~green,both.changes.agg)
pv.green.pl <- ggplot(data = both.changes.agg, aes(x = green, y = pv)) + labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") + 
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100)) +
  geom_abline(slope = pv.stats$coefficients[["green"]], intercept = pv.stats$coefficients[["(Intercept)"]]) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

pv.green.pl

Metrics::rmse(actual = overall.filtered$green, 
              predicted = overall.filtered$pv)


npv.stats <- lm(npv~brown,both.changes.agg)
npv.brown.pl <- ggplot(data = both.changes.agg, aes(x = brown, y = npv), colour = 'blue') + geom_point() + labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100)) +  geom_abline(slope = npv.stats$coefficients[["brown"]], 
                                                                                                                           intercept = npv.stats$coefficients[["(Intercept)"]]) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))
npv.brown.pl

# To cleanly incorporate statistics for the overall plot, reformat datasets into long format and merge

dea.fc.change.df.long <- reshape2::melt(dea.fc.change.df, id.vars = c('site_location_name', 'visit_start_date_a', 'visit_start_date_b'), value.name ="remote.cover")
site.fc.change.df.long <- reshape2::melt(site.fc.change.df, id.vars = c('site_location_name', 'visit_start_date_a', 'visit_start_date_b'), value.name = "insitu.cover")

dea.fc.change.df.long$variable <- as.character(dea.fc.change.df.long$variable)
dea.fc.change.df.long$variable[which(dea.fc.change.df.long$variable == 'pv')] <- 'green'
dea.fc.change.df.long$variable[which(dea.fc.change.df.long$variable == 'npv')] <- 'brown'
dea.fc.change.df.long$variable[which(dea.fc.change.df.long$variable == 'bs')] <- 'bare'

both.changes.df.long <- merge(dea.fc.change.df.long, site.fc.change.df.long, by = c("site_location_name", "visit_start_date_a",
                                                                                    "visit_start_date_b", 'variable'))

# Note: This means I averaged the changes in cover 
both.changes.agg.long <-aggregate(cbind(remote.cover, insitu.cover) ~ site_location_name + variable, data = both.changes.df.long, FUN = mean, na.rm = T)

all.stats <- lm(remote.cover~insitu.cover,both.changes.agg.long)

# This is the validation plot - to see if the conversion process did not change the actual data and will produce equivalent statistics 
all.pl.validate <- ggplot(data = both.changes.agg.long, aes(x = insitu.cover, y = remote.cover, colour = variable)) + geom_point() + labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred()  + xlim(c(-100,100)) + ylim(c(-100,100)) + geom_abline(slope = all.stats$coefficients[["insitu.cover"]], 
                                                                                                                           intercept = all.stats$coefficients[["(Intercept)"]]) + stat_poly_eq(mapping = use_label(c("eq", "R2", 'p')))

all.pl <- ggplot(data = both.changes.agg) + geom_point(aes(x = brown, y = npv, colour = 'brown')) + geom_point(aes(x = green, y = pv, colour = 'green')) +
  geom_point(aes(x = bare, y = bs, colour = 'bare')) +labs(x = "\u0394 cover (in-situ)", y = "\u0394 cover (remote)") +
  geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred() + scale_colour_manual(name = 'Cover', values = c('brown' = 'blue', 'green' = 'green', 'bare' = 'red')) +
  xlim(c(-100,100)) + ylim(c(-100,100))  + geom_abline(slope = all.stats$coefficients[["insitu.cover"]],intercept = all.stats$coefficients[["(Intercept)"]]) + stat_poly_eq(data = both.changes.agg.long, 
                                                                                                                                                                            mapping = use_label(c("eq", "R2", 'p'), aes(x = insitu.cover, y =remote.cover)))
all.pl

plot_grid(bs.bare.pl,npv.brown.pl,pv.green.pl, all.pl) 

# This is to validate to see if the long format dataset is equivalent to the wide one
plot_grid(bs.bare.pl,npv.brown.pl,pv.green.pl, all.pl.validate) 

both.changes.agg

summary(lm(bs~bare,both.changes.agg))
summary(lm(npv~brown,both.changes.agg))
summary(lm(pv~green,both.changes.agg))

# By vegetation type ------------------------------------------------------

growth.form.agg <- read.csv("../DATASETS/AusPlots_Sites_Classified_2-0-6.csv")
growth.form.essen <- growth.form.agg[,c("site_location_name", "vegetation_type")]
both.changes.agg$site_location_name <- both.changes.agg$Group.1 
both.changes.agg <- merge(both.changes.agg, growth.form.essen, by = 'site_location_name')


bs.bare.pl <- ggplot(data = both.changes.agg, aes(x = bare, y = bs)) + labs(x = "\u0394 bare cover (in-situ)", y = "\u0394 bare cover (remote)") +
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + facet_wrap(~vegetation_type) + coord_obs_pred() + xlim(c(-100,100)) + 
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) + stat_smooth(method = 'lm', fullrange = F)

bs.bare.pl

overall.filtered <- na.omit(both.changes.agg)


Metrics::rmse(actual = overall.filtered$bare, 
              predicted = overall.filtered$bs)

tree <- overall.filtered[overall.filtered$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$bare, 
              predicted = tree$bs)

shrub <- overall.filtered[overall.filtered$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$bare, 
              predicted = shrub$bs)

grass <- overall.filtered[overall.filtered$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$bare, 
              predicted = grass$bs)



pv.green.pl <- ggplot(data = both.changes.agg, aes(x = green, y = pv)) + labs(x = "\u0394 green cover (in-situ)", y = "\u0394 green cover (remote)") + 
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + coord_obs_pred() + xlim(c(-100,100)) + facet_wrap(~vegetation_type) + ylim(c(-100,100)) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) + stat_smooth(method = 'lm', fullrange = F) 
pv.green.pl



overall.filtered <- na.omit(both.changes.agg)

tree <- overall.filtered[overall.filtered$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$green, 
              predicted = tree$pv)

shrub <- overall.filtered[overall.filtered$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$green, 
              predicted = shrub$pv)

grass <- overall.filtered[overall.filtered$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$green, 
              predicted = grass$pv)



npv.brown.pl <- ggplot(data = both.changes.agg, aes(x = brown, y = npv), colour = 'blue') + geom_point() + labs(x = "\u0394 brown cover (in-situ)", y = "\u0394 brown cover (remote)") +
  facet_wrap(~vegetation_type) + geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_obs_pred() + xlim(c(-100,100)) + ylim(c(-100,100)) +
  stat_poly_eq(mapping = use_label(c("eq", "R2", 'p'))) + stat_smooth(method = 'lm', fullrange = F) 
npv.brown.pl

overall.filtered <- na.omit(both.changes.agg)


Metrics::rmse(actual = overall.filtered$brown, 
              predicted = overall.filtered$npv)

tree <- overall.filtered[overall.filtered$vegetation_type == 'tree',]

Metrics::rmse(actual = tree$brown, 
              predicted = tree$npv)

shrub <- overall.filtered[overall.filtered$vegetation_type == 'shrub',]
Metrics::rmse(actual = shrub$brown, 
              predicted = shrub$npv)

grass <- overall.filtered[overall.filtered$vegetation_type == 'grass',]
Metrics::rmse(actual = grass$brown, 
              predicted = grass$npv)



