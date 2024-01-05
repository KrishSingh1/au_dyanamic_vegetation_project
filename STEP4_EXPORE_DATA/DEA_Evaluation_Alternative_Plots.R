#### DEA EVALUATION PLOTS #### 
# By Krish Singh
# Date: 240105
# Purpose: To visually explore the alignment of AusPlots and DEA data. 


# Libraries ---------------------------------------------------------------

library(ausplotsR)
library(Metrics)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------

# Load AusPlots data 
veg.info <- readRDS("../STEP2_AUSPLOTS_EXTRACTION/site_veg_2-0-3.rds")

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



