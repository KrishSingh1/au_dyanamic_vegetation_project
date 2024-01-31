#### time_windo_analysis ####
## Krish Singh
## 20240124


# Library -----------------------------------------------------------------

library(ggplot2)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

# Relationship with MAP

r2s <- read.csv('../DATASETS/linear_model_fitted_r2_bigger_subset.csv')
load('../STEP4_EXPORE_DATA/annual.precip.data.RData')

r2s <- r2s[,-c(1:2)]
adj.r2s <- r2s[,seq(from = 1, to = ncol(r2s), by = 2)]
max.adj.r2 <- mapply(adj.r2s, FUN = max, na.rm = T,USE.NAMES = T)
max.adj.r2.index <- mapply(adj.r2s, FUN = which.max, USE.NAMES = T)
max.adj.r2.window <- r2s$window.1[max.adj.r2.index]

max.adj.r2.df <- data.frame(max.adj.r2)
max.adj.r2.df$window <- max.adj.r2.window

max.adj.r2.df$site_location_name <- mapply(rownames(max.adj.r2.df),FUN = function(str) {
  return(strsplit(str, split = ".", fixed = T)[[1]][1])
})
rownames(max.adj.r2.df) <- 1:nrow(max.adj.r2.df)

r2.map <- merge(annual.precip.data, max.adj.r2.df, by =  "site_location_name")


classifications <- read.csv("../DATASETS/AusPlots_Sites_Classified_2-0-6.csv")
r2.map <- merge(r2.map, classifications, by = 'site_location_name')


# precip_mean vs max.adj.r2

ggplot(r2.map, mapping = aes(x = precip_mean, y = max.adj.r2)) + geom_point() +
  facet_wrap(~vegetation_type)

# precip cv vs. max.adj.r2
ggplot(r2.map, mapping = aes(x = precip_cv, y = max.adj.r2)) + geom_point() +
  facet_wrap(~vegetation_type)

# precip_mean vs. window 

ggplot(r2.map, mapping = aes(x = precip_mean, y = window)) + geom_point() +
  facet_wrap(~vegetation_type)

# precip_cv vs. window 

ggplot(r2.map, mapping = aes(x = precip_mean, y = window)) + geom_point() +
  facet_wrap(~vegetation_type)


ggplot(r2.map, mapping = aes(x = window)) + geom_histogram(bins = 15) +
  facet_wrap(~vegetation_type)







