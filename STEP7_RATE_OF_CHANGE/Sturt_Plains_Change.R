##### Getting the Rate of Change of FC values in Sturt #######
# Krish Singh
# 20240117



# Library -----------------------------------------------------------------

library(ggplot2)
library(zoo)
library(mgcv)


# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------



# PV ----------------------------------------------------------------------


fc <- read.csv('../DATASETS/Sturt_Plains_DEA_FC/Output/Sturt Plains.csv')
fc$time <- as.Date(fc$time)

ggplot(fc) + geom_line(mapping = aes(x = time, y = pv))
daily.dates <- seq(min(fc$time), max(fc$time), by = "1 day")
#data.dates <- fc.pv$time

fc.pv <- fc[,c("time", "pv")]

fc.pv <- spline(x = fc$time, y = fc$pv, method = "periodic", xout = daily.dates)
fc.pv <- data.frame("time" = as.Date(fc.pv$x), "pv" = fc.pv$y)

ggplot() +geom_line(data = fc.pv, mapping = aes(x = time, y = pv), color = 'red') +
  geom_line(data = fc,mapping = aes(x = time, y = pv))

spline.fit <- smooth.spline(fc.pv$time, fc.pv$pv,spar = 0.37)
spline.fit$x <- as.Date(spline.fit$x)

ggplot() +geom_line(data = data.frame("time" = as.Date(spline.fit$x),"pv" = spline.fit$y), mapping = aes(x = time, y = pv), color = 'red') +
  geom_line(data = fc,mapping = aes(x = time, y = pv))

spline.fit.pv <- data.frame("time" = as.Date(spline.fit$x),"pv" = spline.fit$y)

spline.fit.pv$diff <- c(NA, diff(spline.fit.pv$pv))

ggplot() +geom_line(data = data.frame("time" = as.Date(spline.fit$x),"pv" = spline.fit$y), mapping = aes(x = time, y = pv), color = 'red') +
  geom_line(data = fc,mapping = aes(x = time, y = pv)) +
  geom_line(data = spline.fit.pv,mapping = aes(x = time, y = diff), color = 'green')


# Brown -------------------------------------------------------------------

daily.dates <- seq(min(fc$time), max(fc$time), by = "1 day")
ggplot(fc) + geom_line(mapping = aes(x = time, y = npv))

fc.npv <- fc[,c("time", "npv")]

fc.npv <- spline(x = fc$time, y = fc$npv, method = "p", xout = daily.dates)
fc.npv <- data.frame("time" = as.Date(fc.npv$x), "npv" = fc.npv$y)

ggplot() +geom_line(data = fc.npv, mapping = aes(x = time, y = npv), color = 'red') +
  geom_line(data = fc,mapping = aes(x = time, y = npv))

spline.fit <- smooth.spline(fc.npv$time, fc.npv$npv, control.spar = 'tol', spar = 0.5)
spline.fit$x <- as.Date(spline.fit$x)

ggplot() +geom_line(data = data.frame("time" = as.Date(spline.fit$x),"npv" = spline.fit$y), mapping = aes(x = time, y = npv), color = 'red') +
  geom_line(data = fc,mapping = aes(x = time, y = npv))


spline.fit.npv <- data.frame("time" = as.Date(spline.fit$x),"npv" = spline.fit$y)

spline.fit.npv$diff <- c(NA, diff(spline.fit.npv$npv))

ggplot() +geom_line(data = data.frame("time" = as.Date(spline.fit$x),"npv" = spline.fit$y), mapping = aes(x = time, y = npv), color = 'red') +
  geom_line(data = fc,mapping = aes(x = time, y = npv)) +
  geom_line(data = spline.fit.npv,mapping = aes(x = time, y = diff), color = 'brown')
