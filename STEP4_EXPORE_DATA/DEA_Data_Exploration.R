###### Dea_Data_Exploration #######
## By Krish Singh
## 230707
## Exploring the Dea dataset 

##### Load Essential Librarys ######

library(plotly)
library(ggplot2)
library(xts)
library(forecast)
library(seasonal)

###### Set wd ######

setwd("C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA")
directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"


###### Load Dataset ######
RI = 453 # record index

files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)
site.path <- paste(directory,files[RI],sep = "")

dea.data.satflb0004 <- read.csv(site.path)
site.info.data <- read.csv("sites_info_query.csv")

site.info.data <- site.info.data[site.info.data$site_location_name %in% file.names[RI],]


print(unique(dea.data.satflb0004$time))

###### Validate Visually ######

abs(max(dea.data.satflb0004$x))-abs(min(dea.data.satflb0004$x))
abs(max(dea.data.satflb0004$y))-abs(min(dea.data.satflb0004$y))

MASS::eqscplot(dea.data.satflb0004$x, dea.data.satflb0004$y,tol = .5, main = file.names[RI],
               xlab = "easting", ylab = "northing")

points(x = site.info.data['pit_marker_easting'],y = site.info.data['pit_marker_northing'], pch = 2, col = 'red')
points(x = site.info.data['pit_marker_easting']+100,y = site.info.data['pit_marker_northing'], pch = 2, col= 'red')
points(x = site.info.data['pit_marker_easting'],y = site.info.data['pit_marker_northing']+100, pch = 2, col= 'red')
points(x = site.info.data['pit_marker_easting']+100,y = site.info.data['pit_marker_northing']+100, pch = 2, col= 'red')

###### Evalidate Numerically ######

# check if the satellite imaging captured the right coordinates
# algorithm:
#   1. get coords (x,y of both site and sat)
#   2. get mid.coords for site 
#   2. get largest values of sat x and y
#   3. condition checks:
#       a) site.mid.x > sat.x_min & site.mid.x < sat.X
#       b) site.mid.y > sat.y_min & site.mid.y < sat.y
#   4. If both are true, then the mid-point of the site lies within the site image
#   5. Calculate the offsets of the SW and NE borders 
#   6. Return (True/false for midpoint, offset.x, offset.y)

check.sat.position <- function(site.x.coord, site.y.coord, sat.x.coord, sat.y.coord){
  
  site.mid.x <- sat.x.coord+50
  site.mid.y <- sat.y.coord+50
  
  sat.min.max.x.y.coords <- c("x.min" = min(sat.x.coord), "x.max" = max(sat.x.coord),
                              "y.min" = min(sat.y.coord), "y.max" = max(sat.y.coord))
  
  is.witin.x <- any(site.mid.x > sat.min.max.x.y.coords["x.min"] & 
    site.mid.x < sat.min.max.x.y.coords["x.max"])
  
  is.witin.y <- any(site.mid.y > sat.min.max.x.y.coords["y.min"] &
    site.mid.y < sat.min.max.x.y.coords["y.max"])
  
  is.within.coords <- F
  if(is.witin.x & is.witin.y){
    is.within.coords <- T
  }
  
  ## Get offsets for the corner points (SW, NE)
  offsets.SW.components <- c(site.x.coord - sat.min.max.x.y.coords[["x.min"]],
                 site.y.coord - sat.min.max.x.y.coords[["y.min"]])
  # +100 to to SW to get NE
  offset.NE.components <- c(site.x.coord + 100 - sat.min.max.x.y.coords[["x.max"]],
                 site.y.coord + 100 - sat.min.max.x.y.coords[["y.max"]])
  
  
  offset.SW <- sqrt(sum(offsets.SW.components^2))
  offset.NE <- sqrt(sum(offset.NE.components^2))
  
  return(c("is.within.coords" = is.within.coords, 
           "offset.SW" = offset.SW,
           "offset.NE" = offset.NE))

}


print(check.sat.position(site.x.coord, site.y.coord,sat.x.coord, sat.y.coord))



###### Visualise The Data ######


dea.data.satflb004.agg <- aggregate(dea.data.satflb0004, 
                                    by = list(dea.data.satflb0004$time),
                                    FUN = mean, na.rm = T)

posit.date <- as.POSIXlt(dea.data.satflb004.agg$Group.1) # for use for a later section: smoothing time series 
dea.data.satflb004.agg$Group.1 <- as.Date(dea.data.satflb004.agg$Group.1)

## Get essential data from Ausplots 
site.info.data.essen <- site.info.data[,c("visit_start_date", "bare",
                                          "brown", "green" )]

site.info.data.essen$visit_start_date <- as.Date(
  site.info.data.essen$visit_start_date)


colnames(site.info.data.essen) <- colnames(
  dea.data.satflb004.agg[,c("Group.1","bs","npv","pv")]
  )
dea.data.satflb004.agg.essen <- dea.data.satflb004.agg[,c("Group.1","bs","npv","pv", "ue")]
merged.data <- merge(site.info.data.essen, dea.data.satflb004.agg.essen, all.x = T, all.y = T)



### Using ts_plot


library(TSstudio)
library(tsbox)



TSstudio::ts_plot(merged.data, slider = T,
        type = "multiple", Xgrid = TRUE,Ygrid = TRUE)


## Using ggplotly
pl <- ggplot(dea.data.satflb004.agg.essen, aes(x = Group.1)) +
  geom_line(mapping = aes(y = bs, colour = "bs")) +
  geom_line(mapping = aes(y = npv, colour = "npv")) +
  geom_line(mapping = aes(y = pv, colour = "pv")) +
  geom_line(mapping = aes(y = ue, colour = "ue")) +
  xlab("Time") +
  ylab("Colour Intensity") +
  geom_point(data = site.info.data.essen,
             mapping = aes(x = Group.1, y = bs, colour = "bs.obs")) +
  geom_point(data = site.info.data.essen,
             mapping = aes(x = Group.1, y = npv, colour = "npv.obs")) +
  geom_point(data = site.info.data.essen,
             mapping = aes(x = Group.1, y = pv, colour = "pv.obs")) +
  scale_color_manual(
    name = "Colour Bands", 
    values = c("bs" = "red", "npv" = "blue", "pv" = "green", "ue" = "yellow",
               "bs.obs" = "darkred", "npv.obs" = "darkblue", 
               "pv.obs" = "darkgreen")) +
  theme_minimal() +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust = 1)) +
  geom_hline(yintercept = 0)

p <- ggplotly(pl) %>% 
  rangeslider()
p


### Smoothing the time-series 

dea.data.satflb004.agg.essen <- dea.data.satflb004.agg[,c("Group.1","bs","npv","pv", "ue")]
dea.data.satflb004.agg.essen$Group.1 <- posit.date

sat.xts <- xts(dea.data.satflb004.agg.essen[,-1], 
               order.by = dea.data.satflb004.agg$Group.1)

sat.xts.interp <- na.approx(sat.xts)


plot.new.ts <- function(xts.data, obs){
  
  ts.data <- as.data.frame(xts.data)
  ts.data$time <- as.Date(row.names(ts.data))
  
  obs$Group.1 <- as.Date(obs$Group.1)
  
  
  pl <- ggplot(ts.data, aes(x =time )) +
    geom_line(mapping = aes(y = bs, colour = "bs")) +
    geom_line(mapping = aes(y = npv, colour = "npv")) +
    geom_line(mapping = aes(y = pv, colour = "pv")) +
    xlab("Time") +
    ylab("Colour Intensity") +
    geom_point(data = obs,
               mapping = aes(x = Group.1, y = bs, colour = "bs.obs")) +
    geom_point(data = obs,
               mapping = aes(x = Group.1, y = npv, colour = "npv.obs")) +
    geom_point(data = obs,
               mapping = aes(x = Group.1, y = pv, colour = "pv.obs")) +
    scale_color_manual(
      name = "Colour Bands", 
      values = c("bs" = "red", "npv" = "blue", "pv" = "green",
                 "bs.obs" = "darkred", "npv.obs" = "darkblue", 
                 "pv.obs" = "darkgreen")) +
    theme_minimal() +
    scale_x_date(date_breaks = "3 months", date_labels = "%Y %b",
                 date_minor_breaks = "1 month") +
    theme(axis.text.x=element_text(angle=60, hjust = 1)) +
    geom_hline(yintercept = 0)
  
  p <- ggplotly(pl) %>% 
    rangeslider()
  
  return(p) 
}

library(forecast)
plot.new.ts(sat.xts.interp, site.info.data.essen)
sat.xts.interp.clean <- sat.xts.interp
sat.xts.interp.clean$bs <- tsclean(sat.xts.interp$bs)
sat.xts.interp.clean$pv <- tsclean(sat.xts.interp$pv)
sat.xts.interp.clean$npv <- tsclean(sat.xts.interp$npv)
plot.new.ts(sat.xts.interp.clean, site.info.data.essen)


autoplot(sat.xts)
autoplot(sat.xts.interp)
autoplot(sat.xts.interp.clean)


## Utilising different smoothing techniques ####
## Rolling window 

periodicity(sat.xts.interp)
k = 5 # rolling window
sat.xts.interp.sma <- rollmean(sat.xts.interp,k = k)
plot.new.ts(sat.xts.interp.sma, site.info.data.essen)


Box.test(sat.xts.interp.sma$bs)
Box.test(sat.xts.interp.sma$pv)
Box.test(sat.xts.interp.sma$npv)

plot(ma(sat.xts.interp$bs, order = k, centre = T))
plot(ma(sat.xts.interp$pv, order = k, centre = T))
plot(ma(sat.xts.interp$npv, order = k, centre = T))


## trying despike

library('oce')
sat.xts.interp.despike <- despike(as.data.frame(sat.xts.interp), reference="median")
plot.new.ts(sat.xts.interp.despike, site.info.data.essen)

autoplot(sat.xts.interp)
autoplot(xts(sat.xts.interp.despike,
             order.by = dea.data.satflb004.agg$Group.1))

#### MSD

ts_xts(sat.xts.interp)$bs %>%
  mstl(iterate = 1000) %>% autoplot

ts_xts(sat.xts.interp)$pv %>%
  mstl() %>% autoplot()

ts_xts(sat.xts.interp)$npv %>%
  mstl() %>% autoplot()


#### Adjust the peroidicity so its fixed and can be used with ts 
sat.xts.interp.mon <- cbind("bs" = to.monthly(sat.xts.interp$bs)[,1],
                            "npv" = to.monthly(sat.xts.interp$npv)[,1],
                             "pv" = to.monthly(sat.xts.interp$pv)[,1])

colnames(sat.xts.interp.mon) <- c("bs", "npv", "pv")

sat.ts <- ts(sat.xts.interp.mon, 
             start = c(1987,9), 
             end= c(2023,4),
             frequency = 12 )

sat.ts.bs <- ts(sat.xts.interp.mon$bs, 
             start = c(1987,9), 
             end= c(2023,4),
             frequency = 12 )

sat.ts.pv <- ts(sat.xts.interp.mon$pv, 
                start = c(1987,9), 
                end= c(2023,4),
                frequency = 12 )

sat.ts.npv <- ts(sat.xts.interp.mon$npv, 
                start = c(1987,9), 
                end= c(2023,4),
                frequency = 12 )



# seasonplots
library(seasonal)
ggseasonplot(sat.ts.bs, year.labels=TRUE, year.labels.left=TRUE)
ggseasonplot(sat.ts.bs, polar=TRUE)

ggseasonplot(sat.ts.pv, year.labels=TRUE, year.labels.left=TRUE)
ggseasonplot(sat.ts.pv, polar=TRUE)




###### use X11 decomposition
library(seasonal)
library(forecast)

sat.ts.bs %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()


sat.ts.bs %>%
  mstl() %>% autoplot()


sat.ts.bs %>% decompose() %>%
  autoplot()

sat.ts.bs %>% decompose(type="multiplicative") %>%
  autoplot()


sat.ts.pv %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

sat.ts.pv %>%
  mstl() %>% autoplot()

sat.ts.pv %>% decompose() %>%
  autoplot()

sat.ts.pv %>% decompose(type="multiplicative") %>%
  autoplot()


sat.ts.npv %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()







############ END FOR NOW 




### Now exploring the time-series 
library(ggfortify)
library(stringr)

####### Intrapolerating the site data into the obs data #########
### reaggregate data to obtain the date in a different format 
### to include hours/minutes 

## Sat data 
dea.data.satflb004.agg <- aggregate(dea.data.satflb0004, 
                                    by = list(dea.data.satflb0004$time), FUN = mean)
dea.data.satflb004.agg$Group.1 <- as.POSIXlt(dea.data.satflb004.agg$Group.1)
dea.data.satflb004.agg.essen <- dea.data.satflb004.agg[,c(-5:-2, -9)]


## Site data: make the observations NA for site's intraporlation 

site.empty.df <-  site.info.data[,c("visit_start_date", "bare",
                                    "brown", "green" )]
colnames(site.empty.df) <- colnames(
  dea.data.satflb004.agg[,c("Group.1","bs","npv","pv")]
)

## this block adjusts the date formating for data coercion
site.dates <- site.empty.df$Group.1
site.dates <- str_replace(site.dates, "T", " ")
site.empty.df$Group.1 <- site.dates
site.empty.df$Group.1 <- as.POSIXlt(site.dates)

## set nas 
site.empty.df[,-1] <- NA


## Merge the data 
merged.data <- rbind(dea.data.satflb004.agg.essen,site.empty.df)

## Double check if the dates are added and measurements are na 
print(merged.data$Group.1[is.na(merged.data$pv) |is.na(merged.data$bs) |
                      is.na(merged.data$npv)])


sat.xts <- xts(merged.data[,-1], 
    order.by = merged.data[,1])


## Summary stats of the time series to verify some of the details 
start(sat.xts)

end(sat.xts)

periodicity(sat.xts)

summary(sat.xts)

### Now perform intrapoleration 

sat.xts.interp <- na.approx(sat.xts)


sat.df.interp <- as.data.frame(sat.xts.interp)
site.empty.df[-1] <- sat.df.interp[site.dates,]
## combine observations with sat interp
site.obs.and.interp <- cbind(site.info.data[,c("visit_start_date", "bare",
                                         "brown", "green" )],site.empty.df)
site.obs.and.interp <- site.obs.and.interp[,-5]

### Should we scale the percentages to 255? 
#site.obs.and.interp[,c(2:4)] <- site.obs.and.interp[,c(2:4)]*(255/100)

site.obs.and.interp$green.err <- abs(site.obs.and.interp[,"pv"] -
                                      site.obs.and.interp[,"green"] )
site.obs.and.interp$brown.err <- abs(site.obs.and.interp[,"npv"] -
                                       site.obs.and.interp[,"brown"] )
site.obs.and.interp$bare.err <- abs(site.obs.and.interp[,"bare"] -
                                       site.obs.and.interp[,"bs"] )





### [TODO]: despike (oce)

