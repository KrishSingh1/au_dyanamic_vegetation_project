
veg.info <- readRDS("../STEP2_VEG_EXTRACTION/site_veg.rds")
insitu.fractional.cover <- readRDS("AusPlots_fractional_cover.rds")



### Some functions 

trim_to_nearest_coord <- function(ausplots.info.i.index, veg.info, dea.fc.i ) {
  
  # Site End Points:   
  W.site <- veg.info$site.info$pit_marker_easting[ausplots.info.i.index]
  S.site <- veg.info$site.info$pit_marker_northing[ausplots.info.i.index]
  N.site <- S.site + 100
  E.site <- W.site + 100
  
  #print(W.site)
  
  # Remote End Points: 
  E.remote.incre <- unique(dea.fc.i$x)
  N.remote.incre <- unique(dea.fc.i$y)
  
  # Find Closest Points:
  W.closest <- E.remote.incre[which.min(abs(E.remote.incre - W.site))]
  E.closest <- E.remote.incre[which.min(abs(E.remote.incre - E.site))]
  N.closest <- N.remote.incre[which.min(abs(N.remote.incre - N.site))]
  S.closest <- N.remote.incre[which.min(abs(N.remote.incre - S.site))]
  
  #print(W.closest)
  #print(E.closest)
  #print(N.closest)
  #print(S.closest)
  
  # Trim dataset:
  trimmed <- subset(dea.fc.i, subset = (x >= W.closest & x <= E.closest &
                                          y >= S.closest & y <= N.closest))
  
  #print(unique(trimmed$x))
  #print(unique(trimmed$y))
  
  return(trimmed)
}


###


RI = 453 # record index
directory <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

site.path <- file.path(directory, files[RI])

dea.fc <- read.csv(site.path)
dea.fc.agg <- aggregate(dea.fc, by = list(dea.fc$time), FUN = mean, na.rm = T)


ausplots.fc.i.index <- grep(file.i, insitu.fractional.cover$site_unique)
ausplots.fc.i <- insitu.fractional.cover[ausplots.fc.i.index,]






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