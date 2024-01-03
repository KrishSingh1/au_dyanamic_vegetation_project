#### FC and NDVI product Comparisons Multiple Subset ####
## Author: Krish Singh
## DATE: 20231203
## Purpose: to explore the similarities between the FC and NDVI data products from
## - Vegmachine
## - DEA FC
## - NDVI

# Load Libraries ----------------------------------------------------------
library(TSstudio)
library(readr) # needed to resolve delimiter issue in sfc csv files 
library(ausplotsR)



# Load Site Info ----------------------------------------------------------


ausplots.subset.info.p <- '../STEP2_NDVI_EXTRACTION/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv' 
ausplots.subset.info <- read.csv(ausplots.subset.info.p)[,-1]
ausplots.ids <- ausplots.subset.info$site_unique

exception <- "NSABHC0023" # this is the one site I have not collected data



# Extracting relevant time window -----------------------------------------


evaluation.data <- data.frame(site_unique = NA,
                              pv = NA,
                              NDVI = NA,
                              green_mean = NA,
                              date = as.Date(NA)
                              )

for(id in ausplots.ids) {
  
  # get site name
  #print(id)
  
  ausplots.focus.data <- ausplots.subset.info[
    ausplots.subset.info$site_unique == id,]
  focus.site.name <- ausplots.focus.data$site_location_name
  
  if(focus.site.name != exception){
    
    date.visit <- as.Date(ausplots.focus.data$visit_start_date)
    #print(id)
    
    # Load NDVI
    NDVIs.path <- '../STEP2_NDVI_EXTRACTION/EarthEngine/Output/NDVI_Extraction_2/'
    focus.site.file.name <- paste0(focus.site.name, '_NDVI', '.csv')
    focus.site.ndvi <- read.csv(paste0(NDVIs.path,focus.site.file.name))
    
    # Need to extract the dates from system index (left of the last '_' seperator)
    focus.site.ndvi$date <- unlist(lapply(focus.site.ndvi$system.index,
                                          FUN = function(string){
                                            splitted.str <- strsplit(string, '_')
                                            return(splitted.str[[1]][[length(unlist(splitted.str)) - 1]])
                                          }
    ))
    focus.site.ndvi$date <- as.Date(focus.site.ndvi$date, format = '%Y%m%d')
    focus.site.ndvi <- focus.site.ndvi[order(focus.site.ndvi$date),]
    
    # Load SFC
    SFC.path <- '../STEP2_FC_EXTRACTION/VegMachine/SFC/'
    focus.site.file.name <- paste0(focus.site.name, '_SFC', '.csv')
    focus.site.sfc <- readr::read_csv(paste0(SFC.path,focus.site.file.name))
    
    # To convert to a date, I needed to append a day ('01') while in actuality
    # there are no specific dates 
    focus.site.sfc$date <- as.Date(unlist(lapply(focus.site.sfc$date,
                                                 function(date) 
                                                 {return(paste0(date, '-01'))}
    )))
    focus.site.sfc$green_mean <- as.numeric(focus.site.sfc$green_mean)
    focus.site.sfc <- as.data.frame(focus.site.sfc) # note parsing issue with the last column
  
    # Load DEA
    dea.path <- "/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/"
    focus.site.file.name <- paste0(focus.site.name, '.csv')
    focus.site.dea <- read.csv(paste0(dea.path,focus.site.file.name))
    focus.site.dea <- aggregate(focus.site.dea, 
                                by = list(focus.site.dea$time), 
                                FUN = mean, na.rm = T)
    focus.site.dea$date <- as.Date(focus.site.dea$Group.1)
    
    
    # define date range to aggregate RS data for comparison  
    times.forwards <- seq(date.visit, by='1 days', length = 31)
    times.backwards <- seq(date.visit, by='-1 days', length = 31)
    
    closest.times.dea <- rbind(focus.site.dea[focus.site.dea$date %in%times.forwards,],
                               focus.site.dea[focus.site.dea$date %in%times.backwards,])
    closest.times.dea <- closest.times.dea[order(closest.times.dea$date),]
    
    closest.times.ndvi <- rbind(focus.site.ndvi[focus.site.ndvi$date %in%times.forwards,],
                                focus.site.ndvi[focus.site.ndvi$date %in%times.backwards,])
    closest.times.ndvi <- closest.times.ndvi[order(closest.times.ndvi$date),]
    
    # special case for sfc as it is seasonal - over 3 
    times.forwards.sfc <- seq(date.visit, by='1 days', length = 31*3) 
    
    # I generally want the earliest occurance of a non-na green_mean 
    closest.times.sfc <- rbind(focus.site.sfc[focus.site.sfc$date %in%times.forwards.sfc,])
    closest.times.sfc <- closest.times.sfc[order(closest.times.sfc$date),]
    closest.times.sfc <- subset(closest.times.sfc, !is.na(green_mean))
    closest.times.sfc <- closest.times.sfc[which.min(closest.times.sfc$date),]
    
    #print(closest.times.sfc)
    
    closest.times.binded <- merge(closest.times.ndvi, closest.times.dea, by = 'date')
    closest.times.binded <- closest.times.binded[,c("date", "NDVI", "pv")] 
    
    site.focus.agg.nearest <- data.frame("site_unique" = id, lapply(
      closest.times.binded[,c("pv","NDVI")],
      FUN = mean, na.rm = T))
    
    if(nrow(closest.times.sfc) == 0) {
      site.focus.agg.nearest$green_mean <- NA
    } else {
      site.focus.agg.nearest$green_mean <- closest.times.sfc$green_mean 
    }
    
    site.focus.agg.nearest$date <- date.visit
    
    evaluation.data <- rbind(evaluation.data, site.focus.agg.nearest)
    
    #print(site.focus.agg.nearest) 
  }
}

evaluation.data <- evaluation.data[-1,]

write.csv(evaluation.data, 'Products_Evaluation.csv')


# Normalise NDVI from -1 to 1, to 0 to 100

# evaluation.data$NDVI_norm <- with(evaluation.data, ((NDVI + 1)/2)*100 )

ausplots.sub <- get_ausplots(unique(ausplots.subset.info$site_location_name),site_info = T,
                             veg.PI = T)
insitu.fc.test <-fractional_cover(ausplots.sub$veg.PI)
insitu.fc.test <- insitu.fc.test[,c("site_unique", "green")]
colnames(insitu.fc.test)[2] <- 'green_test'
evaluation.data <- merge(evaluation.data, insitu.fc.test, by = 'site_unique')

insitu.fc <- readRDS("../STEP2_VEG_EXTRACTION/insitu_fractional_cover_default_2-0-3.rds")
insitu.fc <- insitu.fc[,c("site_unique", "green")]
evaluation.data <- merge(evaluation.data, insitu.fc, by = 'site_unique')
evaluation.data <- merge(evaluation.data, 
                         ausplots.subset.info[,c("site_unique",
                                                 
                                                 "climatic_condition",
                                                 "vegetation_condition",
                                                 "erosion_type",
                                                 "disturbance",
                                                 "location_description"
                                                 )],
                         by = 'site_unique')


# Merge with dominant growth form 
gft <- growth_form_table(ausplots.sub$veg.PI, m_kind = "percent_cover")

dgft <- c()
for(i in 1:nrow(gft)) {
  dgft <- c(dgft, names(which.max(gft[i,])))
}
gft$dominant_growth_form <- dgft
gft$site_unique <- rownames(gft)

evaluation.data <- merge(evaluation.data, gft[,c("site_unique", "dominant_growth_form")],
                         by = 'site_unique')




# Visualise the dataset ---------------------------------------------------


plot(evaluation.data$pv, evaluation.data$green_mean)


cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = pv, x = green_mean)) + 
  geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) + 
  labs(x = "green cover (JRSRP)", y = "green cover (dea)")
cal.green.dea.vegmac


cal.green.NDVI.vegmac <- ggplot(evaluation.data, aes(y = NDVI, x = green_mean)) + 
  geom_point() + labs(x = "green cover (JRSRP)", y = "green cover (EE)") + geom_smooth(method = 'lm')

cal.green.NDVI.vegmac



cal.green.NDVI.vegmac <- ggplot(evaluation.data, aes(y = NDVI_norm, x = green_mean)) + 
  geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) + labs(x = "green cover (JRSRP)", y = "green cover (EE)") 

cal.green.NDVI.vegmac



plot(evaluation.data$pv, evaluation.data$NDVI)


cal.green.DEA.NDVI <- ggplot(evaluation.data, aes(y = pv, x = NDVI)) + 
  geom_point() + labs(x = "green cover (EE)", y = "green cover (DEA)") + geom_smooth(method = 'lm')
cal.green.DEA.NDVI


# Compare with insitu data

cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = pv, x = green)) + 
  geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) + 
  labs(x = "green cover (in situ)", y = "green cover (dea)")
cal.green.dea.vegmac


cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = green_mean, x = green)) + 
  geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) + 
  labs(x = "green cover (in situ)", y = "green cover (jrsrp)")
cal.green.dea.vegmac


cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = NDVI, x = green)) + 
  geom_point() + labs(x = "green cover (in situ)", y = "green cover (EE)") 
cal.green.dea.vegmac


# Now with test

cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = pv, x = green_test)) + 
  geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) + 
  labs(x = "green cover (in situ)", y = "green cover (dea)")
cal.green.dea.vegmac


cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = green_mean, x = green_test)) + 
  geom_point() + geom_abline() + 
  xlim(0,100) + ylim(0,100) + 
  labs(x = "green cover (in situ)", y = "green cover (jrsrp)")
cal.green.dea.vegmac


cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = NDVI, x = green_test)) + 
  geom_point() + labs(x = "green cover (in situ)", y = "green cover (EE)") + geom_smooth(method = "lm")
cal.green.dea.vegmac




# Test normalisation ------------------------------------------------------

# Linear transformation to rescale from -1 to 1, to 0 to 100 
evaluation.data$NDVI_norm <- with(evaluation.data, ((NDVI + 1)/2)*100 )

cal.green.dea.vegmac <- ggplot(evaluation.data, aes(y = NDVI_norm, x = green)) + 
  geom_point() + geom_abline() +   xlim(0,100) + ylim(0,100) +   labs(x = "green cover (EE_norm)", y = "green cover (jrsrp)")
cal.green.dea.vegmac




