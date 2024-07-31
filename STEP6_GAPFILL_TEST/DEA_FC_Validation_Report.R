####### DEA FC Validation Report #######
## Author: Krish Singh
## 03-07-2024 (dd-mm-yyyy)
## Purpose: To check and analyse the reprocessed DEA FC for any potential issues 
##          e.g amount of NANs, how well the spatial alignment is, etc


# Libraries ---------------------------------------------------------------

library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(officer)
library(flextable)
library(sfheaders)



# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------
# Look at the directory for all avaliable DEA FC data per site 
directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)
file.names <- unlist(lapply(strsplit(file.names, '_'), FUN = function(x){
  unlist(x)[3]
})) # Grab the site location name 
file.names_state <- unlist(lapply(file.names, FUN = function(x){
  substring(x, first = 1,2)
})) # Grab corresponding state of the site 
states <- unique(file.names_state) 

# Grab the cleaned corner points information of each site 
site.corners.data.cleaned <- read.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv') %>% 
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(3577)

# Get our evaluation data:
evaluation.data <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_with_percentiles.csv') %>%
  mutate(visit_start_date = as.Date(visit_start_date)) %>%
  select(!time) %>%
  filter(is.na(green) == FALSE | is.na(brown) == FALSE | is.na(bare) == FALSE) %>%
  rename(time = visit_start_date,
         pv = green,
         npv = brown,
         bs = bare)

theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats.csv')

for(state in states){
  
  file.selection <- file.names[which(file.names_state == state)]
  doc <- read_docx()
  avaliable_samples <- c()
  plot_file <- tempfile(fileext = ".png")
  counter <- 1
  max_counter <- length(file.selection)
  
  for (query in file.selection) {
    
    print(paste0(counter,'/',max_counter, ' {', query, '}'))
    
    # Read associated DEA FC 
    test <- fread(paste0(directory, 'Input_DataSet_' ,query, '.csv')) %>%
      mutate(time = as.Date(time)) %>%
      select(c('time', 'pv_filter', 'npv_filter', 'bs_filter')) %>%
      rename(pv = pv_filter, 
             npv = npv_filter,
             bs = bs_filter)
    
    # Create doc heading 
    doc <- doc %>%
      body_add_par(value = query, style = "heading 1") 
    
    # If the DEA FC dataset is empty, skip this query
    if(nrow(test) < 1){
      print(paste0('Skip: ', '{', query , '}' ))
      doc <- doc %>%
        body_add_break()
      counter <- counter + 1
      next()
    }
    
    # Subset Evaluation subset 
    evaluation_subset <- evaluation.data %>%
      subset(site_location_name == query)
    
    theil_sen_reg_subset <- theil_sen_reg %>%
      subset(site_location_name == query)
    
    # Plot time series per fractional cover 
    for (fraction in c('pv', 'npv', 'bs')) {
      
      if (fraction == 'pv')
        colour <- 'darkgreen'
      else if (fraction == 'npv')
        colour <- 'steelblue'
      else if (fraction == 'bs')
        colour <- 'darkred'
      
      doc <- doc %>%
        body_add_table(as.data.frame(rbind(summary(test[[fraction]]))), style = 'centered')
      
      t <- ggplot(data = test, aes(x = time))  +
        geom_line(aes(y = .data[[fraction]], ), size = 0.3, color = colour) + 
        scale_y_continuous(limits = c(0, 100), breaks = scales::pretty_breaks(n = 5)) +
        scale_x_date(date_labels = "%Y") +
        geom_point(data = evaluation_subset, aes(x =  time, y = .data[[fraction]]), shape = 4, color = 'blue') +
        geom_abline(intercept = theil_sen_reg_subset[[paste0(fraction, '_filter','_intercept')]], 
                    slope = theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope')]], color = 'red') +
        ggtitle(paste0('slope = ', round(theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope_yr')]],7),
                       paste0(' ', fraction, '/yr')))
        
      ggsave(plot_file, t, width = 7, height = 3, units = "in")
      doc <- doc %>%
        body_add_img(src = plot_file, width = 7, height = 3, style = "centered")
      
    }
    
    evaluation_subset <- evaluation_subset %>%
      mutate(time = as.factor(time))
    
    fraction <- 'pv'
    pv_t <- ggplot(data = test) +
      stat_ecdf(mapping = aes(x = .data[[fraction]]), color = 'darkgreen') +
      geom_point(data = evaluation_subset,
                 mapping = aes(x = .data[[paste0(fraction, '_filter')]], y = .data[[paste0(fraction, '_filter', '_percentile')]],
                               color = time), size = 2)
    
    fraction <- 'npv'
    npv_t <- ggplot(data = test) +
      stat_ecdf(mapping = aes(x = .data[[fraction]]), color = 'steelblue') +
      geom_point(data = evaluation_subset,
                 mapping = aes(x = .data[[paste0(fraction, '_filter')]], y = .data[[paste0(fraction, '_filter', '_percentile')]],
                               color = time), size = 2)
    
    fraction <- 'bs'
    bs_t <- ggplot(data = test) +
      stat_ecdf(mapping = aes(x = .data[[fraction]]), color = 'darkred') +
      geom_point(data = evaluation_subset,
                 mapping = aes(x = .data[[paste0(fraction, '_filter')]], y = .data[[paste0(fraction, '_filter', '_percentile')]],
                               color = time), size = 2)
    
    cowplot::save_plot(plot = cowplot::plot_grid(pv_t, npv_t, bs_t), filename = plot_file,
                       base_width = 7, base_height = 5, units = 'in')
    doc <- doc %>%
      body_add_img(src = plot_file, width = 7, height = 5, style = "centered")
    
    test.dea.trimed <- fread(paste0('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL/', query, '.csv'))
    test.dea.trimed <- test.dea.trimed %>% 
      subset(ue <= 25.5 & is.na(pv) == FALSE &
               is.na(bs) == FALSE & is.na(npv) == FALSE ) 
    # Now Add A plot for counting pixels per time slice 
    result <- test.dea.trimed %>%
      group_by(time) %>%
      summarize(count = n())
    result$time <- as.Date(result$time)
    
    t <- ggplot(data = result, aes(x = time))  +
      geom_line(aes(y = count)) +
      scale_x_date(date_labels = "%Y") +
      ylim(c(0, 50))
    
    ggsave(plot_file, t, width = 7, height = 2, units = "in")
    doc <- doc %>%
      body_add_img(src = plot_file, width = 7, height = 2, style = "centered")
    
    doc <- doc %>%
      body_add_par(paste0(as.data.frame(cbind(summary(result)))[,2], collapse = " "), 
                   style = "centered")
    
    # Add a plot that shows the spatial reference 
    test.dea.trimed.2 <- test.dea.trimed %>%
      st_as_sf(coords = c('x', 'y')) %>%
      st_set_crs(3577)
    
    t <- ggplot() + geom_sf(data = test.dea.trimed.2, colour = 'red') +
      geom_sf(data = site.corners.data.cleaned[which(site.corners.data.cleaned$site_location_name == query),], color = 'blue') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggsave(plot_file, t, width = 7, height = 4, units = "in")
    doc <- doc %>%
      body_add_img(src = plot_file, width = 7, height = 4, style = "centered")
    
    # Now show the number of time slices for each year 
    result <- test %>%
      mutate(year = format(time, "%Y")) %>%  # Extracting the year from the date column
      group_by(year) %>%
      summarize(count = n())
    
    result$year <- as.factor(result$year)
    t <- ggplot(data =result, mapping = aes(x = year, y = count)) + 
      geom_col() + geom_hline(yintercept = 365/16) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggsave(plot_file, t, width = 7, height = 2, units = "in")
    doc <- doc %>%
      body_add_img(src = plot_file, width = 7, height = 2, style = "centered")
    
    doc <- doc %>%
      body_add_par(value = paste0('Number of samples: ', nrow(test)),
                   style = "centered") %>%
      body_add_break()
    
    avaliable_samples <- c(avaliable_samples,nrow(test))
    counter <- counter + 1
    print(doc, target = paste0("../DATASETS_TO_SHARE/DEA_Extraction_Site_Validation_Report_", state ,".docx"))
  }
}

nrow(test.dea.trimed[test.dea.trimed$time == '2004-10-14',])

hist(avaliable_samples, breaks = 100)
abline(v = 782.5)

boxplot(avaliable_samples)
summary(avaliable_samples) # About 75% of the data lies above 782.5 avaliable samples 
                           # Will need to be cautious about the rest of the data that lies below this range

directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)
time_slices_counter <- c()
for (query in file.names) {
  test <- fread(paste0(directory, query, '.csv'))
  time_slices_counter <- c(time_slices_counter, nrow(test))
}


time_slices_counter <- as.data.frame(time_slices_counter)
time_slices_counter$site <- file.names

ggplot(data = time_slices_counter, mapping = aes(y = time_slices_counter)) +
  geom_boxplot() + geom_hline(yintercept = 400, color = 'red') +
  geom_hline(yintercept = 800, color = 'green')

ggplot(data = time_slices_counter, mapping = aes(x= time_slices_counter)) +
  geom_histogram() + geom_vline(xintercept = 400, color = 'red') +
  geom_vline(xintercept = 800, color = 'green')


# Note:
## I need to check coordinates references with only a SW point 
## 



