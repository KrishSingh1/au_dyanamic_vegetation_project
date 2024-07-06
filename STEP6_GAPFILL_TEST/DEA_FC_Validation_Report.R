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

directory <- 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

site.corners.data <- read.csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Location/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv')
site.corners.data.cleaned <- site.corners.data %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(3577)

doc <- read_docx()
avaliable_samples <- c()
plot_file <- tempfile(fileext = ".png")
counter <- 1
max_counter <- length(file.names)

for (query in file.names) {
  
  print(paste0(counter,'/',max_counter, ' {', query, '}'))

  
  test <- fread(paste0(directory, query, '.csv'))
  test$time <- as.Date(test$time)
  
  doc <- doc %>%
    body_add_par(value = query, style = "heading 1") 
  
  if(nrow(test) < 1){
    next()
    print(paste0('Skip: ', '{', query , '}' ))
  }
  
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
      geom_line(aes(y = .data[[fraction]]), color = colour, size = 0.3) + 
      geom_point(aes(y = .data[[fraction]]), size = 0.2) +
      scale_x_date(date_labels = "%Y") +
      ylim(c(0,100))
    
    ggsave(plot_file, t, width = 7, height = 2, units = "in")
    
    doc <- doc %>%
      body_add_img(src = plot_file, width = 7, height = 2, style = "centered")
    
  }
  
  test.dea.trimed <- fread(paste0('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL/', query, '.csv'))
  # Now Add A plot for counting pixels per time slice 
  result <- test.dea.trimed%>%
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
  print(doc, target = "DEA_Extraction_Site_Validation_Report.docx")
}
print(doc, target = "DEA_Extraction_Site_Validation_Report.docx")


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



