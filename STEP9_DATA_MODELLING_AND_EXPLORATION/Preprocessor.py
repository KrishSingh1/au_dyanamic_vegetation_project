# -*- coding: utf-8 -*-
"""
Created on Wed Mar 20 15:19:33 2024

@author: krish
"""
# %% Import Library
import numpy as np
import pandas as pd

import sys
import os 
from sklearn.pipeline import Pipeline
from PreprocessData import * # import from custom transformers 

sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS')
#%% Main 
#%% Read in datasets 
dom_veg = pd.read_csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv') # vegetation cover data
growth_forms_selector = ['grass', 'shrub', 'tree'] # The growth forms to include

SLGA_attributes = pd.read_csv('../DATASETS/Soils_and_Landscape_Grid_of_Australia/Output/SGLA_PCA_3.csv', index_col = 0) # vegetation cover data
SLGA_attributes_selector = ['SLGA_1', 'SLGA_2',	'SLGA_3', 'DER_000_999'] # the soil attributes to include

CO2_data = pd.read_csv('../DATASETS/CO2_Dataset/global_co2_ann_1700_2022.txt', sep = "\s\s", header = None, engine = 'python') # co2 data 
CO2_data.rename(columns = {0: 'year', 1: 'CO2'}, inplace = True)


historical_fire_ds = pd.read_csv('../DATASETS/AusPlotsBurnData/Combined_Data/AusPlots_Combined_Fire_Dataset.csv', parse_dates = ['ignition_d']) # Fire Dataset
site_info =  pd.read_csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv',
                             index_col = 0).copy() 
#%% Define a list of sites to preprocess 

smaller_subset = pd.read_csv('../DATASETS/Sites_Subset_20231010/ausplots_site_info/sites_subset.csv').copy()
bigger_subset = pd.read_csv('../DATASETS/Sites_Bigger_Subset_20240124/ausplots_bigger_subset.csv').copy()

smaller_list = np.unique(list(np.unique(smaller_subset.site_location_name.values)))

combined_list = np.unique(list(np.unique(bigger_subset.site_location_name.values)) +
                       list(np.unique(smaller_subset.site_location_name.values))) # smaller subset

## Trees
tree_sites = dom_veg[dom_veg['vegetation_type'] == 'tree' ]['site_location_name']
b_tree_sites = set(tree_sites).intersection(bigger_subset['site_location_name']) # get tree sites from the subset 
s_tree_sites = set(tree_sites).intersection(smaller_subset['site_location_name'])
tree_sites_focus = np.unique(list(s_tree_sites) + list(b_tree_sites))

## Shrubs
shrub_sites = dom_veg[dom_veg['vegetation_type'] == 'shrub' ]['site_location_name']
b_shrub_sites = set(shrub_sites).intersection(bigger_subset['site_location_name']) # get tree sites from the subset 
s_shrub_sites = set(shrub_sites).intersection(smaller_subset['site_location_name'])
shrub_sites_focus = np.unique(np.unique(list(s_shrub_sites) + list(b_shrub_sites)))
shrub_sites_focus = shrub_sites_focus[shrub_sites_focus != 'SAAEYB0001']
# SAAEYB0001 -> no soil data

## Grass
grass_sites = dom_veg[dom_veg['vegetation_type'] == 'grass' ]['site_location_name']
b_grass_sites = set(grass_sites).intersection(bigger_subset['site_location_name']) # get tree sites from the subset 
s_grass_sites = set(grass_sites).intersection(smaller_subset['site_location_name'])
grass_sites_focus = np.unique(list(s_grass_sites) + list(b_grass_sites))


#sites_list = ['NSTSYB0005']

Tussock_Grasses = ['QDACYP0010', 'QDASEQ0002', 'QDAEIU0009', 'QDAEIU0004', 'QDABBS0001',
 'WAACEK0002', 'NTADAC0001', 'QDAGUP0015', 'VCAAUA0011', 'WAANOK0004']
Hummock_Grasses = ['NTAGFU0007', 'WAAMUR0029', 'WAAMUR0031', 'WAAPIL0002', 'WAAGSD0001',
 'QDASSD0004', 'WAAPIL0031', 'SATFLB0018', 'WAAMUR0028', 'QDASSD0001']

# Decide which set to use 
sites_list = tree_sites

all_sites_2024_names = np.unique(site_info['site.info.site_location_name'])

# All avaliable DEA FC Data 

files = os.listdir(f'../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/')
files = [i.split('.')[0] for i in files]
all_dea_fc = files[1:]

# QDADEU0002 ---> no veg information
# QDASEQ0001 --> no DEA FC
Error_list = ['QDADEU0002', 'QDASEQ0001']
sites_list = tree_sites



profiling_site_list = smaller_list
#%% Which list of sites to use 

sites_list = all_dea_fc
#%% Keeping an Error Log 
# This is for keeping a log of any missing data in the sites we are preprocessing 
column_names = ['Error']
site_log = pd.DataFrame(columns = column_names, index = sites_list)
site_log['Error'] = ''
site_log.head()

# '' -> no error
# 1 -> missing/non recorded fire history data, but don't skip the preprocessing
# 2 -> missing veg data, skip preprocessing
# 3 -> missing SLGA data, skip preprocessing
# 4 -> missing DEA FC data, skip preprocessing
# These will form combinations between 1,2,3,4 if multiple errors exsist eg.
#   123 -> missing or non-recorded fire, veg, SLGA data 

#%% Begin Preprocessing

max_counter = len(sites_list)
counter = 1
print(f'The Data Awaiting Preprocessing: {sites_list}')
preprocess_data = True
for site_location_name in sites_list:
    
    skip_preprocess = False 
    
    print(f'Processing Data for {site_location_name}, {counter}/{max_counter}')
    
    time_lag = 1
    window_length = 5
    time_range = 3
    
    # savgol_filter parameters 
    window_length_smooth = 15
    polyorder = 4 
    
    # Preprocess fire dataset
    historical_fire_pipeline = Pipeline([
        ('historical_burn_date_preprocess', historical_burn_date_preprocess(site_location_name))
        ])
    historical_fire_ds_site = historical_fire_pipeline.fit_transform(historical_fire_ds)
    
    if historical_fire_ds_site.empty:
        site_log.loc[site_log.index == site_location_name,'Error'] += '1' 
        print('Error 1') # 
    
    # Get Latitude of Site to derive daylength 
    site_specific_info = site_info[site_info['site.info.site_location_name'] == site_location_name]
    latitude = site_specific_info['site.info.latitude'][site_specific_info.index[0]] # only need the first entry
    
    # Get Site-specific growth and soil information
    site_dom_veg = dom_veg[dom_veg.site_location_name == site_location_name]
    site_SLGA_attributes = SLGA_attributes[SLGA_attributes.index == site_location_name]
    
    if site_dom_veg.empty:
        site_log.loc[site_log.index == site_location_name,'Error'] += '2' 
        skip_preprocess = True
        print('Error 2')
    if site_SLGA_attributes.empty:
        site_log.loc[site_log.index == site_location_name,'Error'] += '3'
        skip_preprocess = True 
        print('Error 3')
        
    # Get the FC time series of the site 
    try:
        site = pd.read_csv(f'../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/{site_location_name}.csv', parse_dates=['time'])
    except:
        site_log.loc[site_log.index == site_location_name,'Error'] += '4'
        skip_preprocess = True 
        print('Error 4')

    if skip_preprocess == True | preprocess_data == False:
        print(f'Data for {site_location_name} SKIPPED  {counter}/{max_counter}')
        counter += 1
        continue # skip this loop iteration 
         
    # Preprocess FC time series, add daylength and fire information
    time_fc_pipeline = Pipeline([
        ('preprocess_fc_time_series', 
         preprocess_fc_time_series(window_length = window_length_smooth, polyorder = polyorder)),
        ('time_attributes_adder',
         time_attributes_adder()),
        ('daylength_attributes_adder',
         daylength_attributes_adder(latitude)),
        ('historical_burn_date_attribute_adder', 
         historical_burn_date_attribute_adder(historical_fire_ds_site)),
        ('historical_burn_date_index_attribute_adder',
         historical_burn_date_index_attribute_adder(verbose = False,
                                                    historical_fire_ds = historical_fire_ds_site,
                                                    time_range = time_range)),
        ('growth_forms_adder',
         growth_forms_adder(site_dom_veg, growth_forms_selector)),
        ('SLGA_soil_atributes_adder',
         SLGA_soil_atributes_adder(site_SLGA_attributes, SLGA_attributes_selector))
         ])
    site_resampled = time_fc_pipeline.fit_transform(site)
    
    print('FC and fire data successfully preprocessed')
    # Used to add climate attributes directly from AGCD 
    precip = pd.read_csv(f'../DATASETS/Climate_Gridded/precip/{site_location_name}_1980_2022.csv', 
                         parse_dates=['time'], usecols = ['time', 'precip'])
    tmin = pd.read_csv(f'../DATASETS/Climate_Gridded/tmin/{site_location_name}_1980_2022.csv',
                       parse_dates=['time'],  usecols = ['time', 'tmin'])
    tmax = pd.read_csv(f'../DATASETS/Climate_Gridded/tmax/{site_location_name}_1980_2022.csv',
                       parse_dates=['time'],  usecols = ['time', 'tmax'])
    vapourpres_h09 = pd.read_csv(f'../DATASETS/Climate_Gridded/vapourpres_h09/{site_location_name}_1980_2022.csv', 
                                 parse_dates=['time'],  usecols = ['time', 'vapourpres_h09'])
    vapourpres_h15 = pd.read_csv(f'../DATASETS/Climate_Gridded/vapourpres_h15/{site_location_name}_1980_2022.csv',
                                 parse_dates=['time'], usecols = ['time', 'vapourpres_h15'])
    
    # Now merge everying as climate data 
    # Note: the proceeding indented line continues from the previous one as indicated by '.\'
    climate_data = tmin.merge(tmax, left_on = 'time', right_on = 'time').merge(vapourpres_h09, left_on = 'time', right_on = 'time').\
        merge(vapourpres_h15, left_on = 'time', right_on = 'time').merge(precip, left_on = 'time', right_on = 'time').sort_values('time')
    climate_data = climate_data.sort_values('time')
    climate_data = climate_data.set_index('time')
    
    # Derive Climate Variables from preprocessed AGCD time series 
    derive_climate_vars_pipeline = Pipeline([
            ('mean_annual_variables_adder', mean_annual_variables_adder(climate_data)),
            ('pages_precip_variables_adder', pages_precip_variables_adder(climate_data, False)),
            ('pages_VPD_variables_adder', pages_VPD_variables_adder(climate_data, False)),
            ('pages_temp_variables_adder', pages_temp_variables_adder(climate_data, False))
        ])
    site_resampled = derive_climate_vars_pipeline.fit_transform(site_resampled)
        
    # Add CO2 data
    site_resampled['time'] = site_resampled.index
    site_resampled = site_resampled.merge(CO2_data, how = 'left', on = 'year', suffixes = ('', '_DUPLICATE'))
    site_resampled = site_resampled.drop(columns =  site_resampled.filter(regex = '_DUPLICATE$').columns)
    
    site_merged = site_resampled.copy()
    # site_merged.to_csv(f'Input_DataSet_{site_location_name}.csv')
    # site_merged.to_csv(f'C:/Users/krish/Desktop/Grassses_DEA_FC/Tussock_Grasses/{site_location_name}.csv')
    site_merged.to_csv(f'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/Input_DataSet_{site_location_name}.csv')
        
    print(f'Data for {site_location_name} Exported  {counter}/{max_counter}')
    counter += 1
    
site_log.to_csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/Log/Site_Preprocessing_Log_1.csv')
#sys.stdout = stdout_obj
