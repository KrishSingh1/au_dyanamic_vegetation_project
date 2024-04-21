# -*- coding: utf-8 -*-
"""
Created on Wed Mar 20 15:19:33 2024

@author: krish
"""

# %% Import Library

import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import os
from sklearn.model_selection import TimeSeriesSplit
import sys
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS')
import xgboost as xgb
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline


from PreprocessData import * # import from custom transformers 

#%% Main 
#  %% Preprocess and create train/test'


# WAAPIL0003
# NSABHC0023 [issue with fire history]
# TCATCH0006 [Issue with fire history]
# WAAGAS0002 [Issue with fire history]
# NSAMDD0014 [Issue with fire history]
# NTAGFU0021 [Issue with fire history]
# NSANSS0001 [Issue with fire history]
# SATSTP0005 [Issue with fie history]
# QDASSD0015 [Issue  with fire history]
# NTAFIN0002 []
# NSANAN0002
# QDAEIU0010

#site_location_name = 'NSAMDD0002' # no fire, seasonal
#site_location_name = 'NSANAN0002' # fire, seasonal, big drop



# sites_list = ['WAAPIL0003', 'NSABHC0023', 'TCATCH0006',
#                'WAAGAS0002', 'NSAMDD0014', 'NTAGFU0021', 
#                'NSANSS0001', 'SATSTP0005', 'QDASSD0015', 
#                'NTAFIN0002', 'NSANAN0002', 'QDAEIU0010'] # smaller subset

# sites_list = ['NSABBS0001','NSABHC0011','NSACOP0001',
#              'NSAMDD0001','NSAMDD0011','NSAMDD0020',
#              'NSAMDD0028','NSAMUL0003','NSANAN0001',
#              'NSANAN0002','NSANSS0002','NSTSYB0003',] # bigger subset - NSW

# sites_list = np.unique(['NSABBS0001',
#  'NSABHC0011',
#  'NSACOP0001',
#  'NSAMDD0001',
#  'NSAMDD0011',
#  'NSAMDD0020',
#  'NSAMDD0028',
#  'NSAMUL0003',
#  'NSANAN0001',
#  'NSANAN0002',
#  'NSANSS0002',
#  'NSTSYB0003',
#  'NSTSYB0005',
#  'NSTSYB0006',
#  'NSABHC0023',
#  'NSAMDD0014',
#  'NSANSS0001',
#  'NSANAN0002'])


# sites_list = np.unique(['NTADAC0001',
#  'NTADMR0001',
#  'NTAFIN0003',
#  'NTAFIN0006',
#  'NTAFIN0015',
#  'NTAFIN0018',
#  'NTAGFU0014',
#  'NTAGFU0020',
#  'NTAGFU0030',
#  'NTAGFU0034',
#  'NTASTU0004',
#  'NTTDMR0003',
#  'NTAGFU0021',
#  'NTAFIN0002'])

# sites_list = np.unique(['QDABBN0002',
#  'QDABBS0002',
#  'QDABBS0010',
#  'QDACHC0003',
#  'QDACYP0006',
#  'QDACYP0018',
#  'QDACYP0020',
#  'QDACYP0022',
#  'QDAEIU0005',
#  'QDAGUP0006',
#  'QDAGUP0009',
#  'QDAGUP0019',
#  'QDAGUP0021',
#  'QDAMGD0002',
#  'QDAMGD0023',
#  'QDAMGD0024',
#  'QDAMGD0025',
#  'QDAMUL0002',
#  'QDAMUL0003',
#  'QDASEQ0004',
#  'QDASSD0015', 
#  'QDAEIU0010'])

# sites_list = np.unique(['WAAAVW0006',
#  'WAACAR0002',
#  'WAACAR0004',
#  'WAACOO0007',
#  'WAACOO0016',
#  'WAACOO0024',
#  'WAACOO0026',
#  'WAACOO0027',
#  'WAACOO0029',
#  'WAACOO0030',
#  'WAAGAS0001',
#  'WAAGES0001',
#  'WAALSD0002',
#  'WAANOK0006',
#  'WAANUL0003',
#  'WAAPIL0010',
#  'WAAPIL0023',
#  'WAAPIL0024',
#  'WAAPIL0031',
#  'WAAPIL0003',
#  'WAAGAS0002'])

# sites_list = np.unique(['SAAEYB0001',
#  'SAAEYB0021',
#  'SAAEYB0028',
#  'SAAEYB0029',
#  'SAAFLB0003',
#  'SAAFLB0005',
#  'SAAFLB0008',
#  'SAAGAW0008',
#  'SAAKAN0009',
#  'SAASTP0023',
#  'SAASTP0033',
#  'SAASTP0034',
#  'SASMDD0005',
#  'SASMDD0009',
#  'SASMDD0014',
#  'SATFLB0003',
#  'SATFLB0019',
#  'SATFLB0020',
#  'SATFLB0022',
#  'SATFLB0023',
#  'SATSTP0005'])


#sites_list = np.unique(['TCATCH0004', 'TCATNM0001', 'TCATNM0003', 'TCATCH0006'])

sites_list = ['NSTSYB0003']

for site_location_name in sites_list:
    
    time_lag = 1
    window_length = 5
    time_range = 3
    
    # savgol_filter parameters 
    window_length_smooth = 15
    polyorder = 4 
    
    historical_fire_ds = gpd.read_file('../DATASETS/AusPlots_Historical_BurnDates.geojson', parse_dates = ['igntn_d'])
    historical_fire_pipeline = Pipeline([
        ('historical_burn_date_preprocess', historical_burn_date_preprocess(site_location_name))
        ])
    historical_fire_ds = historical_fire_pipeline.fit_transform(historical_fire_ds)
    
    # Get Latitude of Site to derive daylength 
    site_info = pd.read_csv('../DATASETS/site_info_2-0-6.csv', usecols = ['site_location_name','latitude'])
    site_info = site_info[site_info['site_location_name'] == site_location_name]
    latitude = site_info.latitude[site_info.index[0]] # only need the first entry
    
    site = pd.read_csv(f'../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/{site_location_name}.csv', parse_dates=['time'])
    
    time_fc_pipeline = Pipeline([
        ('preprocess_fc_time_series', preprocess_fc_time_series(window_length = window_length_smooth, polyorder = polyorder)),
        ('time_attributes_adder', time_attributes_adder()),
        ('time_attributes_fc_lag_adder', time_attributes_fc_lag_adder(time_lag)),
        ('time_attributes_fc_diff_adder', time_attributes_fc_diff_adder(False)),
        ('daylength_attributes_adder', daylength_attributes_adder(latitude)),
        ('historical_burn_date_attribute_adder', historical_burn_date_attribute_adder(historical_fire_ds)),
        ('historical_burn_date_index_attribute_adder', historical_burn_date_index_attribute_adder(verbose = True,
                                                                                                  historical_fire_ds = historical_fire_ds,
                                                                                                  time_range = time_range))
        #('historical_burn_date_index_attribute_adder_lag', historical_burn_date_index_attribute_adder_lag(time_lag = time_lag,
        #                                                                                            month_baseline = month_baseline, verbose = True))
     ])
    site_resampled = time_fc_pipeline.fit_transform(site)
    print('FC and fire data successfully preprocessed')
    
    
    # The climate variables as named in my directory and the resampling method
    climate_variables = pd.DataFrame({'climate_var': ['precip','tmax','tmin','vapourpres_h09','vapourpres_h15'],
                                     'resample_type': ['sum', 'mean','mean','mean','mean']})
    datasets = dict()
    
    # Used to add climate attributes directly from climate data 
    for index, row in climate_variables.iterrows():
    
        climate = pd.read_csv(f'../DATASETS/Climate_Gridded/{row["climate_var"]}/{site_location_name}_1980_2022.csv', parse_dates=['time'])
        print(climate)
        
        datasets[row['climate_var']] = climate
        
        time_climate_pipeline = Pipeline([
            ('preprocess_climate_time_series', preprocess_climate_time_series()),
            ('climate_time_series_downsample', climate_time_series_downsample(start_time = site_resampled.index[0], resample_method = row['resample_type'])),
            ('time_attributes_adder', time_attributes_adder()),
            ('climate_time_series_attributes_adder', climate_time_series_attributes_adder(window = window_length, 
                                                                                          lag = window_length))
        ])
        climate_new = time_climate_pipeline.fit_transform(climate)
        site_resampled = site_resampled.merge(climate_new, how = 'left', left_index = True, right_index = True, validate = "one_to_one",
                                           suffixes = ('', '_DUPLICATE'))
        site_resampled = site_resampled.drop(columns =  site_resampled.filter(regex = '_DUPLICATE$').columns)
        
    ## Derive VPD from preprocessed climate data 
    derive_climate_vars_pipeline = Pipeline([
        ('calc_VPD', calc_VPD()),
        ('pages_precip_variables_adder', pages_precip_variables_adder(site_location_name)),
        ('pages_VPD_variables_adder', pages_VPD_variables_adder(site_location_name)),
        ('pages_temp_variables_adder', pages_temp_variables_adder(site_location_name))
    ])
    site_resampled = derive_climate_vars_pipeline.fit_transform(site_resampled)
    site_merged = site_resampled.copy()
    site_merged.to_csv(f'Input_DataSet_{site_location_name}.csv')