"""
Training and Set up

Krish Singh
13-08-2024

From each site group, select sites to leave out as a test set (90/10). Then, perform a 0-1 min-max scaling measure. 
"""


#%% Imports 
import numpy as np
import pandas as pd
import os 


import sys
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')

import random
from sklearn import preprocessing # for min-max-scaling


#%% Functions 


#%% Main    

#%%  Select relevant Parameters 
    
# Selection of features for the training/test set
# Note temporarily the SITE_LOCATION_NAME is included to identify site names for the purpose of Kfold creation
SEASONAL_FEATURES = ['photoperiod', 'photoperiod_gradient']

PRECIP_FEATURES = ['precip_30', 'precip_90', 'precip_180', 
                   'precip_365', 'precip_730', 'precip_1095', 
                   'precip_1460', 'MAP']

TEMP_FEATURES = ['tmax_lag', 'tmax_7', 'tmax_14', 
                 'tmax_30', 'tmin_lag', 'tmin_7', 
                 'tmin_14', 'tmin_30', 'MAT']

VPD_FEATURES = ['VPD_lag','VPD_7', 'VPD_14',
                'VPD_30']

FIRE_FEATURES = ['days_since_fire', 'fire_severity']

CO2_FEATURES = ['CO2']

SOIL_FEATURES = ['SLGA_1','SLGA_2','SLGA_3', 'DER_000_999'] # the soil attributes to include

TOPOGRAPHIC_FEATURES = ['aspect_1s', 'twi_1s']

SITE_LOCATION_NAME = ['site_location_name']

FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES + CO2_FEATURES + SOIL_FEATURES + TOPOGRAPHIC_FEATURES + SITE_LOCATION_NAME # NOTE ALWAYS LEAVE SITE LOCATION_NAME AT THE LAST, so the script knows to exclude it from the normalisation
TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
scores = []


directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/MODELLED_TRAINING_DATA'

super_group_list = ['Desert Chenopod', 'Desert Forb', 'Desert Hummock.grass',
       'Desert Shrub', 'Desert Tree.Palm', 'Desert Tussock.grass',
       'Temp/Med Shrub', 'Temp/Med Tree.Palm', 'Temp/Med Tussock.grass',
       'Tropical/Savanna Tree.Palm', 'Tropical/Savanna Tussock.grass']

target_group_list = ['Desert Tree.Palm'] 

apply_scaler = False # whether or not to apply a scaling

max_counter = len(super_group_list)
for counter, s in enumerate(super_group_list):
    
    print(f'Processing {s} sites ({counter + 1}/{max_counter})')
    
    selected_super_group = s
    super_groups_classified = pd.read_csv('C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Extracted_Data/Final/sites_super_classified.csv')
    selected_super_group_list = super_groups_classified.loc[super_groups_classified['super_group'] == selected_super_group]['site_location_name']
    
    sites_list = selected_super_group_list
    
    #%% Create Train/test set 
    
    # Training and test set 
    datasets =  {} # entire set - for final evaluation 
    training_set = {} # training set 
    test_set = {} # test set 
    
    # Iterate through the site list 
    # Set up a random seed, based on the date it was set YYYYMMDD
    random.seed(20240807)
    
    sample_size = len(sites_list.values)
    training_ratio = 0.9
    training_size = int(np.floor(sample_size * 0.9))
    
    test_size = sample_size - training_size
    
    randomised_site_list = [i for i in sites_list.values]
    random.shuffle(randomised_site_list)
    
    test_set_selections = randomised_site_list[-test_size:]
    train_set_selections = randomised_site_list[:training_size]
    
    print(f'Train size: {training_size} ({training_ratio}%)\n{train_set_selections}')
    print(f'Test size: {test_size} ({round(1 - training_ratio,2)}%)\n{test_set_selections}')

    # Create a CSV file detailing the current training and test selections 
    
    train_code = [0 for i in range(len(train_set_selections))]
    test_code = [1 for i in range(len(test_set_selections))]
    
    training_list_df = pd.DataFrame({'site_location_name' : train_set_selections,
                                     'train_code' : train_code})
    testing_list_df = pd.DataFrame({'site_location_name' : test_set_selections,
                                     'train_code' : test_code})
    
    combined_list_df = pd.concat([training_list_df, testing_list_df]).reset_index(drop = True)
    
    # Now Construct the training and test dataset 
    for i, row in combined_list_df.iterrows():
        
       
        # Get associated site_location_name and Modelled Processed DEA FC 
        site_location_name = row['site_location_name'] # get the site_location_name 
        site_merged = pd.read_csv(f'../DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/Input_DataSet_{site_location_name}.csv',
                                  parse_dates = ['time'], index_col = 0).copy() # read 
        site_merged['site_location_name'] = site_location_name 
        site_merged.sort_values('time', inplace = True)
        site_merged.reset_index(inplace = True)
        site_merged = site_merged.dropna(subset = FEATURES)
        
        
        
        # Based on training code, merge with training and test set 
        datasets[site_location_name] = site_merged # have everything combined just in case 
        if row['train_code'] == 0:
            training_set[site_location_name] = site_merged # assign training sites here 
        elif row['train_code'] == 1:
            test_set[site_location_name] = site_merged # assign testing sites here 
        else: 
            print('Error: Mislabelled train_code. Please check the site allocations.')
       
    training_merged = pd.concat(training_set).dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
    training_merged.sort_values('time', inplace = True)
    training_merged.set_index('time', inplace = True)
    
    test_merged = pd.concat(test_set).dropna(subset = FEATURES)
    test_merged.sort_values('time', inplace = True)
    test_merged.set_index('time', inplace = True)
    
    # Apply the 0-1 min-max scale on the variables 
    if apply_scaler == True:
        scaler = preprocessing.MinMaxScaler()
        training_merged[FEATURES[:-1]] = scaler.fit_transform(training_merged[FEATURES[:-1]])
        test_merged[FEATURES[:-1]] = scaler.transform(test_merged[FEATURES[:-1]])
    
    datasets = pd.concat(datasets).dropna(subset = FEATURES)
    datasets.sort_values('time', inplace = True)
    datasets.set_index('time', inplace = True)    

#%% Set Up directories to store data  

    selected_super_group = '_'.join(s.split('/')) 
    results_dir = f'{directory}/{selected_super_group}'
        
    # Create Directory If this does not exist 
    if os.path.exists(results_dir) == False:
        os.makedirs(results_dir)
        
    if os.path.exists(results_dir + '/Training') == False:
        os.makedirs(results_dir + '/Training') # also create a plots folder under since we can assume this folder did not initialy exist 
        
    if os.path.exists(results_dir + '/Test') == False:
        os.makedirs(results_dir + '/Test')
            
#%% Create csv files to store data

    combined_list_df.to_csv(f'{results_dir}/{selected_super_group}_Site_Allocations.csv')
    datasets.to_csv(f'{results_dir}/{selected_super_group}_Combined_Train_Test.csv')
    training_merged.to_csv(f'{results_dir}/Training/{selected_super_group}_Train_Set.csv')
    test_merged.to_csv(f'{results_dir}/Test/{selected_super_group}_Test_Set.csv')
    