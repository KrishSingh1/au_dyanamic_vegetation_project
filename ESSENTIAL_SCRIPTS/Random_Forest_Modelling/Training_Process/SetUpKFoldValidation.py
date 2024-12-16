import numpy as np
import pandas as pd
import os 
import sys
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')

from sklearn.model_selection import KFold
#%% Functions 


#%% Main 

directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/MODELLED_TRAINING_DATA'

super_group_list = ['Desert Chenopod', 'Desert Forb', 'Desert Hummock.grass',
       'Desert Shrub', 'Desert Tree.Palm', 'Desert Tussock.grass',
       'Temp/Med Shrub', 'Temp/Med Tree.Palm', 'Temp/Med Tussock.grass',
       'Tropical/Savanna Tree.Palm', 'Tropical/Savanna Tussock.grass']

target_group_list = ['Desert Tree.Palm'] 

k_folds = 10 # the number of k-folds


max_counter = len(super_group_list)
for counter, s in enumerate(super_group_list):
    
    print(f'Processing {s} sites ({counter + 1}/{max_counter})')
    
    selected_super_group = s
    
    super_group_folder_name = '_'.join(s.split('/')) 
    training_directory = f'{directory}/{super_group_folder_name}/Training'
    
    #%% Create the folds 
    
    
    training_set = pd.read_csv(f'{training_directory}/{super_group_folder_name}_Train_Set.csv', index_col = 0).copy()
    sites_list = np.unique(training_set['site_location_name'])
    
    # Use Sklearn's kfold to generate a split 
    kf = KFold(n_splits=k_folds, shuffle = True, random_state = 20240807) # Set up a random seed, based on the date it was set YYYYMMDD, purely arbituary
    kf.get_n_splits(sites_list)
    
    intersect_checker = []

    # Create a CSV file detailing the current training and test selections 
    for i, (train_index, test_index) in enumerate(kf.split(sites_list)):
        
        print(f'Processing {i+1} kfold ({i + 1}/{k_folds})')
        
        intersect_checker += list(sites_list[test_index])

        # Get the training fold 
        training_site_fold_site_names = sites_list[train_index]
        training_fold = training_set[training_set['site_location_name'].isin(training_site_fold_site_names)] # subset for sites in train fold
    
        # Get the validation fold 
        val_site_fold_site_names = sites_list[test_index]
        val_fold = training_set[training_set['site_location_name'].isin(val_site_fold_site_names)] # subset for sites in val fold
        
        # Create a directory per loop 
        file_number = i+1
        kfold_training_dir = f'{directory}/{super_group_folder_name}/Validation/KFolds_{file_number}'
        kfold_test_dir = f'{directory}/{super_group_folder_name}/Validation/KFolds_{file_number}'
        
        if os.path.exists(kfold_training_dir) == False:
            os.makedirs(kfold_training_dir)
            
        if os.path.exists(kfold_test_dir) == False:
            os.makedirs(kfold_test_dir)
            
        # Save the CSVs in the created directories per loop 
        training_fold.to_csv(f'{kfold_training_dir}/{super_group_folder_name}_training_fold_{file_number}.csv')
        val_fold.to_csv(f'{kfold_test_dir}/{super_group_folder_name}_validation_fold_{file_number}.csv')
    
    # Print out any intersections between train/test 
    if len(np.unique(intersect_checker)) == len(intersect_checker):
        print('No intersections')
    else:
        print('Intersection found')
        


# Test for intersection of sites between kfolds:
