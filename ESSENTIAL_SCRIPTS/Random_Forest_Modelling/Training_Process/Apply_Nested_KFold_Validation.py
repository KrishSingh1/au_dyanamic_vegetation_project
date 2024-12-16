#%% Libraries

import numpy as np
import pandas as pd
import os 
import sys
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')
import matplotlib.pyplot as plt

from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn.metrics import root_mean_squared_error


from skopt import BayesSearchCV
from skopt.space import Real, Categorical, Integer

from sklearn.inspection import permutation_importance

import time
from joblib import dump
#%% Functions 

def plotPredictions(df, TARGET, directory_plot_output = '', msg = '', split = '', fire_split = ''):
    fig, ax = plt.subplots(nrows = 3, figsize = (15,10))
    fig.suptitle(msg, fontsize=30)
    for i,v in enumerate(TARGET):
        df[v].plot(ax=ax[i], color = 'blue', alpha = 0.4, linestyle='dashed', label = f'Observed {v.split("_")[0]}')
        df[f'pred_{v}'].plot(ax=ax[i], color = 'orange', ylim = (0,100), label = f'Modelled {v.split("_")[0]}')
        ax[i].legend()
        ax[i].grid(True)
        if split:
            for s in split:
                ax[i].axvline(s, color='black', ls='--')
        if fire_split:
            for f in fire_split:
                ax[i].axvline(f, color='red', ls='--')
    
    if directory_plot_output:
        plt.savefig(fname = directory_plot_output)
        plt.close()



def perform_permutation(val_set, FEATURES, TARGET, n_repeats, random_state, model):
    
    perm_importance = permutation_importance(model, val_set[FEATURES], val_set[TARGET],
                                              n_repeats=n_repeats, random_state = random_state, scoring = 'neg_mean_squared_error')
    arr_importances = np.array([list(perm_importance['importances_mean']), list(perm_importance['importances_std'])]).T
    perm_importance_df_2 = pd.DataFrame(arr_importances, columns = ['importances_mean', 'importances_std'], index = FEATURES)
    perm_importance_df_2.sort_values('importances_mean', ascending = True, inplace = True)
    
    return perm_importance_df_2

def perform_feature_removal(training_set, val_set, FEATURES, TARGET, n_repeats, random_state, model):
    
    results = dict()
    
    for i in range(len(FEATURES) - 3):
        
        if i == 0:
            current_features = [i for i in FEATURES] # do a deep copy
            
        n_features = len(current_features)    
        #print(f'Fit {n_features} with {current_features}')
        
        # Fit the Model 
        model.fit(training_set[current_features], training_set[TARGET])

        # Get the scores 
        train_score = root_mean_squared_error(model.predict(training_set[current_features]), training_set[TARGET])
        test_score = root_mean_squared_error(model.predict(val_set[current_features]), val_set[TARGET])
        #print(train_score)
        #print(test_score)
        
         # Perform permutation importance 
        perm_results = perform_permutation(val_set, current_features, TARGET, n_repeats, random_state, model)
        least_important = perm_results.index[0]
        #print(f'Least Important: {least_important}')

        # Record results                                   
        results[f'{n_features} Features'] = [train_score, test_score, least_important, n_features]
        
        # Remove the least important feature

        del current_features[current_features.index(least_important)] 
    
    # Do a final fit for a final score 
    #print('Feature selected, final fit for final score:')
    n_features = len(current_features)    
    #print(f'Fit {n_features} with {current_features}')
    model.fit(training_set[current_features], training_set[TARGET])
    train_score = root_mean_squared_error(model.predict(training_set[current_features]), training_set[TARGET])
    test_score = root_mean_squared_error(model.predict(val_set[current_features]), val_set[TARGET])
    results[f'{n_features} Features'] = [train_score, test_score, 'NAN', n_features]
        
    return [current_features, pd.DataFrame(results).T]
    
def perform_feature_selection(training_set, val_set, FEATURES, TARGET, n_repeats, random_state, model):
    
    # Select the three most important features 
    selected_features, perm_importance_data = perform_feature_removal(training_set, val_set, FEATURES, TARGET, n_repeats, random_state, model)
    
    # Get the baseline error
    model.fit(training_set[selected_features], training_set[TARGET])
    baseline_score = root_mean_squared_error(model.predict(val_set[selected_features]), val_set[TARGET])
    #print(f'Baseline score: {baseline_score}')
    
    #print(selected_features)
    # Now include one by one the excluded features
    #print(FEATURES)
    excluded_features = list(set(FEATURES).difference(selected_features))
    #print(excluded_features)
    #print(f'Now adding one by one {excluded_features}')
    
    combination_scores = dict()
    
    for i in range(len(FEATURES) - len(selected_features)):
        
        tested_features = excluded_features
        #print(f'Features to test: {tested_features}')
        scores = []
        train_scores = []
        
        combination_scores[f'{i}'] = [[], [], [], []]
        
        for f in tested_features:
            
            combination = selected_features + [f]
            model.fit(training_set[combination], training_set[TARGET])
            test_score = root_mean_squared_error(model.predict(val_set[combination]), val_set[TARGET])
            scores.append(test_score)
            
            train_score = root_mean_squared_error(model.predict(training_set[combination]), training_set[TARGET])
            train_scores.append(train_score)
            
            combination_scores[f'{i}'][0].append(f)
            combination_scores[f'{i}'][1].append(train_score)
            combination_scores[f'{i}'][2].append(test_score)
            combination_scores[f'{i}'][3].append(baseline_score)
        
        index_max = np.argmin(scores) # index that maximises the score (or minimises the loss)
        #print(np.argmin(scores))
        #print(scores)
        
        # Now check if the highest scoring combination is better than the combination without any of these variables:
        if scores[index_max] < baseline_score:
            feature_to_include = tested_features[index_max]
            selected_features = selected_features + [feature_to_include]
            baseline_score = scores[index_max] # make the new baseline_score the max score 
            print(f'Most useful feature: {feature_to_include}')
            print(f'New baseline: {baseline_score}' )
            
            # Remove the feature from the tested feature list
            del tested_features[tested_features.index(feature_to_include)]
        else:
            break
    
    print(f'Final features: {selected_features}')
    return [perm_importance_data, selected_features, combination_scores]
    
    
#%% Main


# Results_dir

results_dir = 'D:/Krish_New/Dynamic_Vegetation_Project_Storage/Random_Forest_Results_On_Super_Group_Results'
directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/MODELLED_TRAINING_DATA'

# Parameters

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

FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + VPD_FEATURES + FIRE_FEATURES + CO2_FEATURES + TEMP_FEATURES + SOIL_FEATURES + TOPOGRAPHIC_FEATURES # final features 
TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
scores = dict()

# Start time

start_time = time.time()

# Here, we want to estimate the generalisation error using KFold Validation 


super_group_list = ['Desert Chenopod', 'Desert Forb', 'Desert Hummock.grass',
       'Desert Shrub', 'Desert Tree.Palm', 'Desert Tussock.grass',
       'Temp/Med Shrub', 'Temp/Med Tree.Palm', 'Temp/Med Tussock.grass',
       'Tropical/Savanna Tree.Palm', 'Tropical/Savanna Tussock.grass']
max_counter = len(super_group_list)

random_state = 20240808

targeted_group = ['Tropical/Savanna Tussock.grass']


for counter, s in enumerate(targeted_group):
    
    print(f'Running model on {s} sites ({counter + 1}/{max_counter})')
    super_group_folder_name = '_'.join(s.split('/')) 
    
    directory_val = f'{directory}/{super_group_folder_name}/Validation' 
    number_of_folds = len(os.listdir(directory_val)) # note; this assumes that each folder in this directory is a K-Fold
    
    for i in range(2, number_of_folds):
        folder_num = i+1
        fold_time = time.time()
        print(f'Running model on {s}, K-fold ({folder_num}/{number_of_folds}, duration: {fold_time - start_time})')
        
        # Read the associated training fold, preprocess the data -> make time a time object, subset by desired features
        training_fold = pd.read_csv(f'{directory_val}/KFolds_{folder_num}/{super_group_folder_name}_training_fold_{folder_num}.csv', parse_dates = ['time']).copy()
        training_fold = training_fold.dropna(subset = FEATURES).sort_values('time').set_index('time')
        print(f"Included out {np.unique(training_fold['site_location_name'].values)}")
        
        validation_fold = pd.read_csv(f'{directory_val}/KFolds_{folder_num}/{super_group_folder_name}_validation_fold_{folder_num}.csv', parse_dates = ['time'])
        validation_fold = validation_fold.dropna(subset = FEATURES).sort_values('time').set_index('time')
        print(f"Left out {np.unique(validation_fold['site_location_name'].values)}")
        
        print(f'Running model on {s}, time-series cv ({folder_num}/{number_of_folds})')
        # Now for each site-level time series, we split them into 10 time blocks 
        training_fold['index_reference'] = [i for i in range(len(training_fold))] # get index numbers 
        
        sites_list = np.unique(training_fold['site_location_name'])
        site_training_tscv_data = dict()
        
        #ts_splits = 5
        # loop through sites 
        #for site in sites_list:
        #    ts = TimeSeriesSplit(n_splits = ts_splits) # split ts
        #    site_level_data = training_fold.loc[training_fold['site_location_name'] == site]
        #    for i, (train_index, test_index) in enumerate(ts.split(site_level_data)): # get index of each split 
        #        
        #        # Record the train/test indices in a dictionary 
        #        if i in site_training_tscv_data.keys():
        #            # Update the dictionary
        #            site_training_tscv_data[i][0] += list(site_level_data.iloc[train_index]['index_reference'].values)
        #            site_training_tscv_data[i][1] += list(site_level_data.iloc[test_index]['index_reference'].values)
        #        else: # Insert new key (for the first iteration)
        #            site_training_tscv_data[i] = [list(site_level_data.iloc[train_index]['index_reference'].values),
        #                                         list(site_level_data.iloc[test_index]['index_reference'].values)]
                    
        # Now we want the recorded indices in the form:
        # ((train1, test1), (train2, test,2), ..., (train n, test n)), for us n = 10 
        # In order to use randomised cv 
        #cv_splits = []
        #for i in range(ts_splits):
        #    cv_splits.append(tuple(site_training_tscv_data[i]))
        #cv_splits = tuple(cv_splits)

        #n_features = 33
        #print(f'Selecting {n_features} features')
        
        #if is_using_xgb:
        #    print('Using XGB boost')
        #    param_grid = {
        #        #'max_depth': Integer(50, 100),
        #        'n_estimators': Integer(400, 1000),
        #        'booster': Categorical(['gbtree']),
        #        'grow_policy': Categorical(['depthwise', 'lossguide']),
        #        'multi_strategy': Categorical(['one_output_per_tree', 'multi_output_tree'])
        #    }
        #    
        #    rf_reg = XGBRegressor(n_jobs = 8, random_state = random_state)
        #    rf_reg = RFECV(rf_reg, step=1, verbose = 2, cv = cv_splits)
        #    rf_reg.fit(X = training_fold[FEATURES], y= training_fold[TARGET])
        #    new_features = np.array(FEATURES)[rf_reg.support_]
        #    rf_reg = XGBRegressor(n_jobs = 8, random_state = random_state)
        #    random_grid = BayesSearchCV(rf_reg, param_grid, random_state = random_state, n_iter = 3,
        #                                 cv = cv_splits, verbose = 3, scoring = 'neg_root_mean_squared_error')
            

        
        print('Using RF')

        rf_reg = RandomForestRegressor(n_jobs = 8, random_state= random_state, n_estimators = 100)
        selected_features = perform_feature_selection(training_fold, validation_fold, 
                                                      FEATURES, TARGET, n_repeats = 10,
                                                      random_state = random_state, model = rf_reg)
        
             
        # Extract the train + test results 
        test_1 = pd.DataFrame([selected_features[2]['0'][0], 
                       selected_features[2]['0'][1], 
                       selected_features[2]['0'][2],
                       selected_features[2]['0'][3]]).T

        test_1 = test_1.set_index(0)
        test_1 = test_1.rename(columns = {1: f'{0}_train', 2: f'{0}_test', 3: f'{0}_baseline'})
        
        for key in selected_features[2].keys():
            
            if key == '0':
                continue 
        
            test_2 = pd.DataFrame([selected_features[2][key][0], 
                                   selected_features[2][key][1], 
                                   selected_features[2][key][2],
                                   selected_features[2][key][3]]).T
            test_2 = test_2.set_index(0)
            test_2 = test_2.rename(columns = {1: f'{key}_train', 2: f'{key}_test', 3: f'{key}_baseline'})
            test_1 = test_1.merge(test_2, left_index = True, right_index = True, how = 'left', suffixes = [key, key + 'i'])
            
        # Get the permutation data 
        permutation_data = selected_features[0]
        
        # get the new feature 
        new_features = selected_features[1]
        rf_reg = RandomForestRegressor(n_jobs = 8, random_state= random_state, n_estimators = 100) 
        rf_reg.fit(training_fold[new_features], training_fold[TARGET])
        
        # Now get test the model on unseen data - the validation set 
        y_pred_val = rf_reg.predict(validation_fold[new_features])
        y_pred_train = rf_reg.predict(training_fold[new_features]) # test on the training data to check to see if the model is learning on the training set sufficiently
        
        training_row = mean_squared_error(y_pred_train, training_fold[TARGET], multioutput = 'raw_values')
        validation_row = mean_squared_error(y_pred_val, validation_fold[TARGET], multioutput = 'raw_values')
        
        # Calculate RMSE scores
        training_row = np.round(np.sqrt(training_row),3)
        training_row_mean = np.round(np.mean(training_row),3)
        
        validation_row = np.round(np.sqrt(validation_row),3)
        validation_row_mean = np.round(np.mean(validation_row),3)
        
        # Basically collapse the arrays into one list
        combined_data = [i for i in training_row] + [ i for i in validation_row] + [training_row_mean, validation_row_mean]
        column_names = [f'train_{i}' for i in TARGET] + [f'val_{i}' for i in TARGET] + ['train_mean', 'val_mean']
        
        # Append the results into a dictionary
        scores[f'fold_{folder_num}'] = combined_data
        # Save the predictions for each fold for exploratory purposes 
        pred_names = [f'pred_{i}' for i in TARGET]
        
        if os.path.exists(f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}') == False:
            os.makedirs(f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}')
            
        # Save the data into a directory 
        training_fold[pred_names] = y_pred_train
        training_fold[pred_names].to_csv(f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}/Train_Predictions.csv')
        
        validation_fold[pred_names] = y_pred_val
        validation_fold[pred_names].to_csv(f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}/Validation_Predictions.csv')
        
        permutation_data.to_csv(f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}/Permutation.csv')
        test_1.to_csv(f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}/Feature_Selection.csv')

        dump(rf_reg, f'{results_dir}/{super_group_folder_name}/Results/KFold_{folder_num}/Random_Forest.joblib', compress=True)
    # Now convert the dictionary to a dataframe to save as a csv file 
    scores_df = pd.DataFrame.from_dict(scores, columns = column_names, orient = 'index')
    scores_df.to_csv(f'{results_dir}/{super_group_folder_name}/Results/KFold_results_scores.csv')
    scores_df.describe().to_csv(f'{results_dir}/{super_group_folder_name}/Results/KFold_results_described.csv')
    
    
    
    group_time = time.time()
    print(f'Ending model on {s}, duration: {group_time - start_time})')

#%% Plot some results 

target_group = ['']


sites_unique = np.unique(validation_fold['site_location_name'])

for i in sites_unique:
    plotPredictions(validation_fold[validation_fold['site_location_name'] == i], TARGET = TARGET, msg = f'{i}')



#random_state = 1
#n_repeats = 50
#perm_importance = permutation_importance(rf_reg, validation_fold[FEATURES], validation_fold[TARGET],
#                                          n_repeats=n_repeats, random_state = random_state, n_jobs = 8, scoring = 'neg_mean_squared_error')
#perm_sorted_idx = perm_importance.importances_mean.argsort()
#perm_importance_df = pd.DataFrame(perm_importance.importances[perm_sorted_idx].T, columns = validation_fold[FEATURES].columns[perm_sorted_idx])

#arr_importances = np.array([list(perm_importance['importances_mean']), list(perm_importance['importances_std'])]).T
#perm_importance_df_2 = pd.DataFrame(arr_importances, columns = ['importances_mean', 'importances_std'], index = FEATURES)
#perm_importance_df_2.sort_values('importances_mean', ascending = True, inplace = True)

#fig, ax = plt.subplots(1)

#perm_importance_df_2['importances_mean'].plot.barh(figsize = (15, 10), ax = ax)
#xticks = [i for i in range(0, round(max(perm_importance_df_2['importances_mean']) + 10), 10)]
#ax.set_xticks(xticks)
#plt.grid(True)
#plt.xlabel('Mean Gain in MSE')


