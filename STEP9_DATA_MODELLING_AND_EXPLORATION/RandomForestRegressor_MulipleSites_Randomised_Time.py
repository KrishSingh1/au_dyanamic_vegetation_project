# -*- coding: utf-8 -*-
"""
Created on Thu Apr  4 17:58:38 2024

@author: krish
"""

import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from sklearn.model_selection import TimeSeriesSplit
from sklearn.model_selection import cross_validate
import graphviz 

import sys
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')

from sklearn.ensemble import RandomForestRegressor
import random
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline
from PreprocessData import * # import from custom transformers
from sklearn.model_selection import GridSearchCV
from sklearn.inspection import PartialDependenceDisplay
from sklearn.inspection import permutation_importance
from skopt import BayesSearchCV
from sklearn.model_selection import KFold
from sklearn.model_selection import GroupKFold


#%% Functions 

def plotPredictions(actual, prediction, TARGET, msg = '', split = ''):
    fig, ax = plt.subplots(nrows = 3, figsize = (15,10))
    fig.suptitle(msg, fontsize=30)
    for i,v in enumerate(TARGET):
        actual[v].plot(ax=ax[i], color = 'blue', alpha = 0.4, linestyle='dashed' )
        prediction[f'prediction_{v}'].plot(ax=ax[i], color = 'orange', ylim = (0,100))
        ax[i].legend()
        if split:
            for s in split:
                ax[i].axvline(s, color='black', ls='--')

#%% Main 

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

# sites_list = ['WAAPIL0003', 'NSABHC0023', 'TCATCH0006',
#                 'WAAGAS0002', 'NSAMDD0014', 'NTAGFU0021', 
#                 'NSANSS0001', 'SATSTP0005', 'QDASSD0015', 
#                 'NTAFIN0002', 'NSANAN0002', 'QDAEIU0010'] # smalller subset 

# sites_list = np.unique(['NSABBS0001',
#   'NSABHC0011',
#   'NSACOP0001',
#   'NSAMDD0001',
#   'NSAMDD0011',
#   'NSAMDD0020',
#   'NSAMDD0028',
#   'NSAMUL0003',
#   'NSANAN0001',
#   'NSANAN0002',
#   'NSANSS0002',
#   'NSTSYB0003',
#   'NSTSYB0005',
#   'NSTSYB0006',
#   'NSABHC0023',
#   'NSAMDD0014',
#   'NSANSS0001',
#   'NSANAN0002'])# bigger subset - NSW

# sites_list = np.unique(['NTADAC0001',
#   'NTADMR0001',
#   'NTAFIN0003',
#   'NTAFIN0006',
#   'NTAFIN0015',
#   'NTAFIN0018',
#   'NTAGFU0014',
#   'NTAGFU0020',
#   'NTAGFU0030',
#   'NTAGFU0034',
#   'NTASTU0004',
#   'NTTDMR0003',
#   'NTAGFU0021',
#   'NTAFIN0002']) # NT

sites_list = np.unique(['QDABBN0002',
  'QDABBS0002',
  'QDABBS0010',
  'QDACHC0003',
  'QDACYP0006',
  'QDACYP0018',
  'QDACYP0020',
  'QDACYP0022',
  'QDAEIU0005',
  'QDAGUP0006',
  'QDAGUP0009',
  'QDAGUP0019',
  'QDAGUP0021',
  'QDAMGD0002',
  'QDAMGD0023',
  'QDAMGD0024',
  'QDAMGD0025',
  'QDAMUL0002',
  'QDAMUL0003',
  'QDASEQ0004',
  'QDASSD0015', 
  'QDAEIU0010']) # QD 

# sites_list = np.unique(['WAAAVW0006',
#   'WAACAR0002',
#   'WAACAR0004',
#   'WAACOO0007',
#   'WAACOO0016',
#   'WAACOO0024',
#   'WAACOO0026',
#   'WAACOO0027',
#   'WAACOO0029',
#   'WAACOO0030',
#   'WAAGAS0001',
#   'WAAGES0001',
#   'WAALSD0002',
#   'WAANOK0006',
#   'WAANUL0003',
#   'WAAPIL0010',
#   'WAAPIL0023',
#   'WAAPIL0024',
#   'WAAPIL0031',
#   'WAAPIL0003',
#   'WAAGAS0002'])


# sites_list = np.unique(['SAAEYB0021',
#   'SAAEYB0028',
#   'SAAEYB0029',
#   'SAAFLB0003',
#   'SAAFLB0005',
#   'SAAFLB0008',
#   'SAAGAW0008',
#   'SAAKAN0009',
#   'SAASTP0023',
#   'SAASTP0033',
#   'SAASTP0034',
#   'SASMDD0005',
#   'SASMDD0009',
#   'SASMDD0014',
#   'SATFLB0003',
#   'SATFLB0019',
#   'SATFLB0020',
#   'SATFLB0022',
#   'SATFLB0023',
#   'SATSTP0005'])
# Remove SAAEYB0001 as it does not have Soil data

# sites_list = np.unique(['TCATCH0004', 'TCATNM0001', 'TCATNM0003', 'TCATCH0006'])

# Development (after 2015 set)
after_2015_sites = ['WAAPIL0003', 'NSABHC0023', 'TCATCH0006',
                 'WAAGAS0002', 'NSAMDD0014', 'NTAGFU0021', 
                 'NSANSS0001', 'SATSTP0005', 'QDASSD0015', 
                 'NTAFIN0002', 'NSANAN0002', 'QDAEIU0010'] # smalller subset 



#%% Model the dataset

SEASONAL_FEATURES = ['photoperiod', 'photoperiod_gradient']

PRECIP_FEATURES = ['precip_30', 'precip_90', 'precip_180', 
                   'precip_365', 'precip_730', 'precip_1095', 
                   'precip_1460', 'MAP']

TEMP_FEATURES = ['tmax_lag', 'tmax_7', 'tmax_14', 
                 'tmax_30', 'tmin_lag', 'tmin_7', 
                 'tmin_14', 'tmin_30', 'MAT']

VPD_FEATURES = ['VPD_lag','VPD_7', 'VPD_14',
                'VPD_30']

LAG_FEATURES = ['pv_lag', 'npv_lag', 'bs_lag']

LAGGED_CHANGE_FEATURES = ['pv_change', 'npv_change', 'bs_change']

FIRE_FEATURES = ['days_since_fire', 'fire_severity']

CO2_FEATURES = ['CO2']

VEGETATION_FEATURES = ['Bryophyte', 'Chenopod', 'Cycad', 'Epiphyte',
                         'Fern', 'Forb', 'Fungus', 'Grass.tree', 'Heath.shrub', 
                         'Hummock.grass', 'Rush', 'Sedge', 'Shrub', 'Shrub.Mallee', 
                         'Tree.fern','Tree.Mallee', 'Tree.Palm', 'Tussock.grass', 'Vine']

SOIL_FEATURES = ['CLY_000_005', 'CLY_005_015', 'CLY_015_030', 'CLY_030_060', 'CLY_060_100',
                            'DER_000_999', 'NTO_000_005', 'NTO_005_015', 'NTO_015_030', 'NTO_030_060',
                            'NTO_060_100', 'PTO_000_005', 'PTO_005_015', 'PTO_015_030', 'PTO_030_060',
                            'PTO_060_100', 'SLT_000_005', 'SLT_005_015', 'SLT_030_060', 'SLT_060_100',
                            'pHc_000_005', 'pHc_005_015', 'pHc_015_030', 'pHc_030_060', 'pHc_060_100'] # the soil attributes to include

FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES + CO2_FEATURES + VEGETATION_FEATURES + SOIL_FEATURES # final features 
TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
scores = []
#%% Create Train/test set 


# Training and test set 
datasets =  {} # entire set - for final evaluation 
training_set = {} # training set 
test_set = {} # test set 

# 7 years is roughly 20% of the dataset (of n rows)
# i.e. 161 data points 
# A solution is take a random number of the lower bounds (l), such that l >= 0 and l < n - 161
# The upper bounds (u) is simply u = l + 161 

# Iterate through the site list 


random.seed(20240514)

choices = [0, 161, 322, 483, 644] # approx 20% splits 
number_of_choices = len(sites_list)/len(choices)

after_2015_test = True
after_2015_list = list(set(sites_list).intersection(after_2015_sites)) # the sites where I want to predict 2015-2022

duplicator = [round(np.floor(number_of_choices)) for i in range(len(choices))]
print(duplicator)
# If there are specific sites I want as having 2015 >, I need to force some order 
if after_2015_test:
    if duplicator[-1] < len(after_2015_list):
        duplicator[-1] = len(after_2015_list)
    #number_of_choices_adj = (len(sites_list) - len(after_2015_list))/(len(choices) - 1)
    #for i in range(len(choices) - 1):
     #   duplicator[i] = round(np.floor(number_of_choices_adj))

print(duplicator)
already_chosen = []
print(number_of_choices)
while sum(duplicator) != len(sites_list): # if there is an uneven split, keep adding 1 more until it sums to the avaliable number of datasets 
    chosen_index = random.randrange(0,len(duplicator),1)
    if chosen_index not in already_chosen:
        duplicator[chosen_index] += 1
        already_chosen.append(chosen_index)
    #number_of_choices = len(sites_list)/sum(duplicator)

print(duplicator)

choice_adj = []
for index ,i in enumerate(choices):
    for j in range(duplicator[index]):
        choice_adj.append(i)
        
print(choice_adj)
random.shuffle(choice_adj)
print(choice_adj)

if after_2015_test:
    after_2015_indices = [np.where(sites_list == i)[0][0] for i in after_2015_list]
    for i in range(len(after_2015_list)):
        if choice_adj[after_2015_indices[i]] != choices[-1]:
            print(choice_adj) # swap with an element that is 644 and NOT an index of one the after 2015 sites
            index_choices = set(np.where(np.array(choice_adj)== 644)[0]).difference(after_2015_indices)
            swap_index = random.choice(list(index_choices))
            choice_adj[after_2015_indices[i]], choice_adj[swap_index] =  choice_adj[swap_index], choice_adj[after_2015_indices[i]]


period = 161 # approx. 20% 
for i, site_location_name in enumerate(sites_list):
    site_merged = pd.read_csv(f'Input_DataSet_{site_location_name}.csv', parse_dates = ['time']).copy().dropna(subset = FEATURES) # read and drop na
    site_merged.sort_values('time', inplace = True)
    site_merged.reset_index(inplace = True)

    
    lower_bound = choice_adj[i]
    if lower_bound == choices[-1]: # if its the last 20%, simply take all time points from there up to the most recent time point
        upper_bound = len(site_merged) -1
    else:
        upper_bound = lower_bound + period 
        
    print(f'{site_location_name} : {(site_merged.time[lower_bound], site_merged.time[upper_bound])}')
    
    #print((lower_bound, upper_bound))
    # get test set 
    test = site_merged[(site_merged.index >= lower_bound) & (site_merged.index <= upper_bound)]
    # get train set (note the selection condition is logically opposite to selection condition of the test set )
    train = site_merged[(site_merged.index < lower_bound) | (site_merged.index > upper_bound)]
    
    datasets[site_location_name] = site_merged
    training_set[site_location_name] = train
    test_set[site_location_name] = test
    
training_merged = pd.concat(training_set).dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
training_merged.sort_values('time', inplace = True)
training_merged.set_index('time', inplace = True)

test_merged = pd.concat(test_set).dropna(subset = FEATURES)
test_merged.sort_values('time', inplace = True)
test_merged.set_index('time', inplace = True)


#%% Run The model 

random_state = 20240228
main_scorer = 'neg_mean_squared_error'
# Possible scorers:
    #  neg_mean_absolute_percentage_error'
    #  mean_squared_log_error
    # See more below:
    # https://scikit-learn.org/stable/modules/model_evaluation.html#regression-metrics
    

## Test with Default RF 
reg = RandomForestRegressor(n_estimators = 100, random_state = random_state, n_jobs = 7)

# default_RF_CV = pd.DataFrame(cross_validate(reg, X = training_merged[FEATURES],
#                      y = training_merged[TARGET], cv=cv_splits, 
#                      scoring=np.unique(['neg_mean_squared_error', main_scorer]).tolist(),
#                      return_train_score = True))
# print(f'Default\nTrain R2 {default_RF_CV["train_" + "neg_mean_squared_error"].mean()}\nTest R2 {default_RF_CV["test_" + "neg_mean_squared_error"].mean()}')


#%% Fit the Models

# Default Model 

reg.fit(X = training_merged[FEATURES], y = training_merged[TARGET])


for site in sites_list:
    fig, ax = plt.subplots(2)

    site_data = datasets[site].set_index('time').dropna(subset = FEATURES)
    
    train_site = site_data.iloc[training_set[site].index]
    train_site['pv_filter'].plot(figsize = (15,5), ax = ax[0])
    
    test_site = site_data.iloc[test_set[site].index]
    test_site['pv_filter'].plot(figsize = (15,5), ax = ax[1])
    
    print(f'{test_site.index.min()}')
    
    y_pred = reg.predict(site_data[FEATURES])
    TARGET_names = ['prediction_' + i for i in TARGET]
    df = pd.DataFrame(y_pred, columns = TARGET_names)
    df.index = site_data[FEATURES].index
    
    print(test[TARGET])
    print(df.iloc[training_set[site].index])
    
    train_score = mean_squared_error(train_site[TARGET], df.iloc[training_set[site].index])
    test_score = mean_squared_error(test_site[TARGET],  df.iloc[test_set[site].index])
    print(train_score)
    print(test_score)
    #test_score = mean_squared_error(site_data[site_data.index > time_split][TARGET], df[site_data.index > time_split])
    #plotPredictions(site_data, df, TARGET, msg = f'{site} RF . MSE Scores: train:{train_score:.2f}, test:{test_score:.2f}')
    plotPredictions(site_data, df, TARGET, msg = f'{site} MSE Scores: train:{train_score:.2f}, test:{test_score:.2f}', split = [f'{test_site.index.min()},', f'{test_site.index.max()}'])



# Fine-Tuned Model
# reg = RandomForestRegressor(**grid.best_params_)
# reg.fit(train[FEATURES], train[TARGET])
# y_pred = reg.predict(site_merged[FEATURES])
# TARGET_names = [ 'prediction_' + i for i in TARGET]
# df = pd.DataFrame(y_pred, columns = TARGET_names)
# df.index = site_merged[FEATURES].index
# plotPredictions(site_merged,df,TARGET, split = time_split)


#%% Examine Importances 

# Get importance 
# https://scikit-learn.org/stable/modules/generated/sklearn.inspection.permutation_importance.html#sklearn.inspection.permutation_importance
# https://scikit-learn.org/stable/auto_examples/inspection/plot_permutation_importance_multicollinear.html#sphx-glr-auto-examples-inspection-plot-permutation-importance-multicollinear-py
# Comparion between plots code inspired by above links 

#reg = RandomForestRegressor(**grid.best_params_)
reg.fit(train[FEATURES], train[TARGET])

fig, ax = plt.subplots(1,2)
gini_importance = pd.DataFrame(reg.feature_importances_.T, index = FEATURES, columns = ['Gini_importance'])
gini_importance.sort_values(by = 'Gini_importance', inplace = True, ascending = True)
gini_importance.plot.barh(figsize = (15,5), ax = ax[0])

n_repeats = 10
perm_importance = permutation_importance(reg, train[FEATURES], train[TARGET], n_repeats=n_repeats, random_state = random_state)
perm_sorted_idx = perm_importance.importances_mean.argsort()

perm_importance_df = pd.DataFrame(perm_importance.importances[perm_sorted_idx].T, columns = train[FEATURES].columns[perm_sorted_idx])
perm_importance_df.boxplot(vert = False, figsize = (15,10), ax = ax[1])
fig.tight_layout()

