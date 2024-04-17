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
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline
from PreprocessData import * # import from custom transformers
from sklearn.model_selection import GridSearchCV
from sklearn.inspection import PartialDependenceDisplay
from sklearn.inspection import permutation_importance
from skopt import BayesSearchCV

#%% Functions 

def plotPredictions(actual, prediction, TARGET, msg = '', split = ''):
    fig, ax = plt.subplots(nrows = 3, figsize = (15,10))
    fig.suptitle(msg, fontsize=30)
    for i,v in enumerate(TARGET):
        actual[v].plot(ax=ax[i], color = 'blue', alpha = 0.4, linestyle='dashed' )
        prediction[f'prediction_{v}'].plot(ax=ax[i], color = 'orange', ylim = (0,100))
        ax[i].legend()
        if split:
            ax[i].axvline(time_split, color='black', ls='--')
            
def antiOverFitterScorer(y_train_pred, y_train_act, y_val_pred, y_val_act):
    train_score = -mean_squared_error(y_train_pred, y_train_act)
    val_score = -mean_squared_error(y_val_pred, y_val_act)
    score = ((train_score + val_score)/2) * val_score/train_score
    return score
    


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

site_location_name = 'NSANAN0002'
site_merged = pd.read_csv(f'Input_DataSet_{site_location_name}.csv', parse_dates = ['time']).copy()
site_merged = site_merged.set_index('time')

#%% Model the dataset

SEASONAL_FEATURES = ['photoperiod', 'photoperiod_gradient', 'year']
PRECIP_FEATURES = ['precip_30', 'precip_90', 'precip_180', 'precip_365', 'precip_730', 'precip_1095', 'precip_1460']
TEMP_FEATURES = ['tmax_lag', 'tmax_7', 'tmax_14', 'tmax_30', 'tmin_lag', 'tmin_7', 'tmin_14', 'tmin_30']
VPD_FEATURES = ['VPD_lag','VPD_7', 'VPD_14', 'VPD_30']
LAG_FEATURES = ['pv_lag', 'npv_lag', 'bs_lag']
LAGGED_CHANGE_FEATURES = ['pv_change', 'npv_change', 'bs_change']
FIRE_FEATURES = ['days_since_fire', 'fire_severity']


FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES# final features 
TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
site_merged = site_merged.dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
scores = []

splits = 10
tss = TimeSeriesSplit(n_splits= splits)

#%% Create Train/test set 
time_split = '2015-12-01' # This aprox splits the dataset from 80/20
train = site_merged.iloc[site_merged.index <= time_split]
test = site_merged.iloc[site_merged.index > time_split]
random_state = 20240228


#%% Show CV splits 
fig, axs = plt.subplots(splits, 1, figsize=(15, 15), sharex=True)

fold = 0
for train_idx, val_idx in tss.split(train): # split train into k-folds 
    train_f = train.iloc[train_idx]
    val_f = train.iloc[val_idx]
    train_f['pv_filter'].plot(ax=axs[fold],
                          label='Training Set',
                          title=f'Data Train/Test Split Fold {fold}', ylim = (0,100))
    val_f['pv_filter'].plot(ax=axs[fold],
                         label='Test Set', ylim = (0,100))
    axs[fold].axvline(val_f.index.min(), color='black', ls='--')
    fold += 1
plt.show()

#%% Run The model 

main_scorer = 'neg_mean_squared_error'

#%% Checking the CV calculations 
scores_train = [] 
scores_test = [] 
n_estimators = range(25,325, 25)

i = 0
for train_idx, val_idx in tss.split(train):
    
    train_f = train.iloc[train_idx]
    val_f = train.iloc[val_idx]
    
    scores_train_temp = []
    scores_test_temp = []
    print(f'On split {i + 1}')
    for trees in n_estimators:
    
        reg= RandomForestRegressor(random_state = random_state, n_estimators= trees)
        
        reg.fit(train_f[FEATURES], train_f[TARGET])

        scores_train_temp.append(mean_squared_error(reg.predict(train_f[FEATURES]), train_f[TARGET]))
        scores_test_temp.append(mean_squared_error(reg.predict(val_f[FEATURES]), val_f[TARGET]))
    
    scores_train.append(np.mean(scores_train_temp))
    scores_test.append(np.mean(scores_test_temp))

# print(np.mean(np.array(scores_train)))
# print(np.mean(np.array(scores_test)))

fig, ax = plt.subplots(2,1,figsize = (15,5))

ax[0].plot([i for i in n_estimators],scores_train)
ax[1].plot(n_estimators,scores_test)
