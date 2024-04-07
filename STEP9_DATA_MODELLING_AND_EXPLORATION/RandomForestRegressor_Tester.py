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
import graphviz 

import sys
sys.path.append('/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/STEP9_DATA_MODELLING_AND_EXPLORATION')

from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline
from PreprocessData import * # import from custom transformers
from sklearn.model_selection import GridSearchCV
from sklearn.inspection import PartialDependenceDisplay

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


#%% Main 

# NSAMDD0002
site_location_name = 'NSANAN0002'
site_merged = pd.read_csv(f'Input_DataSet_{site_location_name}.csv', parse_dates = ['time']).copy()
site_merged = site_merged.set_index('time')

#%% Model the dataset

SEASONAL_FEATURES = ['photoperiod', 'photoperiod_gradient', 'year']
PRECIP_FEATURES = ['precip_30', 'precip_90', 'precip_180', 'precip_365', 'precip_730', 'precip_1095', 'precip_1460']
TEMP_FEATURES = ['tmax_lag', 'tmax_7', 'tmax_14', 'tmax_30', 'tmean_lag', 'tmean_7', 'tmean_14', 'tmean_30']
VPD_FEATURES = ['VPD_lag','VPD_7', 'VPD_14', 'VPD_30']
LAG_FEATURES = ['pv_lag', 'npv_lag', 'bs_lag']
LAGGED_CHANGE_FEATURES = ['pv_change', 'npv_change', 'bs_change']
FIRE_FEATURES = ['mean_pv_drop_after_fire', 'days_since_fire']


FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES# final features 

TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
site_merged = site_merged.dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
scores = []

splits = 5
tss = TimeSeriesSplit(n_splits= splits)

#%% Create Train/test set 
time_split = '2015-12-01' # This aprox splits the dataset from 80/20
train = site_merged.iloc[site_merged.index <= time_split]
test = site_merged.iloc[site_merged.index > time_split]
random_state = 20240228


#%% Show CV splits 
fig, axs = plt.subplots(splits, 1, figsize=(15, 15), sharex=True,)

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

## Test with Default RF 

reg = RandomForestRegressor(random_state = random_state)
reg.fit(train[FEATURES], train[TARGET])
y_pred = reg.predict(site_merged[FEATURES])

TARGET_names = [ 'prediction_' + i for i in TARGET]
df = pd.DataFrame(y_pred, columns = TARGET_names)
df.index = site_merged[FEATURES].index
plotPredictions(site_merged,df,TARGET, split = time_split)



reg = RandomForestRegressor(random_state = random_state, max_depth = 5)
reg.fit(train[FEATURES], train[TARGET])
y_pred = reg.predict(site_merged[FEATURES])

TARGET_names = [ 'prediction_' + i for i in TARGET]
df = pd.DataFrame(y_pred, columns = TARGET_names)
df.index = site_merged[FEATURES].index
plotPredictions(site_merged,df,TARGET, split = time_split)



#Get 3 data points of pv before the fire (a)
#Get 3 data points of pv after the fire  (b)
#Get the mean a, and get the mean of b
#fire severity = a - b



    
# number_of_trees = range(50,500,10)
# scores = []
# for n in number_of_trees:
#     reg = RandomForestRegressor(n_estimators= n, random_state = random_state)
#     reg.fit(train[FEATURES], train[TARGET])
    
#     y_pred = reg.predict(test[FEATURES])
#     df = pd.DataFrame(y_pred, columns = TARGET_names)
#     df.index = test[FEATURES].index
    
#     score_temp = []
#     for col in range(3):
#         y_test_col = test[TARGET[col]]
#         y_pred_col = [i[col] for i in y_pred]
#         score = np.sqrt(mean_squared_error(y_pred_col, y_test_col))
#         score_temp.append(score)
#     scores.append(score_temp)
    
#     #plotPredictions(test,df,TARGET, msg = f'Number of Trees {n}')


# scores_df = pd.DataFrame(scores, columns = TARGET, index = number_of_trees)
# scores_df.plot()


