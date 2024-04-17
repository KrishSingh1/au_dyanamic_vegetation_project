# -*- coding: utf-8 -*-
"""
Created on Thu Feb 29 12:45:35 2024

@author: krish
"""

#%% Libaries

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
TEMP_FEATURES = ['tmax_lag', 'tmax_7', 'tmax_14', 'tmax_30', 'tmean_lag', 'tmean_7', 'tmean_14', 'tmean_30']
VPD_FEATURES = ['VPD_lag','VPD_7', 'VPD_14', 'VPD_30']
LAG_FEATURES = ['pv_lag', 'npv_lag', 'bs_lag']
LAGGED_CHANGE_FEATURES = ['pv_change', 'npv_change', 'bs_change']
FIRE_FEATURES = ['mean_pv_drop_after_fire', 'days_since_fire']


FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES# final features 

TARGET = ['pv_filter', 'bs_filter', 'npv_filter']
site_merged = site_merged.dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
scores = []

n_splits = 5
tss = TimeSeriesSplit(n_splits= n_splits)

#%%% Create Train/test set 
time_split = '2015-12-01'
train = site_merged.iloc[site_merged.index <= time_split]
test = site_merged.iloc[site_merged.index > time_split]


#%% Show CV splits 
fig, axs = plt.subplots(n_splits, 1, figsize=(15, 15), sharex=True)

fold = 0
for train_idx, val_idx in tss.split(train): # split train into k-folds 
    train_f = train.iloc[train_idx]
    val_f = train.iloc[val_idx]
    train_f['pv_filter'].plot(ax=axs[fold],
                          label='Training Set',
                          title=f'Data Train/Test Split Fold {fold}')
    val_f['pv_filter'].plot(ax=axs[fold],
                         label='Test Set')
    axs[fold].axvline(val_f.index.min(), color='black', ls='--')
    fold += 1
plt.show()

#%% Run Grid Search

random_state = 20240228

reg = RandomForestRegressor()

# Perform optimisation based on given hyper params
hyp_params = { 
    'n_estimators': [300],
    #'max_depth': [100],
    #'min_samples_split' : np.linspace(0.05,0.5,5),
    #'min_samples_leaf' : np.linspace(0.05,0.5,5),
    #'max_features'     : ['sqrt', 'log2', None] + list(np.linspace(0.1,1,5)),
    #'criterion': ['squared_error','friedman_mse', 'poisson'],
    'random_state' : [random_state]
}
grid = GridSearchCV(estimator=reg,
             param_grid = hyp_params,
             cv = tss, return_train_score = True,
             n_jobs = 7)

# Run fine-tuning on the grid 
grid.fit(X = train[FEATURES], y = train[TARGET])

#%% Run Cross Validation
## This will be commented for now 

#for train_idx, val_idx in tss.split(site_merged):
    
    # train = site_merged.iloc[train_idx]
    # test = site_merged.iloc[val_idx]

    # X_train = train[FEATURES]
    # y_train = train[TARGET]

    # X_test = test[FEATURES]
    # y_test = test[TARGET]
    
    # # multi_strategy
    # reg = grid.best_estimator_
    # reg.fit(X_train, y_train)
    
    # y_pred = reg.predict(X_test)
    
    # ## Append the scores for each of the fractions 
    # score_temp = []
    # for col in range(3):
    #     y_test_col = y_test[TARGET[col]]
    #     y_pred_col = [i[col] for i in y_pred]

    #     score = np.sqrt(mean_squared_error(y_pred_col, y_test_col))
    #     score_temp.append(score)
    # scores.append(score_temp)

#print(pd.DataFrame(scores, columns = TARGET))
#print(f"Mean RMSE of fractions: {np.mean(scores,axis = 0)}")
#print(f"Overall Mean RMSE of fractions: {np.mean(scores)}")

#%% Fit the Model 


#%% Evaluate Model (using a single example)


reg = grid.best_estimator_
reg.fit(train[FEATURES], train[TARGET])
TARGET_names = [ 'prediction_' + i for i in TARGET]
prediction_df = pd.DataFrame(reg.predict(site_merged[FEATURES]), columns = TARGET_names)
prediction_df.index = site_merged.index
plotPredictions(site_merged, prediction_df, TARGET, split = time_split)

# fig, ax = plt.subplots(nrows = 3, figsize = (15,10))

# site_merged_eval['pv_filter'].plot(ax=ax[0], color = 'blue', alpha = 0.4, linestyle='dashed' )
# site_merged_eval['prediction_pv_filter'].plot(ax=ax[0], color = 'orange')
# ax[0].legend()
# site_merged_eval['npv_filter'].plot(ax=ax[1], color = 'blue', alpha = 0.4, linestyle='dashed')
# site_merged_eval['prediction_npv_filter'].plot(ax=ax[1], color = 'orange')
# ax[1].legend()
# site_merged_eval['bs_filter'].plot(ax=ax[2], color = 'blue', alpha = 0.4,linestyle='dashed')
# site_merged_eval['prediction_bs_filter'].plot(ax=ax[2], color = 'orange')
# ax[2].legend()
# plt.show()

# print("-"*10 + "Current RMSE" + "-"*10)

# score_pv = np.sqrt(mean_squared_error(test['pv_filter'], test['prediction_pv_filter']))
# print(f'RMSE Score on Test set pv: {score_pv:0.2f}')

# score_npv = np.sqrt(mean_squared_error(test['npv_filter'], test['prediction_npv_filter']))
# print(f'RMSE Score on Test set npv: {score_npv:0.2f}')

# score_bs = np.sqrt(mean_squared_error(test['bs_filter'], test['prediction_bs_filter']))
# print(f'RMSE Score on Test set bs: {score_bs:0.2f}')

# score_avg = np.mean([score_pv,score_npv,score_bs])
# print(f'Average RMSE Score on Test set: {score_avg:0.2f}')

# print("-"*10 + "CV RMSE" + "-"*10)

# print(pd.DataFrame(scores, columns = TARGET))

# print(f"Mean RMSE of fractions: {np.mean(scores,axis = 0)}")

# print(f"Overall Mean RMSE of fractions: {np.mean(scores)}")


#%% Partial Dependence

#RandomForestRegressor(max_features=0.4, min_samples_leaf=0.05,
#                     min_samples_split=0.05, random_state=20240228)
#
#


#PartialDependenceDisplay.from_estimator(reg, site_merged[FEATURES], FEATURES, target = 'pv_filter')
#fig = plt.gcf()
#fig.tight_layout()
#fig.set_size_inches(10, 10)

#PartialDependenceDisplay.from_estimator(reg, site_merged[FEATURES], FEATURES, target = 'npv_filter')
#fig = plt.gcf()
#fig.tight_layout()
#fig.set_size_inches(10, 10)

#PartialDependenceDisplay.from_estimator(reg, site_merged[FEATURES], FEATURES, target = 'bs_filter')
#fig = plt.gcf()
#fig.tight_layout()
#fig.set_size_inches(10, 10)

# for f in FEATURES:
#     print(f)
#     PartialDependenceDisplay.from_estimator(reg, X = site_merged[FEATURES], features = [f], target = 'bs_filter',
#                                             kind = 'average')
#     fig = plt.gcf()
#     fig.tight_layout()
#     fig.set_size_inches(10, 10)

    


