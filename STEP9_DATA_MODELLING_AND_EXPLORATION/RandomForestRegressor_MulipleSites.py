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


# sites_list = np.unique(['QDABBN0002',
#   'QDABBS0002',
#   'QDABBS0010',
#   'QDACHC0003',
#   'QDACYP0006',
#   'QDACYP0018',
#   'QDACYP0020',
#   'QDACYP0022',
#   'QDAEIU0005',
#   'QDAGUP0006',
#   'QDAGUP0009',
#   'QDAGUP0019',
#   'QDAGUP0021',
#   'QDAMGD0002',
#   'QDAMGD0023',
#   'QDAMGD0024',
#   'QDAMGD0025',
#   'QDAMUL0002',
#   'QDAMUL0003',
#   'QDASEQ0004',
#   'QDASSD0015', 
#   'QDAEIU0010']) # QD 



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


# sites_list = np.unique([
#   'SAAEYB0021',
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

datasets = {}

for site_location_name in sites_list:
    
    site_merged = pd.read_csv(f'Input_DataSet_{site_location_name}.csv', parse_dates = ['time']).copy()
    datasets[site_location_name] = site_merged
    print(site_merged)
    

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
                            'pHc_000_005', 'pHc_005_015', 'pHc_015_030', 'pHc_030_060', 'pHc_060_100'] 

FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES + CO2_FEATURES + VEGETATION_FEATURES + SOIL_FEATURES# final features 
TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
site_merged = pd.concat(datasets).dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
site_merged.sort_values('time', inplace = True)
site_merged.set_index('time', inplace = True)
scores = []
#%% Create Train/test set 
time_split = '2015-12-01' # This aprox splits the dataset from 80/20
train = site_merged.iloc[site_merged.index <= time_split]
test = site_merged.iloc[site_merged.index > time_split]
random_state = 20240228

n_splits = 5
total = len(train)//n_splits
train_size = np.floor(total*0.7).astype(int)
test_size = total - train_size

cv_splits = []

for n in range(n_splits):
    train_idx = np.array([(number + n*total) for number in range(1,train_size +1)])
    test_idx = np.array([(number + train_idx.max()) for number in range(1, test_size + 1)])
    cv_splits.append((train_idx, test_idx))

#%% Show CV splits 
fig, axs = plt.subplots(n_splits, 1, figsize=(15, 15), sharex=True)

fold = 0
for train_idx, val_idx in cv_splits: # split train into k-folds 
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
# Possible scorers:
    #  neg_mean_absolute_percentage_error'
    #  mean_squared_log_error
    # See more below:
    # https://scikit-learn.org/stable/modules/model_evaluation.html#regression-metrics
    

## Test with Default RF 
reg = RandomForestRegressor(n_estimators = 100, random_state = random_state, n_jobs = 7)

default_RF_CV = pd.DataFrame(cross_validate(reg, X = train[FEATURES],
                     y = train[TARGET], cv=cv_splits, 
                     scoring=np.unique(['neg_mean_squared_error', main_scorer]).tolist(),
                     return_train_score = True))
print(f'Default\nTrain R2 {default_RF_CV["train_" + "neg_mean_squared_error"].mean()}\nTest R2 {default_RF_CV["test_" + "neg_mean_squared_error"].mean()}')



#%%

# reg = RandomForestRegressor()
# # Perform optimisation based on given hyper params
# hyp_params = { 
#     'n_estimators': [100],
#     'max_depth': [10,20,30,40],
#     #'min_weight_fraction_leaf': np.linspace(0,0.5,5),
#     #'min_samples_split': [2, 10, 30],
#     'bootstrap': [True],
#     #'max_leaf_nodes': [10,20,30,100, None],
#     'max_samples': [0.4],
#     'max_features'     : ['sqrt', 'log2', None, 1.0],
#     'criterion': ['squared_error','friedman_mse', 'poisson', 'absolute_error'],
#     'random_state' : [random_state]
# }

# grid = GridSearchCV(estimator=reg,
#               param_grid = hyp_params,
#               cv = cv_splits, scoring = (main_scorer),
#               n_jobs = 7)

# grid.fit(X = train[FEATURES], y = train[TARGET])
# reg = grid.best_estimator_

#{'bootstrap': True, 'criterion': 'poisson', 'max_depth': 30, 'max_features': 'sqrt', 
#   'n_estimators': 100, 'random_state': 20240228}

# print(grid.best_params_)

# Tuned_RF_CV = pd.DataFrame(cross_validate(RandomForestRegressor(**grid.best_params_), X = train[FEATURES],
#                       y = train[TARGET], cv=cv_splits, 
#                       scoring=np.unique(['r2', main_scorer]).tolist(),
#                       return_train_score = True))

# print(f'Tuned\nTrain R2 {Tuned_RF_CV["train_" + main_scorer].mean()}\nTest R2 {Tuned_RF_CV["test_" + main_scorer].mean()}')


#%% Fit the Models

# Default Model 

reg.fit(X = train[FEATURES], y = train[TARGET])

for site in sites_list:
    print(site)
    site_data = datasets[site].set_index('time').dropna(subset = FEATURES)
    y_pred = reg.predict(site_data[FEATURES])
    TARGET_names = ['prediction_' + i for i in TARGET]
    df = pd.DataFrame(y_pred, columns = TARGET_names)
    df.index = site_data[FEATURES].index
    train_score = mean_squared_error(site_data[site_data.index <= time_split][TARGET], df[site_data.index <= time_split])
    test_score = mean_squared_error(site_data[site_data.index > time_split][TARGET], df[site_data.index > time_split])
    plotPredictions(site_data, df, TARGET, split = time_split, msg = f'{site} RF . MSE Scores: train:{train_score:.2f}, test:{test_score:.2f}')
    



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

