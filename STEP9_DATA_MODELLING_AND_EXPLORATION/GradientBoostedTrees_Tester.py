# -*- coding: utf-8 -*-


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
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.multioutput import RegressorChain

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
# Possible scorers:
    # neg_mean_absolute_percentage_error'
    #  mean_squared_log_error
    # See more below:
    # https://scikit-learn.org/stable/modules/model_evaluation.html#regression-metrics
    

## Test with Default RF 
reg = RegressorChain(GradientBoostingRegressor(random_state = random_state))

default_RF_CV = pd.DataFrame(cross_validate(reg, X = train[FEATURES],
                      y = train[TARGET], cv=tss, 
                      scoring=('r2', main_scorer),
                      return_train_score = True))
print(f'Default\nTrain R2 {default_RF_CV["train_" + main_scorer].mean()}\nTest R2 {default_RF_CV["test_" + main_scorer].mean()}')


# Default Model 
reg.fit(train[FEATURES], train[TARGET])
y_pred = reg.predict(site_merged[FEATURES])
TARGET_names = [ 'prediction_' + i for i in TARGET]
df = pd.DataFrame(y_pred, columns = TARGET_names)
df.index = site_merged[FEATURES].index
plotPredictions(site_merged,df,TARGET, split = time_split)



reg = Pipeline([
    ('RegressorChain', RegressorChain(GradientBoostingRegressor()))
])

# Perform optimisation based on given hyper params
hyp_params = { 
    'RegressorChain__base_estimator__learning_rate': np.linspace(0, 5, 10),
    'RegressorChain__base_estimator__random_state' : [random_state]
}

grid = GridSearchCV(estimator=reg,
              param_grid = hyp_params,
              cv = tss, scoring = ('neg_mean_squared_error'),
              n_jobs = 7)

grid.fit(X = train[FEATURES], y = train[TARGET])
reg = grid.best_estimator_

print(grid.best_params_)

Tuned_RF_CV = pd.DataFrame(cross_validate(reg, X = train[FEATURES],
                      y = train[TARGET], cv=tss, 
                      scoring=('r2', main_scorer),
                      return_train_score = True))
print(f'Tuned\nTrain R2 {Tuned_RF_CV["train_" + main_scorer].mean()}\nTest R2 {Tuned_RF_CV["test_" + main_scorer].mean()}')


# #%% Fit the Models


# # Default Model 


# Fine-Tuned Model
reg.fit(train[FEATURES], train[TARGET])
y_pred = reg.predict(site_merged[FEATURES])
TARGET_names = [ 'prediction_' + i for i in TARGET]
df = pd.DataFrame(y_pred, columns = TARGET_names)
df.index = site_merged[FEATURES].index
plotPredictions(site_merged,df,TARGET, split = time_split)



# #%% Examine Importances 

# # Get importance 
# # https://scikit-learn.org/stable/modules/generated/sklearn.inspection.permutation_importance.html#sklearn.inspection.permutation_importance
# # https://scikit-learn.org/stable/auto_examples/inspection/plot_permutation_importance_multicollinear.html#sphx-glr-auto-examples-inspection-plot-permutation-importance-multicollinear-py
# # Comparion between plots code inspired by above links 

# reg = RandomForestRegressor(**grid.best_params_)
# reg.fit(train[FEATURES], train[TARGET])

# fig, ax = plt.subplots(1,2)
# gini_importance = pd.DataFrame(reg.feature_importances_.T, index = FEATURES, columns = ['Gini_importance'])
# gini_importance.sort_values(by = 'Gini_importance', inplace = True, ascending = True)
# gini_importance.plot.barh(figsize = (15,5), ax = ax[0])

# n_repeats = 10
# perm_importance = permutation_importance(reg, train[FEATURES], train[TARGET], n_repeats=n_repeats, random_state = random_state)
# perm_sorted_idx = perm_importance.importances_mean.argsort()

# perm_importance_df = pd.DataFrame(perm_importance.importances[perm_sorted_idx].T, columns = train[FEATURES].columns[perm_sorted_idx])
# perm_importance_df.boxplot(vert = False, figsize = (15,10), ax = ax[1])
# fig.tight_layout()
