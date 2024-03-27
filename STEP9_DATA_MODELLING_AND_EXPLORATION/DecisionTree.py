# -*- coding: utf-8 -*-
"""
Created on Wed Feb 28 13:48:01 2024

@author: krish
"""

import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from sklearn.model_selection import TimeSeriesSplit
import graphviz 


from sklearn import tree
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline
from PreprocessData import * # import from custom transformers
from sklearn.model_selection import GridSearchCV

#%% Main 
#  %% Preprocess and create train/test'

#site_location_name = 'NSAMDD0002' # no fire, seasonal
#site_location_name = 'NSANAN0002' # fire, seasonal, big drop
#site_location_name = 'WAAPIL0003'

site_location_name = 'NSANAN0002'
site_merged = pd.read_csv(f'Input_DataSet_{site_location_name}.csv', parse_dates = ['time']).copy()
site_merged = site_merged.set_index('time')


#%% Model the dataset

SEASONAL_FEATURES = ['photoperiod', 'photoperiod_gradient', 'year']
PRECIP_FEATURES = ['precip_30', 'precip_90', 'precip_180', 'precip_365', 'precip_730', 'precip_1095', 'precip_1460', 'precip_sum']
TEMP_FEATURES = ['tmax_lag', 'tmax_7', 'tmax_14', 'tmax_30', 'tmax_sum']
VPD_FEATURES = ['VPD_lag','VPD_7, VPD_14, VPD_30']
LAG_FEATURES = ['pv_lag', 'npv_lag', 'bs_lag']
LAGGED_CHANGE_FEATURES = ['pv_change', 'npv_change', 'bs_change']
FIRE_FEATURES = ['mean_pv_drop_after_fire', 'days_since_fire']
FEATURES =  FIRE_FEATURES + SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES

TARGET = ['pv_filter', 'bs_filter', 'npv_filter']
site_merged = site_merged.dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
scores = []

tss = TimeSeriesSplit(n_splits= 7)

#%% Show CV splits 
fig, axs = plt.subplots(10, 1, figsize=(15, 15), sharex=True)

fold = 0
for train_idx, val_idx in tss.split(site_merged):
    train = site_merged.iloc[train_idx]
    test = site_merged.iloc[val_idx]
    train['pv_filter'].plot(ax=axs[fold],
                          label='Training Set',
                          title=f'Data Train/Test Split Fold {fold}')
    test['pv_filter'].plot(ax=axs[fold],
                         label='Test Set')
    axs[fold].axvline(test.index.min(), color='black', ls='--')
    fold += 1
plt.show()

#%% Run Grid Search

random_state = 20240228

hyp_params = { 
    'min_samples_split' : np.linspace(0.05,0.5,20),
    'min_samples_leaf' : np.linspace(0.05,0.5,20),
    'max_features'     : ['sqrt', 'log2', None] + list(np.linspace(0.1,1,10)),
    'criterion': ['squared_error','friedman_mse', 'poisson'],
    'splitter': ['best', 'random'],
    'random_state' : [random_state]
}

grid = GridSearchCV(estimator=tree.DecisionTreeRegressor(),
             param_grid = hyp_params,
             cv = tss, return_train_score = True,
             n_jobs = 7)
grid.fit(X = site_merged[FEATURES], y = site_merged[TARGET])



#%% Run Cross Validation

for train_idx, val_idx in tss.split(site_merged):
    
    train = site_merged.iloc[train_idx]
    test = site_merged.iloc[val_idx]

    X_train = train[FEATURES]
    y_train = train[TARGET]

    X_test = test[FEATURES]
    y_test = test[TARGET]
    
    # multi_strategy
    reg = tree.DecisionTreeRegressor(**grid.best_params_)
    reg.fit(X_train, y_train)
    
    y_pred = reg.predict(X_test)
    
    ## Append the scores for each of the fractions 
    score_temp = []
    for col in range(3):
        y_test_col = y_test[TARGET[col]]
        y_pred_col = [i[col] for i in y_pred]

        score = np.sqrt(mean_squared_error(y_pred_col, y_test_col))
        score_temp.append(score)
    scores.append(score_temp)

print(pd.DataFrame(scores, columns = TARGET))

print(f"Mean RMSE of fractions: {np.mean(scores,axis = 0)}")

print(f"Overall Mean RMSE of fractions: {np.mean(scores)}")

#%% Fit the Model 

# multi_output_tree
# one_output_per_tree
reg = tree.DecisionTreeRegressor(**grid.best_params_)

time_split = '2016-01-01'
train = site_merged.iloc[site_merged.index <= time_split]
test = site_merged.iloc[site_merged.index > time_split]

X_train = train[FEATURES]
y_train = train[TARGET]

X_test = test[FEATURES]
y_test = test[TARGET]

reg.fit(X_train, y_train)

dot_data = tree.export_graphviz(reg, out_file=None, feature_names = FEATURES) 
graph = graphviz.Source(dot_data) 
graph.render(f"{site_location_name}_Decision_Tree", 
             view = True, 
             overwrite_source= True) 

#print(reg.feature_importances_[0])
#print(reg.feature_importances_[1])

#%% Evaluate Model (using a single example)

TARGET_names = [ 'prediction_' + i for i in TARGET]
prediction_df = pd.DataFrame(reg.predict(X_test), columns = TARGET_names)
prediction_df.index = test.index
test = pd.concat([test, prediction_df], axis=1)


site_merged = site_merged.merge(test[TARGET_names], how='left', left_index=True, right_index=True)


fig, ax = plt.subplots(nrows = 3, figsize = (15,10))
site_merged['pv_filter'].plot(ax=ax[0], color = 'blue', alpha = 0.4, linestyle='dashed' )
site_merged['prediction_pv_filter'].plot(ax=ax[0], color = 'orange')
ax[0].legend()
site_merged['npv_filter'].plot(ax=ax[1], color = 'blue', alpha = 0.4, linestyle='dashed')
site_merged['prediction_npv_filter'].plot(ax=ax[1], color = 'orange')
ax[1].legend()
site_merged['bs_filter'].plot(ax=ax[2], color = 'blue', alpha = 0.4,linestyle='dashed')
site_merged['prediction_bs_filter'].plot(ax=ax[2], color = 'orange')
ax[2].legend()
plt.show()

print("-"*10 + "Current RMSE" + "-"*10)

score_pv = np.sqrt(mean_squared_error(test['pv_filter'], test['prediction_pv_filter']))
print(f'RMSE Score on Test set pv: {score_pv:0.2f}')

score_npv = np.sqrt(mean_squared_error(test['npv_filter'], test['prediction_npv_filter']))
print(f'RMSE Score on Test set npv: {score_npv:0.2f}')

score_bs = np.sqrt(mean_squared_error(test['bs_filter'], test['prediction_bs_filter']))
print(f'RMSE Score on Test set bs: {score_bs:0.2f}')

score_avg = np.mean([score_pv,score_npv,score_bs])
print(f'Average RMSE Score on Test set: {score_avg:0.2f}')

print("-"*10 + "CV RMSE" + "-"*10)

print(pd.DataFrame(scores, columns = TARGET))

print(f"Mean RMSE of fractions: {np.mean(scores,axis = 0)}")

print(f"Overall Mean RMSE of fractions: {np.mean(scores)}")

#%% Export Dataset


site_merged.to_csv(f'{site_location_name}_DecisionTree_{np.mean(scores):0.2f}.csv')



 


