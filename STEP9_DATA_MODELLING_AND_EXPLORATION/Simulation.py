# -*- coding: utf-8 -*-
"""
Created on Wed May 22 23:22:31 2024

@author: krish
"""

import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from sklearn.model_selection import TimeSeriesSplit
from sklearn.model_selection import cross_validate
import graphviz 
import matplotlib.cm as cm

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


after_2015_sites = ['WAAPIL0003', 'NSABHC0023', 'TCATCH0006',
                 'WAAGAS0002', 'NSAMDD0014', 'NTAGFU0021', 
                 'NSANSS0001', 'SATSTP0005', 'QDASSD0015', 
                 'NTAFIN0002', 'NSANAN0002', 'QDAEIU0010'] # smalller subset 


tree_sites = ['NSABBS0001', 'NSACOP0001', 'NSAMDD0011', 'NSAMDD0020',
       'NSAMUL0003', 'NSANAN0001', 'NSANAN0002', 'NSANSS0001',
       'NSANSS0002', 'NSTSYB0003', 'NSTSYB0006', 'NTAFIN0003',
       'NTAFIN0015', 'NTAGFU0030', 'NTAGFU0034', 'QDABBN0002',
       'QDACHC0003', 'QDACYP0020', 'QDAGUP0006', 'QDAMGD0025',
       'QDAMUL0002', 'QDAMUL0003', 'QDASEQ0004', 'SAAFLB0008',
       'SATFLB0003', 'SATFLB0020', 'SATFLB0022', 'TCATCH0004',
       'WAACAR0002', 'WAAGAS0001', 'WAAPIL0023']

shrub_sites = ['NSABHC0011', 'NSAMDD0028', 'NSTSYB0005', 'NTAFIN0018',
       'QDABBS0010', 'QDACYP0018', 'QDAGUP0021', 'SAAEYB0021',
       'SAAEYB0028', 'SAAFLB0005', 'SAAGAW0008', 'SAAKAN0009',
       'SAASTP0023', 'SAASTP0033', 'SAASTP0034', 'SASMDD0009',
       'SASMDD0014', 'SATFLB0023', 'WAACAR0004', 'WAACOO0007',
       'WAACOO0016', 'WAACOO0024', 'WAACOO0026', 'WAACOO0027',
       'WAACOO0029', 'WAAGES0001', 'WAALSD0002', 'WAANUL0003',
       'WAAPIL0010']

grass_sites = ['NSABHC0023', 'NSAMDD0001', 'NSAMDD0014', 'NTADAC0001',
       'NTADMR0001', 'NTAFIN0002', 'NTAFIN0006', 'NTAGFU0014',
       'NTAGFU0020', 'NTAGFU0021', 'NTASTU0004', 'NTTDMR0003',
       'QDABBS0002', 'QDACYP0006', 'QDACYP0022', 'QDAEIU0005',
       'QDAEIU0010', 'QDAGUP0009', 'QDAGUP0019', 'QDAMGD0002',
       'QDAMGD0023', 'QDAMGD0024', 'QDASSD0015', 'SAAEYB0029',
       'SAAFLB0003', 'SASMDD0005', 'SATFLB0019', 'SATSTP0005',
       'TCATCH0006', 'TCATNM0001', 'TCATNM0003', 'VCAAUA0012',
       'WAAAVW0006', 'WAACOO0030', 'WAAGAS0002', 'WAANOK0006',
       'WAAPIL0003', 'WAAPIL0024', 'WAAPIL0031']

sites_list = grass_sites

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

VEGETATION_FEATURES = ['grass', 'shrub', 'tree']

SOIL_FEATURES = ['SLGA_1','SLGA_2','SLGA_3', 'DER_000_999'] # the soil attributes to include

FEATURES =  SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURES + VPD_FEATURES + FIRE_FEATURES + CO2_FEATURES + VEGETATION_FEATURES + SOIL_FEATURES # final features 
TARGET = ['pv_filter', 'npv_filter', 'bs_filter']
scores = []


#%% Create Train/test set 

datasets = {}

for site_location_name in sites_list:
    
    site_merged = pd.read_csv(f'Input_DataSet_{site_location_name}.csv', parse_dates = ['time']).copy()
    datasets[site_location_name] = site_merged

site_merged = pd.concat(datasets).dropna(subset = FEATURES) # drop na based on chosen features, needed for random forest 
site_merged.sort_values('time', inplace = True)
site_merged.set_index('time', inplace = True)
scores = []

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

reg.fit(X = train[FEATURES], y = train[TARGET])

#%% Create Constant Variables

datasets_simulated = site_merged.copy()

VARYING_FEATURES = ['CO2']
TWO_LEVEL_FEATURE = ['precip_90']
CONSTANT_VARIABLES = list(set(FEATURES).difference(VARYING_FEATURES + TWO_LEVEL_FEATURE))
CONSTANT_VARIABLES_NEW = []

for i in CONSTANT_VARIABLES:
    datasets_simulated[i] = datasets_simulated[i].mean()
    
level_one = datasets_simulated[TWO_LEVEL_FEATURE].quantile(0.25)
level_two = datasets_simulated[TWO_LEVEL_FEATURE].quantile(0.75)

datasets_simulated_1 = datasets_simulated.copy()
datasets_simulated_1[TWO_LEVEL_FEATURE] = level_one

datasets_simulated_2 = datasets_simulated.copy()
datasets_simulated_2[TWO_LEVEL_FEATURE] = level_two

#%% Perform Simulation 

FC_sim_1 = reg.predict(X = datasets_simulated_1[FEATURES])
datasets_simulated_1[['pv_filter_sim', 'npv_filter_sim', 'bs_filter_sim']] = FC_sim_1

FC_sim_2 = reg.predict(X = datasets_simulated_2[FEATURES])
datasets_simulated_2[['pv_filter_sim', 'npv_filter_sim', 'bs_filter_sim']] = FC_sim_2

#%% Create Contour Plots 

# x = list(datasets_simulated_1['CO2'].values) + list(datasets_simulated_2['CO2'].values)
# y = list(datasets_simulated_1['precip_180']) + list(datasets_simulated_2['precip_180'].values)
# Z = [[datasets_simulated_1['pv_filter'].values], [datasets_simulated_2['pv_filter'].values]] 

# y = datasets_simulated_2['precip_180']
# X, Y = np.meshgrid(x, y)
# Z = datasets_simulated_2['pv_filter']
# fig, ax = plt.subplots()
# CS = ax.contour(X, Y, Z)


ylimit = 100

fig, ax = plt.subplots(3,1 , figsize = (10,10), sharex= True)

datasets_simulated_2.plot(x = VARYING_FEATURES[0], y = 'pv_filter_sim',
                          ax = ax[0], 
                          ylim = (0,ylimit),
                          #label = f'{TWO_LEVEL_FEATURE[0]} 75% Percentile'
                          )
datasets_simulated_1.plot(x = VARYING_FEATURES[0], y = 'pv_filter_sim', ax = ax[0], 
                          #label = f'{TWO_LEVEL_FEATURE[0]} 25% Percentile',
                          ylim = (0,ylimit),
                          xlabel = '$CO_2$ (ppm)',
                          ylabel = 'PV (%)')

ax[0].grid(True)
ax[0].get_legend().remove()

datasets_simulated_2.plot(x = VARYING_FEATURES[0], 
                          y = 'npv_filter_sim', ax = ax[1], 
                          ylim = (0,ylimit),
                          #label = f'{TWO_LEVEL_FEATURE[0]} 75% Percentile',
                          xlabel = '$CO_2$ (ppm)',
                          ylabel = 'NPV (%)')
datasets_simulated_1.plot(x = VARYING_FEATURES[0], y = 'npv_filter_sim', ax = ax[1], 
                          ylim = (0,ylimit),
                          #label = f'{TWO_LEVEL_FEATURE[0]} 25% Percentile',
                          xlabel = '$CO_2$ (ppm)',
                          ylabel = 'NPV (%)')

ax[1].grid(True)
ax[1].get_legend().remove()

datasets_simulated_2.plot(x = VARYING_FEATURES[0], 
                          y = 'bs_filter_sim', ax = ax[2], 
                          ylim = (0,ylimit),
                          #label = f'{TWO_LEVEL_FEATURE[0]} 75% Percentile',
                          xlabel = '$CO_2$ (ppm)',
                          ylabel = 'BS (%)')
datasets_simulated_1.plot(x = VARYING_FEATURES[0], y = 'bs_filter_sim', ax = ax[2], 
                          ylim = (0,ylimit),
                          xlabel = '$CO_2$ (ppm)',
                          ylabel = 'BS (%)')

ax[2].grid(True)
ax[2].get_legend().remove()

# labels = ['$\Sigma_{l=91}^{180}PPT_{t-l}$ (75%)', 
#           '$\Sigma_{l=91}^{180}PPT_{t-l}$ (25%) ' ]

labels = ['$\Sigma_{l=31}^{90}PPT_{t-l}$ (75%)', 
          '$\Sigma_{l=31}^{90}PPT_{t-l}$ (25%) ' ]
fig.legend(labels, loc='upper right', labelcolor = 'linecolor', fontsize = 15, bbox_to_anchor=(1.2,0.9))


# historical_fire_ds = gpd.read_file('../DATASETS/AusPlots_Historical_BurnDates.geojson', parse_dates = ['igntn_d']) # for fire dates

# for site in sites_list:
#     fig, ax = plt.subplots(2)

#     site_data = datasets[site].set_index('time').dropna(subset = FEATURES)
    
#     train_site = site_data.iloc[training_set[site].index]
#     train_site['pv_filter'].plot(figsize = (15,5), ax = ax[0])
    
#     test_site = site_data.iloc[test_set[site].index]
#     test_site['pv_filter'].plot(figsize = (15,5), ax = ax[1])
    
#     print(f'{test_site.index.min()}')
    
#     y_pred = reg.predict(site_data[FEATURES])
#     TARGET_names = ['prediction_' + i for i in TARGET]
#     df = pd.DataFrame(y_pred, columns = TARGET_names)
#     df.index = site_data[FEATURES].index
    
#     print(test[TARGET])
#     print(df.iloc[training_set[site].index])
    
#     train_score = mean_squared_error(train_site[TARGET], df.iloc[training_set[site].index])
#     test_score = mean_squared_error(test_site[TARGET],  df.iloc[test_set[site].index])
#     print(train_score)
#     print(test_score)
    
#     historical_fire_pipeline = Pipeline([
#         ('historical_burn_date_preprocess', historical_burn_date_preprocess(site))
#         ])
#     historical_fire_ds_site = historical_fire_pipeline.fit_transform(historical_fire_ds)
#     print(historical_fire_ds_site)

#     #test_score = mean_squared_error(site_data[site_data.index > time_split][TARGET], df[site_data.index > time_split])
#     #plotPredictions(site_data, df, TARGET, msg = f'{site} RF . MSE Scores: train:{train_score:.2f}, test:{test_score:.2f}')
#     plotPredictions(site_data, df, TARGET, msg = f'{site} MSE Scores: train:{train_score:.2f}, test:{test_score:.2f}', split = [f'{test_site.index.min()},', f'{test_site.index.max()}'])



# Fine-Tuned Model
# reg = RandomForestRegressor(**grid.best_params_)
# reg.fit(train[FEATURES], train[TARGET])
# y_pred = reg.predict(site_merged[FEATURES])
# TARGET_names = [ 'prediction_' + i for i in TARGET]
# df = pd.DataFrame(y_pred, columns = TARGET_names)
# df.index = site_merged[FEATURES].index
# plotPredictions(site_merged,df,TARGET, split = time_split)



