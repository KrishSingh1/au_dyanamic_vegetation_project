# -*- coding: utf-8 -*-
"""
Created on Thu Feb  8 13:12:32 2024

@author: krish

Acknowledgement for XGBtree kaggle tutorials for procedures + code for fitting and evaluating XGB tree models: 

    - https://www.kaggle.com/code/robikscube/tutorial-time-series-forecasting-with-xgboost
    - https://www.kaggle.com/code/robikscube/pt2-time-series-forecasting-with-xgboost
    
"""


# %% Import Library
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from sklearn.model_selection import TimeSeriesSplit

import xgboost as xgb
from sklearn.metrics import mean_squared_error
from scipy.signal import savgol_filter

from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.pipeline import Pipeline
from dateutil.relativedelta import relativedelta

#%% Functions

class time_attributes_adder(BaseEstimator, TransformerMixin):
    
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        #X['quarter'] = X.index.quarter
        X['month'] = X.index.month
        X['year'] = X.index.year
        X['dayofyear'] = X.index.dayofyear
        #X['weekofyear'] = X.index.isocalendar().week
        
        X['month_cir'] = np.sin(X['month']/12)
        X['dayofyear_cir'] =  (X['dayofyear']/365)
        print(X)
      
        return X
    
class time_attributes_fc_lag_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, time_lag):
        self.time_lag = time_lag
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        #  Add time lag variables 
        X = X.copy()
        X['pv_lag'] = X["pv_filter"].shift(time_lag)
        X['npv_lag'] = X["npv_filter"].shift(time_lag)
        X['bs_lag'] = X["bs_filter"].shift(time_lag)
        print(X)
        return X
    
class time_attributes_fc_diff_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, is_lagged, window_size = 2):
        self.is_lagged = is_lagged
        self.window_size = window_size
       
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        #  Add time lag variables
        X = X.copy()
        
        if self.is_lagged:
            X['pv_change'] = X["pv_lag"].diff().rolling(window = self.window_size).mean()
            X['npv_change'] = X["npv_lag"].diff().rolling(window = self.window_size).mean()
            X['bs_change'] = X["bs_lag"].diff().rolling(window = self.window_size).mean()
            
        else: 
            X['pv_change'] = X["pv"].diff().rolling(window = self.window_size).mean()
            X['npv_change'] = X["npv"].diff().rolling(window = self.window_size).mean()
            X['bs_change'] = X["bs"].diff().rolling(window = self.window_size).mean()
        print(X)
          
        return X

class preprocess_fc_time_series(BaseEstimator, TransformerMixin):
    
    def __init__(self, polyorder, window_length):
        self.polyorder = polyorder
        self.window_length = window_length
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        X = X.sort_values(by = 'time')
        X.set_index('time', inplace=True)
        X = X.resample('16D').mean() # resample into a regular time interval of 16
        X = X.interpolate(method = 'linear') # interpolate missing data 
        
        # Now apply Savitzky–Golay filter on the fractions, (ideally) smoothing out the noise 
        fractions = ['pv', 'npv', 'bs']
        for f in fractions:
            variable_name = f + '_filter'
            X[variable_name] = savgol_filter(X[f], window_length = self.window_length, polyorder = self.polyorder)
            X.loc[X[variable_name] < 0, variable_name] = 0 # correct for any negative-valued fractions 
        print(X)

        return X
    
class preprocess_climate_time_series(BaseEstimator, TransformerMixin):
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        X = X.copy()
        X = X.sort_values('time')
        X.set_index('time', inplace=True)
        return X

class climate_time_series_attributes_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, window):
        self.window = window 
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        climate_variable = X.columns[1] # assumption that the climate var name is the second variable name

        ## Calculate yearly cv 
        X_std = X.groupby('year').std()
        X_mean = X.groupby('year').mean()
        X_sum = X.groupby('year').sum()
        
        X_cv = X_std[[climate_variable]]/X_mean[[climate_variable]]
        X_cv = X_cv.rename(columns= {climate_variable: climate_variable + "_cv"})
        X_cv[climate_variable + '_sum'] = X_sum[climate_variable]
        X = X.merge(X_cv, how = 'left', left_on = 'year', right_index = True)
        
        
        ## Add rolling time window  
        ## Here, I took the rolling window, then I shifted it by the window size,
        ## Note: the fact that they, the size of rolling window and lag number, the same is arbituary 
        X[climate_variable + '_rolling'] = X[climate_variable].rolling(window = self.window).sum().shift(self.window)
        
        return X


class climate_time_series_downsample(BaseEstimator, TransformerMixin):
    
    def __init__(self, start_time, resample_method):
        self.start_time = start_time 
        self.resample_method = resample_method.lower()
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        X = X.copy()
        X = X[X.index >= self.start_time]
        
        if self.resample_method == 'sum':
            X = X.resample('16D').sum()
        elif self.resample_method == 'mean':
            X = X.resample('16D').mean()
        else:
            print(f'Warning: Resample method {self.resample_method}')
        
        return X

class historical_burn_date_preprocess(BaseEstimator, TransformerMixin):
    
    def __init__(self, site_location_name):
        self.site_location_name = site_location_name 
        
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        X = X[['igntn_d', 'Name']]
        X = X[site_location_name == X['Name']]
        X = X.iloc[[i is not None for i in  X['igntn_d']]]
        X['igntn_d'] = pd.to_datetime(X['igntn_d'])
        X = X.sort_values(by = 'igntn_d')
        print(X)
        return X
    
    
class historical_burn_date_attribute_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, historical_fire_ds, time_lag, verbose = False):
        self.dates = historical_fire_ds
        self.verbose = verbose
        self.time_lag = time_lag
        
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        
        days_since = []
        
        for i in range(len(historical_fire_ds['igntn_d']) - 1):
        
            a_date, b_date = historical_fire_ds['igntn_d'].iloc[i], historical_fire_ds['igntn_d'].iloc[i+1]
            temp_list = []
            
            if i == 0:
                if self.verbose: print(f'{"-"*10} Before {a_date}{"-"*10}')
                temp_df = X[(X.index < a_date)]
                temp_list += [pd.NA for i in range(len(temp_df))]
        
            if self.verbose: print(f'{"-"*10} Between {a_date}-{b_date} {"-"*10}')
            temp_df = X[(X.index >= a_date) & (X.index < b_date)]
            temp_list += [(times_date - a_date).days  for times_date in temp_df.index]
            
            if (i+1) == len(historical_fire_ds['igntn_d']) - 1:
                if self.verbose: print(f'{"-"*10} After {b_date} {"-"*10}')
                temp_df = X[(X.index >= b_date)]
                temp_list += [(times_date - b_date).days for times_date in temp_df.index]
            
            print(len(temp_list))
            days_since += temp_list
        
        X['days_since_fire'] = days_since
        X['days_since_fire'] = X['days_since_fire'].astype('Int64')
        X['days_since_fire_lag'] =  X['days_since_fire'].shift(time_lag)
        print(X)
        return X
    
    
class historical_burn_date_index_attribute_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, verbose = False, time_period = 16, month_baseline = 6):
        self.verbose = verbose
        self.time_period = time_period
        self.month_baseline = month_baseline
 
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
     X = X.copy()
     fire_dates = X.loc[(X['days_since_fire'] < self.time_period) & (X['days_since_fire'] >= 0)]
     if self.verbose: print(fire_dates)
     X['mean_pv_drop_after_fire'] = pd.NA
    
     X = X.astype('Float64')
     for i in range(len(fire_dates)):

        if self.verbose: print(fire_dates.index[i])
        prior = fire_dates.index[i] - relativedelta(months = self.month_baseline)
        if self.verbose: print(prior)
        mean_selector = (X.index >= prior) & (X.index < fire_dates.index[i])
        mean_pv = X.iloc[mean_selector]['pv'].mean()
        if self.verbose: print(mean_pv)
        
        if i != (len(fire_dates) - 1):
            df_selector = (X.index >= fire_dates.index[i]) & (X.index <fire_dates.index[i+1]) # only go up to the next fire date
        else:
            df_selector = (X.index >= fire_dates.index[i]) # there is no recorded fire date so go up to the very end of the dataset 
            
        selected_df = X.iloc[df_selector].copy()
        selected_df['days_since_fire'].loc[selected_df['days_since_fire'] == 0] = 1
        
        X.loc[df_selector,'mean_pv_drop_after_fire'] = (mean_pv - selected_df['pv_filter'])/(mean_pv)
        
       # for f in ['pv_filter', 'npv_filter', 'bs_filter']:
       #     selected_df[f].loc[(selected_df['days_since_fire'] >= 1) & (selected_df['days_since_fire'] <= month_baseline*16*2)] = pd.NA
       #     X.loc[df_selector, f] = selected_df[f]
            
        # problem here is that the machine learning model does not accept NA for 
    
     print(X)
     return X


# =============================================================================
class historical_burn_date_index_attribute_adder_lag(BaseEstimator, TransformerMixin):
     
    def __init__(self, time_lag, verbose = False, time_period = 16, month_baseline = 6):
        self.verbose = verbose
        self.time_period = time_period
        self.month_baseline = month_baseline
        self.time_lag = time_lag
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
      X = X.copy()
      fire_dates = X.loc[(X['days_since_fire_lag'] < self.time_period) & (X['days_since_fire_lag'] >= 0)]
      if self.verbose: print(fire_dates)
      X['mean_pv_drop_per_days_since_fire'] = pd.NA

      for i in range(len(fire_dates)):
  
        #if self.verbose: print(lag_correction)
        if self.verbose: print(fire_dates.index[i])
        
        prior = fire_dates.index[i] - relativedelta(months = self.month_baseline)
        if self.verbose: print(prior)
        mean_selector = ( X.index >= prior) & (X.index  < fire_dates.index[i])
        mean_pv = X.iloc[mean_selector]['pv_lag'].mean()
        if self.verbose: print(mean_pv)
        
        if i != 2:
            df_selector = (X.index >= fire_dates.index[i]) & (X.index <fire_dates.index[i+1]) # only go up to the next fire date
        else:
            df_selector = (X.index >= fire_dates.index[i]) # there is no recorded fire date so go up to the very end of the dataset 
            
        selected_df = X.iloc[df_selector].copy()
        selected_df['days_since_fire_lag'].loc[selected_df['days_since_fire_lag'] == 0] = 1
        #selected_df['days_since_fire'] -= (self.time_period * self.time_lag) # correct for lag in the pv_lag
        if self.verbose: print(selected_df[['pv','pv_lag','days_since_fire', 'days_since_fire_lag']])
        
        X.loc[df_selector,'mean_pv_drop_per_sqrt_days_since_fire'] = (mean_pv - selected_df['pv_lag'])/np.sqrt(selected_df['days_since_fire_lag'])
        X.loc[df_selector,'mean_pv_drop_per_log_days_since_fire'] = (mean_pv - selected_df['pv_lag'])/(np.log(selected_df['days_since_fire_lag']) + 1)
        X.loc[df_selector,'mean_pv_drop_per_days_since_fire'] = (mean_pv - selected_df['pv_lag'])/(selected_df['days_since_fire_lag'])
        
        X = X.astype('Float64')
        
      return X
# =============================================================================

#%% Main 
#  %% Preprocess and create train/test'

#site_location_name = 'NSAMDD0002' # no fire, seasonal
#site_location_name = 'NSANAN0002' # fire, seasonal, big drop
site_location_name = 'WAAPIL0003'
historical_fire_ds = gpd.read_file('..\DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/AusPlots_Historical_BurnDates.shp', parse_dates = ['igntn_d'])
print(historical_fire_ds['Name'])
time_lag = 5
window_length = 5
month_baseline = 3

# savgol_filter parameters 
window_length_smooth = 15
polyorder = 4 

historical_fire_pipeline = Pipeline([
    ('historical_burn_date_preprocess',historical_burn_date_preprocess(site_location_name))
    ])
historical_fire_ds = historical_fire_pipeline.fit_transform(historical_fire_ds)
print(historical_fire_ds)


site = pd.read_csv(f'au_dyanamic_vegetation_project/DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/{site_location_name}.csv', parse_dates=['time'])

time_fc_pipeline = Pipeline([
    ('preprocess_fc_time_series', preprocess_fc_time_series(window_length = window_length_smooth, polyorder = polyorder)),
    ('time_attributes_adder', time_attributes_adder()),
    ('time_attributes_fc_lag_adder', time_attributes_fc_lag_adder(time_lag)),
    ('time_attributes_fc_diff_adder', time_attributes_fc_diff_adder(False)),
    ('historical_burn_date_attribute_adder', historical_burn_date_attribute_adder(historical_fire_ds, time_lag = time_lag)),
    ('historical_burn_date_index_attribute_adder', historical_burn_date_index_attribute_adder(verbose = True,
                                                                                              month_baseline = month_baseline))
    #('historical_burn_date_index_attribute_adder_lag', historical_burn_date_index_attribute_adder_lag(time_lag = time_lag,
    #                                                                                            month_baseline = month_baseline, verbose = True))
 ])
site_resampled = time_fc_pipeline.fit_transform(site)
print('FC and fire data successfully preprocessed')


climate_variables = pd.DataFrame({'climate_var': ['Precip','tmax'],
                                 'resample_type': ['sum', 'mean']})
datasets = dict()

for index, row in climate_variables.iterrows():

    climate = pd.read_csv(f'..\DYNAMIC MODEL VEGETATION PROJECT/au_dyanamic_vegetation_project/DATASETS/Climate_Gridded/{row["climate_var"]}/{site_location_name}_1987_2022.csv', parse_dates=['time'])
    print(climate)
    
    datasets[row['climate_var']] = climate
    
    time_climate_pipeline = Pipeline([
        ('preprocess_climate_time_series', preprocess_climate_time_series()),
        ('climate_time_series_downsample', climate_time_series_downsample(start_time = site_resampled.index[0], resample_method = row['resample_type'])),
        ('time_attributes_adder', time_attributes_adder()),
        ('climate_time_series_attributes_adder', climate_time_series_attributes_adder(window = window_length))
    ])
    climate_new = time_climate_pipeline.fit_transform(climate)
    site_resampled = site_resampled.merge(climate_new, how = 'left', left_index = True, right_index = True, validate = "one_to_one",
                                       suffixes = ('', '_DUPLICATE'))
    site_resampled = site_resampled.drop(columns =  site_resampled.filter(regex = '_DUPLICATE$').columns)

site_merged = site_resampled.copy()

tss = TimeSeriesSplit(n_splits= 5)
#%% Model the dataset

# SEASONAL_FEATURES: ['month_x', 'year_x', 'dayofyear_x', 'month_cir_x', ''dayofyear_cir_x'']
SEASONAL_FEATURES = ['dayofyear_cir', 'year']

# PRECIP_FEATURES =  ['precip_rolling', 'precip_sum', 'precip_cv']
PRECIP_FEATURES =  ['precip_rolling', 'precip_sum']
TEMP_FEATURESS = ['tmax']
LAG_FEATURES = ['pv_lag', 'npv_lag', 'bs_lag']
LAGGED_CHANGE_FEATURES = ['pv_change', 'npv_change', 'bs_change']
#FIRE_FEATURES = ['mean_pv_drop_per_days_since_fire', 'mean_pv_drop_per_sqrt_days_since_fire',
#                 'mean_pv_drop_per_log_days_since_fire','days_since_fire']
FIRE_FEATURES = ['mean_pv_drop_after_fire', 'days_since_fire']
#FIRE_FEATURES = ['days_since_fire']

# FEATURES = SEASONAL_FEATURES + PRECIP_FEATURES + LAG_FEATURES
# PRECIP_FEATURES
FEATURES = LAG_FEATURES + LAGGED_CHANGE_FEATURES + FIRE_FEATURES + SEASONAL_FEATURES + PRECIP_FEATURES + TEMP_FEATURESS


TARGET = ['pv_filter', 'bs_filter', 'npv_filter']
scores = []

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


#%% Run Cross Validation

for train_idx, val_idx in tss.split(site_merged):
    
    train = site_merged.iloc[train_idx]
    test = site_merged.iloc[val_idx]

    X_train = train[FEATURES]
    y_train = train[TARGET]

    X_test = test[FEATURES]
    y_test = test[TARGET]
    
    # multi_strategy
    reg = xgb.XGBRegressor(base_score=0.6, booster='gbtree',    
                       n_estimators=1000,
                       early_stopping_rounds=100,
                       objective='reg:squarederror',
                       max_depth=100,
                       learning_rate=0.1, multi_strategy="one_output_per_tree", n_jobs = 7)
    reg.fit(X_train, y_train,
        eval_set=[(X_train, y_train)],
        verbose=100)
    
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
reg = xgb.XGBRegressor(base_score=0.6, booster='gbtree',    
                   n_estimators=1000,
                   early_stopping_rounds=100,
                   objective='reg:squarederror',
                   max_depth=100,
                   learning_rate=0.1,
                   multi_strategy="one_output_per_tree", 
                   n_jobs = 7)

time_split = '2016-01-01'
train = site_merged.iloc[site_merged.index <= time_split]
test = site_merged.iloc[site_merged.index > time_split]

X_train = train[FEATURES]
y_train = train[TARGET]

X_test = test[FEATURES]
y_test = test[TARGET]

reg.fit(X_train, y_train,
    eval_set=[(X_train, y_train)],
    verbose=100)

#print(reg.feature_importances_[0])
#print(reg.feature_importances_[1])

xgb.plot_importance(reg, importance_type = 'weight')
xgb.plot_importance(reg, importance_type = 'gain')
xgb.plot_importance(reg, importance_type = 'cover')
xgb.plot_tree(reg, num_trees = 10)

fig = plt.gcf()
fig.set_size_inches(150, 100)
fig.savefig('tree.png')
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


#%%


    

