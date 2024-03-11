# -*- coding: utf-8 -*-
"""
Created on Wed Feb 28 13:28:25 2024

@author: krish

The transformer classes that are used to preprocess the data for ML modelling 
"""


# %% Import Library
import numpy as np
import pandas as pd
from scipy.signal import savgol_filter

from sklearn.base import BaseEstimator, TransformerMixin
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
        X['dayofyear_cir'] =  np.sin(X['dayofyear']/365)
      
        return X
    
class time_attributes_fc_lag_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, time_lag):
        self.time_lag = time_lag
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        #  Add time lag variables 
        X = X.copy()
        X['pv_lag'] = X["pv_filter"].shift(self.time_lag)
        X['npv_lag'] = X["npv_filter"].shift(self.time_lag)
        X['bs_lag'] = X["bs_filter"].shift(self.time_lag)
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
    
    def __init__(self, window, lag):
        self.window = window 
        self.lag = lag
        
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
        X[climate_variable + '_rolling'] = X[climate_variable].rolling(window = self.window).sum().shift(self.lag)
        
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
        X = X[self.site_location_name == X['Name']]
        X = X.iloc[[i is not None for i in  X['igntn_d']]]
        X['igntn_d'] = pd.to_datetime(X['igntn_d'])
        X = X.sort_values(by = 'igntn_d')
        
        # Mention that the dataset is empty
        if(X.empty):
            print(f'No Fire Dates for {self.site_location_name} avaliable')
        
        return X
    
    
class historical_burn_date_attribute_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, historical_fire_ds, time_lag, verbose = False):
        self.historical_fire_ds = historical_fire_ds
        self.verbose = verbose
        self.time_lag = time_lag
        
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        
        days_since = []
        
        for i in range(len(self.historical_fire_ds['igntn_d']) - 1):
        
            a_date, b_date = self.historical_fire_ds['igntn_d'].iloc[i], self.historical_fire_ds['igntn_d'].iloc[i+1]
            temp_list = []
            
            if i == 0:
                if self.verbose: print(f'{"-"*10} Before {a_date}{"-"*10}')
                temp_df = X[(X.index < a_date)]
                temp_list += [pd.NA for i in range(len(temp_df))]
        
            if self.verbose: print(f'{"-"*10} Between {a_date}-{b_date} {"-"*10}')
            temp_df = X[(X.index >= a_date) & (X.index < b_date)]
            temp_list += [(times_date - a_date).days  for times_date in temp_df.index]
            
            if (i+1) == len(self.historical_fire_ds['igntn_d']) - 1:
                if self.verbose: print(f'{"-"*10} After {b_date} {"-"*10}')
                temp_df = X[(X.index >= b_date)]
                temp_list += [(times_date - b_date).days for times_date in temp_df.index]
            
            print(len(temp_list))
            days_since += temp_list
        
        X['days_since_fire'] = days_since
        X['days_since_fire'] = X['days_since_fire'].astype('Int64')
        X['days_since_fire'] = X['days_since_fire'].replace(pd.NA, -100)
        #X['days_since_fire_lag'] =  X['days_since_fire'].shift(self.time_lag)
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
     X['mean_pv_drop_after_fire'] = X['mean_pv_drop_after_fire'].replace(pd.NA, -100)
     return X
 

class precip_scenarios_adder(BaseEstimator, TransformerMixin):
    
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        X['precip_null'] = [0 for i in range(len(X))] # add a null precip scenario 
        X['precip_constant'] = X['precip']
        
        return X

    
 

# =============================================================================
