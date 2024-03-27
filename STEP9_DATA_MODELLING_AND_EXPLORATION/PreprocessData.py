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
   
        X['month'] = X.index.month
        X['year'] = X.index.year
        X['dayofyear'] = X.index.dayofyear
        
        #X['month_cir'] = np.sin(X['month']/12)
        #X['dayofyear_cir'] =  np.sin(X['dayofyear']/365)
      
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
    

## Used to calculate VPD 
# Based on documentation from 'bigleaf' R package
class calc_VPD(BaseEstimator, TransformerMixin):
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        X = X.copy()
        
        a, b, c = 611.2, 17.62, 243.12
        
        Esat_9am = a*np.exp(
            (b * X['tmin'])/
            (c + X['tmin'])
            )/1000
        
        Esat_3pm = a*np.exp(
            (b * X['tmax'])/
            (c + X['tmax'])
            )/1000
        
        VPD_9am = Esat_9am - X['vapourpres_h09']/10 # convert to kPa
        VPD_3pm = Esat_3pm - X['vapourpres_h15']/10 # convert to kPa
        VPD = np.maximum(0, (VPD_9am + VPD_3pm)/2) # enforce constraint that VPD >= 0 
        X['VPD'] = VPD
        
        return X 
        
        
    
    
# Translated from R code by R package 'geosphere', 
# Who used the following paper to calc photoperiod:
    #Forsythe, William C., Edward J. Rykiel Jr., Randal S. Stahl, Hsin-i Wu and Robert M. Schoolfield, 1995. A model comparison for daylength as a function of latitude and day of the year. Ecological Modeling 80:87-95.
# https://github.com/rspatial/geosphere/blob/master/R/daylength.R
def calc_photoperiod(J, L):
    theta = 0.2163108 + 2 * np.arctan(0.9671396 * np.tan(0.0086 * (J - 186))) # calc theta
    psi = np.arcsin(0.39795 * np.cos(theta)) # calc psi 
    square_bracket = (np.sin(0.8333*np.pi/180) + np.sin(L*np.pi/180)*np.sin(psi))/(np.cos(L*np.pi/180)*np.cos(psi))
    square_bracket = np.minimum(np.maximum(square_bracket, -1), 1) # Enforce Constraints 
    D = 24 - (24/np.pi) * np.arccos(square_bracket)   
    return D
    

class daylength_attributes_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, latitude):
        self.latitude = latitude
                
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        X['photoperiod'] =  list(map(calc_photoperiod, X['dayofyear'] , [self.latitude]*len(X)))
        X['photoperiod_gradient'] = X['photoperiod'].diff()
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
        #print(X)

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
    
    
def pages_variables_constructor(X, climate_var, ts_all, variable_columns, aggregate_type = 'sum'):
    
    X = X.copy()
    X = X.reindex(axis = 1, labels = X.columns.tolist() + variable_columns).copy()
    
    for input_date in range(len(X)):
        # Get associated index in daily climate_var data 
        climate_var_index = climate_var.index[climate_var.time == X.index[input_date]]
        climate_var_vars = [] 
    
        if climate_var_index.empty: # when there are no longer a match (meaning that the FC data overextends the climate_var), break
            break
    
        for ts in ts_all:
    
            upper = (climate_var_index - ts[1])[0]
            lower = (climate_var_index - ts[0] + 1)[0]
    
            if upper >= 0: # check if the upperbounds goes above allowable index 
    
                #print(ts)
                temp = climate_var.iloc[upper:lower] # grab the range 
                
                if aggregate_type == 'sum':
                    temp_agg = temp[temp.select_dtypes(include = ["float64"]).columns.tolist()[0]].sum() # calc sum
                elif aggregate_type == 'mean':
                    temp_agg = temp[temp.select_dtypes(include = ["float64"]).columns.tolist()[0]].mean()
                #print(temp)
                #print(temp_sum)
            else: # if not in allowable index, set to na 
                temp_agg = pd.NA
            #print(temp_sum)
            climate_var_vars.append(temp_agg)
            
            # append climate_var column wise for a particular row 
        for i in range(len(variable_columns)):
            X.loc[X.index[input_date], variable_columns[i]]= climate_var_vars[i]
            
    return X
    
    
class pages_precip_variables_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, site_location_name):
        self.site_location_name = site_location_name
    
    def fit(self, X, y=None):
        return self
    
    def transform(self, X, y=None):
        X = X.copy()
        precip = pd.read_csv(f'../DATASETS/Climate_Gridded/precip/{self.site_location_name}_1987_2022.csv', parse_dates = ['time'], usecols = ['precip','time']).copy()
        precip = precip.sort_values('time')
        precip_col_names = ['precip_30', 'precip_90', 'precip_180', 'precip_365', 'precip_730', 'precip_1095', 'precip_1460'] # set columns of precip
        
        # Specify time ranges: 
        ts_1 = [1, 30]
        ts_2 = [31, 90]
        ts_3 = [91, 180]
        ts_4 =  [181, 365]
        ts_5 = [366, 730]
        ts_6 = [731, 1095]
        ts_7 = [1096, 1460]
        ts_all = [ts_1, ts_2, ts_3, ts_4, ts_5, ts_6, ts_7]
        
        X = pages_variables_constructor(X, precip, ts_all, precip_col_names)
        
        return X


class pages_VPD_variables_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, site_location_name):
        self.site_location_name = site_location_name
    
    def fit(self, X, y=None):
        return self
    
    def transform(self, X, y=None):
        X = X.copy()
        
        file_name = f'{self.site_location_name}_1987_2022.csv'
        tmin = pd.read_csv(f'../DATASETS/Climate_Gridded/tmin/{file_name}', usecols = ['tmin', 'time'], parse_dates = ['time']).copy()
        tmax = pd.read_csv(f'../DATASETS/Climate_Gridded/tmax/{file_name}', usecols = ['tmax', 'time'], parse_dates = ['time']).copy()
        vapourpres_h09 = pd.read_csv(f'../DATASETS/Climate_Gridded/vapourpres_h09/{file_name}', usecols = ['vapourpres_h09', 'time'], parse_dates = ['time']).copy()
        vapourpres_h09['vapourpres_h09'] = vapourpres_h09['vapourpres_h09']/10 # divide by 10 to get kPa
        vapourpres_h15 = pd.read_csv(f'../DATASETS/Climate_Gridded/vapourpres_h15/{file_name}', usecols = ['vapourpres_h15', 'time'], parse_dates = ['time']).copy()
        vapourpres_h15['vapourpres_h15'] = vapourpres_h15['vapourpres_h15']/10 # divide by 10 to get kPa
        climate_data = tmin.copy().merge(tmax, left_on = 'time', right_on = 'time').merge(vapourpres_h09, left_on = 'time', right_on = 'time').\
            merge(vapourpres_h15, left_on = 'time', right_on = 'time')
        climate_data = climate_data.sort_values('time')
        
        # Calculate VPD
        # The coefficients
        a = 611.2
        b = 17.62
        c = 243.12
        
        Esat_9am = a*np.exp(
            (b * climate_data['tmin'])/
            (c + climate_data['tmin'])
        )/1000
        VPD_9am = Esat_9am - climate_data['vapourpres_h09']
        
        Esat_3pm = a*np.exp(
            (b * climate_data['tmax'])/
            (c + climate_data['tmax'])
        )/1000
        VPD_3pm = Esat_3pm - climate_data['vapourpres_h15']
        
        climate_data['VPD'] = (VPD_9am + VPD_3pm)/2
        VPD = climate_data[['time', 'VPD']]
        VPD_col_names = ['VPD_lag','VPD_7', 'VPD_14', 'VPD_30']
        
        # Specify time ranges: 
        ts_lag = [1,1] # t-1
        ts_1 = [2, 7]
        ts_2 = [8, 14]
        ts_3 = [15, 30]
        ts_all = [ts_lag, ts_1, ts_2, ts_3]
        X = pages_variables_constructor(X, VPD, ts_all, VPD_col_names)
        
        return X
    
class pages_temp_variables_adder(BaseEstimator, TransformerMixin):
    def __init__(self, site_location_name):
        self.site_location_name = site_location_name
        
    def fit(self, X, y=None):
        return self
        
    def transform(self, X, y=None):
            X = X.copy()
            
            file_name = f'{self.site_location_name}_1987_2022.csv'
            tmin = pd.read_csv(f'../DATASETS/Climate_Gridded/tmin/{file_name}', usecols = ['tmin', 'time'], parse_dates = ['time']).copy()
            tmax = pd.read_csv(f'../DATASETS/Climate_Gridded/tmax/{file_name}', usecols = ['tmax', 'time'], parse_dates = ['time']).copy()
            
            climate_data = tmin.copy().merge(tmax, left_on = 'time', right_on = 'time')
            climate_data = climate_data.sort_values('time')

            temp = climate_data[['time', 'tmax']]
            temp_col_names = ['tmax_lag','tmax_7', 'tmax_14', 'tmax_30']
            
            # Specify time ranges: 
            ts_lag = [1,1] # t-1
            ts_1 = [2, 7]
            ts_2 = [8, 14]
            ts_3 = [15, 30]
            ts_all = [ts_lag, ts_1, ts_2, ts_3]
            
            X = pages_variables_constructor(X, tmax, ts_all, temp_col_names)
                    
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
    
    def __init__(self, historical_fire_ds, verbose = False):
        self.historical_fire_ds = historical_fire_ds
        self.verbose = verbose
        
        
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
