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
        
        # Add circular variables
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
        
        # Note: I am removing records that go above 2023 because climate data only goes so far 
        # Also prevents the inclusion of 2023 into the calculation of MAP, MAT when there is no data 
        X = X[X.index < '01-01-2023']
        return X
    
class preprocess_climate_time_series(BaseEstimator, TransformerMixin):
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        X = X.copy()
        X = X.sort_values('time')
        X.set_index('time', inplace=True)
        
        return X

class mean_annual_variables_adder(BaseEstimator, TransformerMixin):
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
  
        # calculate mean annual temperature 
        mean_temperature = (X['tmin'] + X['tmax'])/2
        MAT = mean_temperature.mean() # simply the mean temperature of all records 
        print(MAT)
        # calculate mean annual precip 
        precip_yearly_total = X[['year','precip']].groupby('year').sum()
        MAP = precip_yearly_total['precip'].mean()
        # take aggregated sum and then take the mean of those sums
        print(MAP)
        
        # Do assignment 
        X['MAT'] = MAT
        X['MAP'] = MAP 
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
        precip = pd.read_csv(f'../DATASETS/Climate_Gridded/precip/{self.site_location_name}_1980_2022.csv', parse_dates = ['time'], usecols = ['precip','time']).copy()
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
        
        file_name = f'{self.site_location_name}_1980_2022.csv'
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
            
            file_name = f'{self.site_location_name}_1980_2022.csv' # now include new data
            
            # Get temperature  data 
            tmin = pd.read_csv(f'../DATASETS/Climate_Gridded/tmin/{file_name}', usecols = ['tmin', 'time'], parse_dates = ['time']).copy()
            tmax = pd.read_csv(f'../DATASETS/Climate_Gridded/tmax/{file_name}', usecols = ['tmax', 'time'], parse_dates = ['time']).copy()

    
            climate_data = tmin.copy().merge(tmax, left_on = 'time', right_on = 'time')
            climate_data = climate_data.sort_values('time')
 
            temp_col_names = ['tmax_lag','tmax_7', 'tmax_14', 'tmax_30']
            
            # Specify time ranges: 
            ts_lag = [1,1] # t-1
            ts_1 = [2, 7]
            ts_2 = [8, 14]
            ts_3 = [15, 30]
            ts_all = [ts_lag, ts_1, ts_2, ts_3]
            
            # Construct Page's variables
            X = pages_variables_constructor(X, tmax, ts_all, temp_col_names) # for tmax
            
            temp_col_names = ['tmin_lag','tmin_7', 'tmin_14', 'tmin_30']
            X = pages_variables_constructor(X, climate_data[['time','tmin']], ts_all, temp_col_names) # for tmean
                    
            return X
        

class historical_burn_date_preprocess(BaseEstimator, TransformerMixin):
    
    def __init__(self, site_location_name):
        self.site_location_name = site_location_name 
        
        
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        
        # Get a subset of the dataset that has the corresponding site_location_name
        X = X[['ignition_d', 'extinguish', 'site_location_name']]
        X = X[X.site_location_name == self.site_location_name]
  
        # Remove all records with 'None' as the ignititon_d
        X = X.iloc[[pd.isnull(i) is False for i in  X['ignition_d']]]
        
        # Convert to date_time
        X['ignition_d'] = pd.to_datetime(X['ignition_d'])
        X['extinguish'] = pd.to_datetime(X['extinguish'])
        
        # Sort by date 
        X = X.sort_values(by = 'ignition_d')
        X = X.reset_index(drop = True)
        
        # Mention that the dataset is empty if empty
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
        
        if self.historical_fire_ds.empty: # if there is no recorded fire, just give the default value
            X['days_since_fire'] = 365 * 30 
            return X
        
        number_of_records = len(self.historical_fire_ds['ignition_d'])
        
        record_idx = 0 # on the current fire record 
        X['days_since_fire'] = 0 
        for index, row in X.iterrows():
            # check whether or not the record index is the last one from the database
            if (record_idx != number_of_records - 1):
                if index >= self.historical_fire_ds.iloc[record_idx + 1]['ignition_d']: # update record_idx whenever the current date goes pass the next fire record 
                    record_idx += 1
            
            current_fire_date = self.historical_fire_ds.iloc[record_idx]['ignition_d']
            if (index < current_fire_date) and (record_idx == 0):
                X.at[index, 'days_since_fire'] = 365 * 30
            else:
                X.at[index, 'days_since_fire'] = (index - current_fire_date).days
    
        return X
    
    
class historical_burn_date_index_attribute_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, historical_fire_ds, verbose = False, time_range = 3):
        self.verbose = verbose
        self.historical_fire_ds = historical_fire_ds
        
        self.time_range = time_range
 
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        # with the actual fire dates, we can
        # for fire date (1), we can do a rough search with records within 3 months before, and use the extinguish date for after the fire
        #   if there are no extinguish date, do a rough rearch with records within 3 months after 
        #       select first three data points after the fire (b)
        # select first three data points prior to the fire (a)
        # select the first three data points after the fire (b) if there are no extinguish dates 
        # get mean pv of (a) and  the minimum of (b) and take the difference 
        
     X = X.copy()
     
     if self.historical_fire_ds.empty: # if there is no recorded fire, just give the default value 0 
         X['fire_severity'] = 0 
         return X
     
     
     X['fire_severity'] = pd.NA
     #X = X[].astype('Float64')
     
     for i, v in self.historical_fire_ds.iterrows():

        if self.verbose: print(v)
        prior_search = v['ignition_d'] - relativedelta(months = self.time_range)
        
        # Note: I am not including the date of the fire in the search
        ## Get df of prior
        a_df = X[(X.index >= prior_search) & (X.index < v['ignition_d'])]
        a_df = a_df.iloc[-3:, :]
        if self.verbose: print(a_df[['pv_filter','days_since_fire', 'fire_severity']])
        a = a_df['pv_filter'].mean()
        if self.verbose: print(a)
        
        # Check if the extinguish value was record, if not, resort to the three data points after
        if pd.isnull(v['extinguish']) == True:
            if self.verbose: print('Extinguish is Null')
            after_search = v['ignition_d'] + relativedelta(months = self.time_range)
            if self.verbose: print(f'from {v["ignition_d"]} to {after_search}')
            ## Get df of after 
            
            b_df = X[(X.index <= after_search) & (X.index > v['ignition_d'])]
            if self.verbose: print(b_df)
            b_df = b_df.iloc[:3, :]
            if self.verbose: print(b_df[['pv_filter','days_since_fire', 'fire_severity']])
            b = b_df['pv_filter'].min()
            if self.verbose: print(b)
            
        else:
            if self.verbose: print(f'Extinguish date found for {v}')
            after_search = v['extinguish']
            b_df = X[(X.index <= after_search) & (X.index > v['ignition_d'])]
            
            
            if self.verbose: print(b_df[['pv_filter','days_since_fire', 'fire_severity']])
            b = b_df['pv_filter'].min()
        
        fire_severity = a - b
        print(f'{a} and {b}')
        
        if self.verbose: print(f"at {v['ignition_d']}:{fire_severity}")
        
        if i != (len(self.historical_fire_ds) - 1):
            df_selector = (X.index >= v['ignition_d']) & (X.index < self.historical_fire_ds.iloc[i + 1]['ignition_d']) # only go up to the next fire date
        else:
            df_selector = (X.index > v['ignition_d']) # there is no recorded fire date so go up to the very end of the dataset 
            
        #selected_df = X.iloc[df_selector].copy()
        X.loc[df_selector,'fire_severity'] = fire_severity
        
     X['fire_severity'] = X['fire_severity'].replace(pd.NA, 0) # set all records prior to the first fire to 0
     X.loc[X.fire_severity < 0, 'fire_severity'] = 0 # in cases where PV increases after the fire, set to 0
     return X
 

class growth_forms_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, site_dom_veg, growth_forms_selector):
        self.site_dom_veg = site_dom_veg # select only the site's plant growth forms 
        self.growth_forms_selector = growth_forms_selector
    
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        
        # iterate through the list of growth forms to add into the dataset
        for growth_form in self.growth_forms_selector:
            X[growth_form] = self.site_dom_veg[growth_form].values[0]
        
        return X

# Note: this is pretty much the same alg as 'growth forms adder '
class SLGA_soil_atributes_adder(BaseEstimator, TransformerMixin):
    
    def __init__(self, site_SLGA, SLGA_selector):
        self.site_SLGA = site_SLGA # select only the site's plant growth forms 
        self.SLGA_selector = SLGA_selector
    
    def fit(self, X, y=None):
        return self 
    
    def transform(self, X, y=None):
        
        X = X.copy()
        
        # iterate through the list of soil attributes to add into the dataset
        for attribute in self.SLGA_selector:
            X[attribute] = self.site_SLGA[attribute].values[0]
        
        return X
        
    


# =============================================================================
