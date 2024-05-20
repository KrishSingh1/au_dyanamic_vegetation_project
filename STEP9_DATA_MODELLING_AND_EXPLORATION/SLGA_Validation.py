# -*- coding: utf-8 -*-
"""
Created on Tue May 14 11:28:24 2024

@author: krish
"""

#%% Modules 

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 
from scipy import stats


#%% Functions

def plot_boxplot(grade_value, classifications, dataset):
    
    dataset = dataset[dataset['soil.char.texture_grade'] == grade_value]
    
    if len(dataset) > 0:
    
        dataset.boxplot(column = ['slga_clay'], by = ['depth_interval'], figsize = (10,5))
        plt.axhline(y = classifications[grade_value][0], color = 'red', linestyle = 'dashed')
        plt.axhline(y = (classifications[grade_value][0] + classifications[grade_value][1])/2, color = 'black', linestyle = 'dashed')
        plt.axhline(y = classifications[grade_value][1], color = 'red', linestyle = 'dashed')
        plt.suptitle(grade_value) 
        plt.ylim(0, 100)
        plt.show()
        
        

#%% Main

site_slga_data = pd.read_csv('../DATASETS/Soils_and_Landscape_Grid_of_Australia/Output/site_slga_data.csv', index_col = 0).copy()
soils_char = pd.read_csv('../DATASETS/AusPlots_Extracted_Data/extracted_soil_char_2-0-6.csv', index_col = 0).copy()

classifications = {
    'S': [0, 5],
    'LS': [5, 5],
    'CS': [5, 10],
    'SL': [10, 20],
    'L' : [25, 25],
    'ZL': [25, 25],
    'SCL' : [20, 30],
    'CL': [30, 35],
    'CLS' : [30, 35],
    'ZCL' : [30, 35],
    'LC' : [35, 40],
    'LMC' : [40, 45],
    'MC': [45, 55],
    'MHC': [50, 100],
    'HC': [50, 100],
    'NC': [-1, -1]
}
classifications = pd.DataFrame(classifications)

soils_char.loc[soils_char['soil.char.texture_grade'].isna(), 'soil.char.texture_grade'] = 'NC'


cly_min = [] 
cly_max = [] 
for i,v in soils_char.iterrows():
    texture_grade = v['soil.char.texture_grade']
    min_range = np.min(classifications.loc[0, texture_grade])
    max_range = np.max(classifications.loc[1, texture_grade])
    cly_min.append(min_range)
    cly_max.append(max_range)
    

soils_char['cly_min'] = cly_min
soils_char['cly_max'] = cly_max
soils_char['clay_mid'] = (soils_char['cly_min'] + soils_char['cly_max'])/2

soils_char['depth_mid'] = (soils_char['soil.char.upper_depth'] + soils_char['soil.char.lower_depth'])/2 

depth_intervals = []
for i in soils_char['depth_mid']:
    if i > 0.00 and i < 0.05:
        depth_intervals.append('CLY_000_005')
    elif i >= 0.05 and i < 0.15:
        depth_intervals.append('CLY_005_015')
    elif i >= 0.15 and i < 0.30:
        depth_intervals.append('CLY_015_030')
    elif i >= 0.30 and i < 0.60:
        depth_intervals.append('CLY_030_060')
    elif i >= 0.60 and i < 1.00:
        depth_intervals.append('CLY_060_100')
    elif i >= 1.00 and i < 2.00:
        depth_intervals.append('CLY_100_200')
    else:
        depth_intervals.append(-1)

soils_char['depth_interval'] = depth_intervals
soils_char['depth_length'] = abs(soils_char['soil.char.upper_depth'] - soils_char['soil.char.lower_depth'])

soils_char_essential = soils_char[['soil.char.site_location_name', 'soil.char.upper_depth',
                                   'soil.char.lower_depth','depth_mid',
                                   'clay_mid', 'depth_interval', 'depth_length', 
                                   'soil.char.texture_grade']]

soils_char_essential = soils_char_essential[(soils_char_essential['clay_mid'] != -1.0) &
                                            (soils_char_essential['depth_interval'] != -1)]


slga_clay = []
for i, v in soils_char_essential.iterrows():
    #print(v['depth_interval'])
    clay_value = site_slga_data.loc[site_slga_data.index == v['soil.char.site_location_name'], 
                                    v['depth_interval']].values[0]
    slga_clay.append(clay_value)

soils_char_essential['slga_clay'] = slga_clay
soils_char_essential = soils_char_essential[soils_char_essential['slga_clay'].isna() == False]

# Create Boxplots
for i in classifications.keys():
    if i != 'NC':
        plot_boxplot(i, classifications, soils_char_essential)
        
# Create Evaluation Plots
for i in np.unique(soils_char_essential['depth_interval']):
    subset = soils_char_essential[soils_char_essential['depth_interval'] == i]
    interval = subset.depth_interval[subset.index[0]].split('_')
    interval_upper_lower = interval[1:]
    lo = '-'.join(interval_upper_lower) + ' cm'
    n_sites = len(np.unique(subset['soil.char.site_location_name']))
    n_rows = len(subset)
    subset.plot.scatter(x = 'clay_mid', y = 'slga_clay', title = f'CLY% {lo}, n sites = {n_sites}, samples = {n_rows}',
                        ylim = (0,100), xlim = (0,100), 
                        xlabel = "TERN Mean Clay Content (%)", ylabel = "SLGA Mean Clay Content (%)")
    slope, intercept, r, p, se = stats.linregress(x = subset['clay_mid'], y = subset['slga_clay'])
    plt.plot(subset['clay_mid'],intercept + slope*subset['clay_mid'], label='fitted line')
    one_to_one = [i for i in range(101)]
    plt.plot(one_to_one, one_to_one, linestyle = 'dashed')
    plt.annotate(f'$y = {slope:.3f}x + {intercept:.3f}$\n$R^2$ = {r:.3f}\n$p = {p:.5f}$', xy = (10, 80))
    plt.grid(True)


