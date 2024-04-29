# -*- coding: utf-8 -*-
"""
Created on Mon Apr 22 12:35:06 2024

@author: krish
"""

#%% Libraries

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

#%% Functions

#%% Main

growth_type_classification = pd.read_csv('../DATASETS/growth_forms_classification_by_dom_species_2-0-6.csv').copy()

vegetation_type_target = 'Hummock.grass'
site = growth_type_classification[growth_type_classification.vegetation_type == vegetation_type_target]
site_ordered = site.sort_values('scaled_dom', ascending = False)
sites_list = site_ordered['site_location_name'].values[:5]

columns_selector = ['Aquatic', 'Bryophyte', 'Chenopod', 'Cycad', 'Epiphyte',
       'Fern', 'Forb', 'Fungus', 'Grass.tree', 'Heath.shrub', 'Hummock.grass',
       'NC', 'Rush', 'Sedge', 'Shrub', 'Shrub.Mallee', 'Tree.fern',
       'Tree.Mallee', 'Tree.Palm', 'Tussock.grass', 'Vine']

for site_name in sites_list:
    s = site_ordered[site_ordered['site_location_name'] == site_name][columns_selector]
    s = s.transpose()
    s = s.rename(columns = {s.columns[0]:'Percent Cover'})
    s.sort_values('Percent Cover', ascending = False, inplace = True)
    s.plot.bar(ylim = (0,120), title = f'{site_name} Vegetation Percentage Cover')
    
    site_fractions = pd.read_csv(f'../STEP9_DATA_MODELLING_AND_EXPLORATION/Input_DataSet_{site_name}.csv', 
                                 parse_dates = ['time']).copy()
    site_fractions.set_index('time', inplace = True)
                                 
    fig, ax = plt.subplots(nrows = 3, figsize = (15,10))
    fig.suptitle(site_name, fontsize=30)
    TARGET = ['pv_filter','npv_filter', 'bs_filter']                       
    for i,v in enumerate(TARGET):
        site_fractions[v].plot(ax=ax[i], ylim = (0,100), label = v)
        ax[i].legend()
        ax[i].grid()
