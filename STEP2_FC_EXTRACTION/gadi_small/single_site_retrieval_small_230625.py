
####### IMPORT PACKAGES ####### 

# Inbuilt tools 
import numpy as np
import pandas as pd
import os 

# DEA Tools 
import datacube
from datacube.utils import masking
import sys
sys.path.insert(1, '../Tools/')
from dea_tools.datahandling import wofs_fuser
from dea_tools.plotting import rgb, plot_wo

###### Create a directory for dataset ######
dir_name = 'fc_preprocessed_one_small'
path = os.path.join(os.getcwd(), dir_name)

if not os.path.isdir(path): # create directory if doesn't exist
    os.mkdir(dir_name)

###### Create Query #######
## Load in query file (sites coords of interest)
site_info = pd.read_csv("sites_info_query.csv") # get df for sites info 
site_info = site_info.drop(columns = "Unnamed: 0").copy() 


## Define parameters for Query 
RI = 0 # record index
time = ("2023-01-01","2023-04-20") # the pre-determined start-end date (Note: keep at ("1987-09-01","2023-04-20")!!)
resolution = (-10,10) 

extent = 100 # defines the extent of the coordinates 
y1, y2 = site_info["pit_marker_northing"][RI], site_info["pit_marker_northing"][RI] + extent # coords/northing
x1, x2 = site_info["pit_marker_easting"][RI], site_info["pit_marker_easting"][RI] + extent # coords/easting 

mga_zone = site_info['pit_marker_mga_zones'][RI]
output_crs = f'EPSG:283{mga_zone}' # get output_crs based on the zone 

## Create query 
query = {
    'y': (y1, y2),
    'x': (x1, x2),
    'time': time,
    'crs': output_crs
}

print(f"Query ({site_info['site_location_name'][RI]}, Index = {RI})\n{query}") # Print query

###### Retrieve Data and preprocess ######
### Load DEA Fractional Cover data from the datacube
dc = datacube.Datacube(app='DEA_Fractional_Cover') # Call for dataset tools 
fc = dc.load(product='ga_ls_fc_3',
             measurements=['bs', 'pv', 'npv', 'ue'],
             resolution= resolution,
             group_by='solar_day',
             output_crs = output_crs,
             **query)
fc = masking.mask_invalid_data(fc) # turn invalid data into NAN 

### Retrieve additional data for water masking  
wo = dc.load(product='ga_ls_wo_3',
             group_by='solar_day',
             fuse_func=wofs_fuser,
             like=fc)
wo_mask = masking.make_mask(wo.water, dry=True)  # find wet pixels 
fc_masked = fc.where(wo_mask) # turn wet pixels into NAN

###### Export processed as a csv file #######
file_name = f"{site_info['site_location_name'][RI]}.csv" # Simply location_name.csv
file_path = os.path.join(path,file_name)
region = fc_masked.to_dataframe()
df = region.to_csv(file_path)