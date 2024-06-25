####### Script for Obtaining Site Fractional Cover from DEA Fractional Cover Data set, With Polygon ######
# Author: Krish Singh
# Major Contributor: Drew Holzworth
# Date 230731
# Purpose: Obtaining Site Fractional Cover from DEA Fractional Cover Data set 

# Required arguments:
# 0. Input directory name.
# 1. Output directory name.

###### Specify debug mode #######
debug = 2
# 0 represents debug mode 'off' for pbs environment - where it is intended to run on 
# 1 represents debug mode 'on-1' for non pbs environment (e.g. jupyterlabs, jupyternotebook, python terminal etc.)
# 2 represents debug mode 'on-2' for pbs environment 
# Any other value will be ignored and the program will not run by design

## Print messages for debug mode 
def write_to_log(msg):

    if debug == 1: # on non pbs environment 
        print(f'{msg}')

    elif debug == 2: # on pbs environment
        print(f"[node {rank}] {msg}")
    

if debug == 0 or debug == 1 or debug == 2:
    
    
    ####### IMPORT PACKAGES ####### 
    # Inbuilt common python modules  
    import numpy as np
    import pandas as pd
    import os, sys
    import datetime
    from dateutil.relativedelta import relativedelta

    # DEA Tools 
    sys.path.insert(1, '/home/590/ks0104/dea-notebooks/Tools') # uncomment if using the sandbox server 
    import datacube
    from dea_tools.datahandling import wofs_fuser
    from datacube.utils import masking
    
    import geopandas as gpd
    import odc.geo.xr
    from odc.geo.geom import Geometry

    if debug == 0 or debug == 2: # run this code when debug is 'off' or 'on-2' (pbs environment)
      
      # load mpi and insert DEA tools from my directory   
      from mpi4py import MPI
      #sys.path.insert(1, '/home/590/ks0104/dea-notebooks/Tools')
    
      comm = MPI.COMM_WORLD 
      input_path = sys.argv[1]  
      output_path = sys.argv[2] 

      # Run directories are numbered 1..N but rank is 0..N-1
      rank = comm.Get_rank() + 1 

      write_to_log(f"MPI rank={rank}") 
      write_to_log(f"input_path='{input_path}'")  
      write_to_log(f"output_path={output_path}") 
    
      os.chdir(os.path.join(input_path, f"run{rank}")) 

      if not os.path.isdir(output_path): # create directory if doesn't exist
        os.makedirs(output_path)

    elif debug == 1: # run this code when debug is 'on-1' (non-pbs environment)

      # change current directory to the query folder to access query files
      input_path = os.getcwd()
      output_path_name = 'fc_processed_one_small'
      output_path = os.path.join(input_path, output_path_name)

      if not os.path.isdir(output_path): # create directory if doesn't exist
        os.makedirs(output_path)

      os.chdir(input_path)


    ###### Create Query #######
    ## Load in query file (sites coords of interest)
    site_info = gpd.read_file('AusPlots_Merged_Polygons_20240624.geojson')

    ###### Set up spatiotemporal extents:
    
    resolution = (-30,30) # the spatial resolution by 30m by 30m, the default
    
    # Set the start and end date 
    start_date = datetime.date(1987, 9, 1)
    end_date = datetime.date(2023, 1, 1) # the lower bound of the end date increment 
    ## e.g. (2023,3,20) will enquiry all data up to (2023,4,20)
    # Note: keep query at ("1987-09-01","2023-04-20")!
    
    #Apply debug mode (1/1): reducing the spatiotemporal extent
    if debug == 1 or debug == 2:
        end_date = datetime.date(1993, 1, 1) 
    
    ###### Get time increments ##### 
    
    a_time = start_date 
    b_time = start_date
    times_query = [] # store time increments

    # get queries that run from start time to end time in increments of 1 month  
    while b_time <= end_date:
    
      if a_time <= end_date - relativedelta(months=1): 
        b_time = a_time + relativedelta(months=1) 
      else:
        b_time = end_date + relativedelta(months=1) 
    
      time = (a_time.strftime("%Y-%m-%d"),b_time.strftime("%Y-%m-%d")) 
      times_query.append(time)    
      write_to_log(f'{time}')
      a_time = b_time + relativedelta(days=1) # increase lower bounds by 1 day for the next time interval to avoid overlap with the current time interval upper bounds


    ###### Initiaise columns of the exported dataset ######
    
    columns = ['time', 'x', 'y', 'bs', 'pv', 'npv', 'ue', 'spatial_ref']
    # time: the recorded time (in YYYY-MM-DD HH:MM:SS.MS...) when this data was captured
    # x: easting 
    # y: northing
    # bs, pv, npv: spectral fractional cover value  
    # spatial_ref: the crs codes for the coordinates 


    ###### Run Query ######
    ## Define parameters for Query 
    #RI - record index
    #for RI in range(len(site_info)):  # (i.e, for each site in query dataset)
    for RI in range(1):  # (i.e, for each site in query dataset)
        
      geom = Geometry(geom=site_info.iloc[RI].geometry, crs=site_info.crs) # get the geometry of the site 

      sites_results_df = pd.DataFrame(columns=columns)

      for time in times_query:

        # Create query 
        output_crs = 'EPSG:3577' # I am using the default EPSG of the dataset 
        query = {
            "geopolygon": geom,
            'time': time,
        }

        write_to_log(f"Start Query ({site_info['site_location_name'][RI]}, Index = {RI})\n{query}") # write_to_log query

        # Retrieve Data and preprocess 
        # Load DEA Fractional Cover data from the datacube
        dc = datacube.Datacube(app='DEA_Fractional_Cover') # Call for dataset tools 
        fc = dc.load(product='ga_ls_fc_3',
                   measurements=['bs', 'pv', 'npv', 'ue'],
                   resolution= resolution,
                   group_by='solar_day',
                   **query)

        fc = masking.mask_invalid_data(fc) # turn invalid data into NAN 

        if fc: # check if the query was found, (i.e., the retrieved data is not null)
            try:
              # Retrieve additional data for water masking  
              wo = dc.load(product='ga_ls_wo_3',
                           group_by='solar_day',
                           fuse_func=wofs_fuser,
                           like=fc)  
                               
              if wo: # check if the query was found, (i.e., the retrieved data is not null)
                wo_mask = masking.make_mask(wo.water, dry=True)  # find wet pixels 
                fc_masked = fc.where(wo_mask).broadcast_like(fc) # turn wet pixels into NAN
  
                # Append to the site result 
                region = fc_masked.to_dataframe().reset_index()
                sites_results_df = pd.concat([sites_results_df, region])
  
                write_to_log(f"Query (sucessful!) ({site_info['site_location_name'][RI]}, Index = {RI})\n{query})") # write_to_log query
            except:
              write_to_log(f"Query (unsucessful) ({site_info['site_location_name'][RI]}, Index = {RI})\n{query})") # write_to_log query

      # Export site data as a csv file
      file_name = f"{site_info['site_location_name'][RI]}.csv" # Simply location_name.csv
      file_path = os.path.join(output_path,file_name)
      sites_results_df.reset_index(drop= True).to_csv(file_path, index = False) # Reset the index, then export as csv

      write_to_log(f"End Export ({site_info['site_location_name'][RI]})") # write_to_log query
    
