Folder Contents and Structure

This folder contains essential data files and scripts, along with additional 'secondary' or dependent scripts and datasets for context. More scripts/datasets can be provided upon request for a complete context. Below is a detailed description of the contents:

AusPlots_FireDates

---> Combines MODIS Burn data (MCD64A1, https://lpdaac.usgs.gov/products/mcd64a1v061/) and Historical bushfires data (https://digital.atlas.gov.au/datasets/db9ae2c1d2374e20b60f26c45118f6f3_0/explore?location=-21.892570%2C134.777077%2C4.50). 
	---> Utilizes polygons derived from preprocessed corner points to query any intersection between fires and the polygons.
	---> Collects associated fire dates.
---> Relevant Data Files: Located in 'AusPlots_Location'
---> Relevant Script: 'Enquiry_Historical_BushFires.R'

AusPlots_Location
	---> Three files:
		1. 'Published Plot Corners_extract26062024.csv': Provided corner points by AusPlots.
		2. 'Published Plot Corners_extract26062024_cleaned.csv': Preprocessed corner points, gap-filled missing sites and corner points.
			---> Relevant Script: 'AusPlots_Corner_Points_Cleaning.R'
		3. Derived Polygons (shapefiles) from 'Published Plot Corners_extract26062024_cleaned.csv'

DEA_Evaluation_Plots
	---> General evaluation plots between smoothed DEA FC and AusPlots FC to the nearest available time point.
	---> Relevant Data Files:
		---> 'DEA_FC_Ground_Truth_Evaluation_File.csv':
			---> Columns 1-6: AusPlots data
			---> Columns 7+: DEA data
		---> 'growth_forms_classification_by_dom_species_final_2-0-6.csv': Sites classified by the dominant growth form.
		---> 'AusPlots_FC_Iter_2_0_6.csv': AusPlots FC obtained by iteratively querying FC for every site.
			---> Error Column:
				---> Error 0: No error
				---> Error 1: Missing veg.PI
				---> Error 2: Fractional_cover returns empty character
				---> Error 3: Partially missing fractional cover data row-wise (missing observations entirely)
				---> Error 4: Partially missing fractional cover data column (Ignore this error, it is recorded incorrectly)
			---> Produced by Script: 'AusPlots_FC_Extraction.R'
		---> Relevant Script: 'DEA_Evaluation_Nearest_Point_shared_vers.R'

DEA_FC_Smoothed_1987_2022
	---> Contains AusPlots pv, npv, bs, site_location_name, and timestamp of image.
		---> Time series smoothed by Savitzky–Golay filter with a window size of 15 and polynomial order of 4.

DEA_Site_Validation_Reports_by_State
---> Separated per state. Each site contains 6 plots:
	---> Smoothed remotely sensed pv, npv, bs time series overlaid by AusPlots fc (1-3).
	---> Number of pixels available per timestamp (4).
	---> Pixel coordinate locations (red dots) overlaid by AusPlots corner points (5).
	---> Number of available time points per year (6).

