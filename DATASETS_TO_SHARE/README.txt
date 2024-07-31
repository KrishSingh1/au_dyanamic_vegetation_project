-----------------------------
Author: Krish Karan Singh
Date: 31-07-2024
-----------------------------

Folder Contents and Structure


--> AusPlots_Dominant_Growth_Form
	---> 'growth_forms_classification_by_dom_species_final_2-0-6.csv': the dataset contains the percent cover of growth forms, with the last column identifying the growth form with the maximum percent cover. The original growth form data was called using the following function and arguments with ausplotsR version 2.0.6
		---> growth_form_table(veg_info$veg.PI, m_kind = "percent_cover", cover_type = "PFC", species_name = "SN", cumulative = F)


--> AusPlots_FC
	---> 'AusPlots_FC_Iter_2_0_6.csv': AusPlots FC obtained by iteratively querying FC for every site using ausplotsR 2.0.6 fractional_cover function with default arguments.
		---> Error Column for noting down site observations with missing data, mainly due to data publication:
			---> Error 0: No error
			---> Error 1: Missing veg.PI
			---> Error 2: Fractional_cover returns empty character
			---> Error 3: Partially missing fractional cover data row-wise (missing observations entirely)
			---> Error 4: Partially missing fractional cover data column (Ignore this error, it is recorded incorrectly)


--> AusPlots_FireDates
	--> AusPlots_Combined_Fire_Dataset.csv
		---> Combines MODIS Burn data (MCD64A1, https://lpdaac.usgs.gov/products/mcd64a1v061/) and Historical bushfires data 			(https://digital.atlas.gov.au/datasets/db9ae2c1d2374e20b60f26c45118f6f3_0/explore?location=-21.892570%2C134.777077%2C4.50). 
		---> The last 4 columns are to do with data from MODIS, while the rest are from the Historical Bushfires data. See the links above for further details on the information of the columns.
		---> For both datasets, we utilised polygons derived from preprocessed corner points ('Published Plot Corners_extract26062024_cleaned.csv') to query any intersection between recorded fires in Modis and Historical Bushfires dat and the polygons.
			

--> AusPlots_Location
	---> Published Plot Corners_extract26062024.csv: Provided corner points by AusPlots.
	---> Published Plot Corners_extract26062024_cleaned.csv: Preprocessed corner points, gap-filled missing sites and corner points.
	--->  'Derived_Boundary_Polygons'
		----> Derived Polygons (shapefiles) from 'Published Plot Corners_extract26062024_cleaned.csv'


--> DEA_Evaluation_Plots
	---> General evaluation plots between smoothed DEA FC and AusPlots FC to the nearest available time point.
	---> There are there four main plots as highlighted by the suffix of 'DEA_VS_AUSPLOTS_FC_' for the naming scheme of these plots.
		---> '[FRACTION]': an evaluation plot of remotely sensed FC versus in-situ measured FC
		---> '[FRACTION]_[CHANGE]': an evaluation plot of the change in remotely sensed FC versus the change in in-situ measured FC. Note: for sites revisited more than 2 times, we separately looked at changes from visit 1 to visit 2, visit 2 to visit 3, visit 3 to visit 4, ..., visit n-1 to visit n. 
		---> '[FRACTION]_PER_GROWTHFORM': evaluation plots of remotely sensed FC versus the in-situ measured FC grouped by sites categorised by the dominant growth form.
		---> 'CHANGE_[FRACTION]_PER_GROWTHFORM': evaluation plots of the change in remotely sensed FC versus the change in in-situ measured FC grouped by sites categorised by the dominant growth form.
	---> Files:
		---> DEA_VS_AUSPLOTS_FC_ALL.png
		---> DEA_VS_AUSPLOTS_FC_BARE.png
		---> DEA_VS_AUSPLOTS_FC_BARE_CHANGE.png
		---> DEA_VS_AUSPLOTS_FC_BARE_PER_GROWTHFORM.png
		---> DEA_VS_AUSPLOTS_FC_CHANGE_BARE_PER_GROWTHFORM.png
		---> DEA_VS_AUSPLOTS_FC_BROWN.png
		---> DEA_VS_AUSPLOTS_FC_BROWN_CHANGE.png
		---> DEA_VS_AUSPLOTS_FC_BROWN_PER_GROWTHFORM.png
		---> DEA_VS_AUSPLOTS_FC_CHANGE_BROWN_PER_GROWTHFORM.png
		---> DEA_VS_AUSPLOTS_FC_GREEN.png
		---> DEA_VS_AUSPLOTS_FC_GREEN_CHANGE.png
		---> DEA_VS_AUSPLOTS_FC_GREEN_PER_GROWTHFORM.png
		---> DEA_VS_AUSPLOTS_FC_CHANGE_GREEN_PER_GROWTHFORM.png 


--> DEA_FC_Ground_Truth_Evaluation_File
	---> DEA_FC_Ground_Truth_Evaluation.csv
		---> The csv file that contains site location id information, in situ fraction ('green', 'brown', 'bare'), and the date of visited site 'visit_start_date.' Also, the nearest timestamp of DEA FC ('time'), with smoothed remotely-sensed fractional cover ('[FRACTION]_filter')
		---> the error column can be ignored here, see 'AusPlots_FC' for a description
	---> DEA_FC_Ground_Truth_Evaluation_with_percentiles.csv
		---> The same as DEA_FC_Ground_Truth_Evaluation.csv, however with three extra columns '[FRACTION]_percentile', which are the percentiles of the DEA FC at the nearest timestamp to the site visit. Percentiles are taken from the cumulative empirical distribution function and are in the range of 0-1. 1 is the highest fractional cover value, 0.5 is about the median, 0 is the lowest value.


--> DEA_FC_Linear_Trends_File
	---> AusPlots_Theil_Sen_Regression_Stats.csv
		---> Contains the coefficients (slope and intercept) of the theil-sen regression analysis for smoothed DEA FC.
		---> The regression analysis was performed individually on each fraction per site. The '[FRACTION]_slope_yr' attribute is the approximate slope in units of [FRACTIONAL_COVER_VALUE]/yr (simply scaled '[FRACTION]_slope' by 365).


--> DEA_FC_Smoothed_1987_2022
	---> Contains AusPlots pv, npv, bs, site_location_name, and timestamp of image.
		---> Time series smoothed by Savitzky–Golay filter with a window size of 15 and polynomial order of 4.


--> DEA_Site_Validation_Reports_by_State
	---> Separated per state. Named by: DEA_Extraction_Site_Validation_Report_[STATE ABBREV.].
	---> Contains a list of plots detailing the DEA FC time series, distribution, and information about pixel retrieval for each site belonging to the state.
	---> Each site contains 7 plots and 4 tables:
		---> Plot 1-3: Smoothed remotely sensed pv, npv, bs time series overlaid by AusPlots fc (the x marker), and the theil-sen regression (red line).
		---> Plot 4: The cumulative empirical distribution graph of DEA FC, overlayed by the nearest DEA FC timestamp of site visit.
		---> Plot 5: Number of pixels available per timestamp.
		---> Plot 6: Pixel coordinate locations (red dots) overlaid by AusPlots corner points (blue dots).
		---> Plot 7: Number of available time points per year. The horizontal line ~22 is the expected number of DEA FC samples given that the reported time period is 16 days, when a new satellite image is taken.
		---> Table 1-3: The summary table (quartiles and mean) for DEA FC pv, npv, and bs. Located above each of plot 1-3. 
		---> Table 4: The summary table (quartiles and mean) corresponding to plot 5. Located below plot 5. 
	---> Files:
		---> DEA_Extraction_Site_Validation_Report_NS.docx
		---> DEA_Extraction_Site_Validation_Report_QD.docx
		---> DEA_Extraction_Site_Validation_Report_NT.docx
		---> DEA_Extraction_Site_Validation_Report_WA.docx
		---> DEA_Extraction_Site_Validation_Report_VC.docx
		---> DEA_Extraction_Site_Validation_Report_TC.docx
		---> DEA_Extraction_Site_Validation_Report_SA.docx


--> Site_Info
	---> extracted_Final_site_info_2-0-6.csv
		---> site information retrieved using the ausplotsR 2.0.6 from the function get_ausplots


--> Supporting_Code
	---> 'AusPlots_Corner_Points_Cleaning.R'
		---> Used to clean and preprocess the given corner points 'Published Plot Corners_extract26062024.csv' (see AusPlots_Location)
	---> 'AusPlots_FC_Extraction.R'
		---> Used to extract fractional cover from AusPlots by iteratively querying each site individually, due to some bug causing inconsistent  results when sites are queried by batches (See AusPlots_FC).
	---> 'DEA_Data_Repeated_Sites_Change.R'
		---> Used to produce the evaluation plots for the change in DEA FC versus the change in Ausplots FC (see DEA_Evaluation_Plots).
	---> DEA_Evaluation_Nearest_Point_shared_vers.R
		---> Used to produce the dataset DEA_FC_Ground_Truth_Evaluation.csv (see DEA_FC_Ground_Truth_Evaluation_File).
	---> DEA_FC_Distribution_at_Site_Visit.R
		---> Used to generate DEA_FC_Ground_Truth_Evaluation_with_percentiles.csv (see DEA_FC_Ground_Truth_Evaluation_File).
	---> 'Enquiry_Historical_BushFires.R'
		---> Used to enquiry the Historical BushFires dataset using polygons representing Ausplots sites (see AusPlots_FireDates). Note: the Historical Bushfires dataset needs to be downloaded separately. 
	---> 'Theil-Sen_Regression_shared_vers.R'
		---> Used to generate AusPlots_Theil_Sen_Regression_Stats.csv (see DEA_FC_Linear_Trends_File). 


