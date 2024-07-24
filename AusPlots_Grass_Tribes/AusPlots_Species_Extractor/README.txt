This folder contains:

---> 'Input': The input datasets used to generate the percent cover of the grass tribes Chloridoideae and Andropogoneae

	--> 'site_veg_Final2-0-6.rds': site information, structural summaries, soil information, vegetation point intercepts and species. Saved 
		as a rds, which can be read into R via 'site_info <- readRDS('Input/site_veg_Final2-0-6.rds')'. Further information about the data is included in 'https://cran.r-project.org/web/packages/ausplotsR/ausplotsR.pdf' on pp. 13-14. 

	--> 'Grass_Tribes.csv':  Includes the provided species list read into a excel spreadsheet and converted to a csv file.

	--> 'Grass_Tribes_Cleaned.csv': Cleaned and preprocessed 'Grass_Tribes.csv', renaming columns and removing empty rows. 

	--> 'FCOV30_C4_fraction.tif': [ENTER DESCRIPTION HERE]


---> 'Output': The generated percent cover of the grass tribes Chloridoideae and Andropogoneae datasets and generated map visualisations of Chloridoidae and Andropogoneae.

	--> 'Andropogoneae Map.png': The image file of a map showing the percent cover of Andropogoneae across AusPlots Sites in Australia. This percent cover overlays the C4 Fraction map 'FCOV30_C4_fraction.tif'. This image was generated through 'Grass_Cover_Geovisualisation_qgis_Andro_file.qgz' using the datasets 'ausplots_grass_cover_by_tribe_agg.csv'. Also includes a base map layer 'OpenStreetMap' (https://www.openstreetmap.org/#map=4/-30.86/115.14) already integrated in qgis. 
	
	--> 'Chloridoideae Map.png': The image file of a map showing the percent cover of Chloridoideae across AusPlots Sites in Australia. This percent cover overlays the C4 Fraction map 'FCOV30_C4_fraction.tif'. This image was generated through 'Grass_Cover_Geovisualisation_qgis_Andro_file.qgz' using the datasets 'ausplots_grass_cover_by_tribe_agg.csv'. Also includes a base map layer 'OpenStreetMap' (https://www.openstreetmap.org/#map=4/-30.86/115.14) already integrated in qgis. 

	--> 'ausplots_species_cover_tribe_labelled.csv': Contains all species (rows) percent cover by site visit (columns). The last column 'tribe_name' is the label that categorizes the species fraction as either Andropogoneae or Chloridoideae or 'non_applicable/no_tribe'.

	--> 'ausplots_grass_cover_by_tribe.csv': Contains data of the percent cover of Andropogoneae and Chloridoideae for each site visit. 'site_unique'
		are unique for each site visit (site_location_name-[visit_id]), while 'site_location_name' is the name of the site that was visited. Latitude and longitude coordinates are provided as well.

	--> 'ausplots_grass_cover_by_tribe_agg.csv': Contains aggregated data of the percent cover of Andropogoneae, Chloridoideae, and non_applicable/no_tribe for each visit grouped by the site location name. Basically 'ausplots_grass_cover_by_tribe.csv' but aggregated by 'site_location_name', which the mean percent cover was taken per aggregate. 

	--> 'Grass_Cover_Geovisualisation_qgis_Andro_file.qgz': The QGIS project file used to produce 'Andropogoneae Map.png'. If you have it opened on QGIS, see 'Project > Layout Manager > Andro_Grass_Map', for the map. However, you can normally interact with this map without access at the image. Data used: 'FCOV30_C4_fraction.tif' and 'ausplots_grass_cover_by_tribe_agg.csv.'

	--> 'Grass_Cover_Geovisualisation_qgis_Chlor_file.qgz': The QGIS project file used to produce 'Chloridoideae Map.png'. If you have it opened on QGIS, see 'Project > Layout Manager > Chlor_Grass_Map', for the map. However, you can normally interact with this map without access at the image. Data used: 'FCOV30_C4_fraction.tif' and 'ausplots_grass_cover_by_tribe_agg.csv.'
		
		

	