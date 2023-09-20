## script that extracts climate variables of all sites in a site list by executing $extract_script
# start this script with ./extract_all_sites.sh

extract_script="/home/599/jk8585/misc/extract_climate_vars.sh"  # name of script that extracts climate variables for one site
sitelist="/home/599/jk8585/misc/sites_info_query_latlon.csv"    # site list including site name, latitude, and longitude

sites=$(awk -F "," '{ print $2 }' $sitelist | tail -n+2)

for site in $sites ; do

    lat=$(awk -F',' -v site=${site} '{ if ($2 == site) print $6}' $sitelist) 
    lon=$(awk -F',' -v site=${site} '{ if ($2 == site) print $7}' $sitelist) 

    sed -i "s!^site=.*!site=${site}!" $extract_script
    sed -i "s!^lat=.*!lat=${lat}!" $extract_script
    sed -i "s!^lon=.*!lon=${lon}!" $extract_script

    qsub $extract_script
    
done
