#!/bin/bash

# Script to extract climate data for individual sites
# see also https://code.mpimet.mpg.de/boards/2/topics/301 

#PBS -N extract_agcd_site
#PBS -P x45
#PBS -q normal
#PBS -l walltime=01:00:00
#PBS -l mem=16GB
#PBS -l ncpus=1
#PBS -l storage=gdata/vl59+gdata/zv2+gdata/x45
#PBS -l software=netCDF:MPI:Intel
#PBS -r y
#PBS -l wd
#PBS -j oe
#PBS -S /bin/bash

## Inputs
site="NSTSYB0005"
lat=-33.6479138888889
lon=150.319194444444

module load cdo/2.0.5
module load nco/5.0.5

metpath="/g/data/zv2/agcd/v1-0-1"
outpath="/g/data/vl59/ausplots_agcd"


startyear=1987
endyear=2022

vars="precip"
#vars="precip tmax tmin vapourpres_h09 vapourpres_h15"

for var in $vars; do
    mkdir -p ${outpath}/${var}
    if [[ "${var}" == "precip" ]] ; then
        fname="total"
    else 
        fname="mean"
    fi
    for ((year=${startyear};year<=${endyear};year++)) ; do
        cdo remapnn,lon=${lon}/lat=${lat} ${metpath}/${var}/${fname}/r005/01day/agcd_v1-0-1_${var}_${fname}_r005_daily_${year}.nc ${outpath}/${var}/${site}_${year}_out.nc
    done
    cdo mergetime ${outpath}/${var}/${site}_????_out.nc ${outpath}/${var}/${site}_${startyear}_${endyear}.nc
    rm ${outpath}/${var}/${site}_*_out.nc
done

