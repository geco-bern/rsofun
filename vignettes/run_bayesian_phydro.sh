#!/bin/bash

NCORES=50

# declare -a arr=$( ls ~/Downloads/fluxdatakit_oct3/FLUXDATAKIT_FLUXNET/*HH* | awk -F "_" '{print $4}' )

declare -a arr=$( cat site_list.txt )

generate_data=true
data_path=/data/scratch/jaideep/fluxdata
out_path=/data/scratch/jaideep/phydro_output

for SITE in ${arr[@]}
do
	echo $SITE
	(
	if [ "$generate_data" = true ]; then
		echo "Generating data for site" $SITE
		Rscript rsofun_phydro_data_generation.R $SITE $data_path > out_data_gen_$SITE.txt
	fi

	echo "Running calibration for site" $SITE
	Rscript phydro_long_calibration.R $SITE $data_path $out_path > out_calib_$SITE.txt
	) &

	# allow to execute up to $NCORES jobs in parallel
    if [[ $(jobs -r -p | wc -l) -ge $NCORES ]]; then
        # now there are $N jobs already running, so wait here for any job
        # to be finished so there is a place to start next one.
        wait -n
    fi

done
wait
