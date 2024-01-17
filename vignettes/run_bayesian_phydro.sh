#!/bin/bash

NCORES=8

# declare -a arr=$( ls ~/Downloads/fluxdatakit_oct3/FLUXDATAKIT_FLUXNET/*HH* | awk -F "_" '{print $4}' )

declare -a arr=$( cat site_list.txt )

generate_data=false

for SITE in "US-Ho2" # ${arr[@]}
do
	echo $SITE
	(
	if [ "$generate_data" = true ]; then
		echo "Generating data for site" $SITE
		Rscript rsofun_phydro_data_generation.R $SITE
	fi

	echo "Running calibration for site" $SITE
	Rscript phydro_long_calibration.R $SITE
	) &

	# allow to execute up to $NCORES jobs in parallel
    if [[ $(jobs -r -p | wc -l) -ge $NCORES ]]; then
        # now there are $N jobs already running, so wait here for any job
        # to be finished so there is a place to start next one.
        wait -n
    fi

done
wait
