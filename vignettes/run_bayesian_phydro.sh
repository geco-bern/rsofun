#!/bin/bash

declare -a arr=( 
	"CH-Dav" 
	"DE-Hai" 
	"DE-Tha" 
	"FR-LBr" 
)


for SITE in "${arr[@]}"
do
	echo $SITE
	(
	generate_data=true

	if [ "$generate_data" = true ]; then
		echo "Generating data for site" $SITE
		Rscript rsofun_phydro_data_generation.R $SITE
	fi

	echo "Running calibration for site" $SITE
	Rscript phydro_long_calibration.R $SITE
	) &
done
wait
