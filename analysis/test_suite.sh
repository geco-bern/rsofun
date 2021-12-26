#!/bin/bash

# Rapid development testing. Given too many moving pieces and the workflow
# which suffers this is a script to resolve some of this when working from
# either VS Code, or the terminal in general.
# 
# This code assumes that it is called from the base directory of the
# rsofun project!

# get name of current directory
dir=`pwd`
dir=`basename ${dir}`

if [ $dir = "rsofun" ]
then
  echo "Starting compilation"
  echo "------------------------------------------------------------"
  cd ..
else
 echo "Not in the project main directory, quiting"
 exit 1
fi

echo "Regenerating data"
echo "------------------------------------------------------------"
  
# regenerate the data
Rscript rsofun/data-raw/generate_data.R rsofun/data-raw/CH-LAE_forcing.rda

echo "Compile and install the package"
echo "------------------------------------------------------------"
  
# reinstall the package
R CMD INSTALL rsofun


echo "Run demo code"
echo "------------------------------------------------------------"

# rerun the vignette, custom outputting the images
# to disk
Rscript -e "rmarkdown::render('rsofun/vignettes/lm3ppa_use.Rmd')"

if [ $1 = "true" ]
then

echo "Display results in browser"
echo "------------------------------------------------------------"

if [ `uname` = "Linux" ]
  then
    firefox rsofun/vignettes/lm3ppa_use.html
  else
   open rsofun/vignettes/lm3ppa_use.html
  fi
fi
