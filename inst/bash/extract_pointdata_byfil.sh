#!/bin/bash

## Extracts point data from the WFDEI SWdown NetCDF dataset and 
## writes time series into a text file.
## Arguments:
## 1. path to directory where file sits
## 2. file name
## 3. variable name
## 4. longitude dimension name
## 5. latitude dimension name
## 6. longitude value of site (point)
## 7. latitude value of site (point)

# echo "extracting from $1, output is in out.txt"
ncks -s '%13.9f\n' -C -H -d $4,$6 -d $5,$7 -v $3 $1/$2 >./tmp/tmp.txt
sed '/^$/d' ./tmp/tmp.txt >./tmp/out.txt
here=`pwd`
echo "writing to $here/tmp/out.txt"
