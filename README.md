# rsofun

This repository contains the code for the R-package that provides functions for all the routine steps in running the SOFUN model:

- setup of the environment
- preparation of input files
- calibrating model parameters
- running the model
- reading outputs into R
- evaluating outputs (benchmarking)

Get this R-package by 
```sh
install_github("stineb/rsofun")
```

## Environment

Available functions require external programs called by `system()` calls from within R. Required programs are:

- [NCO](http://nco.sourceforge.net/). Is used by `get_pointdata_()` functions (executing the Bash script `"./extract_pointdata_byfil.sh"` with NCO command `ncks`). Before using `rsofun`, install NCO on a Mac by
```sh
brew tap homebrew/science
brew install cdo
```
- [CDO](https://code.mpimet.mpg.de/). Is used by the `proc_ncout_sofun_bysite()` function (executing Bash script `"./proc_output_sofun.sh"` with CDO command `cdo mergetime`). Before using `rsofun`, install CDO on a Mac by
```sh
brew install cdo
```

## Examples

Examples are given in several vignettes.