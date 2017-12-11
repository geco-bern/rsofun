# README #

This describes the steps to execute a standard P-model calibration.

### What is this repository for? ###

* At this stage, this repository provides the functionality to calibrate the globally uniform and temporally constant **apparent quantum yield parameter** `q0_apparent` in the P-model. 
* The calibration method is linear least squares.
* The P-model provides a process-based prediction of photosynthetic parameters and light use efficiency as a function of only climate (temperature, VPD, atmospheric pressure) and CO2. In general, parameters are determined independently from data (see Wang Han et al., 2017 *Nature Plants*). For simulating gross primary productivity (GPP), the P-model requires vegetation greenness, or the fraction of absorbed photosynthetically active radiation (fAPAR), to be prescribed from data (see also [here](https://stineb.github.io/pmodel.html)). 
* Calibrating `q0_apparent` is motivated by the fact that there remains some unclarity about how the photosynthetic quantum yield efficiency relates to the quantity quantified by fAPAR. Hence, we aim for a calibration of this parameter, *given fAPAR data*. The parameter value is to be treated temporally constant and globally uniform, applicable for C3 photosynthesis and has the same role as our $\phi_0$ parameter in the [P-model](https://stineb.github.io/pmodel.html).
* The calibration described here as the default uses:
  * GPP based on the night-time flux decomposition (`NT_VUT_REF`) from 167 sites in the FLUXNET 2015 Tier 1 dataset. Data is read in and cleaned using XXX.
  * fAPAR quantified by MODIS FPAR MCD15A3H (Collection 6) data (4 days, 500 m).
  * `q0_apparent` minimises the sum of square model errors, daily data, filtered to exclude cold days (< 5 deg C) and days with dry soil (relative soil water content < 0.5 of maximum water holding capacity).

### How do I get set up? ###

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact