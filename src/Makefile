# The following variables are imported from Makefile in ../
# and don't need to be set here explicitly:
#FCOM
#CPPFLAGS
#COMPFLAGS
#DEBUGFLAGS

# Default list of source code files for standard (full) setup (equal to cnmodel):
# using CNMODEL_MODS and CNMODEL_SOURCES

# Source file of main program
MAIN=sofun.f90

MAIN_BIOMEE=main_biomee.f90

## Use different main program source file when executing all simulations belonging to a given simsuite at once
MAIN_SIMSUITE=sofun_simsuite.f90

## Use different main program source file for demo simulation
MAIN_DEMO_PMODEL=demo_pmodel.f90

# List of source code files for reduced setup, executing only SPLASH: 
SPLASH_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml.mod.f90 params_domain_siterun.mod.f90 grid_siterun.mod.f90 params_soil.mod.f90 forcing_siterun_wmodel.mod.f90 interface_biosphere.mod.f90 tile_wmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 biosphere_wmodel.mod.f90

# List of source code files for reduced setup, executing only SPLASH:
SWBM_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml.mod.f90 params_domain_siterun.mod.f90 grid_siterun.mod.f90 params_soil.mod.f90 forcing_siterun_wmodel.mod.f90 interface_biosphere.mod.f90 tile_wmodel.mod.f90 waterbal_swbm.mod.f90 soiltemp_sitch.mod.f90 biosphere_wmodel.mod.f90

# List of source code files for reduced setup, executing SPLASH and P-MODEL:
PMODEL_MODS=params_core_pmodel.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml_pmodel.mod.f90 params_domain_siterun.mod.f90 grid_siterun_pmodel.mod.f90 params_soil_pmodel.mod.f90 forcing_siterun_pmodel.mod.f90 interface_biosphere_pmodel.mod.f90 plant_pmodel.mod.f90 tile_pmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 photosynth_pmodel.mod.f90 gpp_pmodel.mod.f90 vegdynamics_pmodel.mod.f90 biosphere_pmodel.mod.f90

# List of source code files for reduced setup, executing SPLASH and P-MODEL:
DEMO_PMODEL_MODS=params_core.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 interface_demo_pmodel.mod.f90 plant_demo_pmodel.mod.f90 gpp_pmodel.mod.f90 

# List of source code files for reduced setup, executing SPLASH and P-MODEL:
PMODEL_SWBM_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml.mod.f90 params_domain.mod.f90 grid_siterun.mod.f90 params_soil.mod.f90 forcing_siterun_pmodel.mod.f90 interface_biosphere.mod.f90 tile_pmodel.mod.f90 waterbal_swbm.mod.f90 soiltemp_sitch.mod.f90 plant_pmodel.mod.f90 gpp_pmodel.mod.f90 vegdynamics_pmodel.mod.f90 biosphere_pmodel.mod.f90

# List of source code files for reduced setup, executing SPLASH and P-MODEL for global simulations:
GPMODEL_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml.mod.f90 params_domain_global.mod.f90 grid_global.mod.f90 params_soil_global.mod.f90 forcing_global_pmodel.mod.f90 interface_biosphere.mod.f90 tile_pmodel.mod.f90 plant_pmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 gpp_pmodel.mod.f90 vegdynamics_pmodel.mod.f90 biosphere_pmodel.mod.f90

# List of source code files for reduced setup, executing SPLASH for global simulations:
GSPLASH_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml.mod.f90 params_domain_global.mod.f90 grid_global.mod.f90 params_soil_global.mod.f90 forcing_global_wmodel.mod.f90 interface_biosphere.mod.f90 tile_wmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 biosphere_wmodel.mod.f90

# List of source code files for reduced setup, executing SWBM for global simulations:
GSWBM_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 params_siml.mod.f90 params_domain_global.mod.f90 grid_global.mod.f90 params_soil_global.mod.f90 forcing_global_wmodel.mod.f90 interface_biosphere.mod.f90 tile_wmodel.mod.f90 waterbal_swbm.mod.f90 soiltemp_sitch.mod.f90 biosphere_wmodel.mod.f90

# List of source code files for reduced setup, executing the C-only model
CMODEL_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 io_netcdf.mod.f90 rates.mod.f90 params_siml.mod.f90 params_domain_siterun.mod.f90 grid_siterun.mod.f90 params_soil.mod.f90 forcing_siterun_pmodel.mod.f90 interface_biosphere.mod.f90 tile_cmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 lambertw.mod.f90 plant_cmodel.mod.f90 gpp_pmodel.mod.f90 phenology_lpx.mod.f90 vegdynamics_fix.mod.f90 turnover.mod.f90 npp.mod.f90 littersom_lpj.mod.f90 allocation_cmodel.mod.f90 biosphere_cmodel.mod.f90

# List of source code files for 'biomee' setup, using gs_Leuning() photosynthesis (straight implementation of BiomeE-Allocation model code)
BIOMEE_MODS=params_core.mod.f90 sofunutils.mod.f90 classdefs.mod.f90 params_siml_biomee.mod.f90 grid_siterun.mod.f90 params_soil_biomee.mod.f90 forcing_siterun_biomee.mod.f90 interface_biosphere_biomee.mod.f90 datatypes.mod.f90 soil_biomee.mod.f90 photosynth_pmodel.mod.f90 gpp_biomee.mod.f90 vegetation_biomee.mod.f90 biosphere_biomee.mod.f90 #tile_pmodel.mod.f90 plant_pmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 gpp_pmodel.mod.f90 vegdynamics_pmodel.mod.f90 biosphere_pmodel.mod.f90

# List of source code files for 'biomee' setup, using P-model photosynthesis (rest is BiomeE-Allocation model code)
BIOMEE_PMODEL_MODS=params_core.mod.f90 sofunutils.mod.f90 classdefs.mod.f90 params_siml_biomee.mod.f90 grid_siterun.mod.f90 params_soil_biomee.mod.f90 forcing_siterun_biomee.mod.f90 interface_biosphere_biomee.mod.f90 datatypes.mod.f90 soil_biomee.mod.f90 photosynth_pmodel.mod.f90 gpp_biomee.mod.f90 vegetation_biomee.mod.f90 biosphere_biomee.mod.f90 #tile_pmodel.mod.f90 plant_pmodel.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 gpp_pmodel.mod.f90 vegdynamics_pmodel.mod.f90 biosphere_pmodel.mod.f90

# List of source code files for reduced setup, executing the full CN-only model:
CNMODEL_MODS=params_core.mod.f90 classdefs.mod.f90 sofunutils.mod.f90 params_siml.mod.f90 params_domain.mod.f90 grid_siterun.mod.f90 params_soil.mod.f90 forcing_siterun.mod.f90 interface_biosphere.mod.f90 rates.mod.f90 waterbal_splash.mod.f90 soiltemp_sitch.mod.f90 lambertw.mod.f90 plant.mod.f90 gpp_pmodel.mod.f90 phenology_lpx.mod.f90 findroot_fzeroin.mod.f90 turnover.mod.f90 npp.mod.f90 vegdynamics_fix.mod.f90 ntransform_xuri.mod.f90 littersom_lpj.mod.f90 nuptake.mod.f90 allocation_cnmodel.mod.f90 landuse.mod.f90
CNMODEL_SOURCES=sofun.f90 biosphere_cnmodel.f90

# List of build dependencies (Makefile and include files):
DEPEND=Makefile ../Makefile sofun_module_control.inc

# Generate list of .o, .do files from CNMODEL_SOURCES
OBJS=$(CNMODEL_SOURCES:.f90=.o)
MODOBJS=$(CNMODEL_MODS:.f90=.o)
DEBUGOBJS=$(CNMODEL_SOURCES:.f90=.do)
DEBUGMODOBJS=$(CNMODEL_MODS:.f90=.do)

SPLASH_OBJS=$(MAIN:.f90=.o)
SPLASH_MODOBJS=$(SPLASH_MODS:.f90=.o)
DBGSPLASH_OBJS=$(MAIN:.f90=.do)
DBGSPLASH_MODOBJS=$(SPLASH_MODS:.f90=.do)

SWBM_OBJS=$(MAIN:.f90=.o)
SWBM_MODOBJS=$(SWBM_MODS:.f90=.o)
DBGSWBM_OBJS=$(MAIN:.f90=.do)
DBGSWBM_MODOBJS=$(SWBM_MODS:.f90=.do)

PMODEL_OBJS=$(MAIN:.f90=.o)
PMODEL_MODOBJS=$(PMODEL_MODS:.f90=.o)
DBGPMODEL_OBJS=$(MAIN:.f90=.do)
DBGPMODEL_MODOBJS=$(PMODEL_MODS:.f90=.do)

DEMO_PMODEL_OBJS=$(MAIN_DEMO_PMODEL:.f90=.o)
DEMO_PMODEL_MODOBJS=$(DEMO_PMODEL_MODS:.f90=.o)

PMODEL_SWBM_OBJS=$(MAIN:.f90=.o)
PMODEL_SWBM_MODOBJS=$(PMODEL_SWBM_MODS:.f90=.o)
DBGPMODEL_SWBM_OBJS=$(MAIN:.f90=.do)
DBGPMODEL_SWBM_MODOBJS=$(PMODEL_SWBM_MODS:.f90=.do)

GPMODEL_OBJS=$(MAIN:.f90=.o)
GPMODEL_MODOBJS=$(GPMODEL_MODS:.f90=.o)
DBGGPMODEL_OBJS=$(MAIN:.f90=.do)
DBGGPMODEL_MODOBJS=$(GPMODEL_MODS:.f90=.do)

GSWBM_OBJS=$(MAIN:.f90=.o)
GSWBM_MODOBJS=$(GSWBM_MODS:.f90=.o)
DBGGSWBM_OBJS=$(MAIN:.f90=.do)
DBGGSWBM_MODOBJS=$(GSWBM_MODS:.f90=.do)	

GSPLASH_OBJS=$(MAIN:.f90=.o)
GSPLASH_MODOBJS=$(GSPLASH_MODS:.f90=.o)
DBGGSPLASH_OBJS=$(MAIN:.f90=.do)
DBGGSPLASH_MODOBJS=$(GSPLASH_MODS:.f90=.do)	

CMODEL_OBJS=$(MAIN:.f90=.o)
CMODEL_MODOBJS=$(CMODEL_MODS:.f90=.o)
DBGCMODEL_OBJS=$(MAIN:.f90=.do)
DBGCMODEL_MODOBJS=$(CMODEL_MODS:.f90=.do)

BIOMEE_OBJS=$(MAIN_BIOMEE:.f90=.o)
BIOMEE_MODOBJS=$(BIOMEE_MODS:.f90=.o)
DBGBIOMEE_OBJS=$(MAIN_BIOMEE:.f90=.do)
DBGBIOMEE_MODOBJS=$(BIOMEE_MODS:.f90=.do)

BIOMEE_PMODEL_OBJS=$(MAIN_BIOMEE:.f90=.o)
BIOMEE_PMODEL_MODOBJS=$(BIOMEE_PMODEL_MODS:.f90=.o)
DBGBIOMEE_OBJS=$(MAIN_BIOMEE:.f90=.do)
DBGBIOMEE_PMODEL_MODOBJS=$(BIOMEE_PMODEL_MODS:.f90=.do)

SIMSUITE_OBJS=$(MAIN_SIMSUITE:.f90=.o)
SIMSUITE_MODOBJS=$(CMODEL_MODS:.f90=.o)
DBGSIMSUITE_OBJS=$(MAIN_SIMSUITE:.f90=.do)
DBGSIMSUITE_MODOBJS=$(CMODEL_MODS:.f90=.do)	

CNMODEL_OBJS=$(CNMODEL_SOURCES:.f90=.o)
CNMODEL_MODOBJS=$(CNMODEL_MODS:.f90=.o)
DBGCNMODEL_OBJS=$(CNMODEL_SOURCES:.f90=.do)
DBGCNMODEL_MODOBJS=$(CNMODEL_MODS:.f90=.do)

# Archive file names
ARCHIVE=sofun.a
# CDFARCHIVE=./cdfcode/cdf.a

# Export variables that are needed by Makefiles in the subdirectories (called below)
export FCOM CPPFLAGS COMPFLAGS DPCOMPFLAGS DEBUGFLAGS LIBS

# Targets
# -------
# standard source code: (depends on object files)
all:	$(MODOBJS) $(OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(MODOBJS) $(OBJS) 

# code for debugging:
debug:	$(DEBUGMODOBJS) $(DEBUGOBJS)
	# $(MAKE) debug -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DEBUGMODOBJS) $(DEBUGOBJS)

# reduced model setup: only SPLASH
splash: $(SPLASH_MODOBJS) $(SPLASH_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(SPLASH_MODOBJS) $(SPLASH_OBJS) 

# reduced model setup: only SPLASH
dbgsplash: $(DBGSPLASH_MODOBJS) $(DBGSPLASH_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DBGSPLASH_MODOBJS) $(DBGSPLASH_OBJS) 

# reduced model setup: only SWBM
swbm: $(SWBM_MODOBJS) $(SWBM_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(SWBM_MODOBJS) $(SWBM_OBJS) 

# reduced model setup: only SWBM
dbgswbm: $(DBGSWBM_MODOBJS) $(DBGSWBM_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DBGSWBM_MODOBJS) $(DBGSWBM_OBJS) 

# reduced model setup: only SPLASH and PMODEL
pmodel: $(PMODEL_MODOBJS) $(PMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(PMODEL_MODOBJS) $(PMODEL_OBJS) 

demo_pmodel: $(DEMO_PMODEL_MODOBJS) $(DEMO_PMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DEMO_PMODEL_MODOBJS) $(DEMO_PMODEL_OBJS) 

# reduced model setup: only SPLASH and PMODEL
pmodel_simsuite: $(PMODEL_MODOBJS) $(SIMSUITE_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(PMODEL_MODOBJS) $(SIMSUITE_OBJS) 

# reduced model setup: only SPLASH and PMODEL
dbgpmodel: $(DBGPMODEL_MODOBJS) $DBG(PMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DBGPMODEL_MODOBJS) $DBG(PMODEL_OBJS) 

# reduced model setup: only SPLASH and PMODEL
pmodel_swbm: $(PMODEL_SWBM_MODOBJS) $(PMODEL_SWBM_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(PMODEL_SWBM_MODOBJS) $(PMODEL_SWBM_OBJS) 

# reduced model setup: only SWBM and PMODEL
dbgpmodel_swbm: $(DBGPMODEL_SWBM_MODOBJS) $DBG(PMODEL_SWBM_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DBGPMODEL_SWBM_MODOBJS) $DBG(PMODEL_SWBM_OBJS) 

# global simulations, reduced model setup: only SPLASH and PMODEL
gpmodel: $(GPMODEL_MODOBJS) $(GPMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(GPMODEL_MODOBJS) $(GPMODEL_OBJS) 

# global simulations, reduced model setup: only SPLASH
gsplash: $(GSPLASH_MODOBJS) $(GSPLASH_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(GSPLASH_MODOBJS) $(GSPLASH_OBJS) 

# global simulations, reduced model setup: only SWBM
gswbm: $(GSWBM_MODOBJS) $(GSWBM_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(GSWBM_MODOBJS) $(GSWBM_OBJS) 

# reduced model setup: C-model only
cmodel: $(CMODEL_MODOBJS) $(CMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(CMODEL_MODOBJS) $(CMODEL_OBJS) 

# reduced model setup: C-model only
cmodel_simsuite: $(SIMSUITE_MODOBJS) $(SIMSUITE_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(SIMSUITE_MODOBJS) $(SIMSUITE_OBJS) 

# reduced model setup: C-model only
dbgcmodel: $(DBGCMODEL_MODOBJS) $(DBGCMODEL_OBJS) 
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DBGCMODEL_MODOBJS) $(DBGCMODEL_OBJS) 

# Implementation of BiomeE model
biomee: $(BIOMEE_MODOBJS) $(BIOMEE_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(BIOMEE_MODOBJS) $(BIOMEE_OBJS) 

# Implementation of BiomeE model with P-model photosynthesis
biomee_pmodel: $(BIOMEE_PMODEL_MODOBJS) $(BIOMEE_PMODEL_OBJS)
		# $(MAKE) -C cdfcode
		-rm $(ARCHIVE)
		# cp $(CDFARCHIVE) $(ARCHIVE)
		ar r $(ARCHIVE) $(BIOMEE_PMODEL_MODOBJS) $(BIOMEE_OBJS) 	

# full model setup: CN-model
cnmodel: $(CNMODEL_MODOBJS) $(CNMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(CNMODEL_MODOBJS) $(CNMODEL_OBJS) 

# full model setup with double precitions: CN-model
dpcnmodel: $(CNMODEL_MODOBJS) $(CNMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(CNMODEL_MODOBJS) $(CNMODEL_OBJS) 

# full model setup: CN-model
dbgcnmodel: $(DBGCNMODEL_MODOBJS) $(DBGCNMODEL_OBJS)
	# $(MAKE) -C cdfcode
	-rm $(ARCHIVE)
	# cp $(CDFARCHIVE) $(ARCHIVE)
	ar r $(ARCHIVE) $(DBGCNMODEL_MODOBJS) $(DBGCNMODEL_OBJS) 

# # default rules (.f90 -> .f -> .o/.do)
# # ------------
# %.f: %.f90 $(DEPEND)
# 	rm -f $*.f
# 	$(FCOM) $(CPPFLAGS) $*.f90 > $*.f 

$(MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DEBUGOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DEBUGMODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(SPLASH_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(SPLASH_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGSPLASH_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGSPLASH_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(SWBM_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(SWBM_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGSWBM_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGSWBM_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(PMODEL_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(PMODEL_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DEMO_PMODEL_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DEMO_PMODEL_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGPMODEL_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGPMODEL_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(PMODEL_SWBM_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(PMODEL_SWBM_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGPMODEL_SWBM_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGPMODEL_SWBM_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(GPMODEL_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(GPMODEL_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGSWBM_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGSWBM_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(PMODEL_SWBM_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(PMODEL_SWBM_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGSWBM_SWBM_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGSWBM_SWBM_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(GSWBM_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(GSWBM_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(GSPLASH_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(GSPLASH_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(CMODEL_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(CMODEL_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGCMODEL_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGCMODEL_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(BIOMEE_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(BIOMEE_PMODEL_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(BIOMEE_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(SIMSUITE_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(SIMSUITE_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(CNMODEL_MODOBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(CNMODEL_OBJS): %.o: %.f90
	$(FCOM) -c -o $@ $(COMPFLAGS) $*.f90

$(DBGCNMODEL_MODOBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

$(DBGCNMODEL_OBJS): %.do: %.f90
	$(FCOM) -c -o $@ $(DEBUGFLAGS) $*.f90

# # don't delete the intermediate *.f files
# .SECONDARY: $(SOURCES:.f90=.f) $(MAIN:.f90=.f) $(MAIN:.f90=.f) $(MAIN:.f90=.f) $(MAIN:.f90=.f) $(MAIN:.f90=.f) $(MAIN:.f90=.f) $(CNMODEL_SOURCES:.f90=.f)

# clean: remove .f, .o, .do, and .stb files
.PHONY: clean
clean:
	-rm -f *.f *.o *.do *.stb *.mod
#	rm $(ARCHIVE)

#EOF