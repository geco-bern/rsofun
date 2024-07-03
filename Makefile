############################
# SOFUN master Makefile
# adopted from LPX Makefile
############################

##################################
# Select configuration profile
# Set PROFILE to one of benilaptop, cx1, euler, pgi, or beniimac
##################################
PROFILE=benilaptop

##################
# pgf profile
##################
ifeq ($(PROFILE),benilaptop)

	# Compiler and options
	FCOM=gfortran
	CPPFLAGS=-cpp -E
	#COMPFLAGS=-g -O1 -ffree-line-length-0 -fbacktrace -ffpe-trap=invalid,zero,overflow -pedantic-errors # For biomee compilation, use -O0 or -O1 (not -O2 or above)
    # COMPFLAGS=-g -O2 -fdefault-real-8 -ffree-line-length-0 -fbacktrace -ffpe-trap=invalid,zero,overflow -pedantic-errors # double precision by default
	 COMPFLAGS=-g -O0 -ffree-line-length-0 -fbacktrace -ffpe-trap=invalid,zero,overflow -Wall -Wextra -fcheck=all -fbacktrace # for debug setup

	# COMPFLAGS=-g -O0 -r8 -Mextend -Mbounds -Minfo -Minform=inform -Kieee -Ktrap=fp -Mfreeform  # debug flags, real8
	#COMPFLAGS= -Mextend -Mdalign -Kieee -Ktrap=fp -O2 -Mprof=lines # to analyze computation time by subroutines
	# DEBUGFLAGS=-g -O0 -Mextend -Mbounds -Minfo -Minform=inform -Kieee -Ktrap=fp -Mfreeform

	# # System libraries
	# LIBS = -L $(NETCDF_LIB) -lnetcdf -lnetcdff

	# On Beni's laptop, use this (display by nc-config --libdir and nc-config --includedir)
	NETCDF_INC = /opt/local/include
	NETCDF_LIB = /opt/local/lib

	# LIBS = -L $(NETCDF_LIB) -lgfortran #-lnetcdf -lnetcdff  # avoiding netcdf library
	LIBS = -L $(NETCDF_LIB) -lgfortran #-lnetcdf -lnetcdff 

else

	ifeq ($(PROFILE),cx1)

		# Compiler and options
		FCOM=ifort
		CPPFLAGS=-e -fpp -preprocess_only -E
		COMPFLAGS=-O3 -xSSE4.2 -axAVX,CORE-AVX-I,CORE-AVX2 -extend_source -free -g -traceback ##-r8 -i4 -align -pc64 -fp-model strict 
		DEBUGFLAGS=-O3 -xSSE4.2 -axAVX,CORE-AVX-I,CORE-AVX2 -extend_source -free -warn all -implicitnone -g -traceback -fpe0 -fpstkchk -CU

		# On Imperial CX1, use this:
		NETCDF_INC = /apps/netcdf/4.0.1-mcmodel-medium/include
		NETCDF_LIB = /apps/netcdf/4.0.1-mcmodel-medium/lib -lnetcdf

		LIBS = -L$(NETCDF_LIB) -lnetcdf

	else

		ifeq ($(PROFILE),euler)

			# gfortran compiler
			FCOM=gfortran
			CPPFLAGS=-cpp -E
			COMPFLAGS=-g -O2 -ffree-line-length-0 -fbacktrace -ffpe-trap=invalid,zero,overflow ## for normal setup

			# On ETH Scicomp EULER
			NETCDF_INC = /cluster/apps/netcdf/4.3.2/x86_64/gcc_4.8.2/serial/include
			NETCDF_LIB = /cluster/apps/netcdf/4.3.2/x86_64/gcc_4.8.2/serial/lib

			LIBS = -L $(NETCDF_LIB) -lgfortran #-lnetcdf -lnetcdff # On Beni's laptop

		else

			ifeq ($(PROFILE),beniimac)

				FCOM=gfortran
				CPPFLAGS=-cpp -E
				COMPFLAGS=-g -O2 -ffree-line-length-0 -fbacktrace -ffpe-trap=invalid,zero,overflow ## for normal setup

				NETCDF_INC = /opt/local/include
				NETCDF_LIB = /opt/local/lib

				LIBS = -L $(NETCDF_LIB) -lnetcdf -lnetcdff -lgfortran # On Beni's work computer

			else

				ifeq ($(PROFILE),pgi)
	
					# Compiler and options
					FCOM=pgf95 
					CPPFLAGS=-E
					COMPFLAGS=-r8 -Mextend -Mfreeform -Mdalign -Kieee -Ktrap=fp -O2
					# COMPFLAGS=-g -O0 -r8 -Mextend -Mbounds -Minfo -Minform=inform -Kieee -Ktrap=fp -Mfreeform  # debug flags, real8
					#COMPFLAGS= -Mextend -Mdalign -Kieee -Ktrap=fp -O2 -Mprof=lines # to analyze computation time by subroutines
					DEBUGFLAGS=-g -O0 -Mextend -Mbounds -Minfo -Minform=inform -Kieee -Ktrap=fp -Mfreeform

				    # System libraries
					LIBS = -L $(NETCDF_LIB) -lnetcdf -lnetcdff

				else				

					# Error: select valid profile
					$(error 'ERROR. Select a valid configuration profile in the Makefile (e.g. PROFILE=gfor).')

				endif

			endif

		endif		

	endif	

endif


####################
# general config ##
####################

# Add flags for MPI parallelization (enable the following lines when the parallel_mpi feature is turned on)
#LIBS += $(shell mpif90 --showme:link)
#COMPFLAGS += $(shell mpif90 --showme:compile)
#DEBUGFLAGS += $(shell mpif90 --showme:compile)

# Add library include files to compiler flags
COMPFLAGS += -I$(NETCDF_INC)
DEBUGFLAGS += -I$(NETCDF_INC)

# name of executable
EXE                 = runsofun
SPLASH_EXE          = runsplash
SWBM_EXE            = runswbm
BIOMEE_EXE          = runbiomee
BIOMEE_PMODEL_EXE   = runbiomee_pmodel
PMODEL_EXE          = runpmodel
PMODEL_DEMO_EXE     = rundemo_pmodel
PMODEL_SWBM_EXE     = runpmodel_swbm
PMODEL_SIMSUITE_EXE = runpmodel_simsuite
GPMODEL_EXE         = rungpmodel
GSPLASH_EXE         = rungsplash
GSWBM_EXE           = rungswbm
CMODEL_EXE          = runcmodel
TMODEL_EXE          = runtmodel
CNMODEL_EXE         = runcnmodel
CMODEL_SIMSUITE_EXE = runcmodel_simsuite

ARCHIVES= ./src/sofun.a

# Export variables that are needed by Makefiles in the subdirectories (called below)
export FCOM CPPFLAGS COMPFLAGS DEBUGFLAGS LIBS

# Targets
# -------
standard: 
	 $(MAKE) -C src
	 $(FCOM) -o $(EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)
	 
#  include libraries when necessary
#	 $(FCOM) -o $(EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# code for debugging:
debug: 
	$(MAKE) debug -C src
	$(FCOM) -o $(EXE) $(DEBUGFLAGS) $(ARCHIVES) #$(LIBS)

# reduced model setup: only water balance model, following SPLASH
splash: 
	 $(MAKE) splash -C src
	 $(FCOM) -o $(SPLASH_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: only water balance model, following SWBM
swbm: 
	 $(MAKE) swbm -C src
	 $(FCOM) -o $(SWBM_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: only SPLASH and PMODEL
pmodel: 
	 $(MAKE) pmodel -C src
	 $(FCOM) -o $(PMODEL_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: fixed allocation, no litter, soil and inorganic C and N dynamics
pmodel_simsuite: 
	 $(MAKE) pmodel_simsuite -C src
	 $(FCOM) -o $(PMODEL_SIMSUITE_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: only SPLASH and PMODEL
dbgpmodel: 
	 $(MAKE) pmodel -C src
	 $(FCOM) -o $(PMODEL_EXE) $(DEBUGFLAGS) $(ARCHIVES) $(LIBS)

# demo for P-model, simplest setup
demo_pmodel:
	$(MAKE) demo_pmodel -C src
	$(FCOM) -o $(PMODEL_DEMO_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

pmodel_swbm: 
	 $(MAKE) pmodel_swbm -C src
	 $(FCOM) -o $(PMODEL_SWBM_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# global P-model for lonlat simulations (doesn't necessarily need to cover the whole globe)
gpmodel: 
	 $(MAKE) gpmodel -C src
	 $(FCOM) -o $(GPMODEL_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# global W-model (SWBM water balance) for lonlat simulations (doesn't necessarily need to cover the whole globe)
gswbm: 
	 $(MAKE) gswbm -C src
	 $(FCOM) -o $(GSWBM_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# global W-model (SPLASH water balance) for lonlat simulations (doesn't necessarily need to cover the whole globe)
gsplash: 
	 $(MAKE) gsplash -C src
	 $(FCOM) -o $(GSPLASH_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: fixed allocation, no litter, soil and inorganic C and N dynamics
cmodel: 
	 $(MAKE) cmodel -C src
	 $(FCOM) -o $(CMODEL_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: fixed allocation, no litter, soil and inorganic C and N dynamics
cmodel_simsuite: 
	 $(MAKE) cmodel_simsuite -C src
	 $(FCOM) -o $(CMODEL_SIMSUITE_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# implementation of the BiomeE-Allocation model  
biomee:
	 $(MAKE) biomee -C src
	 $(FCOM) -o $(BIOMEE_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# implementation of the BiomeE-Allocation model  
biomee_pmodel:
	 $(MAKE) biomee_pmodel -C src
	 $(FCOM) -o $(BIOMEE_PMODEL_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# reduced model setup: fixed allocation, no litter, soil and inorganic C and N dynamics
tmodel: 
	 $(MAKE) tmodel -C src
	 $(FCOM) -o $(TMODEL_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# full model setup
cnmodel: 
	 $(MAKE) cnmodel -C src
	 $(FCOM) -o $(CNMODEL_EXE) $(COMPFLAGS) $(ARCHIVES) $(LIBS)

# clean: remove exe and .o and .do files
.PHONY: clean
clean:
	-rm $(EXE) $(SPLASH_EXE) $(SWBM_EXE) $(PMODEL_EXE) $(GPMODEL_EXE) $(GSWBM_EXE) $(GSPLASH_EXE) $(CMODEL_EXE) $(TMODEL_EXE) $(CNMODEL_EXE) $(CALIB_EXE) $(CMODEL_SIMSUITE_EXE) $(PMODEL_SIMSUITE_EXE) $(BIOMEE_EXE)  $(BIOMEE_PMODEL_EXE)
	$(MAKE) clean -C src
# include libraries when necessary
#	$(MAKE) clean -C lpj/cdfcode

#EOF