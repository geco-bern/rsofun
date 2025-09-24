# NOTE: This R script is called by the debugger (launch.json or through CLI) and
#       the function check_NA_output() must be modified as a reproducible example
#       showcasing the issue to be debugged

# ================================================================= ----
# Further reading: 
# Following: https://blog.davisvaughan.com/posts/2022-03-11-using-vs-code-to-debug-r-packages-with-c-code/#setup-function
# which is a detailed description (but only for mac using llvm)
# and: https://www.maths.ed.ac.uk/~swood34/RCdebug/RCdebug.html
# and: https://tdhock.github.io/blog/2019/gdb/

# these links: https://github.com/renkun-ken/vscode-rcpp-demo and https://github.com/renkun-ken/vscode-cpp11-demo
# provide an alternative example project (that I have not tested)

# ================================================================= ----
# Method 1) Debug this locally in VSCode by launching the debugger through 'launch.json' and set breakpoints
# Method 2)
# To use the exact same configuration as in the GitHub actions use: nektos/act or its implementation in form of 
#    a VSCode extension 'GitHub Local Actions', making use of Docker containers.
#    On the CLI: a) run: ~/bin/act -r -W '.github/workflows/test-coverage-debug.yaml' 
#        and b) connect: docker exec -it `docker ps -q | head -n1` bash
#        and c) debug code in Virtual Machine
#    Or (recommended) through VSCode extension:
#        a) GitHub Local Actions:               make sure to activate the Option reuse=true and run the container
#        b) Remote Explorer and Dev Containers: connect VSCode to the running container 
#        c) debug code in Virtual Machine
#    For both approaches step c) requires to install on the virtual machine:
#       GDB: sudo apt-get install gdb
#       FortLS: pipx install fortls
#       and the necessary VSCode extensions incl. path to fortls /root/.local/bin/fortls instead of /home/fabian/.local/bin/fortls). 
# ================================================================= ----


# # Below do 
# devtools::clean_dll()
# devtools::load_all()
# # to re-build and re-install the R pkg from the working directory.
# # Then modify the code in check_NA_output() to create a reproducibe example showcasing the bug.
# # Launch the debugger with launch.json and set breakpoints.


check_NA_output <- function(flag = TRUE){
  if(flag){
    # DOING THIS GIVES NAN: 
    devtools::clean_dll() |> print()
    withr::with_options(list(),
                        devtools::load_all())
    # likely because it builds rsofun with:  
    #       gfortran  -fpic  -g -O2 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -ffile-prefix-map=/build/r-base-4sITk6/r-base-4.4.2=. 
    #       -fstack-protector-strong -fstack-clash-protection -fcf-protection -fdebug-prefix-map=/build/r-base-4sITk6/r-base-4.4.2=/usr/src/r-base-4.4.2-1.2404.0  
    #       -g -O0 -c  params_core.mod.f90 -o params_core.mod.o
    # NOTE HOW THIS IS *having* -g -O0 on the last line before -c
    # see here: https://forum.posit.co/t/getting-rid-of-debug-flags-in-devtools-load-all/186624
  } else {
    # DOING THIS GIVES NO NAN: 
    devtools::clean_dll() |> print()
    withr::with_options(list(pkg.build_extra_flags = FALSE),
                        devtools::load_all())
    # likely because it builds rsofun with:  
    #       gfortran  -fpic  -g -O2 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -ffile-prefix-map=/build/r-base-4sITk6/r-base-4.4.2=. 
    #       -fstack-protector-strong -fstack-clash-protection -fcf-protection -fdebug-prefix-map=/build/r-base-4sITk6/r-base-4.4.2=/usr/src/r-base-4.4.2-1.2404.0  
    #       -c  params_core.mod.f90 -o params_core.mod.o
    # NOTE HOW THIS IS *not having* -g -O0 on the last line before -c
  }

  # THIS YIELDS NA UNDER CERTAIN CONDITIONS
  params_modl_phydro <- list(
    kphio              = 0.04998,
    kphio_par_a        = 0.01,       # set to zero to disable temperature-dependence of kphio
    kphio_par_b        = 1.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    phydro_K_plant     = 5e-17,   # TODO: add documentaiton: Phydro: Plant conductivity                
    phydro_p50_plant   = -0.46,   # TODO: add documentaiton: Phydro: Plant P50               
    phydro_b_plant     = 1,       # TODO: add documentaiton: Phydro: shape parameter of vulnerability curve           
    phydro_alpha       = 0.08,    # TODO: add documentaiton: Phydro: Cost of Jmax              
    phydro_gamma       = 0.065,   # TODO: add documentaiton: Phydro: Cost of hydraulics               
    bsoil              = 3,       # TODO: add documentaiton: Phydro: parameter converting RZWSC to predawn water potential (depends on rooting system hence PFT specific)           
    Ssoil              = 113      # TODO: add documentaiton: Phydro: parameter converting RZWSC to predawn water potential (depends on rooting system hence PFT specific)            
  )
  
  # read in demo data
  df_drivers <- rsofun::p_model_drivers_formatPhydro

  mod4 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = TRUE, use_pml = TRUE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 253),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl_phydro,
    makecheck      = TRUE
  )
  # print(df_drivers$params_siml[[1]])
  # print(slice(tibble(mod4), c(1, 70, 1200, 1400, 2000, 2180)))
  print(slice(tibble(mod4), c(1, 70, 1200, 1400, 2000, 2180)) |> select(gpp, aet, le, wscal, wcont))

  # THIS OUTPUTS SOMETIMES:
  #     gpp    aet        le wscal wcont
  #   <dbl>  <dbl>     <dbl> <dbl> <dbl>
  # 1 1.37   0.109   269513.   NaN   NaN
  # 2 0.759  1.36   3348090.   NaN   NaN
  # 3 0.917  2.76   6818759    NaN   NaN
  # 4 0.782  0.326   803802.   NaN   NaN
  # 5 0.363  5.73  13932128    NaN   NaN
  # 6 1.20  -0.323  -799910    NaN   NaN
  # BUT OTHER TIMES IT PROVIDES:
  #     gpp    aet        le wscal wcont
  #   <dbl>  <dbl>     <dbl> <dbl> <dbl>
  # 1  2.48  0.109   269531. 0.627  159.
  # 2  4.29  1.36   3348880. 0.828  210.
  # 3  6.38  2.76   6819973  0.964  244.
  # 4  1.12  0.326   803872. 0.414  105.
  # 5  4.19  5.73  13935005  0.442  112.
  # 6  1.46 -0.323  -799917  1      253 
}
# check_NA_output(flag = TRUE)  # Returns NaN:  check why NaN appear     with breakpoints at break gpp_pmodel.mod.f90:297 if count >= 325+1
                                #               and then see how e (sapflux?) is set to 0, leading to NaN on that day at photosynth_phydro.mod.f90:1544
check_NA_output(flag = FALSE) # Returns no NaN: check what is computed with breakpoints at break gpp_pmodel.mod.f90:297 if count >= 325+1

