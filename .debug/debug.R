# Following: https://blog.davisvaughan.com/posts/2022-03-11-using-vs-code-to-debug-r-packages-with-c-code/#setup-function
# which is a detailed description (but only for mac using llvm)
# and: https://www.maths.ed.ac.uk/~swood34/RCdebug/RCdebug.html
# and: https://tdhock.github.io/blog/2019/gdb/

# adding flags: PKG_FFLAGS = -frecursive -fbounds-check -fcheck=all -Wall -Wextra -pedantic -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999 -finit-derived
# adding flags to reproduce NaN bug: PKG_FFLAGS = -frecursive -fbounds-check -fcheck=all -Wall -Wextra -pedantic -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999
# building: R CMD build --no-manual --no-build-vignettes rsofun && R CMD INSTALL -c --preclean rsofun_5.0.0.9000.tar.gz 

# usethis::edit_r_makevars(scope = "project") # remove again any flags that we added

# TODO: this link: https://stackoverflow.com/a/72580854 could potentially explain how to do it in the Docker where we see the error
#                  https://github.com/renkun-ken/vscode-rcpp-demo
#                  https://github.com/renkun-ken/vscode-cpp11-demo

# FOR DEBUGGING in the act:
# ~/bin/act -r -W '.github/workflows/test-coverage-debug.yaml'


devtools::clean_dll()
devtools::load_all()
# library(rsofun)
# library(dplyr)
# library(purrr)
check_c13_bug <- function(){
  
  library(tidyr)
  runread_pmodel_f(
    rsofun::p_model_drivers |> unnest(forcing) |> slice(1:365) |> nest(forcing = c(date:ccov)),
    par = list(kphio = 0.04998, kphio_par_a = 0.01, kphio_par_b = 1, soilm_thetastar = 144, 
               soilm_betao = 0.01, beta_unitcostratio = 146, rd_to_vcmax = 0.014, 
               tau_acclim = 30, kc_jmax = 0.41),
    makecheck = TRUE,
    parallel = FALSE,
    ncores = 2
  )
  
}
# check_c13_bug()


check_NA_output <- function(){
  # THIS YIELDS OUTPUT UNDER ALL CONDITIONS
  # print("CASE A: Default: =================================")
  # drvr <- rsofun::biomee_p_model_drivers
  # res <- runread_biomee_f(drvr)
  
  # print(drvr$params_siml[[1]])
  # print("output_daily_tile:"); print(tibble(res$data[[1]]$output_daily_tile))
  # print("output_annual_tile:"); print(tibble(res$data[[1]]$output_annual_tile))
  # print("output_annual_cohort:"); print(tibble(res$data[[1]]$output_annual_cohort))
  
  # THIS YIELDS NA UNDER CERTAIN CONDITIONS
  print("CASE B: spinupyears=0: =================================")
  drvr <- rsofun::biomee_p_model_drivers |> 
    mutate(params_siml = purrr::map(params_siml, ~mutate(.x, spinupyears=0)))
  res <- runread_biomee_f(drvr)
  
  print(drvr$params_siml[[1]])
  print("output_daily_tile:"); print(tibble(res$data[[1]]$output_daily_tile))
  # print("output_annual_tile:"); print(tibble(res$data[[1]]$output_annual_tile))
  # print("output_annual_cohort:"); print(tibble(res$data[[1]]$output_annual_cohort))
}
# check_NA_output()

check_biomee_aggressive_merging <- function(){
  #drivers <- readRDS(".debug/drivers_debug_SolBeech.rds")
  sitename <- "CZ-BK1"
  #drivers <- readRDS(paste0(".debug/drivers_debug_transient_",sitename,"_",Sys.Date(),".rds"))
  drivers <- readRDS(paste0(".debug/drivers_debug_transient_",sitename,"_","2026-04-15",".rds"))

  runread_biomee_f(drivers, makecheck = TRUE, ncores = 1)
}
# res <- check_biomee_aggressive_merging()
#res |> tidyr::unnest_wider(data)
#
#



check_biomass_loss_when_reinitializing <- function(){
  bug_list <- readRDS(".debug/bug_1.RDS")
  #bug_list <- readRDS(".debug/bug_2.RDS")
  run_biomee_f_bysite(
          sitename       = bug_list$sitename,
          params_siml    = bug_list$params_siml,
          site_info      = bug_list$site_info,
          forcing        = bug_list$forcing,
          params_tile    = bug_list$params_tile,
          params_species = bug_list$params_species,
          init_cohort    = bug_list$init_cohort,
          init_soil      = bug_list$init_soil,
          init_lu        = bug_list$init_lu,
          luc_forcing    = bug_list$luc_forcing,
          makecheck      = bug_list$makecheck)


  readRDS(".debug/bug_1.RDS")$init_cohort
  readRDS(".debug/bug_2.RDS")$init_cohort
}
check_biomass_loss_when_reinitializing()