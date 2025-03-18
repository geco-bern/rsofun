# Following: https://blog.davisvaughan.com/posts/2022-03-11-using-vs-code-to-debug-r-packages-with-c-code/#setup-function
# which is a detailed description (but only for mac using llvm)
# and: https://www.maths.ed.ac.uk/~swood34/RCdebug/RCdebug.html
# and: https://tdhock.github.io/blog/2019/gdb/

# adding flags: PKG_FFLAGS = -frecursive -fbounds-check -fcheck=all -Wall -Wextra -pedantic -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999 -finit-derived
# adding flags to reproduce NaN bug: PKG_FFLAGS = -frecursive -fbounds-check -fcheck=all -Wall -Wextra -pedantic -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999
# building: R CMD build --no-manual --no-build-vignettes rsofun && R CMD INSTALL -c --preclean rsofun_5.0.0.9000.tar.gz 

# TODO: this link: https://stackoverflow.com/a/72580854 could potentially explain how to do it in the Docker where we see the error
#                  https://github.com/renkun-ken/vscode-rcpp-demo
#                  https://github.com/renkun-ken/vscode-cpp11-demo

# FOR DEBUGGIN in the act:
# ~/bin/act -r -W '.github/workflows/test-coverage-debug.yaml'

devtools::clean_dll()
devtools::load_all()
# library(rsofun)
# library(dplyr)
# library(purrr)

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
check_NA_output()
