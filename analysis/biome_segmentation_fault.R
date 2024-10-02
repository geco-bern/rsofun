
library(rsofun)
library(dplyr)
library(ggplot2)
library(tibble)

load("data/biomee_p_model_drivers.rda")
# Set spunup to false:
#biomee_p_model_drivers$params_siml[[1]]$spinup <- F

# function to run biomee multiple times and detect the segmentation fault error
biomeextimes <- function(drivers,n){
  results <-  list()
  for(i in 1:n) {
    print(i)
    set.seed(2023)
    out <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE
  )$data[[1]]$output_annual_tile['plantC'] %>%
     slice(tail(row_number(), 1))

    print(paste("Plant C", out))
    
  if(is.na(out)|out==0){
    print(out$plantC)
    stop("Error: Simulation failed")
  }
  results <- append(results, out)
  }
  save(results, file= "data/results.rda")
  return(results)
}

# run the model n times 
n = 5
res = biomeextimes(biomee_p_model_drivers,n)

# always after clearing the environment, the first run yields a slightly different value
