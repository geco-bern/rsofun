library(dplyr)
library(tidyr)
library(readr)

# load data currently in data/ directory (to prevent using potentially outdated 
# data, from the currently loaded rsofun package):
from_data_dir <- rlang::env()
load("data/p_model_drivers.rda",    envir = from_data_dir)
load("data/p_model_validation.rda", envir = from_data_dir)

# transform to csv
from_data_dir[['p_model_drivers']] |> 
  select(params_siml) |> 
  unnest(params_siml) |> 
  readr::write_csv("analysis/paper_inputs_csvs/p_model_drivers_params_siml.csv")

from_data_dir[['p_model_drivers']] |> 
  select(site_info) |> 
  unnest(site_info) |> 
  readr::write_csv("analysis/paper_inputs_csvs/p_model_drivers_site_info.csv")

from_data_dir[['p_model_drivers']] |> select(forcing) |> 
  unnest(forcing) |> 
  readr::write_csv("analysis/paper_inputs_csvs/p_model_drivers_forcing.csv")

from_data_dir[['p_model_validation']] |> 
  unnest(data) |> 
  readr::write_csv("analysis/paper_inputs_csvs/p_model_validation.csv")

