rsofun::p_model_drivers |> select(params_siml) |> unnest(params_siml)  |> readr::write_csv("analysis/paper_inputs_csvs/p_model_drivers_params_siml.csv")
rsofun::p_model_drivers |> select(site_info)   |> unnest(site_info)    |> readr::write_csv("analysis/paper_inputs_csvs/p_model_drivers_site_info.csv")
rsofun::p_model_drivers |> select(forcing)     |> unnest(forcing)      |> readr::write_csv("analysis/paper_inputs_csvs/p_model_drivers_forcing.csv")
rsofun::p_model_validation                     |> unnest(data)         |> readr::write_csv("analysis/paper_inputs_csvs/p_model_validation.csv")

