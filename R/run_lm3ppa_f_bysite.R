#' R wrapper for SOFUN
#'
#' @param sitename Site name
#' @param params_siml Simulation parameters
#' @param siteinfo Site meta info
#' @param forcing forcing (input) dataframe (returned object by \link{prepare_input_sofun})
#' @param params_tile Tile-level model parameters
#' @param params_species Species-specific model parameters
#' @param params_soil Soil parameters (per soil layer)
#' @param init_cohort Initial size of individuals in each of the N initally present cohorts
#' @param init_soil Initial soil pool sizes
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#'
#' @details This is the model
#'
#' @export
#' @useDynLib rsofun
#'
run_lm3ppa_f_bysite <- function( sitename, params_siml, siteinfo, forcing, params_tile, params_species, params_soil, init_cohort, init_soil, makecheck = TRUE ){


  ## re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::select(1:13)

  params_soil <- params_soil %>%
    dplyr::select(-type)

  runyears <- ifelse(params_siml$spinup, (params_siml$spinupyears + params_siml$nyeartrend), params_siml$nyeartrend)
  n_daily  <- runyears * 365

  ## Tests XXX todo: adapt test to forcing for lm3ppa
  do_continue <- TRUE

  if (makecheck){
    is.nanull <- function(x) ifelse(is.na(x) | is.null(x), TRUE, FALSE)
    if (any(is.nanull(forcing$PAR))){
      rlang::warn(paste("Error: Missing value in PAR for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$Swdown))){
      rlang::warn(paste("Error: Missing value in Swdown for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$TEMP))){
      rlang::warn(paste("Error: Missing value in TEMP for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$SoilT))){
      rlang::warn(paste("Error: Missing value in SoilT for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$RH))){
      rlang::warn(paste("Error: Missing value in RH for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$RAIN))){
      rlang::warn(paste("Error: Missing value in RAIN for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$WIND))){
      rlang::warn(paste("Error: Missing value in WIND for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$PRESSURE))){
      rlang::warn(paste("Error: Missing value in PRESSURE for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$aCO2_AW))){
      rlang::warn(paste("Error: Missing value in aCO2_AW for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$amb_co2))){
      rlang::warn(paste("Error: Missing value in amb_co2 for site", sitename, "\n"))
      do_continue <- FALSE
    }
    
    if (nrow(forcing) != params_siml$nyeartrend * 365 * 24){    # xxx todo: must be *48 for lm3ppa
      rlang::warn("Error: Number of years data in forcing does not correspond to number of simulation years (nyeartrend).")
      rlang::warn(paste(" Number of years data: ", nrow(forcing)/(365*24)))
      rlang::warn(paste(" Number of simulation years: ", params_siml$nyeartrend))
      rlang::warn(" Returning a dummy data frame.")
    }
  }

  if (do_continue) {

    # ## Soil texture as matrix (layer x texture parameter)
    # soiltexture <- df_soiltexture %>% 
    #   dplyr::select(fsand, fclay, forg, fgravel) %>% 
    #   as.matrix() %>% 
    #   t()

    ## C wrapper call
     # out <- .Call(

    lm3out <- .Call(

      'lm3ppa_f_C',

      ## Simulation parameters
      spinup                = as.logical(params_siml$spinup),
      spinupyears           = as.integer(params_siml$spinupyears),
      recycle               = as.integer(params_siml$recycle),
      firstyeartrend        = as.integer(params_siml$firstyeartrend),
      nyeartrend            = as.integer(params_siml$nyeartrend),
      outputhourly          = as.logical(params_siml$outputhourly),
      outputdaily           = as.logical(params_siml$outputdaily),
      do_U_shaped_mortality = as.logical(params_siml$do_U_shaped_mortality),
      update_annaulLAImax   = as.logical(params_siml$update_annaulLAImax),
      do_closedN_run        = as.logical(params_siml$do_closedN_run),
      
      ## site meta info
      longitude             = as.numeric(siteinfo$lon),
      latitude              = as.numeric(siteinfo$lat),
      altitude              = as.numeric(siteinfo$elv),
      # 
      ## Tile-level parameters
      soiltype     = as.integer(params_tile$soiltype),
      FLDCAP       = as.numeric(params_tile$FLDCAP),
      WILTPT       = as.numeric(params_tile$WILTPT),
      K1           = as.numeric(params_tile$K1),
      K2           = as.numeric(params_tile$K2),
      K_nitrogen   = as.numeric(params_tile$K_nitrogen),
      etaN         = as.numeric(params_tile$etaN),
      MLmixRatio   = as.numeric(params_tile$MLmixRatio),
      l_fract      = as.numeric(params_tile$l_fract),
      retransN     = as.numeric(params_tile$retransN),
      f_N_add      = as.numeric(params_tile$f_N_add),
      f_initialBSW = as.numeric(params_tile$f_initialBSW),
      # 
      # ## Species-specific parameters
      params_species = as.matrix(params_species),
      
      ## soil parameters
      params_soil = as.matrix(params_soil),
      
      ## initial cohort sizes
      init_cohort = as.matrix(init_cohort),
      # 
      ## initial soil pools
      init_fast_soil_C = as.numeric(init_soil$init_fast_soil_C),
      init_slow_soil_C = as.numeric(init_soil$init_slow_soil_C),
      init_Nmineral    = as.numeric(init_soil$init_Nmineral),
      N_input          = as.numeric(init_soil$N_input),
      # 
      n        = as.integer(nrow(forcing)), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      n_daily  = as.integer(n_daily), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      n_annual = as.integer(runyears), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      forcing  = as.matrix(forcing)
      )
    
    ## Prepare output to be a nice looking tidy data frame (tibble)
    # ddf <- init_dates_dataframe(yrstart = params_siml$firstyeartrend, yrend = siteinfo$year_end, noleap = TRUE)

    out <- list(output_hourly_tile = NULL, output_daily_tile = NULL, output_daily_cohorts = NULL, output_annual_tile = NULL, output_annual_cohorts = NULL)
    # names(out) <- c("output_hourly_tile", "output_daily_tile", "output_daily_cohorts_year", "output_annual_tile", "output_annual_cohorts")

    ## hourly
    out$output_hourly_tile <- lm3out[[1]] %>%
    # out[[1]] <- out[[1]] %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(c("year", "doy", "hour", "rad", "Tair", "Prcp", "GPP", "Resp", "Transp", "Evap", "Runoff", "Soilwater", "wcl", "FLDCAP", "WILTPT")) #%>%
      #dplyr::mutate(sitename = sitename) #%>%
      # dplyr::mutate(date = lubridate::ymd_hm(paste0(as.character(year), "-01-01 00:00")) + lubridate::days(doy-1) + hours(hour)) %>%
      # dplyr::select(-year, -doy, -hour)
      # dplyr::bind_cols(ddf,.) %>% 
      # dplyr::select(-year_dec)
    
    # ## daily_tile
    out$output_daily_tile <- lm3out[[2]] %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(c("year", "doy", "Tc", "Prcp", "totWs", "Trsp", "Evap", "Runoff", "ws1", "ws2", "ws3", "LAI", "GPP", "Rauto", "Rh", "NSC", "seedC", "leafC", "rootC", "SW_C", "HW_C", "NSN", "seedN", "leafN", "rootN", "SW_N", "HW_N", "McrbC", "fastSOM", "slowSOM", "McrbN", "fastSoilN", "slowSoilN", "mineralN", "N_uptk")) #%>%
      #dplyr::mutate(sitename = sitename) #%>%
      #dplyr::mutate(date = lubridate::ymd(paste0(as.character(year), "-01-01")) + lubridate::days(doy-1)) %>%
      #dplyr::select(-year, -doy)
      
    # # ## daily_cohorts
    out$output_daily_cohorts <- lm3out[[3]] %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
      tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "year", names_prefix = "cohort_") %>%
      mutate(year = ifelse(year==0, NA, year)) %>%
        bind_cols(
        .,
        lm3out[[4]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "doy", names_prefix = "cohort_") %>%
          mutate(doy = ifelse(doy==0, NA, doy)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[5]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "hour", names_prefix = "cohort_") %>%
          mutate(hour = ifelse(hour==0, NA, hour)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[6]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "cID", names_prefix = "cohort_") %>%
          mutate(cID = ifelse(cID==0, NA, cID)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[7]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "PFT", names_prefix = "cohort_") %>%
          mutate(PFT = ifelse(PFT==0, NA, PFT)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[8]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "layer", names_prefix = "cohort_") %>%
          mutate(layer = ifelse(layer==0, NA, layer)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[9]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "density", names_prefix = "cohort_") %>%
          mutate(density = ifelse(density==0, NA, density)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[10]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "f_layer", names_prefix = "cohort_") %>%
          mutate(f_layer = ifelse(f_layer==0, NA, f_layer)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[11]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "LAI", names_prefix = "cohort_") %>%
          mutate(LAI = ifelse(LAI==0, NA, LAI)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[12]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "gpp", names_prefix = "cohort_") %>%
          mutate(gpp = ifelse(gpp==0, NA, gpp)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[13]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "resp", names_prefix = "cohort_") %>%
          mutate(resp = ifelse(resp==0, NA, resp)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[14]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "transp", names_prefix = "cohort_") %>%
          mutate(transp = ifelse(transp==0, NA, transp)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[15]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPPleaf", names_prefix = "cohort_") %>%
          mutate(NPPleaf = ifelse(NPPleaf==0, NA, NPPleaf)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[16]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPProot", names_prefix = "cohort_") %>%
          mutate(NPProot = ifelse(NPProot==0, NA, NPProot)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[17]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPPwood", names_prefix = "cohort_") %>%
          mutate(NPPwood = ifelse(NPPwood==0, NA, NPPwood)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[18]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NSC", names_prefix = "cohort_") %>%
          mutate(NSC = ifelse(NSC==0, NA, NSC)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[19]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "seedC", names_prefix = "cohort_") %>%
          mutate(seedC = ifelse(seedC==0, NA, seedC)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[20]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "leafC", names_prefix = "cohort_") %>%
          mutate(leafC = ifelse(leafC==0, NA, leafC)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[21]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "rootC", names_prefix = "cohort_") %>%
          mutate(rootC = ifelse(rootC==0, NA, rootC)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[22]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "SW_C", names_prefix = "cohort_") %>%
          mutate(SW_C = ifelse(SW_C==0, NA, SW_C)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[23]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "HW_C", names_prefix = "cohort_") %>%
          mutate(HW_C = ifelse(HW_C==0, NA, HW_C)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[24]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NSN", names_prefix = "cohort_") %>%
          mutate(NSN = ifelse(NSN==0, NA, NSN)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[25]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "seedN", names_prefix = "cohort_") %>%
          mutate(seedN = ifelse(seedN==0, NA, seedN)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[26]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "leafN", names_prefix = "cohort_") %>%
          mutate(leafN = ifelse(leafN==0, NA, leafN)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[27]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "rootN", names_prefix = "cohort_") %>%
          mutate(rootN = ifelse(rootN==0, NA, rootN)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[28]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "SW_N", names_prefix = "cohort_") %>%
          mutate(SW_N = ifelse(SW_N==0, NA, SW_N)) %>% dplyr::select(-1)) %>%
      bind_cols(
        .,
        lm3out[[29]] %>%
          as.matrix() %>% 
          as_tibble() %>%
          setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
          tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "HW_C", names_prefix = "cohort_") %>%
          mutate(HW_C = ifelse(HW_C==0, NA, HW_C)) %>% dplyr::select(-1)) %>%
      tidyr::drop_na(year)
     
    # ## annual tile
    out$output_annual_tile <- lm3out[[30]] %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(c("year", "CAI", "LAI", "GPP", "Rauto", "Rh", "rain", "SoilWater", "Transp", "Evap", "Runoff", "plantC", "soilC", "plantN", "soilN", "totN", "NSC", "SeedC", "leafC", "rootC", "SapwoodC", "WoodC", "NSN", "SeedN", "leafN", "rootN", "SapwoodN", "WoodN", "McrbC", "fastSOM", "SlowSOM", "McrbN", "fastSoilN", "slowSoilN", "mineralN", "N_fxed", "N_uptk", "N_yrMin", "N_P2S", "N_loss", "totseedC", "totseedN", "Seedling_C", "Seedling_N")) #%>%
    
    # ## annual cohorts
    out$output_annual_cohorts <- lm3out[[31]] %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
      tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "year", names_prefix = "cohort_") %>%
      mutate(year = ifelse(year==0, NA, year)) %>%
      bind_cols(
       .,
      lm3out[[32]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "cID", names_prefix = "cohort_") %>%
        mutate(cID = ifelse(cID==0, NA, cID)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[33]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "PFT", names_prefix = "cohort_") %>%
        mutate(PFT = ifelse(PFT==0, NA, PFT)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[34]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "layer", names_prefix = "cohort_") %>%
        mutate(layer = ifelse(layer==0, NA, layer)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[35]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "density", names_prefix = "cohort_") %>%
        mutate(density = ifelse(density==0, NA, density)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[36]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "f_layer", names_prefix = "cohort_") %>%
        mutate(f_layer = ifelse(f_layer==0, NA, f_layer)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[37]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "dDBH", names_prefix = "cohort_") %>%
        mutate(dDBH = ifelse(dDBH==0, NA, dDBH)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[38]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "dbh", names_prefix = "cohort_") %>%
        mutate(dbh = ifelse(dbh==0, NA, dbh)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[39]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "height", names_prefix = "cohort_") %>%
        mutate(height = ifelse(height==0, NA, height)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[40]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "Acrown", names_prefix = "cohort_") %>%
        mutate(Acrown = ifelse(Acrown==0, NA, Acrown)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[41]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "wood", names_prefix = "cohort_") %>%
        mutate(wood = ifelse(wood==0, NA, wood)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[42]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "nsc", names_prefix = "cohort_") %>%
        mutate(nsc = ifelse(nsc==0, NA, nsc)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[43]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NSN", names_prefix = "cohort_") %>%
        mutate(NSN = ifelse(NSN==0, NA, NSN)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[44]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPPtr", names_prefix = "cohort_") %>%
        mutate(NPPtr = ifelse(NPPtr==0, NA, NPPtr)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[45]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "seed", names_prefix = "cohort_") %>%
        mutate(seed = ifelse(seed==0, NA, seed)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[46]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPPL", names_prefix = "cohort_") %>%
        mutate(NPPL = ifelse(NPPL==0, NA, NPPL)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[47]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPPR", names_prefix = "cohort_") %>%
        mutate(NPPR = ifelse(NPPR==0, NA, NPPR)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[48]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPPW", names_prefix = "cohort_") %>%
        mutate(NPPW = ifelse(NPPW==0, NA, NPPW)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[49]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "GPP", names_prefix = "cohort_") %>%
        mutate(GPP = ifelse(GPP==0, NA, GPP)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[50]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "NPP", names_prefix = "cohort_") %>%
        mutate(NPP = ifelse(NPP==0, NA, NPP)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[51]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "N_uptk", names_prefix = "cohort_") %>%
        mutate(N_uptk = ifelse(N_uptk==0, NA, N_uptk)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[52]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "N_fix", names_prefix = "cohort_") %>%
        mutate(N_fix = ifelse(N_fix==0, NA, N_fix)) %>% dplyr::select(-1)) %>%
    bind_cols(
      .,
      lm3out[[53]] %>%
        as.matrix() %>% 
        as_tibble() %>%
        setNames(paste0("cohort_", as.character(1:ncol(.)))) %>%
        tidyr::pivot_longer(1:ncol(.), names_to = "cohort", values_to = "maxLAI", names_prefix = "cohort_") %>%
        mutate(maxLAI = ifelse(maxLAI==0, NA, maxLAI)) %>% dplyr::select(-1)) %>%
    tidyr::drop_na(year)

  } else {
    out <- NA
  }
    
  return(out)

}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
