module md_interface_out_biomee
  !////////////////////////////////////////////////////////////////
  ! Module defining Biomee output mappings
  !----------------------------------------------------------------

  implicit none

  private



  !=============== Output mappings =============================================================

  !=============== Daily

  integer, public, parameter :: nvars_daily_tile     = 35

  integer, public, parameter :: DAILY_TILE_YEAR               =  1
  integer, public, parameter :: DAILY_TILE_DOY                =  2
  integer, public, parameter :: DAILY_TILE_TK                 =  3
  integer, public, parameter :: DAILY_TILE_PRCP               =  4
  integer, public, parameter :: DAILY_TILE_SOIL_W             =  5
  integer, public, parameter :: DAILY_TILE_TRSP               =  6
  integer, public, parameter :: DAILY_TILE_EVAP               =  7
  integer, public, parameter :: DAILY_TILE_RUNOFF             =  8
  integer, public, parameter :: DAILY_TILE_WS1                =  9
  integer, public, parameter :: DAILY_TILE_WS2                = 10
  integer, public, parameter :: DAILY_TILE_WS3                = 11
  integer, public, parameter :: DAILY_TILE_LAI                = 12
  integer, public, parameter :: DAILY_TILE_GPP                = 13
  integer, public, parameter :: DAILY_TILE_RESP               = 14
  integer, public, parameter :: DAILY_TILE_RH                 = 15
  integer, public, parameter :: DAILY_TILE_NSC                = 16
  integer, public, parameter :: DAILY_TILE_SEED_C             = 17
  integer, public, parameter :: DAILY_TILE_LEAF_C             = 18
  integer, public, parameter :: DAILY_TILE_ROOT_C             = 19
  integer, public, parameter :: DAILY_TILE_SWC                = 20
  integer, public, parameter :: DAILY_TILE_HWC                = 21
  integer, public, parameter :: DAILY_TILE_NSN                = 22
  integer, public, parameter :: DAILY_TILE_SEED_N             = 23
  integer, public, parameter :: DAILY_TILE_LEAF_N             = 24
  integer, public, parameter :: DAILY_TILE_ROOT_N             = 25
  integer, public, parameter :: DAILY_TILE_SW_N               = 26
  integer, public, parameter :: DAILY_TILE_HW_N               = 27
  integer, public, parameter :: DAILY_TILE_MCRB_C             = 28
  integer, public, parameter :: DAILY_TILE_FASTSOM            = 29
  integer, public, parameter :: DAILY_TILE_SLOWSOM            = 30
  integer, public, parameter :: DAILY_TILE_MCRB_N             = 31
  integer, public, parameter :: DAILY_TILE_FS_N               = 32
  integer, public, parameter :: DAILY_TILE_SL_N               = 33
  integer, public, parameter :: DAILY_TILE_INORG_N            = 34
  integer, public, parameter :: DAILY_TILE_N_UPTK             = 35


  !=============== Cohorts

  integer, public, parameter :: nvars_annual_cohorts = 35

  integer, public, parameter :: ANNUAL_COHORTS_ID             =  1
  integer, public, parameter :: ANNUAL_COHORTS_YEAR           =  2
  integer, public, parameter :: ANNUAL_COHORTS_CID            =  3
  integer, public, parameter :: ANNUAL_COHORTS_PFT            =  4
  integer, public, parameter :: ANNUAL_COHORTS_LAYER          =  5
  integer, public, parameter :: ANNUAL_COHORTS_DENSITY        =  6
  integer, public, parameter :: ANNUAL_COHORTS_FLAYER         =  7
  integer, public, parameter :: ANNUAL_COHORTS_DBH            =  8
  integer, public, parameter :: ANNUAL_COHORTS_DDBH           =  9
  integer, public, parameter :: ANNUAL_COHORTS_HEIGHT         = 10
  integer, public, parameter :: ANNUAL_COHORTS_AGE            = 11
  integer, public, parameter :: ANNUAL_COHORTS_BA             = 12
  integer, public, parameter :: ANNUAL_COHORTS_DBA            = 13
  integer, public, parameter :: ANNUAL_COHORTS_ACROWN         = 14
  integer, public, parameter :: ANNUAL_COHORTS_ALEAF          = 15
  integer, public, parameter :: ANNUAL_COHORTS_NCS            = 16
  integer, public, parameter :: ANNUAL_COHORTS_NSN            = 17
  integer, public, parameter :: ANNUAL_COHORTS_SEED_C         = 18
  integer, public, parameter :: ANNUAL_COHORTS_LEAF_C         = 19
  integer, public, parameter :: ANNUAL_COHORTS_ROOT_C         = 20
  integer, public, parameter :: ANNUAL_COHORTS_SW_C           = 21
  integer, public, parameter :: ANNUAL_COHORTS_HW_C           = 22
  integer, public, parameter :: ANNUAL_COHORTS_TREEG          = 23
  integer, public, parameter :: ANNUAL_COHORTS_FSEED          = 24
  integer, public, parameter :: ANNUAL_COHORTS_FLEAF          = 25
  integer, public, parameter :: ANNUAL_COHORTS_FROOT          = 26
  integer, public, parameter :: ANNUAL_COHORTS_FWOOD          = 27
  integer, public, parameter :: ANNUAL_COHORTS_GPP            = 28
  integer, public, parameter :: ANNUAL_COHORTS_NPP            = 29
  integer, public, parameter :: ANNUAL_COHORTS_RESP           = 30
  integer, public, parameter :: ANNUAL_COHORTS_N_UPTK         = 31
  integer, public, parameter :: ANNUAL_COHORTS_N_FIX          = 32
  integer, public, parameter :: ANNUAL_COHORTS_DEATHRATE      = 33
  integer, public, parameter :: ANNUAL_COHORTS_N_LOSS         = 34
  integer, public, parameter :: ANNUAL_COHORTS_C_LOSS         = 35


  !=============== Annual tile

  integer, public, parameter :: nvars_annual_tile    = 60

  integer, public, parameter :: ANNUAL_TILE_YEAR              =  1
  integer, public, parameter :: ANNUAL_TILE_CAI               =  2
  integer, public, parameter :: ANNUAL_TILE_LAI               =  3
  integer, public, parameter :: ANNUAL_TILE_DENSITY           =  4
  integer, public, parameter :: ANNUAL_TILE_DBH               =  5
  integer, public, parameter :: ANNUAL_TILE_DENSITY12         =  6
  integer, public, parameter :: ANNUAL_TILE_DBH12             =  7
  integer, public, parameter :: ANNUAL_TILE_QMD12             =  8
  integer, public, parameter :: ANNUAL_TILE_NPP               =  9
  integer, public, parameter :: ANNUAL_TILE_GPP               = 10
  integer, public, parameter :: ANNUAL_TILE_RESP              = 11
  integer, public, parameter :: ANNUAL_TILE_RH                = 12
  integer, public, parameter :: ANNUAL_TILE_PRCP              = 13
  integer, public, parameter :: ANNUAL_TILE_SOIL_W            = 14
  integer, public, parameter :: ANNUAL_TILE_TRSP              = 15
  integer, public, parameter :: ANNUAL_TILE_EVAP              = 16
  integer, public, parameter :: ANNUAL_TILE_RUNOFF            = 17
  integer, public, parameter :: ANNUAL_TILE_PLANT_C           = 18
  integer, public, parameter :: ANNUAL_TILE_SOIL_C            = 19
  integer, public, parameter :: ANNUAL_TILE_PLANT_N           = 20
  integer, public, parameter :: ANNUAL_TILE_SOIL_N            = 21
  integer, public, parameter :: ANNUAL_TILE_TOT_N             = 22
  integer, public, parameter :: ANNUAL_TILE_NS_C              = 23
  integer, public, parameter :: ANNUAL_TILE_SEED_C            = 24
  integer, public, parameter :: ANNUAL_TILE_LEAF_C            = 25
  integer, public, parameter :: ANNUAL_TILE_ROOT_C            = 26
  integer, public, parameter :: ANNUAL_TILE_SW_C              = 27
  integer, public, parameter :: ANNUAL_TILE_HW_C              = 28
  integer, public, parameter :: ANNUAL_TILE_NSN               = 29
  integer, public, parameter :: ANNUAL_TILE_SEED_N            = 30
  integer, public, parameter :: ANNUAL_TILE_LEAF_N            = 31
  integer, public, parameter :: ANNUAL_TILE_ROOT_N            = 32
  integer, public, parameter :: ANNUAL_TILE_SW_N              = 33
  integer, public, parameter :: ANNUAL_TILE_HW_N              = 34
  integer, public, parameter :: ANNUAL_TILE_MCRB_C            = 35
  integer, public, parameter :: ANNUAL_TILE_FASTSOM           = 36
  integer, public, parameter :: ANNUAL_TILE_SLOWSOM           = 37
  integer, public, parameter :: ANNUAL_TILE_MCRB_N            = 38
  integer, public, parameter :: ANNUAL_TILE_FS_N              = 39
  integer, public, parameter :: ANNUAL_TILE_SL_N              = 40
  integer, public, parameter :: ANNUAL_TILE_INORG_N           = 41
  integer, public, parameter :: ANNUAL_TILE_N_FIX             = 42
  integer, public, parameter :: ANNUAL_TILE_N_UPTK            = 43
  integer, public, parameter :: ANNUAL_TILE_NYRMIN            = 44
  integer, public, parameter :: ANNUAL_TILE_NP2S              = 45
  integer, public, parameter :: ANNUAL_TILE_NLOSS             = 46
  integer, public, parameter :: ANNUAL_TILE_TOTSEED_C         = 47
  integer, public, parameter :: ANNUAL_TILE_TOTSEED_N         = 48
  integer, public, parameter :: ANNUAL_TILE_SEEDLING_C        = 49
  integer, public, parameter :: ANNUAL_TILE_SEEDLING_N        = 50
  integer, public, parameter :: ANNUAL_TILE_MAX_AGE           = 51
  integer, public, parameter :: ANNUAL_TILE_MAX_VOLUME        = 52
  integer, public, parameter :: ANNUAL_TILE_MAX_DBH           = 53
  integer, public, parameter :: ANNUAL_TILE_NPP_L             = 54
  integer, public, parameter :: ANNUAL_TILE_NPP_W             = 55
  integer, public, parameter :: ANNUAL_TILE_DEADTREES_N       = 56
  integer, public, parameter :: ANNUAL_TILE_DEADTREES_C       = 57
  integer, public, parameter :: ANNUAL_TILE_M_TURNOVER        = 58
  integer, public, parameter :: ANNUAL_TILE_C_TURNOVER_TIME   = 59
  integer, public, parameter :: ANNUAL_TILE_LU_FRACTION       = 60


  !=============== Aggregated tile

  integer, public, parameter :: nvars_aggregated_out = nvars_annual_tile + 4

  integer, public, parameter :: AGGREGATED_TILE_PROD_POOL_1_C = nvars_annual_tile + 1
  integer, public, parameter :: AGGREGATED_TILE_PROD_POOL_1_N = nvars_annual_tile + 2
  integer, public, parameter :: AGGREGATED_TILE_PROD_POOL_2_C = nvars_annual_tile + 3
  integer, public, parameter :: AGGREGATED_TILE_PROD_POOL_2_N = nvars_annual_tile + 4

end module md_interface_out_biomee
