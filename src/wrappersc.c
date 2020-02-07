#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

// Fortran subroutine registration

/////////////////////////////////////////////////////////////
// P-model
/////////////////////////////////////////////////////////////
void F77_NAME(pmodel_f)(
    _Bool  *spinup,
    int    *spinupyears,
    int    *recycle,
    int    *firstyeartrend,
    int    *nyeartrend,
    _Bool  *soilmstress,
    _Bool  *tempstress,
    _Bool  *calc_aet_fapar_vpd,
    _Bool  *in_ppfd,
    _Bool  *in_netrad,
    // int *const_clim_year,
    // int *const_lu_year,
    // int *const_co2_year,
    // int *const_ndep_year,
    // int *const_nfert_year,
    int    *outdt,
    _Bool  *ltre,
    _Bool  *ltne,
    _Bool  *ltrd,
    _Bool  *ltnd,
    _Bool  *lgr3,
    _Bool  *lgn3,
    _Bool  *lgr4,
    double *longitude,
    double *latitude,
    double *altitude,
    double *whc,
    double *soiltexture,
    int    *nt,
    double *par,
    double *forcing,
    double *output
    );

// C wrapper function for P-model
extern SEXP pmodel_f_C(
    SEXP spinup,
    SEXP spinupyears,
    SEXP recycle,
    SEXP firstyeartrend,
    SEXP nyeartrend,
    SEXP soilmstress,
    SEXP tempstress,
    SEXP calc_aet_fapar_vpd,
    SEXP in_ppfd,
    SEXP in_netrad,
    // SEXP const_clim_year,
    // SEXP const_lu_year,
    // SEXP const_co2_year,
    // SEXP const_ndep_year,
    // SEXP const_nfert_year,
    SEXP outdt,
    SEXP ltre,
    SEXP ltne,
    SEXP ltrd,
    SEXP ltnd,
    SEXP lgr3,
    SEXP lgn3,
    SEXP lgr4,
    SEXP longitude,
    SEXP latitude,
    SEXP altitude,
    SEXP whc,
    SEXP soiltexture,
    SEXP n,
    SEXP par,
    SEXP forcing
    ){

    // Number of time steps (same in forcing and output)
    const int nt = INTEGER(n)[0] ;

    // Specify output
    SEXP output = PROTECT( allocMatrix(REALSXP, nt, 5) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns

    // Fortran subroutine call
    F77_CALL(pmodel_f)(
        LOGICAL(spinup),
        INTEGER(spinupyears),
        INTEGER(recycle),
        INTEGER(firstyeartrend),
        INTEGER(nyeartrend),
        LOGICAL(soilmstress),
        LOGICAL(tempstress),
        LOGICAL(calc_aet_fapar_vpd),
        LOGICAL(in_ppfd),
        LOGICAL(in_netrad),
        // INTEGER(const_clim_year),
        // INTEGER(const_lu_year),
        // INTEGER(const_co2_year),
        // INTEGER(const_ndep_year),
        // INTEGER(const_nfert_year),
        INTEGER(outdt),
        LOGICAL(ltre),
        LOGICAL(ltne),
        LOGICAL(ltrd),
        LOGICAL(ltnd),
        LOGICAL(lgr3),
        LOGICAL(lgn3),
        LOGICAL(lgr4),
        REAL(longitude),
        REAL(latitude),
        REAL(altitude),
        REAL(whc),
        REAL(soiltexture),
        INTEGER(n),
        REAL(par),
        REAL(forcing),
        REAL(output)
        );

    // // Output as list
    // SEXP out_full = PROTECT( allocVector(VECSXP, 1) );
    // SET_VECTOR_ELT(out_full, 0, output);

    UNPROTECT(1);

    return output;
}

/////////////////////////////////////////////////////////////
// LM3PPA
/////////////////////////////////////////////////////////////
void F77_NAME(lm3ppa_f)(
    _Bool  *spinup,             
    int    *spinupyears,               
    int    *recycle,              
    int    *firstyeartrend,                  
    int    *nyeartrend,                  
    _Bool  *outputhourly,                   
    _Bool  *outputdaily,                   
    _Bool  *do_U_shaped_mortality,             
    _Bool  *update_annualLAImax,                   
    _Bool  *do_closedN_run,                   
    double *longitude,                  
    double *latitude,                  
    double *altitude,                  
    int    *soiltype,                   
    double *FLDCAP,                   
    double *WILTPT,                   
    double *K1,                   
    double *K2,                   
    double *K_nitrogen,                   
    double *etaN,                   
    double *MLmixRatio,                   
    double *l_fract,                   
    double *retransN,                   
    double *f_N_add,                   
    double *f_initialBSW,                  
    double *params_species,                   
    double *params_soil,                   
    double *init_cohort,                   
    double *init_fast_soil_C,                   
    double *init_slow_soil_C,                   
    double *init_Nmineral,                   
    double *N_input,                   
    int    *nt,                     
    int    *nt_daily,                 
    int    *nt_annual,                
    double *forcing,                  
    double *output_hourly_tile,   
    double *output_daily_tile,    
    double *output_daily_cohorts, 
    double *output_annual_tile,   
    double *output_annual_cohorts
    );

// C wrapper function for LM3PPA
extern SEXP lm3ppa_f_C(
    SEXP spinup,                
    SEXP spinupyears,               
    SEXP recycle,                 
    SEXP firstyeartrend,                  
    SEXP nyeartrend,                  
    SEXP outputhourly,                   
    SEXP outputdaily,                   
    SEXP do_U_shaped_mortality,             
    SEXP update_annualLAImax,                   
    SEXP do_closedN_run,                  
    SEXP longitude,                  
    SEXP latitude,                  
    SEXP altitude,                 
    SEXP soiltype,                   
    SEXP FLDCAP,                   
    SEXP WILTPT,                   
    SEXP K1,                   
    SEXP K2,                   
    SEXP K_nitrogen,                   
    SEXP etaN,                   
    SEXP MLmixRatio,                   
    SEXP l_fract,                   
    SEXP retransN,                   
    SEXP f_N_add,                   
    SEXP f_initialBSW,                   
    SEXP params_species,                   
    SEXP params_soil,                   
    SEXP init_cohort,                   
    SEXP init_fast_soil_C,                   
    SEXP init_slow_soil_C,                   
    SEXP init_Nmineral,                   
    SEXP N_input,                  
    SEXP n,                  
    SEXP n_daily,                 
    SEXP n_annual,                
    SEXP forcing                 
    ){

    // // Number of time steps (same in forcing and output)
    const int nt = INTEGER(n)[0];
    const int nt_daily = INTEGER(n_daily)[0];
    const int nt_annual = INTEGER(n_annual)[0];

    // // Specify output
    SEXP output_hourly_tile    = PROTECT( allocMatrix(REALSXP, nt,        15) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_daily_tile     = PROTECT( allocMatrix(REALSXP, nt_daily,  35) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_daily_cohorts  = PROTECT( allocMatrix(REALSXP, nt_daily,  27) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_annual_tile    = PROTECT( allocMatrix(REALSXP, nt_annual, 44) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_annual_cohorts = PROTECT( allocMatrix(REALSXP, nt_annual, 23) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo

    // xxx debug: Specify output
    // SEXP out_list = PROTECT( allocMatrix(REALSXP, nt, 5) );   // nt and 10 2nd agument to allocMatrix is number of rows, 3rd is number of columns

    // Fortran subroutine call
    F77_CALL(lm3ppa_f)(
        LOGICAL(spinup),                
        INTEGER(spinupyears),                  
        INTEGER(recycle),                 
        INTEGER(firstyeartrend),                  
        INTEGER(nyeartrend),                  
        LOGICAL(outputhourly),                   
        LOGICAL(outputdaily),                   
        LOGICAL(do_U_shaped_mortality),                
        LOGICAL(update_annualLAImax),                   
        LOGICAL(do_closedN_run),                  
        REAL(longitude),                  
        REAL(latitude),                  
        REAL(altitude),                  
        INTEGER(soiltype),                   
        REAL(FLDCAP),                   
        REAL(WILTPT),                   
        REAL(K1),                   
        REAL(K2),                   
        REAL(K_nitrogen),                   
        REAL(etaN),                   
        REAL(MLmixRatio),                   
        REAL(l_fract),                   
        REAL(retransN),                   
        REAL(f_N_add),                   
        REAL(f_initialBSW),                   
        REAL(params_species),                   
        REAL(params_soil),                   
        REAL(init_cohort),                  
        REAL(init_fast_soil_C),                   
        REAL(init_slow_soil_C),                   
        REAL(init_Nmineral),                   
        REAL(N_input),                  
        INTEGER(n),                    
        INTEGER(n_daily),                 
        INTEGER(n_annual),                
        REAL(forcing),             
        REAL(output_hourly_tile),  
        REAL(output_daily_tile),    
        REAL(output_daily_cohorts), 
        REAL(output_annual_tile),   
        REAL(output_annual_cohorts)
        );

    // // Output as list
    SEXP out_list = PROTECT( allocVector(VECSXP, 5) );  // maybe try  STRSXP instead of VECSXP
    SET_VECTOR_ELT(out_list, 0, output_hourly_tile);
    SET_VECTOR_ELT(out_list, 1, output_daily_tile);
    SET_VECTOR_ELT(out_list, 2, output_daily_cohorts);
    SET_VECTOR_ELT(out_list, 3, output_annual_tile);
    SET_VECTOR_ELT(out_list, 4, output_annual_cohorts);

    UNPROTECT(6);

    // Debug
    // UNPROTECT(1);

    return out_list;
}

/////////////////////////////////////////////////////////////
// Declarations for all functions
/////////////////////////////////////////////////////////////
static const R_CallMethodDef CallEntries[] = {
  {"pmodel_f_C",   (DL_FUNC) &pmodel_f_C,   26},  // Specify number of arguments to C wrapper as the last number here
  {"lm3ppa_f_C",   (DL_FUNC) &lm3ppa_f_C,   36},  //36, Specify number of arguments to C wrapper as the last number here; xxx adjust this
  {NULL,         NULL,                0}
};

void R_init_rsofun(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("rsofun", "pmodel_f_C",  (DL_FUNC) &pmodel_f_C);
    R_RegisterCCallable("rsofun", "lm3ppa_f_C",  (DL_FUNC) &lm3ppa_f_C);
}
