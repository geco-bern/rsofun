#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

// Fortran subroutine registration
// See https://www.stat.berkeley.edu/~spector/s243/calling.pdf for more details

/////////////////////////////////////////////////////////////
// P-model
/////////////////////////////////////////////////////////////

void F77_NAME(pmodel_f)(
    int    *spinup, // LOGICAL type is not supported in the C interface (LTO)
    int    *spinupyears,
    int    *recycle,
    int    *firstyeartrend,
    int    *nyeartrend,
    int    *secs_per_tstep,
    int    *in_ppfd,// LOGICAL
    int    *in_netrad,// LOGICAL
    int    *outdt,
    int    *ltre,// LOGICAL
    int    *ltne,// LOGICAL
    int    *ltrd,// LOGICAL
    int    *ltnd,// LOGICAL
    int    *lgr3,// LOGICAL
    int    *lgn3,// LOGICAL
    int    *lgr4,// LOGICAL
    double *longitude,
    double *latitude,
    double *altitude,
    double *whc,
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
    SEXP secs_per_tstep,
    SEXP in_ppfd,
    SEXP in_netrad,
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
    SEXP n,
    SEXP par,
    SEXP forcing
    ){

    // Number of time steps (same in forcing and output)
    const int nt = asInteger(n);

    // Specify output
    // 2nd agument to allocMatrix is number of rows, 3rd is number of columns
    SEXP output = PROTECT( allocMatrix(REALSXP, nt, 19) );

    // Fortran subroutine call
    F77_CALL(pmodel_f)(
        INTEGER(spinup),
        INTEGER(spinupyears),
        INTEGER(recycle),
        INTEGER(firstyeartrend),
        INTEGER(nyeartrend),
        INTEGER(secs_per_tstep),
        INTEGER(in_ppfd),
        INTEGER(in_netrad),
        INTEGER(outdt),
        INTEGER(ltre),
        INTEGER(ltne),
        INTEGER(ltrd),
        INTEGER(ltnd),
        INTEGER(lgr3),
        INTEGER(lgn3),
        INTEGER(lgr4),
        REAL(longitude),
        REAL(latitude),
        REAL(altitude),
        REAL(whc),
        INTEGER(n),
        REAL(par),
        REAL(forcing),
        REAL(output)
        );

    UNPROTECT(1);

    return output;
}

/////////////////////////////////////////////////////////////
// biomee
/////////////////////////////////////////////////////////////
void F77_NAME(biomee_f)(
    int    *spinup, // LOGICAL type is not supported in the C interface (LTO)
    int    *spinupyears,               
    int    *recycle,              
    int    *firstyeartrend,                  
    int    *nyeartrend,
    int    *steps_per_day,
    int    *do_U_shaped_mortality, //LOGICAL
    int    *update_annualLAImax, //LOGICAL
    int    *do_closedN_run, //LOGICAL
    int    *code_method_photosynth,
    int    *code_method_mortality,                   
    double *longitude,                  
    double *latitude,                  
    double *altitude,                  
    int    *soiltype,                   
    double *FLDCAP,                   
    double *WILTPT,                   
    double *K1,                   
    double *K2,                   
    double *K_nitrogen,                   
    double *MLmixRatio,                   
    double *etaN,
    double *LMAmin,                                      
    double *fsc_fine,                   
    double *fsc_wood,                   
    double *GR_factor,                   
    double *l_fract,
    double *retransN,
    double *f_initialBSW,
    double *f_N_add,
    double *tf_base,
    double *par_mort,
    double *par_mort_under,
    int    *n_params_species,
    double *params_species,
    int    *n_init_cohort,
    double *init_cohort,
    double *init_soil,
    int    *nt,
    int    *nt_daily,
    int    *nt_annual,
    int    *nt_annual_cohorts,
    double *forcing,
    int    *n_lu,
    double *init_lu,
    int    *n_lu_tr,
    int    *n_lu_tr_years,
    double *luc,
    double *output_daily_tile,
    double *output_annual_tile,
    double *output_annual_cohorts
    );

// C wrapper function for biomee
extern SEXP biomee_f_C(
    SEXP spinup,                
    SEXP spinupyears,
    SEXP recycle,                 
    SEXP firstyeartrend,                  
    SEXP nyeartrend,
    SEXP steps_per_day,
    SEXP do_U_shaped_mortality,             
    SEXP update_annualLAImax,                   
    SEXP do_closedN_run,
    SEXP code_method_photosynth,
    SEXP code_method_mortality,                
    SEXP longitude,                  
    SEXP latitude,                  
    SEXP altitude,                 
    SEXP soiltype,
    SEXP FLDCAP,
    SEXP WILTPT,
    SEXP K1,
    SEXP K2,
    SEXP K_nitrogen,
    SEXP MLmixRatio,
    SEXP etaN,
    SEXP LMAmin,
    SEXP fsc_fine,
    SEXP fsc_wood,
    SEXP GR_factor,
    SEXP l_fract,
    SEXP retransN,
    SEXP f_initialBSW,
    SEXP f_N_add, 
    SEXP tf_base, 
    SEXP par_mort,
    SEXP par_mort_under,
    SEXP n_params_species,
    SEXP params_species,
    SEXP init_cohort,
    SEXP init_soil,
    SEXP n_daily,
    SEXP n_annual,                
    SEXP n_annual_cohorts,                
    SEXP forcing,
    SEXP init_lu,
    SEXP luc
    ){

    // Number of time steps (same in forcing and output)
    int nt_daily = asInteger(n_daily);
    int nt_annual = asInteger(n_annual);
    int nt_annual_cohorts = asInteger(n_annual_cohorts);
    int n_init_cohort, nt, n_lu, n_lu_tr, n_lu_tr_years;
    SEXP Rdim;
    // Output as list
    SEXP out_list = PROTECT( allocVector(VECSXP, 3) );

    /*// Example of 4 dimension array
    // Dimensions
    int pDims[4] = {2, 3, 4, 5};
    SEXP dims = allocVector(INTSXP, 4);
    // INTEGER(dims) is a int* which we initialise with pDims
    memcpy(INTEGER(dims), pDims, 4 * sizeof(int));
    // Allocate 4D array
    SEXP test = PROTECT(allocArray(INTSXP, dims));*/

    // Extracting array dimensions (they need to be passed to fortran separately)
    Rdim = getAttrib(init_cohort,R_DimSymbol);
    n_init_cohort = asInteger(Rdim);
    Rdim = getAttrib(forcing,R_DimSymbol);
    nt = asInteger(Rdim);
    n_lu = length(init_lu);
    Rdim = getAttrib(luc,R_DimSymbol);
    n_lu_tr = INTEGER(Rdim)[0];
    n_lu_tr_years = INTEGER(Rdim)[1];

    // Specify output
    // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.
    SEXP output_daily_tile             = PROTECT( allocMatrix(REALSXP, nt_daily, 35) );
    SEXP output_annual_tile            = PROTECT( allocMatrix(REALSXP, nt_annual, 60) );
    SEXP output_annual_cohort_tile     = PROTECT( alloc3DArray(REALSXP, 50, nt_annual_cohorts, 35) );
    
    // Fortran subroutine call
    F77_CALL(biomee_f)(
        INTEGER(spinup),
        INTEGER(spinupyears),                  
        INTEGER(recycle),                 
        INTEGER(firstyeartrend),                  
        INTEGER(nyeartrend),
        INTEGER(steps_per_day),
        INTEGER(do_U_shaped_mortality),
        INTEGER(update_annualLAImax),
        INTEGER(do_closedN_run),
        INTEGER(code_method_photosynth),
        INTEGER(code_method_mortality),              
        REAL(longitude),                  
        REAL(latitude),                  
        REAL(altitude),                  
        INTEGER(soiltype),                   
        REAL(FLDCAP),                   
        REAL(WILTPT),                   
        REAL(K1),                   
        REAL(K2),                   
        REAL(K_nitrogen),                   
        REAL(MLmixRatio),                   
        REAL(etaN),                   
        REAL(LMAmin),                   
        REAL(fsc_fine),                   
        REAL(fsc_wood),                   
        REAL(GR_factor),  
        REAL(l_fract),  
        REAL(retransN),  
        REAL(f_initialBSW),  
        REAL(f_N_add),  
        REAL(tf_base),  
        REAL(par_mort),  
        REAL(par_mort_under),  
        INTEGER(n_params_species),
        REAL(params_species),
        &n_init_cohort,
        REAL(init_cohort),
        REAL(init_soil),
        &nt,
        &nt_daily,
        &nt_annual,
        &nt_annual_cohorts,
        REAL(forcing),
        &n_lu,
        REAL(init_lu),
        &n_lu_tr,
        &n_lu_tr_years,
        REAL(luc),
        REAL(output_daily_tile),
        REAL(output_annual_tile),
        REAL(output_annual_cohort_tile)
        );

    SET_VECTOR_ELT(out_list, 0,  output_daily_tile);
    SET_VECTOR_ELT(out_list, 1, output_annual_tile);
    SET_VECTOR_ELT(out_list, 2, output_annual_cohort_tile);

    UNPROTECT(4);

    return out_list;
}

/////////////////////////////////////////////////////////////
// Declarations for all functions
/////////////////////////////////////////////////////////////
static const R_CallMethodDef CallEntries[] = {
  {"pmodel_f_C",   (DL_FUNC) &pmodel_f_C,   23},  // Specify number of arguments to C wrapper as the last number here
  {"biomee_f_C",   (DL_FUNC) &biomee_f_C,   43},  // Number of arguments of the C wrapper function for biomee (the SEXP variables, not the output)
  { NULL,          NULL,                    0 }
};

void R_init_rsofun(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("rsofun", "pmodel_f_C",  (DL_FUNC) &pmodel_f_C);
    R_RegisterCCallable("rsofun", "biomee_f_C",  (DL_FUNC) &biomee_f_C);
}
