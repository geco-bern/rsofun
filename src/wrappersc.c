#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

// Fortran subroutine registration
// P-model
void F77_NAME(pmodel_f)(
    _Bool *spinup,
    int *spinupyears,
    int *recycle,
    int *firstyeartrend,
    int *nyeartrend,
    _Bool *soilmstress,
    _Bool *tempstress,
    _Bool *calc_aet_fapar_vpd,
    _Bool *in_ppfd,
    _Bool *in_netrad,
    int *const_clim_year,
    int *const_lu_year,
    int *const_co2_year,
    int *const_ndep_year,
    int *const_nfert_year,
    int *outdt,
    _Bool *ltre,
    _Bool *ltne,
    _Bool *ltrd,
    _Bool *ltnd,
    _Bool *lgr3,
    _Bool *lgn3,
    _Bool *lgr4,
    double *longitude,
    double *latitude,
    double *altitude,
    double *whc,
    double *soiltexture,
    int *nt,
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
    SEXP const_clim_year,
    SEXP const_lu_year,
    SEXP const_co2_year,
    SEXP const_ndep_year,
    SEXP const_nfert_year,
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
        INTEGER(const_clim_year),
        INTEGER(const_lu_year),
        INTEGER(const_co2_year),
        INTEGER(const_ndep_year),
        INTEGER(const_nfert_year),
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


// LM3PPA
void F77_NAME(lm3ppa_f)(
    _Bool *spinup,
    int *spinupyears,
    int *recycle,
    int *firstyeartrend,
    int *nyeartrend,
    _Bool *soilmstress,
    _Bool *tempstress,
    _Bool *calc_aet_fapar_vpd,
    _Bool *in_ppfd,
    _Bool *in_netrad,
    int *const_clim_year,
    int *const_lu_year,
    int *const_co2_year,
    int *const_ndep_year,
    int *const_nfert_year,
    int *outdt,
    _Bool *ltre,
    _Bool *ltne,
    _Bool *ltrd,
    _Bool *ltnd,
    _Bool *lgr3,
    _Bool *lgn3,
    _Bool *lgr4,
    double *longitude,
    double *latitude,
    double *altitude,
    double *whc,
    double *soiltexture,
    int *nt,
    double *par,
    double *forcing,
    double *output
    );

// C wrapper function for LM3PPA
extern SEXP lm3ppa_f_C(
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
    SEXP const_clim_year,
    SEXP const_lu_year,
    SEXP const_co2_year,
    SEXP const_ndep_year,
    SEXP const_nfert_year,
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
    F77_CALL(lm3ppa_f)(
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
        INTEGER(const_clim_year),
        INTEGER(const_lu_year),
        INTEGER(const_co2_year),
        INTEGER(const_ndep_year),
        INTEGER(const_nfert_year),
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


static const R_CallMethodDef CallEntries[] = {
  {"pmodel_f_C",   (DL_FUNC) &pmodel_f_C,   31},  // Specify number of arguments to C wrapper as the last number here
  {"lm3ppa_f_C",   (DL_FUNC) &lm3ppa_f_C,   31},  // Specify number of arguments to C wrapper as the last number here
  {NULL,         NULL,                0}
};


void R_init_rsofun(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("rsofun", "pmodel_f_C",  (DL_FUNC) &pmodel_f_C);
    R_RegisterCCallable("rsofun", "lm3ppa_f_C",  (DL_FUNC) &lm3ppa_f_C);
}
