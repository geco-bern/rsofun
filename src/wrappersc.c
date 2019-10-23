#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

// Fortran subroutine registration
void F77_NAME(sofun_f)(
    _Bool *spinup,
    int *spinupyears,
    int *recycle,
    int *firstyeartrend,
    int *nyeartrend,
    _Bool *soilmstress,
    _Bool *tempstress,
    _Bool *in_ppfd,
    _Bool *in_netrad,
    int *const_clim_year,
    int *const_lu_year,
    int *const_co2_year,
    int *const_ndep_year,
    int *const_nfert_year,
    int *daily_out_startyr,
    int *daily_out_endyr,
    int *outdt,
    _Bool *ltre,
    _Bool *ltne,
    _Bool *ltrd,
    _Bool *ltnd,
    _Bool *lgr3,
    _Bool *lgn3,
    _Bool *lgr4,
    _Bool *loutplant,
    _Bool *loutgpp,
    _Bool *loutwaterbal,
    _Bool *loutforcing,
    _Bool *loutdgpp,
    _Bool *loutdrd,
    _Bool *loutdtransp,
    _Bool *loutdwcont,
    _Bool *loutdaet,
    _Bool *loutdpet,
    _Bool *loutdnetrad,
    _Bool *loutdwbal,
    _Bool *loutdtemp,
    _Bool *loutdfapar,
    _Bool *loutdtemp_soil,
    _Bool *lcalibgpp,
    _Bool *lcalibfapar,
    _Bool *lcalibtransp,
    _Bool *lcaliblatenth,
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

// C wrapper function
extern SEXP sofun_f_C(
    SEXP spinup,
    SEXP spinupyears,
    SEXP recycle,
    SEXP firstyeartrend,
    SEXP nyeartrend,
    SEXP soilmstress,
    SEXP tempstress,
    SEXP in_ppfd,
    SEXP in_netrad,
    SEXP const_clim_year,
    SEXP const_lu_year,
    SEXP const_co2_year,
    SEXP const_ndep_year,
    SEXP const_nfert_year,
    SEXP daily_out_startyr,
    SEXP daily_out_endyr,
    SEXP outdt,
    SEXP ltre,
    SEXP ltne,
    SEXP ltrd,
    SEXP ltnd,
    SEXP lgr3,
    SEXP lgn3,
    SEXP lgr4,
    SEXP loutplant,
    SEXP loutgpp,
    SEXP loutwaterbal,
    SEXP loutforcing,
    SEXP loutdgpp,
    SEXP loutdrd,
    SEXP loutdtransp,
    SEXP loutdwcont,
    SEXP loutdaet,
    SEXP loutdpet,
    SEXP loutdnetrad,
    SEXP loutdwbal,
    SEXP loutdtemp,
    SEXP loutdfapar,
    SEXP loutdtemp_soil,
    SEXP lcalibgpp,
    SEXP lcalibfapar,
    SEXP lcalibtransp,
    SEXP lcaliblatenth,
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
    F77_CALL(sofun_f)(
        LOGICAL(spinup),
        INTEGER(spinupyears),
        INTEGER(recycle),
        INTEGER(firstyeartrend),
        INTEGER(nyeartrend),
        LOGICAL(soilmstress),
        LOGICAL(tempstress),
        LOGICAL(in_ppfd),
        LOGICAL(in_netrad),
        INTEGER(const_clim_year),
        INTEGER(const_lu_year),
        INTEGER(const_co2_year),
        INTEGER(const_ndep_year),
        INTEGER(const_nfert_year),
        INTEGER(daily_out_startyr),
        INTEGER(daily_out_endyr),
        INTEGER(outdt),
        LOGICAL(ltre),
        LOGICAL(ltne),
        LOGICAL(ltrd),
        LOGICAL(ltnd),
        LOGICAL(lgr3),
        LOGICAL(lgn3),
        LOGICAL(lgr4),
        LOGICAL(loutplant),
        LOGICAL(loutgpp),
        LOGICAL(loutwaterbal),
        LOGICAL(loutforcing),
        LOGICAL(loutdgpp),
        LOGICAL(loutdrd),
        LOGICAL(loutdtransp),
        LOGICAL(loutdwcont),
        LOGICAL(loutdaet),
        LOGICAL(loutdpet),
        LOGICAL(loutdnetrad),
        LOGICAL(loutdwbal),
        LOGICAL(loutdtemp),
        LOGICAL(loutdfapar),
        LOGICAL(loutdtemp_soil),
        LOGICAL(lcalibgpp),
        LOGICAL(lcalibfapar),
        LOGICAL(lcalibtransp),
        LOGICAL(lcaliblatenth),
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
  {"sofun_f_C",   (DL_FUNC) &sofun_f_C,   51},  // Specify number of arguments to C wrapper as the last number here
  {NULL,         NULL,                0}
};

void R_init_rsofun(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("rsofun", "sofun_f_C",  (DL_FUNC) &sofun_f_C);
}
