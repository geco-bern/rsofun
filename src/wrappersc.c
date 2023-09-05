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
    int    *spinup, // LOGICAL can be defined as _Bool but it gives a warming  
    int    *spinupyears,
    int    *recycle,
    int    *firstyeartrend,
    int    *nyeartrend,
    int    *secs_per_tstep,
    int    *in_ppfd,
    int    *in_netrad,
    int    *outdt,
    int    *ltre,
    int    *ltne,
    int    *ltrd,
    int    *ltnd,
    int    *lgr3,
    int    *lgn3,
    int    *lgr4,
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
    const int nt = INTEGER(n)[0] ;

    // Specify output
    // 2nd agument to allocMatrix is number of rows, 3rd is number of columns
    SEXP output = PROTECT( allocMatrix(REALSXP, nt, 18) );

    // Fortran subroutine call
    F77_CALL(pmodel_f)(
        LOGICAL(spinup),
        INTEGER(spinupyears),
        INTEGER(recycle),
        INTEGER(firstyeartrend),
        INTEGER(nyeartrend),
        INTEGER(secs_per_tstep),
        LOGICAL(in_ppfd),
        LOGICAL(in_netrad),
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
// biomee
/////////////////////////////////////////////////////////////
void F77_NAME(biomee_f)(
    int    *spinup, // LOGICAL can be defined as _Bool but it gives a warming
    int    *spinupyears,               
    int    *recycle,              
    int    *firstyeartrend,                  
    int    *nyeartrend, 
    int    *outputhourly,                   
    int    *outputdaily,                   
    int    *do_U_shaped_mortality,             
    int    *update_annualLAImax,                   
    int    *do_closedN_run,                   
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
    int    *nt_annual_cohorts,                
    double *forcing,                  
    double *output_hourly_tile,   
    double *output_daily_tile,    
    double *output_daily_cohorts_year,
    double *output_daily_cohorts_doy,
    double *output_daily_cohorts_hour,
    double *output_daily_cohorts_cID,
    double *output_daily_cohorts_PFT,
    double *output_daily_cohorts_layer,
    double *output_daily_cohorts_density,
    double *output_daily_cohorts_f_layer,
    double *output_daily_cohorts_LAI,
    double *output_daily_cohorts_gpp,
    double *output_daily_cohorts_resp,
    double *output_daily_cohorts_transp,
    double *output_daily_cohorts_NPPleaf,
    double *output_daily_cohorts_NPProot,
    double *output_daily_cohorts_NPPwood,
    double *output_daily_cohorts_NSC,
    double *output_daily_cohorts_seedC,
    double *output_daily_cohorts_leafC,
    double *output_daily_cohorts_rootC,
    double *output_daily_cohorts_SW_C,
    double *output_daily_cohorts_HW_C,
    double *output_daily_cohorts_NSN,
    double *output_daily_cohorts_seedN,
    double *output_daily_cohorts_leafN,
    double *output_daily_cohorts_rootN,
    double *output_daily_cohorts_SW_N,
    double *output_daily_cohorts_HW_N,
    double *output_annual_tile,   
    double *output_annual_cohorts_year,
    double *output_annual_cohorts_cID,
    double *output_annual_cohorts_PFT,
    double *output_annual_cohorts_layer,
    double *output_annual_cohorts_density,
    double *output_annual_cohorts_f_layer,
    double *output_annual_cohorts_dDBH,
    double *output_annual_cohorts_dbh,
    double *output_annual_cohorts_height,
    double *output_annual_cohorts_age,
    double *output_annual_cohorts_Acrown,
    double *output_annual_cohorts_wood,
    double *output_annual_cohorts_nsc,
    double *output_annual_cohorts_NSN,
    double *output_annual_cohorts_NPPtr,
    double *output_annual_cohorts_seed,
    double *output_annual_cohorts_NPPL,
    double *output_annual_cohorts_NPPR,
    double *output_annual_cohorts_NPPW,
    double *output_annual_cohorts_GPP,
    double *output_annual_cohorts_NPP,
    double *output_annual_cohorts_Rauto,
    double *output_annual_cohorts_N_uptk,
    double *output_annual_cohorts_N_fix,
    double *output_annual_cohorts_maxLAI,
    double *output_annual_cohorts_Volume,
    double *output_annual_cohorts_n_deadtrees,
    double *output_annual_cohorts_c_deadtrees,
    double *output_annual_cohorts_deathrate
    );

// C wrapper function for biomee
extern SEXP biomee_f_C(
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
    SEXP n_annual_cohorts,                
    SEXP forcing                 
    ){

    // // Number of time steps (same in forcing and output)
    const int nt = INTEGER(n)[0];
    const int nt_daily = INTEGER(n_daily)[0];
    const int nt_annual = INTEGER(n_annual)[0];
    const int nt_annual_cohorts = INTEGER(n_annual_cohorts)[0];

    // // Specify output
    SEXP output_hourly_tile            = PROTECT( allocMatrix(REALSXP, nt,       15) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_daily_tile             = PROTECT( allocMatrix(REALSXP, nt_daily, 35) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_daily_cohorts_year     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_doy      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_hour     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_cID      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_PFT      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_layer    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_density  = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_f_layer  = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_LAI      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_gpp      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_resp     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_transp   = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_NPPleaf  = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_NPProot  = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_NPPwood  = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_NSC      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_seedC    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_leafC    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_rootC    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_SW_C     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_HW_C     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_NSN      = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_seedN    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_leafN    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_rootN    = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_SW_N     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_daily_cohorts_HW_N     = PROTECT( allocMatrix(REALSXP, nt_daily, 50) );
    SEXP output_annual_tile            = PROTECT( allocMatrix(REALSXP, nt_annual, 59) );   // 2nd agument to allocMatrix is number of rows, 3rd is number of columns.  xxx todo
    SEXP output_annual_cohorts_year    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_cID     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_PFT     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_layer   = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_density = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_f_layer = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_dDBH    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_dbh     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_height  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_age     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_Acrown  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_wood    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_nsc     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_NSN     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_NPPtr   = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_seed    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_NPPL    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_NPPR    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_NPPW    = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_GPP     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_NPP     = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_Rauto   = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_N_uptk  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_N_fix   = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_maxLAI  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_Volume  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_n_deadtrees  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_c_deadtrees  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    SEXP output_annual_cohorts_deathrate  = PROTECT( allocMatrix(REALSXP, nt_annual_cohorts, 50) );
    
    // Fortran subroutine call
    F77_CALL(biomee_f)(
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
        INTEGER(n_annual_cohorts),                
        REAL(forcing),             
        REAL(output_hourly_tile),  
        REAL(output_daily_tile),    
        REAL(output_daily_cohorts_year),
        REAL(output_daily_cohorts_doy),
        REAL(output_daily_cohorts_hour),
        REAL(output_daily_cohorts_cID),
        REAL(output_daily_cohorts_PFT),
        REAL(output_daily_cohorts_layer),
        REAL(output_daily_cohorts_density),
        REAL(output_daily_cohorts_f_layer),
        REAL(output_daily_cohorts_LAI),
        REAL(output_daily_cohorts_gpp),
        REAL(output_daily_cohorts_resp),
        REAL(output_daily_cohorts_transp),
        REAL(output_daily_cohorts_NPPleaf),
        REAL(output_daily_cohorts_NPProot),
        REAL(output_daily_cohorts_NPPwood),
        REAL(output_daily_cohorts_NSC),
        REAL(output_daily_cohorts_seedC),
        REAL(output_daily_cohorts_leafC),
        REAL(output_daily_cohorts_rootC),
        REAL(output_daily_cohorts_SW_C),
        REAL(output_daily_cohorts_HW_C),
        REAL(output_daily_cohorts_NSN),
        REAL(output_daily_cohorts_seedN),
        REAL(output_daily_cohorts_leafN),
        REAL(output_daily_cohorts_rootN),
        REAL(output_daily_cohorts_SW_N),
        REAL(output_daily_cohorts_HW_N),
        REAL(output_annual_tile),  
        REAL(output_annual_cohorts_year),
        REAL(output_annual_cohorts_cID),
        REAL(output_annual_cohorts_PFT),
        REAL(output_annual_cohorts_layer),
        REAL(output_annual_cohorts_density),
        REAL(output_annual_cohorts_f_layer),
        REAL(output_annual_cohorts_dDBH),
        REAL(output_annual_cohorts_dbh),
        REAL(output_annual_cohorts_height),
        REAL(output_annual_cohorts_age),
        REAL(output_annual_cohorts_Acrown),
        REAL(output_annual_cohorts_wood),
        REAL(output_annual_cohorts_nsc),
        REAL(output_annual_cohorts_NSN),
        REAL(output_annual_cohorts_NPPtr),
        REAL(output_annual_cohorts_seed),
        REAL(output_annual_cohorts_NPPL),
        REAL(output_annual_cohorts_NPPR),
        REAL(output_annual_cohorts_NPPW),
        REAL(output_annual_cohorts_GPP),
        REAL(output_annual_cohorts_NPP),
        REAL(output_annual_cohorts_Rauto),
        REAL(output_annual_cohorts_N_uptk),
        REAL(output_annual_cohorts_N_fix),
        REAL(output_annual_cohorts_maxLAI),
        REAL(output_annual_cohorts_Volume),
        REAL(output_annual_cohorts_n_deadtrees),
        REAL(output_annual_cohorts_c_deadtrees),
        REAL(output_annual_cohorts_deathrate)
        );

    // // Output as list
    SEXP out_list = PROTECT( allocVector(VECSXP, 59) );  // maybe try  STRSXP instead of VECSXP
    
    SET_VECTOR_ELT(out_list, 0,  output_hourly_tile);
    SET_VECTOR_ELT(out_list, 1,  output_daily_tile);
    SET_VECTOR_ELT(out_list, 2,  output_daily_cohorts_year ); 
    SET_VECTOR_ELT(out_list, 3,  output_daily_cohorts_doy );  
    SET_VECTOR_ELT(out_list, 4,  output_daily_cohorts_hour );  
    SET_VECTOR_ELT(out_list, 5,  output_daily_cohorts_cID );  
    SET_VECTOR_ELT(out_list, 6,  output_daily_cohorts_PFT );  
    SET_VECTOR_ELT(out_list, 7,  output_daily_cohorts_layer );  
    SET_VECTOR_ELT(out_list, 8,  output_daily_cohorts_density );  
    SET_VECTOR_ELT(out_list, 9,  output_daily_cohorts_f_layer );  
    SET_VECTOR_ELT(out_list, 10, output_daily_cohorts_LAI );  
    SET_VECTOR_ELT(out_list, 11, output_daily_cohorts_gpp );  
    SET_VECTOR_ELT(out_list, 12, output_daily_cohorts_resp );  
    SET_VECTOR_ELT(out_list, 13, output_daily_cohorts_transp );  
    SET_VECTOR_ELT(out_list, 14, output_daily_cohorts_NPPleaf );  
    SET_VECTOR_ELT(out_list, 15, output_daily_cohorts_NPProot );  
    SET_VECTOR_ELT(out_list, 16, output_daily_cohorts_NPPwood );  
    SET_VECTOR_ELT(out_list, 17, output_daily_cohorts_NSC );  
    SET_VECTOR_ELT(out_list, 18, output_daily_cohorts_seedC );  
    SET_VECTOR_ELT(out_list, 19, output_daily_cohorts_leafC );  
    SET_VECTOR_ELT(out_list, 20, output_daily_cohorts_rootC );  
    SET_VECTOR_ELT(out_list, 21, output_daily_cohorts_SW_C );  
    SET_VECTOR_ELT(out_list, 22, output_daily_cohorts_HW_C );  
    SET_VECTOR_ELT(out_list, 23, output_daily_cohorts_NSN );  
    SET_VECTOR_ELT(out_list, 24, output_daily_cohorts_seedN );  
    SET_VECTOR_ELT(out_list, 25, output_daily_cohorts_leafN );  
    SET_VECTOR_ELT(out_list, 26, output_daily_cohorts_rootN );  
    SET_VECTOR_ELT(out_list, 27, output_daily_cohorts_SW_N );  
    SET_VECTOR_ELT(out_list, 28, output_daily_cohorts_HW_N );  

    SET_VECTOR_ELT(out_list, 29, output_annual_tile);

    SET_VECTOR_ELT(out_list, 30, output_annual_cohorts_year);
    SET_VECTOR_ELT(out_list, 31, output_annual_cohorts_cID);
    SET_VECTOR_ELT(out_list, 32, output_annual_cohorts_PFT);
    SET_VECTOR_ELT(out_list, 33, output_annual_cohorts_layer);
    SET_VECTOR_ELT(out_list, 34, output_annual_cohorts_density);
    SET_VECTOR_ELT(out_list, 35, output_annual_cohorts_f_layer);
    SET_VECTOR_ELT(out_list, 36, output_annual_cohorts_dDBH);
    SET_VECTOR_ELT(out_list, 37, output_annual_cohorts_dbh);
    SET_VECTOR_ELT(out_list, 38, output_annual_cohorts_height);
    SET_VECTOR_ELT(out_list, 39, output_annual_cohorts_age);
    SET_VECTOR_ELT(out_list, 40, output_annual_cohorts_Acrown);
    SET_VECTOR_ELT(out_list, 41, output_annual_cohorts_wood);
    SET_VECTOR_ELT(out_list, 42, output_annual_cohorts_nsc);
    SET_VECTOR_ELT(out_list, 43, output_annual_cohorts_NSN);
    SET_VECTOR_ELT(out_list, 44, output_annual_cohorts_NPPtr);
    SET_VECTOR_ELT(out_list, 45, output_annual_cohorts_seed);
    SET_VECTOR_ELT(out_list, 46, output_annual_cohorts_NPPL);
    SET_VECTOR_ELT(out_list, 47, output_annual_cohorts_NPPR);
    SET_VECTOR_ELT(out_list, 48, output_annual_cohorts_NPPW);
    SET_VECTOR_ELT(out_list, 49, output_annual_cohorts_GPP);
    SET_VECTOR_ELT(out_list, 50, output_annual_cohorts_NPP);
    SET_VECTOR_ELT(out_list, 51, output_annual_cohorts_Rauto);
    SET_VECTOR_ELT(out_list, 52, output_annual_cohorts_N_uptk);
    SET_VECTOR_ELT(out_list, 53, output_annual_cohorts_N_fix);
    SET_VECTOR_ELT(out_list, 54, output_annual_cohorts_maxLAI);
    SET_VECTOR_ELT(out_list, 55, output_annual_cohorts_Volume);
    SET_VECTOR_ELT(out_list, 56, output_annual_cohorts_n_deadtrees);
    SET_VECTOR_ELT(out_list, 57, output_annual_cohorts_c_deadtrees);
    SET_VECTOR_ELT(out_list, 58, output_annual_cohorts_deathrate);

    UNPROTECT(60);

    return out_list;
}

/////////////////////////////////////////////////////////////
// Declarations for all functions
/////////////////////////////////////////////////////////////
static const R_CallMethodDef CallEntries[] = {
  {"pmodel_f_C",   (DL_FUNC) &pmodel_f_C,   23},  // Specify number of arguments to C wrapper as the last number here
  {"biomee_f_C",   (DL_FUNC) &biomee_f_C,   46},  // Number of the SEXP variables (not the output)
  {NULL,         NULL,                0}
};

void R_init_rsofun(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("rsofun", "pmodel_f_C",  (DL_FUNC) &pmodel_f_C);
    R_RegisterCCallable("rsofun", "biomee_f_C",  (DL_FUNC) &biomee_f_C);
}
