module md_nuptake
  !////////////////////////////////////////////////////////////////
  ! FUN NITROGEN UPTAKE MODULE
  ! Contains the "main" subroutine 'nuptake' and all necessary 
  ! subroutines for handling input/output. 
  ! Every module that implements 'nuptake' must contain this list 
  ! of subroutines (names that way).
  !   - nuptake
  !   - getpar_modl_nuptake
  !   - initio_nuptake
  !   - initoutput_nuptake
  !   - getout_daily_nuptake
  !   - getout_monthly_nuptake
  !   - writeout_ascii_nuptake
  ! Required module-independent model state variables (necessarily 
  ! updated by 'nuptake') are:
  !   - daily NPP ('dnpp')
  !   - soil temperature ('xxx')
  !   - inorganic N _pools ('no3', 'nh4')
  !   - xxx 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_params_core, only: ndayyear, nmonth, nlu, npft, maxgrid
  use md_classdefs
  use md_tile
  use md_plant

  implicit none

  private
  public nuptake, getpar_modl_nuptake, initdaily_nuptake, &
    calc_dnup, outtype_calc_dnup

  !----------------------------------------------------------------
  ! Public, module-specific state variables
  !----------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Uncertain (unknown) parameters. Runtime read-in
  !-----------------------------------------------------------------------
  type params_nuptake_type
    real :: eff_nup           ! uptake efficiency for equation (gN/gC)
    real :: minimumcostfix    ! Minimum cost of N-fixation at optimal soil temperature, is 4.8 gC/gN, value from Gutschik (1981)
    real :: fixoptimum        ! Optimum soil temperature for N fixation. Taken to be equal to optimum temperature for nitrogenase activity as given by Houlton et al. (2008), Nature: 25.15+-0.66 
    real :: a_param_fix       ! shape parameter of the N fixation function taken to be equal to nitrogenase activity function given Houlton et al. (2008), Nature: -3.62+-0.52
    real :: b_param_fix       ! shape parameter of the N fixation function taken to be equal to nitrogenase activity function given Houlton et al. (2008), Nature: 0.27+-0.04 
  end type params_nuptake_type

  type( params_nuptake_type ) :: params_nuptake

  !----------------------------------------------------------------
  ! module-specific (private) variables
  !----------------------------------------------------------------
  real, dimension(npft) :: dccost           ! daily mean C cost of N uptake [gC/gN] 
  real, dimension(npft) :: dnup_pas         ! daily passive N uptake [gN/m2/d]
  real, dimension(npft) :: dnup_act         ! daily active N uptake [gN/m2/d]  
  real, dimension(npft) :: dnup_ret         ! daily N uptake [gN/m2/d]

  type outtype_calc_dnup
    real :: act_nh4                         ! NH4 acquisition by active uptake [gN/m2/d]
    real :: act_no3                         ! NO3 acquisition by active uptake [gN/m2/d]
    real :: fix                             ! N acquisition by fixation [gN/m2/d]
  end type outtype_calc_dnup

  ! !----------------------------------------------------------------
  ! ! Module-specific output variables
  ! !----------------------------------------------------------------
  ! ! daily
  ! real, allocatable, dimension(:,:,:) :: outdccost   ! daily mean C cost of N uptake (gC/gN) 
  ! real, allocatable, dimension(:,:,:) :: outdnup_pas
  ! real, allocatable, dimension(:,:,:) :: outdnup_act
  ! real, allocatable, dimension(:,:,:) :: outdnup_fix
  ! real, allocatable, dimension(:,:,:) :: outdnup_ret


contains


  subroutine nuptake( tile, tile_fluxes )
    !/////////////////////////////////////////////////////////////////
    ! SUBROUTINE NUPTAKE ASSUMING CONSTANT EXUDATION PER UNIT ROOT MASS
    !-----------------------------------------------------------------
    ! This model calculates first the passive uptake of N via the 
    ! transpiration stream.
    !-----------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes

    ! local variables
    integer :: lu, pft
    real    :: avail_ninorg                        ! available inorganic N in soil layer (gN/m2)
    real    :: ninorg_conc                         ! inorganic N concentration (gN/gH2O)
    real    :: n_uptake_pass                       ! (gN)
    real    :: n_uptake_retrans
    ! real    :: dNacq_act
    ! real    :: dNacq_fix
    real    :: dmean_cost

    type( outtype_calc_dnup ) :: out_calc_dnup

    pftloop: do pft = 1, npft

      lu = params_pft_plant(pft)%lu_category

      ! xxx think about this: order in which PFTs get access to Ninorg matters!
      
      ! write(0,*) '---- in nuptake:'

      !//////////////////////////////////////////////////////////////////////////
      ! INITIALIZATION
      !-------------------------------------------------------------------------
      n_uptake_pass         = 0.0
      out_calc_dnup%act_nh4 = 0.0                          ! active uptake, sum over sub-timesteps
      out_calc_dnup%act_no3 = 0.0                          ! active uptake, sum over sub-timesteps
      out_calc_dnup%fix     = 0.0                          ! N fixation, sum over sub-timesteps
      n_uptake_retrans      = 0.0

      if ( tile_fluxes(lu)%plant(pft)%dcex > 0.0 ) then
        !//////////////////////////////////////////////////////////////////////////
        ! USE STORED N (RETRANSLOCATION)
        !--------------------------------------------------------------------------
        ! As opposed to original FUN model, in which N is retranslocated at a
        ! variable cost during leaf fall (turnover), a fraction of N is retained here
        ! from turnover. It is stored at the end of the last year and available to
        ! cover N demand during next year.
        ! Just reduce the demand by amount retranslocated, not the labile N pool itself
        !--------------------------------------------------------------------------
        ! xxx debug
        ! n_uptake_retrans = min( n_demand, plabl(pft,jpngr)%n%n14 )
        ! n_demand_remaining = n_demand_remaining - n_uptake_retrans


        ! !//////////////////////////////////////////////////////////////////////////
        ! ! PASSIVE UPTAKE
        ! ! No active control on passive uptake - always occurrs even if the unmet N
        ! ! demand is zero.
        ! !--------------------------------------------------------------------------
        ! n_uptake_pass = ninorg_conc * dtransp(pft) / 1000.0     ! [dtransp] = g H2O; [ninorg_conc] = g N / (mm H2O) = g N / (kg H2O)
        ! n_uptake_pass = min( n_uptake_pass, avail_ninorg )

        !//////////////////////////////////////////////////////////////////////////
        ! ACTIVE UPTAKE
        ! Active N uptake is a function of initial N available and C exuded
        !--------------------------------------------------------------------------
        out_calc_dnup  = calc_dnup( tile_fluxes(lu)%plant(pft)%dcex, &
                                    tile(lu)%soil%pnh4%n14, &
                                    tile(lu)%soil%pno3%n14, &
                                    params_pft_plant(pft)%nfixer, &
                                    tile(lu)%soil%phy%temp &
                                    )

        if ((out_calc_dnup%act_no3 + out_calc_dnup%act_nh4 + out_calc_dnup%fix)>0.0) then
          dmean_cost = tile_fluxes(lu)%plant(pft)%dcex  / (out_calc_dnup%act_no3 + out_calc_dnup%act_nh4 + out_calc_dnup%fix)
        else
          dmean_cost = 9999.0
        end if

        ! Update
        tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 - out_calc_dnup%act_no3
        tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 - out_calc_dnup%act_nh4

      end if

      !--------------------------------------------------------------------------
      ! Update N-uptake of this PFT. N-retranslocation is not considered
      ! N-uptake.
      !--------------------------------------------------------------------------
      ! daily
      tile_fluxes(lu)%plant(pft)%dnup%n14 = n_uptake_pass + out_calc_dnup%act_no3 + out_calc_dnup%act_nh4 + out_calc_dnup%fix  ! n_uptake_retrans is not considered uptake
      tile_fluxes(lu)%plant(pft)%dnup_pas = n_uptake_pass
      tile_fluxes(lu)%plant(pft)%dnup_act = out_calc_dnup%act_no3 + out_calc_dnup%act_nh4                
      tile_fluxes(lu)%plant(pft)%dnup_fix = out_calc_dnup%fix  
      tile_fluxes(lu)%plant(pft)%dnup_ret = n_uptake_retrans

      if (tile_fluxes(lu)%plant(pft)%dnup%n14 > 0.0) then
        dccost(pft) = dmean_cost       
      else
        dccost(pft) = 0.0
      endif

      !--------------------------------------------------------------------------
      ! N acquisition to labile pool
      !--------------------------------------------------------------------------
      call ncp( tile_fluxes(lu)%plant(pft)%dnup, tile(lu)%plant(pft)%plabl%n )

    end do pftloop

  end subroutine nuptake


  function calc_dnup( cexu, nh4, no3, isnfixer, soiltemp ) result( out_calc_dnup )
    !/////////////////////////////////////////////////////////////////
    ! With a FUN-like approach:
    ! dCexu/dNup = K / (N0 - Nup); K=1/eff_nup
    ! => Nup(Cexu) = N0 * ( 1.0 - exp( - eff_nup * cexu ) )
    !-----------------------------------------------------------------
    ! arguments
    real, intent(in)    :: cexu      ! C exuded (gC/m2/d)
    real, intent(in)    :: nh4       ! initially available NH4 (gN/m2)
    real, intent(in)    :: no3       ! initially available NO3 (gN/m2)
    logical, intent(in) :: isnfixer  ! true if pft is N-fixer
    real, intent(in)    :: soiltemp  ! soil temperature (deg C)

    ! function return variable
    type( outtype_calc_dnup ) :: out_calc_dnup

    ! local variables
    real :: n0    ! initially available total inorganic N (gN/m2)
    real :: fno3  ! NO3 share of total inorganic N (unitless)

    real :: mydnup_act
    real :: mydnup_fix
    real :: cost_bnf
    real :: eff_bnf
    real :: cexu_act
    real :: cexu_bnf

    !-----------------------------------------------------------------
    ! Get total inorganic N. N uptake is assumed to deplete NO3 and NH4
    ! in proportion to their relative shares.
    !-----------------------------------------------------------------
    n0 = nh4 + no3
    ! if (n0>0.0) then
    fno3 = no3 / n0

    if (isnfixer) then
      !-----------------------------------------------------------------
      ! N FIXER
      !-----------------------------------------------------------------
      ! Get efficiency of BNF in gN/gC as a function of soil temperature
      !-----------------------------------------------------------------
      eff_bnf = eff_fix( soiltemp )

      !-----------------------------------------------------------------
      ! Find amount of active uptake (~Cex) for which eff_act = eff_bnf
      ! eff_act = dNup_act / dCex
      ! Nup_act = N0 * ( 1.0 - exp( -K * Cex ) )
      ! dNup_act / dCex = K * exp( -K * Cex)
      ! dNup_act / dCex = eff_bnf
      ! ==> Cex = - 1/K * ln( bnf_eff/K )
      !-----------------------------------------------------------------
      cexu_act = -1.0 / params_nuptake%eff_nup * log( eff_bnf / ( n0 * params_nuptake%eff_nup ) )

      if (cexu_act < cexu) then
        !-----------------------------------------------------------------
        ! Remaining Cex is consumed by N fixing processes
        !-----------------------------------------------------------------
        cexu_bnf = cexu - cexu_act

        !-----------------------------------------------------------------
        ! N uptake via BNF
        !-----------------------------------------------------------------
        out_calc_dnup%fix = cexu_bnf * eff_bnf

        !-----------------------------------------------------------------
        ! N uptake via active uptake
        !-----------------------------------------------------------------
        out_calc_dnup%act_no3 = fno3         * n0 * ( 1.0 - exp( - params_nuptake%eff_nup * cexu_act ) )
        out_calc_dnup%act_nh4 = (1.0 - fno3) * n0 * ( 1.0 - exp( - params_nuptake%eff_nup * cexu_act ) )

      else

        out_calc_dnup%fix     = 0.0
        out_calc_dnup%act_no3 = fno3         * n0 * ( 1.0 - exp( - params_nuptake%eff_nup * cexu ) )
        out_calc_dnup%act_nh4 = (1.0 - fno3) * n0 * ( 1.0 - exp( - params_nuptake%eff_nup * cexu ) )

      end if

    else
      !-----------------------------------------------------------------
      ! NOT N FIXER
      !-----------------------------------------------------------------
      ! N uptake function of C exuded
      out_calc_dnup%act_no3 = fno3         * n0 * ( 1.0 - exp( - params_nuptake%eff_nup * cexu ) )
      out_calc_dnup%act_nh4 = (1.0 - fno3) * n0 * ( 1.0 - exp( - params_nuptake%eff_nup * cexu ) )

      ! no N fixation considered
      out_calc_dnup%fix = 0.0

    end if 


  end function calc_dnup


  function eff_fix( soiltemp ) result( out_eff_fix )
    !////////////////////////////////////////////////////////////////
    ! Calculates N fixation efficiency (dNfix/dCex) as a function of 
    ! soil temperature. The functional form is chosen to be equal to
    ! the (fitted) relationship between soil temperature and nitro-
    ! genase activity derived by Houlton et al. (2008), Nature. The 
    ! maximum efficiency is 0.21 gN/gC, which corresponds to the 
    ! inverse of the "minimum cost of N-fixation" of 4.8 gC/gN given 
    ! Gutschik (1981). At higher and lower temperature, efficiency de-
    ! clines to zero.
    !-----------------------------------------------------------------
    ! arguments    
    real, intent(in) :: soiltemp

    ! local variables
    real, parameter :: norm = 1.25  ! used to normalise efficiency (nitrogenase activiy) function to 1 at optimal soil temperature (see Houlton et al., 2008)

    ! function return variable
    real :: out_eff_fix             ! function return variable

    out_eff_fix = 1.0 / params_nuptake%minimumcostfix * norm * &
      exp( params_nuptake%a_param_fix + &
           params_nuptake%b_param_fix * soiltemp * ( 1.0 - 0.5 * soiltemp / params_nuptake%fixoptimum ) )

  end function eff_fix


  ! function calc_avail_ninorg( ninorg, wtot ) result( avail_ninorg )
  !   !//////////////////////////////////////////////////////////////////////////
  !   ! Returns N available for uptake accounting for constraint of mobility by
  !   ! soil moisture.
  !   !--------------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in)  :: ninorg 
  !   real, intent(in)  :: wtot           ! total soil water content (mm)

  !   ! function return value
  !   real, intent(out) :: avail_ninorg

  !   if ( wtot > EPSILON_WTOT ) then 
  !     avail_ninorg = ninorg - EPSILON_WTOT * ninorg / wtot
  !   else
  !     avail_ninorg = 0.0
  !   endif

  ! end function calc_avail_ninorg


  ! function calc_conc_ninorg( ninorg, wtot ) result( conc_ninorg )
  !   !//////////////////////////////////////////////////////////////////////////
  !   ! Returns N available for uptake accounting for constraint of mobility by
  !   ! soil moisture.
  !   !--------------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in)  :: ninorg 
  !   real, intent(in)  :: wtot           ! total soil water content (mm)

  !   ! function return value
  !   real, intent(out) :: conc_ninorg

  !   if ( wtot > EPSILON_WTOT ) then 
  !     conc_ninorg = ninorg / wtot
  !   else
  !     conc_ninorg = 0.0
  !   endif

  ! end function calc_conc_ninorg


  subroutine initdaily_nuptake()
    !////////////////////////////////////////////////////////////////
    ! Initialise daily variables with zero
    !----------------------------------------------------------------
    dnup_pas(:)    = 0.0
    dnup_act(:)    = 0.0
    dnup_ret(:)    = 0.0

  end subroutine initdaily_nuptake


  subroutine getpar_modl_nuptake()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads nuptake module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    ! initial N uptake efficiency from soil
    params_nuptake%eff_nup = 0.600000   ! xxx make calibratable getparreal( 'params/params_nuptake.dat', 'eff_nup' )

    ! Minimum cost of N-fixation is 4.8 gC/gN, value from Gutschik (1981)
    params_nuptake%minimumcostfix = 1.000000    !  xxx make calibratable getparreal( 'params/params_nuptake.dat', 'minimumcostfix' )

    ! Optimum temperature for N fixation. Taken to be equal to optimum temperature for 
    ! nitrogenase activity as given by Houlton et al. (2008), Nature: 25.15+-0.66 
    params_nuptake%fixoptimum = 25.15000   ! xxx make calibratable getparreal( 'params/params_nuptake.dat', 'fixoptimum' )
 
    ! shape parameter of the N fixation function taken to be equal to nitrogenase activity 
    ! function given Houlton et al. (2008), Nature: -3.62+-0.52 
    params_nuptake%a_param_fix = -3.62000   ! getparreal( 'params/params_nuptake.dat', 'a_param_fix' )

    ! shape parameter of the N fixation function taken to be equal to nitrogenase activity 
    ! function given Houlton et al. (2008), Nature: 0.27+-0.04 
    params_nuptake%b_param_fix = 0.270000  ! getparreal( 'params/params_nuptake.dat', 'b_param_fix' )


  end subroutine getpar_modl_nuptake



  ! subroutine n_fixation_cryptogam( day, lu, jpngr, dnfix_cpc, dnfix_cgc )
  ! !******************************************************************************
  ! ! SUBROUTINE N_UPTAKE BY CRYPTOGAMIC COVERS
  ! !-------------------------------------------------------------------------
  ! ! Simulated to match pattern and global total fixed N after Elbert et al.
  ! ! (2012), Nature Geoscience. Basic assumption: N uptake is driven by energy
  ! ! available (solar radiation ~ photosynthetically active radiation) and not
  ! ! absorbed by leafs or stems. N fixation by cryptogamic ground cover (CGC)
  ! ! thus scales with (1-VPC), where VPC is analogous to FPC but takes into
  ! ! account the shading by branches and stems. N fixation by cryptogamic
  ! ! plant covers (CPC) scales with SPC. 
  ! !-------------------------------------------------------------------------
  !
  ! implicit none
  !
  ! ! ARGUMENTS
  ! INTEGER day, lu, jpngr
  ! REAL*8 dnfix_cpc, dnfix_cgc
  ! 
  ! ! LOCAL VARIABLES
  ! INTEGER
  !$     pft,ppft
  ! 
  ! REAL*8
  !$     fpc_ind,               ! phenology-modulated (!) fractional plant cover
  !$     local_fpc_grid,        ! FPC w.r.t. grid cell area (is not the same as the global variable fpc_grid)
  !$     vpc_ind,               ! fractional vegetation cover including stems and branches
  !$     vpc_grid,              ! VPC w.r.t. grid cell area
  !$     spc_grid,              ! fractional stem/branches cover
  !$     fpc_grid_total,        ! fpc_grid summed over all PFTs in the present LU
  !$     vpc_grid_total,        ! vpc_grid summed over all PFTs in the present LU
  !$     spc_grid_total,        ! spc_grid summed over all PFTs in the present LU
  !$     lm_tot(npft),
  !$     scale
  !
  ! ! Initialisations
  ! vpc_grid_total = 0.
  ! fpc_grid_total = 0.
  ! spc_grid_total = 0.
  !
  ! !  ! Calculate ftemp
  ! !  if (soiltemp.ge.-40.) then
  ! !    tshift = 46.02d0
  ! !    ftemp = exp(308.56d0*(1.0/(20.+tshift)-1.0/
  ! ! $       (soiltemp+tshift)))                             ! Eq.8, XP08 (canexch.cpp:1018)
  ! !  else
  ! !    ftemp = 0.
  ! !  endif
  ! !  ftemp = min(ftemp, 1.)                              ! (canexch.cpp:1023)
  ! !  ftemp = max(ftemp, 0.)                              ! (canexch.cpp:1024)      
  !
  ! do pft=1,npft
  !   if ( present(pft,jpngr) .and. lu_category(pft) .eq. lu ) then
  !
  !   ! LM_TOT
  !   !--------------------------------------------------------------------------
  !   ! Non-linearity of Beer-Law causes very high FPC values when 2 Grasses are present.
  !   ! (Beer Law does NOT make sense for grasses, anyway.)
  !   ! Thus, use sum of all grass/moss-leaf masses and calculate FPC based on the sum.
  !   ! Then compute each PFT's FPC as the product of total-grass FPC times each PFT's leaf mass.
  !   !-------------------------------------------------------------------------
  !     lm_tot(pft) = 0.
  !     ! Grass: C3, C4 on natural, croplands, pasture, peatlands
  !     if (grass(pft)) then
  !       do ppft=1,npft
  !         if (lu_category(ppft).eq.lu_category(pft)) then
  !           if (grass(ppft)) lm_tot(pft) =
  !$               lm_tot(pft)+lm_ind(ppft,jpngr,1)
  !         endif
  !       enddo
  !     ! Moss: moss on peatlands
  !     elseif (moss(pft)) then
  !       do ppft=1,npft
  !         if (lu_category(ppft).eq.lu_category(pft)) then
  !           if (moss(ppft)) lm_tot(pft) =
  !$               lm_tot(pft)+lm_ind(ppft,jpngr,1)
  !         endif
  !       enddo
  !     ! Tree: tree on natural lands, peatlands
  !     else
  !       lm_tot(pft) = lm_ind(pft,jpngr,1)
  !     endif
  !     
  !     ! LAI
  !     !--------------------------------------------------------------------------
  !     if (crownarea(pft,jpngr).gt.0.) then
  !       lai_ind(pft,jpngr)=(lm_tot(pft)*sla(pft))/
  !$           crownarea(pft,jpngr)
  !     else
  !       lai_ind(pft,jpngr)=0.
  !     endif
  !     
  !     ! FPC and VPC
  !     !--------------------------------------------------------------------------
  !     ! Note that this is not identical to how it's calculated in SR update_fpc,
  !     ! where the phenology scaling factor is not included in the exponent.
  !     ! Fractional plant cover accounts for the fraction of the grid cell covered
  !     ! by the photosyntesic plant tissue. To be modulated by daily phenology!
  !     !--------------------------------------------------------------------------
  !     fpc_ind = 1.-dexp(
  !$                        -1.*kbeer*lai_ind(pft,jpngr)*dphen(day,pft)
  !$                        )
  !     vpc_ind = 1.-dexp(
  !$                        -1.*kbeer*(
  !$                                      lai_ind(pft,jpngr)*dphen(day,pft)
  !$                                      + pftpar(pft,46)
  !$                                      )
  !$                        )
  !     
  !     local_fpc_grid = fpc_ind * crownarea(pft,jpngr) * nind(pft,jpngr)
  !     vpc_grid       = vpc_ind * crownarea(pft,jpngr) * nind(pft,jpngr)
  !     
  !     if (lm_tot(pft).gt.0.) then
  !       local_fpc_grid = local_fpc_grid*lm_ind(pft,jpngr,1)
  !$           /lm_tot(pft)
  !       vpc_grid = vpc_grid*lm_ind(pft,jpngr,1)/lm_tot(pft)
  !     else
  !       local_fpc_grid = 0.
  !       vpc_grid       = 0. 
  !     endif
  !
  !     spc_grid = vpc_grid - local_fpc_grid
  !
  !     ! Sum over pfts
  !     !--------------------------------------------------------------------------
  !     fpc_grid_total = fpc_grid_total + local_fpc_grid
  !     vpc_grid_total = vpc_grid_total + vpc_grid
  !     spc_grid_total = spc_grid_total + spc_grid
  !
  !     ! print*,'spc_grid',spc_grid
  !     
  !     !!          call update_fpc(pft,jpngr)
  !     !          
  !     !      ! VAI is analogous to LAI but accounts for stems and branches in addition to
  !     !      ! leafs.
  !     !          vpc_ind = 1. - dexp(
  !     !     $                          - 1.*kbeer*(
  !     !     $                                         lai_ind(pft,jpngr)*dphen(day,pft)
  !     !     $                                         + pftpar(pft,46)
  !     !     $                                         )
  !     !     $                          )
  !     !          vpc_grid = vpc_ind * crownarea(pft,jpngr) * nind(pft,jpngr)
  !     !          vpc_grid_total = vpc_grid_total + vpc_grid
  !     !
  !     !      ! Calculate local FCP treating dphen analogously as for the calulation of VAI:
  !     !      ! FPC = 1-exp(-kbeer*LAI*dphen) instead of FPC = dphen*(1-exp(-kbeer*LAI))
  !     !!           fpc_ind = 1. - dexp(
  !     !!     $                           -1.*kbeer*(
  !     !!     $                                         lai_ind(pft,jpngr)*dphen(day,pft)
  !     !!     $                                         )
  !     !!     $                           )
  !     !          fpc_ind = (1. - dexp(
  !     !     $                           -1.*kbeer*(
  !     !     $                                         lai_ind(pft,jpngr)
  !     !     $                                         )
  !     !     $                           ))!*dphen(day,pft)
  !     !          local_fpc_grid = fpc_ind * crownarea(pft,jpngr) * nind(pft,jpngr)
  !     !          fpc_grid_total = fpc_grid_total + local_fpc_grid
  !     !
  !     !          print*,'pft',pft
  !     !          print*,'local_fpc_grid     ',local_fpc_grid
  !     !          print*,'fpc_grid(pft,jpngr)',fpc_grid(pft,jpngr)
  !     !          
  !     !      ! Calculate fractional stem/branch cover of grid cell as the difference
  !     !          spc_grid = vpc_grid - local_fpc_grid
  !     !          spc_grid_total = spc_grid_total + spc_grid
  !    
  !   endif
  ! enddo
  !
  ! 
  ! if (vpc_grid_total.gt.1.) then
  !   !        print*,'-----------------scaling-------------------'
  !   !        print*,'fpc_grid_total',fpc_grid_total
  !   !        print*,'vpc_grid_total',vpc_grid_total
  !   scale = 1. / vpc_grid_total
  !   fpc_grid_total = fpc_grid_total * scale
  !   vpc_grid_total = vpc_grid_total * scale
  !   spc_grid_total = spc_grid_total * scale
  !   !        print*,'fpc_grid_total',fpc_grid_total
  !   !        print*,'vpc_grid_total',vpc_grid_total
  ! endif
  !
  ! if (fpc_grid_total.gt.1.) then
  !   !        print*,'fpc_grid_total',fpc_grid_total
  !   stop 
  ! endif
  !
  ! ! Daily N fixed by cryptogamic ground and plant covers (communicated to calling SR)
  ! !-------------------------------------------------------------------------
  ! ! Fixation scales with daily photosynthetically active radiation and the
  ! ! branch/stem surface for CPC and the bare ground surface for CGC.
  ! 
  ! dnfix_cpc = par_day(day) * max( 0., spc_grid_total) / glob_CPC_scal
  ! dnfix_cgc = par_day(day) * max( 0., (1.0 - vpc_grid_total) ) / glob_CGC_scal
  !
  ! end subroutine n_fixation_cryptogam


  !******************************************************************************
  ! Derivation of Cacq (C spent to cover cost of N-uptake) after
  ! Fisher et al., 2010 (Equation numbers from paper)
  ! 
  !    C_growth = C_npp - C_acq                (eq.6b)
  !    N_acq    = C_acq / Cost_acq             (eq.6c)
  !    r_cton   = C_growth / (N_passive+N_acq) (eq.6d)  [equation presented in paper is incorrect!]

  ! Using 6b and 6c, eq.6d becomes
  !    r_cton   = (C_npp - C_acq) / (N_passive + C_acq/Cost_acq)

  ! Solving for C_acq yields
  !    C_acq    = (C_npp - r_cton * N_pass)/(r_cton/Cost_acq + 1)

  ! Identify terms with variables in code:
  ! (C_npp - r_cton * N_pass) <=> npp_remaining_step
  ! C_acq <=> Cacq
  ! N_acq <=> Nacq   [rest is obvious]
  ! 
  !******************************************************************************


end module md_nuptake
