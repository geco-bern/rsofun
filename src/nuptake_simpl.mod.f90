module md_nuptake_simpl
  !////////////////////////////////////////////////////////////////
  ! SIMPLE NITROGEN UPTAKE MODULE
  !----------------------------------------------------------------
  use md_params_core, only: ndayyear, nmonth, nlu, npft
  use md_classdefs
  use md_tile_cnmodel
  use md_plant_cnmodel

  implicit none

  private
  public nuptake, getpar_modl_nuptake

  !-----------------------------------------------------------------------
  ! Module parameters
  !-----------------------------------------------------------------------
  type params_nuptake_type
    real :: kc         ! half-saturation constant with respect to root biomass (gC m-2)
    real :: kv         ! half-saturation constant with respect to inorganic soil N (gN m-2)
    real :: vmax       ! maximum rate of N uptake (at saturating root biomass and soil inorganic N) (gN d-1)
  end type params_nuptake_type

  type( params_nuptake_type ) :: params_nuptake

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
    real :: fno3
    real :: dnup


    pftloop: do pft = 1, npft

      lu = params_pft_plant(pft)%lu_category

      if ( tile_fluxes(lu)%plant(pft)%dcex > 0.0 ) then
        !//////////////////////////////////////////////////////////////////////////
        ! N uptake of NO3 and NH4 in proportion to their relative pool sizes.
        !--------------------------------------------------------------------------
        fno3 = tile(lu)%soil%pno3%n14 / (tile(lu)%soil%pnh4%n14 + tile(lu)%soil%pno3%n14)
        dnup  = calc_dnup( tile(lu)%plant(pft)%proot%c%c12, &
                           tile(lu)%soil%pnh4%n14 + tile(lu)%soil%pno3%n14 &
                           )

        ! Update
        tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 - dnup * fno3
        tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 - dnup * (1.0 - fno3)

      end if

      !--------------------------------------------------------------------------
      ! Update N-uptake of this PFT. N-retranslocation is not considered
      ! N-uptake.
      !--------------------------------------------------------------------------
      ! daily
      tile_fluxes(lu)%plant(pft)%dnup%n14 = dnup
      tile_fluxes(lu)%plant(pft)%dnup_pas = 0.0
      tile_fluxes(lu)%plant(pft)%dnup_act = dnup           
      tile_fluxes(lu)%plant(pft)%dnup_fix = 0.0  

      !--------------------------------------------------------------------------
      ! N acquisition to labile pool
      !--------------------------------------------------------------------------
      call ncp( tile_fluxes(lu)%plant(pft)%dnup, tile(lu)%plant(pft)%plabl%n )

    end do pftloop

  end subroutine nuptake


  function calc_dnup( croot, ninorg ) result( nup )
    !/////////////////////////////////////////////////////////////////
    ! N uptake is saturating both with respect to the root biomass and 
    ! with respect to soil inorganic N.
    !-----------------------------------------------------------------
    ! arguments
    real, intent(in) :: croot        ! root biomass (gC/m2)
    real, intent(in) :: ninorg       ! total inorganic soil N (gN/m2)

    ! function return variable
    real :: nup

    nup = calc_vn( ninorg ) * croot / (params_nuptake%kc + croot)

    ! print*,'nuptake: Croot saturation ', croot / (params_nuptake%kc + croot)

  end function calc_dnup


  function calc_vn( ninorg ) result( vn )
    !/////////////////////////////////////////////////////////////////
    ! N uptake capacity as a saturating function of soil inorganic N.
    !-----------------------------------------------------------------
    ! arguments
    real, intent(in) :: ninorg       ! total inorganic soil N (gN/m2)

    ! function return variable
    real :: vn

    vn = params_nuptake%vmax * ninorg / (params_nuptake%kv + ninorg)

    ! print*,'nuptake: Ninorg saturation ', ninorg / (params_nuptake%kv + ninorg)

  end function calc_vn


  subroutine getpar_modl_nuptake()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads nuptake module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface

    params_nuptake%kc   = myinterface%params_calib%nuptake_kc
    params_nuptake%kv   = myinterface%params_calib%nuptake_kv
    params_nuptake%vmax = myinterface%params_calib%nuptake_vmax

  end subroutine getpar_modl_nuptake  


end module md_nuptake_simpl
