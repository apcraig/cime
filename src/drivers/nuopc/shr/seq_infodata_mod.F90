MODULE seq_infodata_mod

  ! !DESCRIPTION: A module to get, put, and store some standard scalar data

  ! !USES:
  use shr_kind_mod, only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_IN, SHR_KIND_R8, SHR_KIND_I8
  use shr_sys_mod,  only: shr_sys_flush, shr_sys_abort
  use shr_orb_mod,  only: SHR_ORB_UNDEF_INT, SHR_ORB_UNDEF_REAL, shr_orb_params
  use seq_comm_mct, only: logunit, loglevel
  use seq_comm_mct, only: num_inst_atm, num_inst_lnd, num_inst_rof
  use seq_comm_mct, only: num_inst_ocn, num_inst_ice, num_inst_glc
  use seq_comm_mct, only: num_inst_wav

  implicit none

  private  ! default private

  ! !PUBLIC TYPES:

  public :: seq_infodata_type

  ! !PUBLIC MEMBER FUNCTIONS

  public :: seq_infodata_Init1           ! Initialize before clocks are initialized
  public :: seq_infodata_Init2           ! Init after clocks are initialized
  public :: seq_infodata_GetData         ! Get values from infodata object
  public :: seq_infodata_PutData         ! Change values in infodata object

  ! !PUBLIC DATA MEMBERS:
  public :: seq_infodata_infodata        ! instance of infodata datatype

  ! Type to hold pause/resume signaling information
  type seq_pause_resume_type
     private
     character(SHR_KIND_CL) :: atm_resume(num_inst_atm) = ' ' ! atm resume file
     character(SHR_KIND_CL) :: lnd_resume(num_inst_lnd) = ' ' ! lnd resume file
     character(SHR_KIND_CL) :: ice_resume(num_inst_ice) = ' ' ! ice resume file
     character(SHR_KIND_CL) :: ocn_resume(num_inst_ocn) = ' ' ! ocn resume file
     character(SHR_KIND_CL) :: glc_resume(num_inst_glc) = ' ' ! glc resume file
     character(SHR_KIND_CL) :: rof_resume(num_inst_rof) = ' ' ! rof resume file
     character(SHR_KIND_CL) :: wav_resume(num_inst_wav) = ' ' ! wav resume file
     character(SHR_KIND_CL) :: cpl_resume = ' '               ! cpl resume file
  end type seq_pause_resume_type

  ! InputInfo derived type
  type seq_infodata_type
     private     ! This type is opaque

     !--- set via components and held fixed after initialization ---
     logical                 :: dead_comps              ! do we have dead models
     logical                 :: rofice_present          ! does rof have iceberg coupling on
     logical                 :: rof_prognostic          ! does rof component need input data
     logical                 :: flood_present           ! does rof have flooding on
     logical                 :: ocnrof_prognostic       ! does component need rof data
     logical                 :: iceberg_prognostic      ! does the ice model support icebergs
     logical                 :: glclnd_present          ! does glc have land coupling fields on
     logical                 :: glcocn_present          ! does glc have ocean runoff on
     logical                 :: glcice_present          ! does glc have iceberg coupling on
     logical                 :: glc_coupled_fluxes      ! does glc send fluxes to other components (only relevant if glc_present is .true.)

     !--- set via components and may be time varying ---
     integer(SHR_KIND_IN)    :: atm_phase = 1                  ! atm phase
     integer(SHR_KIND_IN)    :: lnd_phase = 1                  ! lnd phase
     integer(SHR_KIND_IN)    :: ice_phase = 1                  ! ice phase
     integer(SHR_KIND_IN)    :: ocn_phase = 1                  ! ocn phase
     integer(SHR_KIND_IN)    :: glc_phase = 1                  ! glc phase
     integer(SHR_KIND_IN)    :: rof_phase = 1                  ! rof phase
     integer(SHR_KIND_IN)    :: wav_phase = 1                  ! wav phase
     integer(SHR_KIND_IN)    :: esp_phase = 1                  ! esp phase
     logical                 :: atm_aero = .false.             ! atmosphere aerosols
     real(SHR_KIND_R8)       :: nextsw_cday = -1.0_SHR_KIND_R8 ! calendar of next atm shortwave
     real(SHR_KIND_R8)       :: precip_fact = 1.0_SHR_KIND_R8  ! precip factor
     type(seq_pause_resume_type), pointer :: pause_resume => NULL()

     !--- set by driver and may be time varying
     logical                 :: glc_valid_input = .true.  ! is valid accumulated data being sent to prognostic glc
  end type seq_infodata_type

  type (seq_infodata_type), target :: seq_infodata_infodata ! single instance for cpl and all comps
  !===============================================================================

CONTAINS

  !===============================================================================
  SUBROUTINE seq_infodata_Init1(infodata, ID)

    ! !DESCRIPTION:
    ! Initialize pause_resume

    ! !INPUT/OUTPUT PARAMETERS:
    type(seq_infodata_type), intent(INOUT) :: infodata  ! infodata object
    integer(SHR_KIND_IN),    intent(IN)    :: ID        ! seq_comm ID

    !----- local -----
    character(len=*),    parameter :: subname = '(seq_infodata_Init1) '
    !-------------------------------------------------------------------------------

    if (associated(infodata%pause_resume)) then
       deallocate(infodata%pause_resume)
    end if
    nullify(infodata%pause_resume)

  end SUBROUTINE seq_infodata_Init1

  !===============================================================================
  SUBROUTINE seq_infodata_Init2(infodata)

    ! !DESCRIPTION: re-initialize pause-resume depending on the time manager setup

    ! !USES:
    use seq_timemgr_mod, only : seq_timemgr_pause_active

    ! !INPUT/OUTPUT PARAMETERS:
    type(seq_infodata_type), intent(INOUT) :: infodata  ! infodata object
    !----------------------------------------------------------

    !| If pause/resume is active, initialize the resume data
    if (seq_timemgr_pause_active() .and. (.not. associated(infodata%pause_resume))) then
       allocate(infodata%pause_resume)
    end if

  END SUBROUTINE seq_infodata_Init2

  !===============================================================================
  SUBROUTINE seq_infodata_GetData( infodata, &
       flux_epbal              , &
       ocnrof_prognostic       , &
       glc_coupled_fluxes      , &
       flood_present           , &
       rofice_present          , &
       glclnd_present          , &
       glcocn_present          , &
       glcice_present          , &
       glc_valid_input         , &
       iceberg_prognostic      , &
       dead_comps              , &
       flux_epbalfact          , &
       nextsw_cday             , &
       precip_fact             , &
       atm_aero                , &
       glc_phase               , &
       rof_phase               , &
       atm_phase               , &
       lnd_phase               , &
       ocn_phase               , &
       ice_phase               , &
       wav_phase               , &
       esp_phase               , &
       atm_resume              , &
       lnd_resume              , &
       ocn_resume              , &
       ice_resume              , &
       glc_resume              , &
       rof_resume              , &
       wav_resume              , &
       cpl_resume              )

    implicit none

    ! !DESCRIPTION:!    Get values out of the infodata object.

    ! !INPUT/OUTPUT PARAMETERS:

    type(seq_infodata_type),          intent(IN)  :: infodata                ! Input CCSM structure
    character(SHR_KIND_CL), optional, intent(IN)  :: flux_epbal     ! selects E,P,R adjustment technique
    logical,                optional, intent(OUT) :: dead_comps              ! do we have dead models
    logical,                optional, intent(OUT) :: rofice_present
    logical,                optional, intent(OUT) :: flood_present
    logical,                optional, intent(OUT) :: ocnrof_prognostic
    logical,                optional, intent(OUT) :: iceberg_prognostic
    logical,                optional, intent(OUT) :: glclnd_present
    logical,                optional, intent(OUT) :: glcocn_present
    logical,                optional, intent(OUT) :: glcice_present
    logical,                optional, intent(OUT) :: glc_coupled_fluxes
    real(SHR_KIND_R8),      optional, intent(OUT) :: nextsw_cday             ! calendar of next atm shortwave
    real(SHR_KIND_R8),      optional, intent(OUT) :: precip_fact             ! precip factor
    real(SHR_KIND_R8),      optional, intent(OUT) :: flux_epbalfact          ! adjusted precip factor
    integer(SHR_KIND_IN),   optional, intent(OUT) :: atm_phase               ! atm phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: lnd_phase               ! lnd phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: ice_phase               ! ice phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: ocn_phase               ! ocn phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: glc_phase               ! glc phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: rof_phase               ! rof phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: wav_phase               ! wav phase
    integer(SHR_KIND_IN),   optional, intent(OUT) :: esp_phase               ! wav phase
    logical,                optional, intent(OUT) :: atm_aero                ! atmosphere aerosols
    logical,                optional, intent(OUT) :: glc_valid_input
    character(SHR_KIND_CL), optional, intent(OUT) :: atm_resume(:) ! atm read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: lnd_resume(:) ! lnd read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: ice_resume(:) ! ice read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: ocn_resume(:) ! ocn read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: glc_resume(:) ! glc read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: rof_resume(:) ! rof read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: wav_resume(:) ! wav read resume state
    character(SHR_KIND_CL), optional, intent(OUT) :: cpl_resume ! cpl read resume state

    !----- local -----
    character(len=*), parameter :: subname = '(seq_infodata_GetData) '

    !-------------------------------------------------------------------------------

    if ( present(dead_comps)          ) dead_comps         = infodata%dead_comps
    if ( present(rofice_present)      ) rofice_present     = infodata%rofice_present
    if ( present(flood_present)       ) flood_present      = infodata%flood_present
    if ( present(ocnrof_prognostic)   ) ocnrof_prognostic  = infodata%ocnrof_prognostic
    if ( present(iceberg_prognostic)  ) iceberg_prognostic = infodata%iceberg_prognostic
    if ( present(glclnd_present)      ) glclnd_present     = infodata%glclnd_present
    if ( present(glcocn_present)      ) glcocn_present     = infodata%glcocn_present
    if ( present(glcice_present)      ) glcice_present     = infodata%glcice_present
    if ( present(glc_coupled_fluxes)  ) glc_coupled_fluxes = infodata%glc_coupled_fluxes
    if ( present(glc_valid_input)     ) glc_valid_input    = infodata%glc_valid_input
    if ( present(atm_aero)            ) atm_aero           = infodata%atm_aero
    if ( present(nextsw_cday)         ) nextsw_cday        = infodata%nextsw_cday
    if ( present(precip_fact)         ) precip_fact        = infodata%precip_fact
    if ( present(atm_phase)           ) atm_phase          = infodata%atm_phase
    if ( present(lnd_phase)           ) lnd_phase          = infodata%lnd_phase
    if ( present(ice_phase)           ) ice_phase          = infodata%ice_phase
    if ( present(ocn_phase)           ) ocn_phase          = infodata%ocn_phase
    if ( present(glc_phase)           ) glc_phase          = infodata%glc_phase
    if ( present(rof_phase)           ) rof_phase          = infodata%rof_phase
    if ( present(wav_phase)           ) wav_phase          = infodata%wav_phase
    if ( present(esp_phase)           ) esp_phase          = infodata%esp_phase

    if ( present(flux_epbalfact) ) then
       if (.not. present(flux_epbal)) then
          call shr_sys_abort(subname // "Must provide flux_epbal as an input argument to determine infodata%precip_fact")
       end if
       flux_epbalfact = 1.0_SHR_KIND_R8
       if (trim(flux_epbal) == 'ocn') then
          flux_epbalfact = infodata%precip_fact
       end if
       if (flux_epbalfact <= 0.0_SHR_KIND_R8) then
          if (loglevel > 0) write(logunit,'(2a,e16.6)') &
               trim(subname),' WARNING: factor from ocn = ',flux_epbalfact
          if (loglevel > 0) write(logunit,'(2a)') &
               trim(subname),' WARNING: resetting flux_epbalfact to 1.0'
          flux_epbalfact = 1.0_SHR_KIND_R8
       end if
    endif

    if ( present(atm_resume) ) then
       if (associated(infodata%pause_resume)) then
          atm_resume(:)  = infodata%pause_resume%atm_resume(:)
       else
          atm_resume(:) = ' '
       end if
    end if
    if ( present(lnd_resume) ) then
       if (associated(infodata%pause_resume)) then
          lnd_resume(:)  = infodata%pause_resume%lnd_resume(:)
       else
          lnd_resume(:) = ' '
       end if
    end if
    if ( present(ice_resume) ) then
       if (associated(infodata%pause_resume)) then
          ice_resume(:)  = infodata%pause_resume%ice_resume(:)
       else
          ice_resume(:) = ' '
       end if
    end if
    if ( present(ocn_resume) ) then
       if (associated(infodata%pause_resume)) then
          ocn_resume(:)  = infodata%pause_resume%ocn_resume(:)
       else
          ocn_resume(:) = ' '
       end if
    end if
    if ( present(glc_resume) ) then
       if (associated(infodata%pause_resume)) then
          glc_resume(:)  = infodata%pause_resume%glc_resume(:)
       else
          glc_resume(:) = ' '
       end if
    end if
    if ( present(rof_resume) ) then
       if (associated(infodata%pause_resume)) then
          rof_resume(:)  = infodata%pause_resume%rof_resume(:)
       else
          rof_resume(:) = ' '
       end if
    end if
    if ( present(wav_resume) ) then
       if (associated(infodata%pause_resume)) then
          wav_resume(:)  = infodata%pause_resume%wav_resume(:)
       else
          wav_resume(:) = ' '
       end if
    end if
    if ( present(cpl_resume) ) then
       if (associated(infodata%pause_resume)) then
          cpl_resume     = infodata%pause_resume%cpl_resume
       else
          cpl_resume = ' '
       end if
    end if

  END SUBROUTINE seq_infodata_GetData

  !===============================================================================
  SUBROUTINE seq_infodata_PutData( infodata , &
       ocnrof_prognostic       , &
       glc_coupled_fluxes      , &
       flood_present           , &
       rofice_present          , &
       glclnd_present          , &
       glcocn_present          , &
       glcice_present          , &
       glc_valid_input         , &
       iceberg_prognostic      , &
       dead_comps              , &
       nextsw_cday             , &
       precip_fact             , &
       atm_aero                , &
       glc_phase               , &
       rof_phase               , &
       atm_phase               , &
       lnd_phase               , &
       ocn_phase               , &
       ice_phase               , &
       wav_phase               , &
       esp_phase               , &
       atm_resume              , &
       lnd_resume              , &
       ocn_resume              , &
       ice_resume              , &
       glc_resume              , &
       rof_resume              , &
       wav_resume              , &
       cpl_resume              )

    implicit none

    ! !DESCRIPTION:  Put values into the infodata object.

    ! !INPUT/OUTPUT PARAMETERS:
    type(seq_infodata_type),          intent(INOUT) :: infodata                ! Input CCSM structure
    logical,                optional, intent(IN) :: dead_comps     ! do we have dead models
    logical,                optional, intent(IN) :: rofice_present
    logical,                optional, intent(IN) :: flood_present
    logical,                optional, intent(IN) :: ocnrof_prognostic
    logical,                optional, intent(IN) :: iceberg_prognostic
    logical,                optional, intent(IN) :: glclnd_present
    logical,                optional, intent(IN) :: glcocn_present
    logical,                optional, intent(IN) :: glcice_present
    logical,                optional, intent(IN) :: glc_coupled_fluxes
    logical,                optional, intent(IN) :: glc_valid_input
    logical,                optional, intent(IN) :: atm_aero       ! atm aerosols
    real(SHR_KIND_R8),      optional, intent(IN) :: nextsw_cday    ! calendar of next atm shortwave
    real(SHR_KIND_R8),      optional, intent(IN) :: precip_fact    ! precip factor
    integer(SHR_KIND_IN),   optional, intent(IN) :: atm_phase      ! atm phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: lnd_phase      ! lnd phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: ice_phase      ! ice phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: ocn_phase      ! ocn phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: glc_phase      ! glc phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: rof_phase      ! rof phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: wav_phase      ! wav phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: esp_phase      ! esp phase
    character(SHR_KIND_CL), optional, intent(IN) :: atm_resume(:)  ! atm resume
    character(SHR_KIND_CL), optional, intent(IN) :: lnd_resume(:)  ! lnd resume
    character(SHR_KIND_CL), optional, intent(IN) :: ice_resume(:)  ! ice resume
    character(SHR_KIND_CL), optional, intent(IN) :: ocn_resume(:)  ! ocn resume
    character(SHR_KIND_CL), optional, intent(IN) :: glc_resume(:)  ! glc resume
    character(SHR_KIND_CL), optional, intent(IN) :: rof_resume(:)  ! rof resume
    character(SHR_KIND_CL), optional, intent(IN) :: wav_resume(:)  ! wav resume
    character(SHR_KIND_CL), optional, intent(IN) :: cpl_resume     ! cpl resume

    !----- local -----
    character(len=*), parameter :: subname = '(seq_infodata_PutData) '
    !-------------------------------------------------------------------------------

    if ( present(dead_comps)            ) infodata%dead_comps         = dead_comps
    if ( present(rofice_present)        ) infodata%rofice_present     = rofice_present
    if ( present(flood_present)         ) infodata%flood_present      = flood_present
    if ( present(ocnrof_prognostic)     ) infodata%ocnrof_prognostic  = ocnrof_prognostic
    if ( present(iceberg_prognostic)    ) infodata%iceberg_prognostic = iceberg_prognostic
    if ( present(glclnd_present)        ) infodata%glclnd_present     = glclnd_present
    if ( present(glcocn_present)        ) infodata%glcocn_present     = glcocn_present
    if ( present(glcice_present)        ) infodata%glcice_present     = glcice_present
    if ( present(glc_coupled_fluxes)    ) infodata%glc_coupled_fluxes = glc_coupled_fluxes
    if ( present(glc_valid_input)       ) infodata%glc_valid_input    = glc_valid_input
    if ( present(atm_aero)              ) infodata%atm_aero           = atm_aero
    if ( present(nextsw_cday)           ) infodata%nextsw_cday        = nextsw_cday
    if ( present(precip_fact)           ) infodata%precip_fact        = precip_fact
    if ( present(atm_phase)             ) infodata%atm_phase          = atm_phase
    if ( present(lnd_phase)             ) infodata%lnd_phase          = lnd_phase
    if ( present(ice_phase)             ) infodata%ice_phase          = ice_phase
    if ( present(ocn_phase)             ) infodata%ocn_phase          = ocn_phase
    if ( present(glc_phase)             ) infodata%glc_phase          = glc_phase
    if ( present(rof_phase)             ) infodata%rof_phase          = rof_phase
    if ( present(wav_phase)             ) infodata%wav_phase          = wav_phase
    if ( present(esp_phase)             ) infodata%esp_phase          = esp_phase

    if ( present(atm_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%atm_resume(:) = atm_resume(:)
       else if (ANY(len_trim(atm_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%atm_resume(:) = atm_resume(:)
       end if
    end if
    if ( present(lnd_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%lnd_resume(:) = lnd_resume(:)
       else if (ANY(len_trim(lnd_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%lnd_resume(:) = lnd_resume(:)
       end if
    end if
    if ( present(ice_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%ice_resume(:) = ice_resume(:)
       else if (ANY(len_trim(ice_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%ice_resume(:) = ice_resume(:)
       end if
    end if
    if ( present(ocn_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%ocn_resume(:) = ocn_resume(:)
       else if (ANY(len_trim(ocn_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%ocn_resume(:) = ocn_resume(:)
       end if
    end if
    if ( present(glc_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%glc_resume(:) = glc_resume(:)
       else if (ANY(len_trim(glc_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%glc_resume(:) = glc_resume(:)
       end if
    end if
    if ( present(rof_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%rof_resume(:) = rof_resume(:)
       else if (ANY(len_trim(rof_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%rof_resume(:) = rof_resume(:)
       end if
    end if
    if ( present(wav_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%wav_resume(:) = wav_resume(:)
       else if (ANY(len_trim(wav_resume(:)) > 0)) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%wav_resume(:) = wav_resume(:)
       end if
    end if
    if ( present(cpl_resume) ) then
       if (associated(infodata%pause_resume)) then
          infodata%pause_resume%cpl_resume = cpl_resume
       else if (len_trim(cpl_resume) > 0) then
          allocate(infodata%pause_resume)
          infodata%pause_resume%cpl_resume = cpl_resume
       end if
    end if

  END SUBROUTINE seq_infodata_PutData

END MODULE seq_infodata_mod
