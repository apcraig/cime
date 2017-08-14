MODULE seq_infodata_mod

  ! !DESCRIPTION: A module to get, put, and store some standard scalar data

  ! !USES:
  use ESMF
  use NUOPC

  use shr_kind_mod, only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_IN, SHR_KIND_R8, SHR_KIND_I8
  use shr_sys_mod,  only: shr_sys_flush, shr_sys_abort, shr_sys_getenv
  use shr_orb_mod,  only: SHR_ORB_UNDEF_INT, SHR_ORB_UNDEF_REAL, shr_orb_params

  use seq_comm_mct, only: logunit, loglevel
  use seq_comm_mct, only: seq_comm_setptrs, seq_comm_iamroot
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

  ! Strings of valid start_type options
  character(len=*), public, parameter :: seq_infodata_start_type_start     = "startup"
  character(len=*), public, parameter :: seq_infodata_start_type_cont      = "continue"
  character(len=*), public, parameter :: seq_infodata_start_type_brnch     = "branch"
  character(len=*), public, parameter :: seq_infodata_orb_fixed_year       = 'fixed_year'
  character(len=*), public, parameter :: seq_infodata_orb_variable_year    = 'variable_year'
  character(len=*), public, parameter :: seq_infodata_orb_fixed_parameters = 'fixed_parameters'

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

     !--- set via config attributes and held fixed ----
     character(SHR_KIND_CS)  :: cime_model              ! acme or cesm
     character(SHR_KIND_CL)  :: start_type              ! Type of startup
     character(SHR_KIND_CL)  :: case_name               ! Short case identification
     character(SHR_KIND_CL)  :: case_desc               ! Long description of this case
     character(SHR_KIND_CL)  :: model_version           ! Model version
     character(SHR_KIND_CS)  :: username                ! Current user
     character(SHR_KIND_CS)  :: hostname                ! Current machine
     character(SHR_KIND_CL)  :: timing_dir              ! Dir for timing files
     character(SHR_KIND_CL)  :: tchkpt_dir              ! Dir for timing checkpoint files
     logical                 :: aqua_planet             ! No ice/lnd, analytic ocn, perpetual time (cam aquaplanet testing mode only)
     integer(SHR_KIND_IN)    :: aqua_planet_sst = 1     ! aqua planet analytic sst type (cam aquaplanet testing mode only)
     logical                 :: run_barriers            ! barrier component run calls
     logical                 :: brnch_retain_casename   ! If branch and can use same casename
     logical                 :: read_restart            ! read the restart file, based on start_type (only for data models)
     logical                 :: single_column           ! single column mode
     real (SHR_KIND_R8)      :: scmlat                  ! single column lat
     real (SHR_KIND_R8)      :: scmlon                  ! single column lon
     character(SHR_KIND_CS)  :: logFilePostFix          ! postfix for output log files
     character(SHR_KIND_CL)  :: outPathRoot             ! root for output log files
     logical                 :: perpetual = .false.     ! perpetual flag
     integer(SHR_KIND_IN)    :: perpetual_ymd = -999    ! perpetual date
     integer(SHR_KIND_IN)    :: orb_iyear               ! orbital year
     integer(SHR_KIND_IN)    :: orb_iyear_align         ! model year associated with orb year
     character(SHR_KIND_CL)  :: orb_mode                ! orbital mode
     real(SHR_KIND_R8)       :: orb_eccen               ! See shr_orb_mod
     real(SHR_KIND_R8)       :: orb_obliq               ! See shr_orb_mod
     real(SHR_KIND_R8)       :: orb_mvelp               ! See shr_orb_mod
     real(SHR_KIND_R8)       :: orb_obliqr              ! See shr_orb_mod
     real(SHR_KIND_R8)       :: orb_lambm0              ! See shr_orb_mod
     real(SHR_KIND_R8)       :: orb_mvelpp              ! See shr_orb_mod
     character(SHR_KIND_CS)  :: tfreeze_option          ! Freezing point calculation
     character(SHR_KIND_CL)  :: flux_epbal              ! selects E,P,R adjustment technique
     logical                 :: flux_albav              ! T => no diurnal cycle in ocn albedos
     logical                 :: flux_diurnal            ! T => diurnal cycle in atm/ocn fluxes
     real(SHR_KIND_R8)       :: gust_fac                ! wind gustiness factor
     character(SHR_KIND_CL)  :: glc_renormalize_smb     ! Whether to renormalize smb sent from lnd -> glc
     real(SHR_KIND_R8)       :: wall_time_limit         ! force stop time limit (hours)
     integer                 :: cpl_decomp              ! coupler decomp
     character(SHR_KIND_CL)  :: cpl_seq_option          ! coupler sequencing option
     logical                 :: cpl_cdf64               ! use netcdf 64 bit offset, large file support
     logical                 :: drv_threading           ! is threading control in driver turned on

     !--- set via namelist and may be time varying ---
     integer(SHR_KIND_IN)    :: info_debug              ! debug level
     logical                 :: bfbflag                 ! turn on bfb option

     !--- set via components and held fixed ---
     logical                 :: atm_present             ! does component model exist
     logical                 :: atm_prognostic          ! does component model need input data from driver
     logical                 :: lnd_present             ! does component model exist
     logical                 :: lnd_prognostic          ! does component model need input data from driver
     logical                 :: rof_present             ! does rof component exist
     logical                 :: rofice_present          ! does rof have iceberg coupling on
     logical                 :: rof_prognostic          ! does rof component need input data
     logical                 :: flood_present           ! does rof have flooding on
     logical                 :: ocn_present             ! does component model exist
     logical                 :: ocn_prognostic          ! does component model need input data from driver
     logical                 :: ocnrof_prognostic       ! does component need rof data
     logical                 :: ice_present             ! does component model exist
     logical                 :: ice_prognostic          ! does component model need input data from driver
     logical                 :: iceberg_prognostic      ! does the ice model support icebergs
     logical                 :: glc_present             ! does component model exist
     logical                 :: glclnd_present          ! does glc have land coupling fields on
     logical                 :: glcocn_present          ! does glc have ocean runoff on
     logical                 :: glcice_present          ! does glc have iceberg coupling on
     logical                 :: glc_prognostic          ! does component model need input data from driver
     logical                 :: glc_coupled_fluxes      ! does glc send fluxes to other components (only relevant if glc_present is .true.)
     logical                 :: wav_present             ! does component model exist
     logical                 :: wav_prognostic          ! does component model need input data from driver
     logical                 :: esp_present             ! does component model exist
     logical                 :: esp_prognostic          ! does component model need input data from driver
     logical                 :: dead_comps              ! do we have dead models
     integer(SHR_KIND_IN)    :: atm_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: atm_ny                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: lnd_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: lnd_ny                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: ice_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: ice_ny                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: ocn_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: ocn_ny                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: rof_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: rof_ny                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: glc_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: glc_ny                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: wav_nx                  ! nx, ny of "2d" grid
     integer(SHR_KIND_IN)    :: wav_ny                  ! nx, ny of "2d" grid

     !--- set via components and may be time varying ---
     real(SHR_KIND_R8)       :: nextsw_cday = -1.0_SHR_KIND_R8 ! calendar of next atm shortwave
     real(SHR_KIND_R8)       :: precip_fact = 1.0_SHR_KIND_R8  ! precip factor
     integer(SHR_KIND_IN)    :: atm_phase = 1                  ! atm phase
     integer(SHR_KIND_IN)    :: lnd_phase = 1                  ! lnd phase
     integer(SHR_KIND_IN)    :: ice_phase = 1                  ! ice phase
     integer(SHR_KIND_IN)    :: ocn_phase = 1                  ! ocn phase
     integer(SHR_KIND_IN)    :: glc_phase = 1                  ! glc phase
     integer(SHR_KIND_IN)    :: rof_phase = 1                  ! rof phase
     integer(SHR_KIND_IN)    :: wav_phase = 1                  ! wav phase
     integer(SHR_KIND_IN)    :: esp_phase = 1                  ! esp phase
     logical                 :: atm_aero = .false.             ! atmosphere aerosols
     logical                 :: glc_g2lupdate = .false.        ! update glc2lnd fields in lnd model
     real(shr_kind_r8)       :: max_cplstep_time               ! abort if cplstep time exceeds this value
     type(seq_pause_resume_type), pointer :: pause_resume => NULL()

     !--- set by driver and may be time varying
     logical                 :: glc_valid_input = .true.  ! is valid accumulated data being sent to prognostic glc

     !--- set from restart file ---
     character(SHR_KIND_CL)  :: rest_case_name          ! Short case identification

  end type seq_infodata_type

  type (seq_infodata_type), target :: seq_infodata_infodata ! single instance for cpl and all comps

  ! --- public interfaces --------------------------------------------------------
  interface seq_infodata_GetData
     module procedure seq_infodata_GetData_explicit
  end interface seq_infodata_GetData

  interface seq_infodata_PutData
     module procedure seq_infodata_PutData_explicit
  end interface seq_infodata_PutData
  !===============================================================================

CONTAINS

  !===============================================================================
  SUBROUTINE seq_infodata_Init1(infodata, ID)

    ! !DESCRIPTION:
    ! Read in input from driver attributes and output derived type for miscillaneous info.

    ! !USES:
    use seq_timemgr_mod, only : seq_timemgr_pause_active

    ! !INPUT/OUTPUT PARAMETERS:
    type(seq_infodata_type), intent(INOUT) :: infodata  ! infodata object
    integer(SHR_KIND_IN),    intent(IN)    :: ID        ! seq_comm ID

    !----- local -----
    character(len=*),    parameter :: subname = '(seq_infodata_Init1) '
    !-------------------------------------------------------------------------------

    !---------------------------------------------------------------
    ! Set via components during runtime and may be time varying
    !---------------------------------------------------------------

    if (associated(infodata%pause_resume)) then
       deallocate(infodata%pause_resume)
    end if
    nullify(infodata%pause_resume)

    !-----------------------------------------------------
    ! Set cam aquaplanet relevant flags
    !-----------------------------------------------------

    if (seq_comm_iamroot(ID)) then
       if (infodata%aqua_planet) then
          infodata%perpetual = .true.
          infodata%perpetual_ymd = 321
       endif
    end if

  end SUBROUTINE seq_infodata_Init1

  !===============================================================================
  SUBROUTINE seq_infodata_Init2(infodata)

    ! !DESCRIPTION: Initialize infodata items that depend on the time manager setup

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
  SUBROUTINE seq_infodata_GetData_explicit( infodata, &
       cime_model              , &
       case_name               , &
       case_desc               , &
       timing_dir              , &
       model_version           , &
       username                , &
       hostname                , &
       rest_case_name          , &
       tchkpt_dir              , &
       start_type              , &
       perpetual               , &
       perpetual_ymd           , &
       aqua_planet             , &
       aqua_planet_sst         , &
       brnch_retain_casename   , &
       read_restart            , & ! only for data modes
       single_column           , &
       scmlat                  , &
       scmlon                  , &
       logFilePostFix          , &
       outPathRoot             , &
       atm_present             , &
       atm_prognostic          , &
       lnd_present             , &
       lnd_prognostic          , &
       rof_prognostic          , &
       rof_present             , &
       ocn_present             , &
       ocn_prognostic          , &
       ocnrof_prognostic       , &
       ice_present             , &
       ice_prognostic          , &
       glc_present             , &
       glc_prognostic          , &
       glc_coupled_fluxes      , &
       flood_present           , &
       wav_present             , &
       wav_prognostic          , &
       rofice_present          , &
       glclnd_present          , &
       glcocn_present          , &
       glcice_present          , &
       iceberg_prognostic      , &
       esp_present             , &
       esp_prognostic          , &
       bfbflag                 , &
       cpl_decomp              , &
       cpl_seq_option          , &
       info_debug              , &
       dead_comps              , &
       flux_epbalfact          , &
       nextsw_cday             , &
       precip_fact             , &
       flux_epbal              , &
       flux_albav              , &
       glc_g2lupdate           , &
       atm_aero                , &
       run_barriers            , &
       drv_threading           , &
       flux_diurnal            , &
       gust_fac                , &
       wall_time_limit         , &
       cpl_cdf64               , &
       orb_iyear               , &
       orb_iyear_align         , &
       orb_mode                , &
       orb_mvelp               , &
       orb_eccen               , &
       orb_obliqr              , &
       orb_obliq               , &
       orb_lambm0              , &
       orb_mvelpp              , &
       tfreeze_option          , &
       glc_renormalize_smb     , &
       glc_phase               , &
       rof_phase               , &
       atm_phase               , &
       lnd_phase               , &
       ocn_phase               , &
       ice_phase               , &
       wav_phase               , &
       esp_phase               , &
       wav_nx                  , &
       wav_ny                  , &
       atm_nx                  , &
       atm_ny                  , &
       lnd_nx                  , &
       lnd_ny                  , &
       rof_nx                  , &
       rof_ny                  , &
       ice_nx                  , &
       ice_ny                  , &
       ocn_nx                  , &
       ocn_ny                  , &
       glc_nx                  , &
       glc_ny                  , &
       atm_resume              , &
       lnd_resume              , &
       ocn_resume              , &
       ice_resume              , &
       glc_resume              , &
       rof_resume              , &
       wav_resume              , &
       cpl_resume              , &
       max_cplstep_time        , &
       glc_valid_input)

    implicit none

    ! !DESCRIPTION:!    Get values out of the infodata object.

    ! !INPUT/OUTPUT PARAMETERS:

    type(seq_infodata_type),          intent(IN)  :: infodata                ! Input CCSM structure
    character(len=*),       optional, intent(OUT) :: cime_model              ! CIME model (acme or cesm)
    character(len=*),       optional, intent(OUT) :: start_type              ! Start type
    character(len=*),       optional, intent(OUT) :: case_name               ! Short case identification
    character(len=*),       optional, intent(OUT) :: case_desc               ! Long case description
    character(len=*),       optional, intent(OUT) :: model_version           ! Model version
    character(len=*),       optional, intent(OUT) :: username                ! Username
    character(len=*),       optional, intent(OUT) :: hostname                ! Hostname
    character(len=*),       optional, intent(OUT) :: rest_case_name          ! restart casename
    character(len=*),       optional, intent(OUT) :: timing_dir              ! timing dir name
    character(len=*),       optional, intent(OUT) :: tchkpt_dir              ! timing checkpoint dir name
    logical,                optional, intent(OUT) :: aqua_planet             ! aqua_planet mode
    integer(SHR_KIND_IN),   optional, intent(OUT) :: aqua_planet_sst         ! aqua_planet sst_type
    logical,                optional, intent(OUT) :: run_barriers            ! barrier component run calls
    logical,                optional, intent(OUT) :: brnch_retain_casename
    logical,                optional, intent(OUT) :: read_restart            ! read restart flag (only for data models)
    logical,                optional, intent(OUT) :: single_column
    real (SHR_KIND_R8),     optional, intent(OUT) :: scmlat
    real (SHR_KIND_R8),     optional, intent(OUT) :: scmlon
    character(len=*),       optional, intent(OUT) :: logFilePostFix          ! output log file postfix
    character(len=*),       optional, intent(OUT) :: outPathRoot             ! output file root
    logical,                optional, intent(OUT) :: perpetual               ! If this is perpetual
    integer,                optional, intent(OUT) :: perpetual_ymd           ! If perpetual, date
    character(len=*),       optional, intent(OUT) :: orb_mode                ! orbital mode
    integer,                optional, intent(OUT) :: orb_iyear               ! orbital year
    integer,                optional, intent(OUT) :: orb_iyear_align         ! orbital year model year align
    real(SHR_KIND_R8),      optional, intent(OUT) :: orb_eccen               ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(OUT) :: orb_obliqr              ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(OUT) :: orb_obliq               ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(OUT) :: orb_lambm0              ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(OUT) :: orb_mvelpp              ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(OUT) :: orb_mvelp               ! See shr_orb_mod
    character(len=*),       optional, intent(OUT) :: tfreeze_option          ! Freezing point of salt water
    character(len=*),       optional, intent(OUT) :: flux_epbal              ! selects E,P,R adjustment technique
    logical,                optional, intent(OUT) :: flux_albav              ! T => no diurnal cycle in ocn albedos
    logical,                optional, intent(OUT) :: flux_diurnal            ! T => diurnal cycle in atm/ocn flux
    real(SHR_KIND_R8),      optional, intent(OUT) :: gust_fac                ! wind gustiness factor
    character(len=*),       optional, intent(OUT) :: glc_renormalize_smb     ! Whether to renormalize smb sent from lnd -> glc
    real(SHR_KIND_R8),      optional, intent(OUT) :: wall_time_limit         ! force stop wall time (hours)
    integer,                optional, intent(OUT) :: cpl_decomp              ! coupler decomp
    character(len=*),       optional, intent(OUT) :: cpl_seq_option          ! coupler sequencing option
    logical,                optional, intent(OUT) :: cpl_cdf64               ! netcdf large file setting
    logical,                optional, intent(OUT) :: drv_threading           ! driver threading control flag

    integer(SHR_KIND_IN),   optional, intent(OUT) :: info_debug
    logical,                optional, intent(OUT) :: bfbflag
    logical,                optional, intent(OUT) :: dead_comps              ! do we have dead models

    logical,                optional, intent(OUT) :: atm_present             ! provide data
    logical,                optional, intent(OUT) :: atm_prognostic          ! need data
    logical,                optional, intent(OUT) :: lnd_present
    logical,                optional, intent(OUT) :: lnd_prognostic
    logical,                optional, intent(OUT) :: rof_present
    logical,                optional, intent(OUT) :: rofice_present
    logical,                optional, intent(OUT) :: rof_prognostic
    logical,                optional, intent(OUT) :: flood_present
    logical,                optional, intent(OUT) :: ocn_present
    logical,                optional, intent(OUT) :: ocn_prognostic
    logical,                optional, intent(OUT) :: ocnrof_prognostic
    logical,                optional, intent(OUT) :: ice_present
    logical,                optional, intent(OUT) :: ice_prognostic
    logical,                optional, intent(OUT) :: iceberg_prognostic
    logical,                optional, intent(OUT) :: glc_present
    logical,                optional, intent(OUT) :: glclnd_present
    logical,                optional, intent(OUT) :: glcocn_present
    logical,                optional, intent(OUT) :: glcice_present
    logical,                optional, intent(OUT) :: glc_prognostic
    logical,                optional, intent(OUT) :: glc_coupled_fluxes
    logical,                optional, intent(OUT) :: wav_present
    logical,                optional, intent(OUT) :: wav_prognostic
    logical,                optional, intent(OUT) :: esp_present
    logical,                optional, intent(OUT) :: esp_prognostic
    integer(SHR_KIND_IN),   optional, intent(OUT) :: atm_nx                  ! nx,ny 2d grid size global
    integer(SHR_KIND_IN),   optional, intent(OUT) :: atm_ny                  ! nx,ny 2d grid size global
    integer(SHR_KIND_IN),   optional, intent(OUT) :: lnd_nx
    integer(SHR_KIND_IN),   optional, intent(OUT) :: lnd_ny
    integer(SHR_KIND_IN),   optional, intent(OUT) :: rof_nx
    integer(SHR_KIND_IN),   optional, intent(OUT) :: rof_ny
    integer(SHR_KIND_IN),   optional, intent(OUT) :: ice_nx
    integer(SHR_KIND_IN),   optional, intent(OUT) :: ice_ny
    integer(SHR_KIND_IN),   optional, intent(OUT) :: ocn_nx
    integer(SHR_KIND_IN),   optional, intent(OUT) :: ocn_ny
    integer(SHR_KIND_IN),   optional, intent(OUT) :: glc_nx
    integer(SHR_KIND_IN),   optional, intent(OUT) :: glc_ny
    integer(SHR_KIND_IN),   optional, intent(OUT) :: wav_nx
    integer(SHR_KIND_IN),   optional, intent(OUT) :: wav_ny
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
    logical,                optional, intent(OUT) :: glc_g2lupdate           ! update glc2lnd fields in lnd model
    real(shr_kind_r8),      optional, intent(out) :: max_cplstep_time
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
    character(len=*), parameter :: subname = '(seq_infodata_GetData_explicit) '

    !-------------------------------------------------------------------------------

    if ( present(cime_model)     ) cime_model     = infodata%cime_model
    if ( present(start_type)     ) start_type     = infodata%start_type
    if ( present(case_name)      ) case_name      = infodata%case_name
    if ( present(case_desc)      ) case_desc      = infodata%case_desc
    if ( present(model_version)  ) model_version  = infodata%model_version
    if ( present(username)       ) username       = infodata%username
    if ( present(hostname)       ) hostname       = infodata%hostname
    if ( present(rest_case_name) ) rest_case_name = infodata%rest_case_name
    if ( present(timing_dir)     ) timing_dir     = infodata%timing_dir
    if ( present(tchkpt_dir)     ) tchkpt_dir     = infodata%tchkpt_dir
    if ( present(aqua_planet)    ) aqua_planet    = infodata%aqua_planet
    if ( present(aqua_planet_sst)) aqua_planet_sst= infodata%aqua_planet_sst
    if ( present(run_barriers)   ) run_barriers   = infodata%run_barriers
    if ( present(brnch_retain_casename) ) brnch_retain_casename =  infodata%brnch_retain_casename
    if ( present(read_restart)   ) read_restart   = infodata%read_restart
    if ( present(single_column)  ) single_column  = infodata%single_column
    if ( present(scmlat)         ) scmlat         = infodata%scmlat
    if ( present(scmlon)         ) scmlon         = infodata%scmlon
    if ( present(logFilePostFix) ) logFilePostFix = infodata%logFilePostFix
    if ( present(outPathRoot)    ) outPathRoot    = infodata%outPathRoot
    if ( present(perpetual)      ) perpetual      = infodata%perpetual
    if ( present(perpetual_ymd)  ) perpetual_ymd  = infodata%perpetual_ymd
    if ( present(orb_iyear)      ) orb_iyear      = infodata%orb_iyear
    if ( present(orb_iyear_align)) orb_iyear_align= infodata%orb_iyear_align
    if ( present(orb_mode)       ) orb_mode       = infodata%orb_mode
    if ( present(orb_eccen)      ) orb_eccen      = infodata%orb_eccen
    if ( present(orb_obliqr)     ) orb_obliqr     = infodata%orb_obliqr
    if ( present(orb_obliq)      ) orb_obliq      = infodata%orb_obliq
    if ( present(orb_lambm0)     ) orb_lambm0     = infodata%orb_lambm0
    if ( present(orb_mvelpp)     ) orb_mvelpp     = infodata%orb_mvelpp
    if ( present(orb_mvelp)      ) orb_mvelp      = infodata%orb_mvelp
    if ( present(tfreeze_option) ) tfreeze_option = infodata%tfreeze_option
    if ( present(flux_epbal)     ) flux_epbal     = infodata%flux_epbal
    if ( present(flux_albav)     ) flux_albav     = infodata%flux_albav
    if ( present(flux_diurnal)   ) flux_diurnal   = infodata%flux_diurnal
    if ( present(gust_fac)       ) gust_fac       = infodata%gust_fac
    if ( present(glc_renormalize_smb)) glc_renormalize_smb = infodata%glc_renormalize_smb
    if ( present(wall_time_limit)) wall_time_limit= infodata%wall_time_limit
    if ( present(cpl_decomp)     ) cpl_decomp     = infodata%cpl_decomp
    if ( present(cpl_seq_option) ) cpl_seq_option = infodata%cpl_seq_option
    if ( present(cpl_cdf64)      ) cpl_cdf64      = infodata%cpl_cdf64
    if ( present(drv_threading)  ) drv_threading  = infodata%drv_threading

    if ( present(info_debug)     ) info_debug     = infodata%info_debug
    if ( present(bfbflag)        ) bfbflag        = infodata%bfbflag
    if ( present(dead_comps)     ) dead_comps     = infodata%dead_comps

    if ( present(atm_present)    ) atm_present    = infodata%atm_present
    if ( present(atm_prognostic) ) atm_prognostic = infodata%atm_prognostic
    if ( present(lnd_present)    ) lnd_present    = infodata%lnd_present
    if ( present(lnd_prognostic) ) lnd_prognostic = infodata%lnd_prognostic
    if ( present(rof_present)    ) rof_present    = infodata%rof_present
    if ( present(rofice_present) ) rofice_present = infodata%rofice_present
    if ( present(rof_prognostic) ) rof_prognostic = infodata%rof_prognostic
    if ( present(flood_present)  ) flood_present  = infodata%flood_present
    if ( present(ocn_present)    ) ocn_present    = infodata%ocn_present
    if ( present(ocn_prognostic) ) ocn_prognostic = infodata%ocn_prognostic
    if ( present(ocnrof_prognostic) ) ocnrof_prognostic = infodata%ocnrof_prognostic
    if ( present(ice_present)    ) ice_present    = infodata%ice_present
    if ( present(ice_prognostic) ) ice_prognostic = infodata%ice_prognostic
    if ( present(iceberg_prognostic)) iceberg_prognostic = infodata%iceberg_prognostic
    if ( present(glc_present)    ) glc_present    = infodata%glc_present
    if ( present(glclnd_present) ) glclnd_present = infodata%glclnd_present
    if ( present(glcocn_present) ) glcocn_present = infodata%glcocn_present
    if ( present(glcice_present) ) glcice_present = infodata%glcice_present
    if ( present(glc_prognostic) ) glc_prognostic = infodata%glc_prognostic
    if ( present(glc_coupled_fluxes)) glc_coupled_fluxes = infodata%glc_coupled_fluxes
    if ( present(wav_present)    ) wav_present    = infodata%wav_present
    if ( present(wav_prognostic) ) wav_prognostic = infodata%wav_prognostic
    if ( present(esp_present)    ) esp_present    = infodata%esp_present
    if ( present(esp_prognostic) ) esp_prognostic = infodata%esp_prognostic
    if ( present(atm_nx)         ) atm_nx         = infodata%atm_nx
    if ( present(atm_ny)         ) atm_ny         = infodata%atm_ny
    if ( present(lnd_nx)         ) lnd_nx         = infodata%lnd_nx
    if ( present(lnd_ny)         ) lnd_ny         = infodata%lnd_ny
    if ( present(rof_nx)         ) rof_nx         = infodata%rof_nx
    if ( present(rof_ny)         ) rof_ny         = infodata%rof_ny
    if ( present(ice_nx)         ) ice_nx         = infodata%ice_nx
    if ( present(ice_ny)         ) ice_ny         = infodata%ice_ny
    if ( present(ocn_nx)         ) ocn_nx         = infodata%ocn_nx
    if ( present(ocn_ny)         ) ocn_ny         = infodata%ocn_ny
    if ( present(glc_nx)         ) glc_nx         = infodata%glc_nx
    if ( present(glc_ny)         ) glc_ny         = infodata%glc_ny
    if ( present(wav_nx)         ) wav_nx         = infodata%wav_nx
    if ( present(wav_ny)         ) wav_ny         = infodata%wav_ny

    if ( present(nextsw_cday)    ) nextsw_cday    = infodata%nextsw_cday
    if ( present(precip_fact)    ) precip_fact    = infodata%precip_fact
    if ( present(flux_epbalfact) ) then
       flux_epbalfact = 1.0_SHR_KIND_R8
       if (trim(infodata%flux_epbal) == 'ocn') then
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
    if ( present(atm_phase)      ) atm_phase      = infodata%atm_phase
    if ( present(lnd_phase)      ) lnd_phase      = infodata%lnd_phase
    if ( present(ice_phase)      ) ice_phase      = infodata%ice_phase
    if ( present(ocn_phase)      ) ocn_phase      = infodata%ocn_phase
    if ( present(glc_phase)      ) glc_phase      = infodata%glc_phase
    if ( present(rof_phase)      ) rof_phase      = infodata%rof_phase
    if ( present(wav_phase)      ) wav_phase      = infodata%wav_phase
    if ( present(esp_phase)      ) esp_phase      = infodata%esp_phase
    if ( present(atm_aero)       ) atm_aero       = infodata%atm_aero
    if ( present(glc_g2lupdate)  ) glc_g2lupdate  = infodata%glc_g2lupdate
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
    if ( present(max_cplstep_time) ) max_cplstep_time = infodata%max_cplstep_time
    if ( present(glc_valid_input)) glc_valid_input = infodata%glc_valid_input

  END SUBROUTINE seq_infodata_GetData_explicit

  !===============================================================================
  SUBROUTINE seq_infodata_PutData_explicit( infodata , &
       cime_model              , &
       case_name               , &
       case_desc               , &
       timing_dir              , &
       model_version           , &
       username                , &
       hostname                , &
       rest_case_name          , &
       tchkpt_dir              , &
       start_type              , &
       perpetual               , &
       perpetual_ymd           , &
       aqua_planet             , &
       aqua_planet_sst         , &
       brnch_retain_casename   , &
       read_restart            , &
       single_column           , &
       scmlat                  , &
       scmlon                  , &
       logFilePostFix          , &
       outPathRoot             , &
       atm_present             , &
       atm_prognostic          , &
       lnd_present             , &
       lnd_prognostic          , &
       rof_prognostic          , &
       rof_present             , &
       ocn_present             , &
       ocn_prognostic          , &
       ocnrof_prognostic       , &
       ice_present             , &
       ice_prognostic          , &
       glc_present             , &
       glc_prognostic          , &
       glc_coupled_fluxes      , &
       flood_present           , &
       wav_present             , &
       wav_prognostic          , &
       rofice_present          , &
       glclnd_present          , &
       glcocn_present          , &
       glcice_present          , &
       iceberg_prognostic      , &
       esp_present             , &
       esp_prognostic          , &
       bfbflag                 , &
       cpl_decomp              , &
       cpl_seq_option          , &
       info_debug              , &
       dead_comps              , &
       run_barriers            , &
       nextsw_cday             , &
       precip_fact             , &
       flux_epbal              , &
       flux_albav              , &
       glc_g2lupdate           , &
       atm_aero                , &
       wall_time_limit         , &
       drv_threading           , &
       flux_diurnal            , &
       gust_fac                , &
       cpl_cdf64               , &
       orb_iyear               , &
       orb_iyear_align         , &
       orb_mode                , &
       orb_mvelp               , &
       orb_eccen               , &
       orb_obliqr              , &
       orb_obliq               , &
       orb_lambm0              , &
       orb_mvelpp              , &
       tfreeze_option          , &
       glc_renormalize_smb     , &
       glc_phase               , &
       rof_phase               , &
       atm_phase               , &
       lnd_phase               , &
       ocn_phase               , &
       ice_phase               , &
       wav_phase               , &
       esp_phase               , &
       wav_nx                  , &
       wav_ny                  , &
       atm_nx                  , &
       atm_ny                  , &
       lnd_nx                  , &
       lnd_ny                  , &
       rof_nx                  , &
       rof_ny                  , &
       ice_nx                  , &
       ice_ny                  , &
       ocn_nx                  , &
       ocn_ny                  , &
       glc_nx                  , &
       glc_ny                  , &
       atm_resume              , &
       lnd_resume              , &
       ocn_resume              , &
       ice_resume              , &
       glc_resume              , &
       rof_resume              , &
       wav_resume              , &
       cpl_resume              , &
       glc_valid_input)

    implicit none

    ! !DESCRIPTION:  Put values into the infodata object.

    ! !INPUT/OUTPUT PARAMETERS:

    type(seq_infodata_type),          intent(INOUT) :: infodata                ! Input CCSM structure
    character(len=*),       optional, intent(IN)    :: cime_model              ! CIME model (acme or cesm)
    character(len=*),       optional, intent(IN)    :: start_type              ! Start type
    character(len=*),       optional, intent(IN)    :: case_name               ! Short case identification
    character(len=*),       optional, intent(IN)    :: case_desc               ! Long case description
    character(len=*),       optional, intent(IN)    :: model_version           ! Model version
    character(len=*),       optional, intent(IN)    :: username                ! Username
    character(len=*),       optional, intent(IN)    :: hostname                ! Hostname
    character(len=*),       optional, intent(IN)    :: rest_case_name          ! restart casename
    character(len=*),       optional, intent(IN)    :: timing_dir              ! timing dir name
    character(len=*),       optional, intent(IN)    :: tchkpt_dir              ! timing checkpoint dir name
    logical,                optional, intent(IN)    :: aqua_planet             ! aqua_planet mode
    integer(SHR_KIND_IN),   optional, intent(IN)    :: aqua_planet_sst         ! aqua_planet sst type
    logical,                optional, intent(IN)    :: run_barriers            ! barrier component run calls
    logical,                optional, intent(IN)    :: brnch_retain_casename
    logical,                optional, intent(IN)    :: read_restart            ! read restart flag (only for data models)
    logical,                optional, intent(IN)    :: single_column
    real (SHR_KIND_R8),     optional, intent(IN)    :: scmlat
    real (SHR_KIND_R8),     optional, intent(IN)    :: scmlon
    character(len=*),       optional, intent(IN)    :: logFilePostFix          ! output log file postfix
    character(len=*),       optional, intent(IN)    :: outPathRoot             ! output file root
    logical,                optional, intent(IN)    :: perpetual               ! If this is perpetual
    integer,                optional, intent(IN)    :: perpetual_ymd           ! If perpetual, date
    character(len=*),       optional, intent(IN)    :: orb_mode                ! orbital mode
    integer,                optional, intent(IN)    :: orb_iyear               ! orbital year
    integer,                optional, intent(IN)    :: orb_iyear_align         ! orbital year model year align
    real(SHR_KIND_R8),      optional, intent(IN)    :: orb_eccen               ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(IN)    :: orb_obliqr              ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(IN)    :: orb_obliq               ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(IN)    :: orb_lambm0              ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(IN)    :: orb_mvelpp              ! See shr_orb_mod
    real(SHR_KIND_R8),      optional, intent(IN)    :: orb_mvelp               ! See shr_orb_mod
    character(len=*),       optional, intent(IN)    :: tfreeze_option          ! Freezing point of salt water
    character(len=*),       optional, intent(IN)    :: flux_epbal              ! selects E,P,R adjustment technique
    logical,                optional, intent(IN)    :: flux_albav              ! T => no diurnal cycle in ocn albedos
    logical,                optional, intent(IN)    :: flux_diurnal            ! T => diurnal cycle in atm/ocn flux
    real(SHR_KIND_R8),      optional, intent(IN)    :: gust_fac                ! wind gustiness factor
    character(len=*),       optional, intent(IN)    :: glc_renormalize_smb     ! Whether to renormalize smb sent from lnd -> glc
    real(SHR_KIND_R8),      optional, intent(IN)    :: wall_time_limit         ! force stop wall time (hours)
    integer,                optional, intent(IN)    :: cpl_decomp              ! coupler decomp
    character(len=*),       optional, intent(IN)    :: cpl_seq_option          ! coupler sequencing option
    logical,                optional, intent(IN)    :: cpl_cdf64               ! netcdf large file setting
    logical,                optional, intent(IN)    :: drv_threading      ! driver threading control flag

    integer(SHR_KIND_IN),   optional, intent(IN)    :: info_debug
    logical,                optional, intent(IN)    :: bfbflag
    logical,                optional, intent(IN)    :: dead_comps         ! do we have dead models

    logical,                optional, intent(IN)    :: atm_present        ! provide data
    logical,                optional, intent(IN)    :: atm_prognostic     ! need data
    logical,                optional, intent(IN)    :: lnd_present
    logical,                optional, intent(IN)    :: lnd_prognostic
    logical,                optional, intent(IN)    :: rof_present
    logical,                optional, intent(IN)    :: rofice_present
    logical,                optional, intent(IN)    :: rof_prognostic
    logical,                optional, intent(IN)    :: flood_present
    logical,                optional, intent(IN)    :: ocn_present
    logical,                optional, intent(IN)    :: ocn_prognostic
    logical,                optional, intent(IN)    :: ocnrof_prognostic
    logical,                optional, intent(IN)    :: ice_present
    logical,                optional, intent(IN)    :: ice_prognostic
    logical,                optional, intent(IN)    :: iceberg_prognostic
    logical,                optional, intent(IN)    :: glc_present
    logical,                optional, intent(IN)    :: glclnd_present
    logical,                optional, intent(IN)    :: glcocn_present
    logical,                optional, intent(IN)    :: glcice_present
    logical,                optional, intent(IN)    :: glc_prognostic
    logical,                optional, intent(IN)    :: glc_coupled_fluxes
    logical,                optional, intent(IN)    :: wav_present
    logical,                optional, intent(IN)    :: wav_prognostic
    logical,                optional, intent(IN)    :: esp_present
    logical,                optional, intent(IN)    :: esp_prognostic
    integer(SHR_KIND_IN),   optional, intent(IN)    :: atm_nx             ! nx,ny 2d grid size global
    integer(SHR_KIND_IN),   optional, intent(IN)    :: atm_ny             ! nx,ny 2d grid size global
    integer(SHR_KIND_IN),   optional, intent(IN)    :: lnd_nx
    integer(SHR_KIND_IN),   optional, intent(IN)    :: lnd_ny
    integer(SHR_KIND_IN),   optional, intent(IN)    :: rof_nx
    integer(SHR_KIND_IN),   optional, intent(IN)    :: rof_ny
    integer(SHR_KIND_IN),   optional, intent(IN)    :: ice_nx
    integer(SHR_KIND_IN),   optional, intent(IN)    :: ice_ny
    integer(SHR_KIND_IN),   optional, intent(IN)    :: ocn_nx
    integer(SHR_KIND_IN),   optional, intent(IN)    :: ocn_ny
    integer(SHR_KIND_IN),   optional, intent(IN)    :: glc_nx
    integer(SHR_KIND_IN),   optional, intent(IN)    :: glc_ny
    integer(SHR_KIND_IN),   optional, intent(IN)    :: wav_nx
    integer(SHR_KIND_IN),   optional, intent(IN)    :: wav_ny
    real(SHR_KIND_R8),      optional, intent(IN)    :: nextsw_cday        ! calendar of next atm shortwave
    real(SHR_KIND_R8),      optional, intent(IN)    :: precip_fact        ! precip factor
    integer(SHR_KIND_IN),   optional, intent(IN)    :: atm_phase          ! atm phase
    integer(SHR_KIND_IN),   optional, intent(IN)    :: lnd_phase          ! lnd phase
    integer(SHR_KIND_IN),   optional, intent(IN)    :: ice_phase          ! ice phase
    integer(SHR_KIND_IN),   optional, intent(IN)    :: ocn_phase          ! ocn phase
    integer(SHR_KIND_IN),   optional, intent(IN)    :: glc_phase          ! glc phase
    integer(SHR_KIND_IN),   optional, intent(IN)    :: rof_phase          ! rof phase
    integer(SHR_KIND_IN),   optional, intent(IN)    :: wav_phase          ! wav phase
    integer(SHR_KIND_IN),   optional, intent(IN) :: esp_phase             ! esp phase
    logical,                optional, intent(IN) :: atm_aero              ! atm aerosols
    logical,                optional, intent(IN) :: glc_g2lupdate         ! update glc2lnd fields in lnd model
    logical,                optional, intent(IN) :: glc_valid_input
    character(SHR_KIND_CL), optional, intent(IN) :: atm_resume(:)         ! atm resume
    character(SHR_KIND_CL), optional, intent(IN) :: lnd_resume(:)         ! lnd resume
    character(SHR_KIND_CL), optional, intent(IN) :: ice_resume(:)         ! ice resume
    character(SHR_KIND_CL), optional, intent(IN) :: ocn_resume(:)         ! ocn resume
    character(SHR_KIND_CL), optional, intent(IN) :: glc_resume(:)         ! glc resume
    character(SHR_KIND_CL), optional, intent(IN) :: rof_resume(:)         ! rof resume
    character(SHR_KIND_CL), optional, intent(IN) :: wav_resume(:)         ! wav resume
    character(SHR_KIND_CL), optional, intent(IN) :: cpl_resume            ! cpl resume

    !----- local -----
    character(len=*), parameter :: subname = '(seq_infodata_PutData_explicit) '
    !-------------------------------------------------------------------------------

    if ( present(cime_model)            ) infodata%cime_model     = cime_model
    if ( present(start_type)            ) infodata%start_type     = start_type
    if ( present(case_name)             ) infodata%case_name      = case_name
    if ( present(case_desc)             ) infodata%case_desc      = case_desc
    if ( present(model_version)         ) infodata%model_version  = model_version
    if ( present(username)              ) infodata%username       = username
    if ( present(hostname)              ) infodata%hostname       = hostname
    if ( present(rest_case_name)        ) infodata%rest_case_name = rest_case_name
    if ( present(timing_dir)            ) infodata%timing_dir     = timing_dir
    if ( present(tchkpt_dir)            ) infodata%tchkpt_dir     = tchkpt_dir
    if ( present(aqua_planet)           ) infodata%aqua_planet    = aqua_planet
    if ( present(aqua_planet_sst)       ) infodata%aqua_planet_sst= aqua_planet_sst
    if ( present(run_barriers)          ) infodata%run_barriers   = run_barriers
    if ( present(brnch_retain_casename) ) infodata%brnch_retain_casename = brnch_retain_casename
    if ( present(read_restart)          ) infodata%read_restart   = read_restart
    if ( present(single_column)         ) infodata%single_column  = single_column
    if ( present(scmlat)                ) infodata%scmlat         = scmlat
    if ( present(scmlon)                ) infodata%scmlon         = scmlon
    if ( present(logFilePostFix)        ) infodata%logFilePostFix = logFilePostFix
    if ( present(outPathRoot)           ) infodata%outPathRoot    = outPathRoot
    if ( present(perpetual)             ) infodata%perpetual      = perpetual
    if ( present(perpetual_ymd)         ) infodata%perpetual_ymd  = perpetual_ymd
    if ( present(orb_iyear)             ) infodata%orb_iyear      = orb_iyear
    if ( present(orb_iyear_align)       ) infodata%orb_iyear_align= orb_iyear_align
    if ( present(orb_mode)              ) infodata%orb_mode       = orb_mode
    if ( present(orb_eccen)             ) infodata%orb_eccen      = orb_eccen
    if ( present(orb_obliqr)            ) infodata%orb_obliqr     = orb_obliqr
    if ( present(orb_obliq)             ) infodata%orb_obliq      = orb_obliq
    if ( present(orb_lambm0)            ) infodata%orb_lambm0     = orb_lambm0
    if ( present(orb_mvelpp)            ) infodata%orb_mvelpp     = orb_mvelpp
    if ( present(orb_mvelp)             ) infodata%orb_mvelp      = orb_mvelp
    if ( present(tfreeze_option)        ) infodata%tfreeze_option    = tfreeze_option
    if ( present(flux_epbal)            ) infodata%flux_epbal     = flux_epbal
    if ( present(flux_albav)            ) infodata%flux_albav     = flux_albav
    if ( present(flux_diurnal)          ) infodata%flux_diurnal   = flux_diurnal
    if ( present(gust_fac)              ) infodata%gust_fac       = gust_fac
    if ( present(glc_renormalize_smb)   ) infodata%glc_renormalize_smb = glc_renormalize_smb
    if ( present(wall_time_limit)       ) infodata%wall_time_limit= wall_time_limit
    if ( present(cpl_decomp)            ) infodata%cpl_decomp     = cpl_decomp
    if ( present(cpl_seq_option)        ) infodata%cpl_seq_option = cpl_seq_option
    if ( present(cpl_cdf64)             ) infodata%cpl_cdf64      = cpl_cdf64
    if ( present(drv_threading)         ) infodata%drv_threading  = drv_threading

    if ( present(info_debug)            ) infodata%info_debug     = info_debug
    if ( present(bfbflag)               ) infodata%bfbflag        = bfbflag
    if ( present(dead_comps)            ) infodata%dead_comps     = dead_comps

    if ( present(atm_present)           ) infodata%atm_present    = atm_present
    if ( present(atm_prognostic)        ) infodata%atm_prognostic = atm_prognostic
    if ( present(lnd_present)           ) infodata%lnd_present    = lnd_present
    if ( present(lnd_prognostic)        ) infodata%lnd_prognostic = lnd_prognostic
    if ( present(rof_present)           ) infodata%rof_present    = rof_present
    if ( present(rofice_present)        ) infodata%rofice_present = rofice_present
    if ( present(rof_prognostic)        ) infodata%rof_prognostic = rof_prognostic
    if ( present(flood_present)         ) infodata%flood_present  = flood_present
    if ( present(ocn_present)           ) infodata%ocn_present    = ocn_present
    if ( present(ocn_prognostic)        ) infodata%ocn_prognostic = ocn_prognostic
    if ( present(ocnrof_prognostic)     ) infodata%ocnrof_prognostic = ocnrof_prognostic
    if ( present(ice_present)           ) infodata%ice_present    = ice_present
    if ( present(ice_prognostic)        ) infodata%ice_prognostic = ice_prognostic
    if ( present(iceberg_prognostic)    ) infodata%iceberg_prognostic = iceberg_prognostic
    if ( present(glc_present)           ) infodata%glc_present    = glc_present
    if ( present(glclnd_present)        ) infodata%glclnd_present = glclnd_present
    if ( present(glcocn_present)        ) infodata%glcocn_present = glcocn_present
    if ( present(glcice_present)        ) infodata%glcice_present = glcice_present
    if ( present(glc_prognostic)        ) infodata%glc_prognostic = glc_prognostic
    if ( present(glc_coupled_fluxes)    ) infodata%glc_coupled_fluxes = glc_coupled_fluxes
    if ( present(wav_present)           ) infodata%wav_present    = wav_present
    if ( present(wav_prognostic)        ) infodata%wav_prognostic = wav_prognostic
    if ( present(esp_present)           ) infodata%esp_present    = esp_present
    if ( present(esp_prognostic)        ) infodata%esp_prognostic = esp_prognostic
    if ( present(atm_nx)                ) infodata%atm_nx         = atm_nx
    if ( present(atm_ny)                ) infodata%atm_ny         = atm_ny
    if ( present(lnd_nx)                ) infodata%lnd_nx         = lnd_nx
    if ( present(lnd_ny)                ) infodata%lnd_ny         = lnd_ny
    if ( present(rof_nx)                ) infodata%rof_nx         = rof_nx
    if ( present(rof_ny)                ) infodata%rof_ny         = rof_ny
    if ( present(ice_nx)                ) infodata%ice_nx         = ice_nx
    if ( present(ice_ny)                ) infodata%ice_ny         = ice_ny
    if ( present(ocn_nx)                ) infodata%ocn_nx         = ocn_nx
    if ( present(ocn_ny)                ) infodata%ocn_ny         = ocn_ny
    if ( present(glc_nx)                ) infodata%glc_nx         = glc_nx
    if ( present(glc_ny)                ) infodata%glc_ny         = glc_ny
    if ( present(wav_nx)                ) infodata%wav_nx         = wav_nx
    if ( present(wav_ny)                ) infodata%wav_ny         = wav_ny

    if ( present(nextsw_cday)           ) infodata%nextsw_cday    = nextsw_cday
    if ( present(precip_fact)           ) infodata%precip_fact    = precip_fact
    if ( present(atm_phase)             ) infodata%atm_phase      = atm_phase
    if ( present(lnd_phase)             ) infodata%lnd_phase      = lnd_phase
    if ( present(ice_phase)             ) infodata%ice_phase      = ice_phase
    if ( present(ocn_phase)             ) infodata%ocn_phase      = ocn_phase
    if ( present(glc_phase)             ) infodata%glc_phase      = glc_phase
    if ( present(rof_phase)             ) infodata%rof_phase      = rof_phase
    if ( present(wav_phase)             ) infodata%wav_phase      = wav_phase
    if ( present(esp_phase)             ) infodata%esp_phase      = esp_phase
    if ( present(atm_aero)              ) infodata%atm_aero       = atm_aero
    if ( present(glc_g2lupdate)         ) infodata%glc_g2lupdate  = glc_g2lupdate
    if ( present(glc_valid_input)       ) infodata%glc_valid_input = glc_valid_input

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

  END SUBROUTINE seq_infodata_PutData_explicit

  !===============================================================================
  subroutine seq_infodata_pauseresume_bcast(infodata, mpicom, pebcast)

    use shr_mpi_mod, only : shr_mpi_bcast

    ! !DESCRIPTION:
    ! Broadcast the pause_resume data from an infodata across pes of mpicom

    ! !INPUT/OUTPUT PARAMETERS:

    type(seq_infodata_type),        intent(INOUT) :: infodata ! assume valid on root pe
    integer(SHR_KIND_IN),           intent(IN)    :: mpicom   ! MPI Communicator
    integer(SHR_KIND_IN), optional, intent(IN)    :: pebcast  ! pe sending

    !----- local -----
    integer                     :: ind
    integer(SHR_KIND_IN)        :: pebcast_local
    character(len=*), parameter :: subname = '(seq_infodata_pauseresume_bcast) '

    if (present(pebcast)) then
       pebcast_local = pebcast
    else
       pebcast_local = 0
    end if

    if (associated(infodata%pause_resume)) then
       do ind = 1, num_inst_atm
          call shr_mpi_bcast(infodata%pause_resume%atm_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       do ind = 1, num_inst_lnd
          call shr_mpi_bcast(infodata%pause_resume%lnd_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       do ind = 1, num_inst_ice
          call shr_mpi_bcast(infodata%pause_resume%ice_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       do ind = 1, num_inst_ocn
          call shr_mpi_bcast(infodata%pause_resume%ocn_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       do ind = 1, num_inst_glc
          call shr_mpi_bcast(infodata%pause_resume%glc_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       do ind = 1, num_inst_rof
          call shr_mpi_bcast(infodata%pause_resume%rof_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       do ind = 1, num_inst_wav
          call shr_mpi_bcast(infodata%pause_resume%wav_resume(ind), mpicom,       &
               pebcast=pebcast_local)
       end do
       call shr_mpi_bcast(infodata%pause_resume%cpl_resume,        mpicom,       &
            pebcast=pebcast_local)
    end if
  end subroutine seq_infodata_pauseresume_bcast

  !===============================================================================
  subroutine seq_infodata_bcast(infodata,mpicom)

    use shr_mpi_mod, only : shr_mpi_bcast

    implicit none

    ! !DESCRIPTION:
    ! Broadcast an infodata across pes of mpicom

    ! !INPUT/OUTPUT PARAMETERS:

    type(seq_infodata_type), intent(INOUT) :: infodata    ! assume valid on root pe
    integer(SHR_KIND_IN),    intent(IN)    :: mpicom      ! mpi comm
    !-------------------------------------------------------------------------------

    call shr_mpi_bcast(infodata%cime_model,              mpicom)
    call shr_mpi_bcast(infodata%start_type,              mpicom)
    call shr_mpi_bcast(infodata%case_desc,               mpicom)
    call shr_mpi_bcast(infodata%model_version,           mpicom)
    call shr_mpi_bcast(infodata%username,                mpicom)
    call shr_mpi_bcast(infodata%hostname,                mpicom)
    call shr_mpi_bcast(infodata%case_name,               mpicom)
    call shr_mpi_bcast(infodata%timing_dir,              mpicom)
    call shr_mpi_bcast(infodata%tchkpt_dir,              mpicom)
    call shr_mpi_bcast(infodata%aqua_planet,             mpicom)
    call shr_mpi_bcast(infodata%aqua_planet_sst,         mpicom)
    call shr_mpi_bcast(infodata%run_barriers,            mpicom)
    call shr_mpi_bcast(infodata%brnch_retain_casename,   mpicom)
    call shr_mpi_bcast(infodata%read_restart,            mpicom)
    call shr_mpi_bcast(infodata%single_column,           mpicom)
    call shr_mpi_bcast(infodata%scmlat,                  mpicom)
    call shr_mpi_bcast(infodata%scmlon,                  mpicom)
    call shr_mpi_bcast(infodata%logFilePostFix,          mpicom)
    call shr_mpi_bcast(infodata%outPathRoot,             mpicom)
    call shr_mpi_bcast(infodata%perpetual,               mpicom)
    call shr_mpi_bcast(infodata%perpetual_ymd,           mpicom)
    call shr_mpi_bcast(infodata%orb_iyear,               mpicom)
    call shr_mpi_bcast(infodata%orb_iyear_align,         mpicom)
    call shr_mpi_bcast(infodata%orb_mode,                mpicom)
    call shr_mpi_bcast(infodata%orb_eccen,               mpicom)
    call shr_mpi_bcast(infodata%orb_obliq,               mpicom)
    call shr_mpi_bcast(infodata%orb_mvelp,               mpicom)
    call shr_mpi_bcast(infodata%orb_obliqr,              mpicom)
    call shr_mpi_bcast(infodata%orb_lambm0,              mpicom)
    call shr_mpi_bcast(infodata%orb_mvelpp,              mpicom)
    call shr_mpi_bcast(infodata%tfreeze_option,          mpicom)
    call shr_mpi_bcast(infodata%flux_epbal,              mpicom)
    call shr_mpi_bcast(infodata%flux_albav,              mpicom)
    call shr_mpi_bcast(infodata%flux_diurnal,            mpicom)
    call shr_mpi_bcast(infodata%gust_fac,                mpicom)
    call shr_mpi_bcast(infodata%glc_renormalize_smb,     mpicom)
    call shr_mpi_bcast(infodata%wall_time_limit,         mpicom)
    call shr_mpi_bcast(infodata%cpl_decomp,              mpicom)
    call shr_mpi_bcast(infodata%cpl_seq_option,          mpicom)
    call shr_mpi_bcast(infodata%cpl_cdf64,               mpicom)
    call shr_mpi_bcast(infodata%drv_threading,           mpicom)
    call shr_mpi_bcast(infodata%info_debug,              mpicom)
    call shr_mpi_bcast(infodata%bfbflag,                 mpicom)
    call shr_mpi_bcast(infodata%dead_comps,              mpicom)

    call shr_mpi_bcast(infodata%atm_present,             mpicom)
    call shr_mpi_bcast(infodata%atm_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%lnd_present,             mpicom)
    call shr_mpi_bcast(infodata%lnd_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%rof_present,             mpicom)
    call shr_mpi_bcast(infodata%rofice_present,          mpicom)
    call shr_mpi_bcast(infodata%rof_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%flood_present,           mpicom)
    call shr_mpi_bcast(infodata%ocn_present,             mpicom)

    call shr_mpi_bcast(infodata%ocn_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%ocnrof_prognostic,       mpicom)
    call shr_mpi_bcast(infodata%ice_present,             mpicom)
    call shr_mpi_bcast(infodata%ice_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%iceberg_prognostic,      mpicom)
    call shr_mpi_bcast(infodata%glc_present,             mpicom)
    call shr_mpi_bcast(infodata%glclnd_present,          mpicom)
    call shr_mpi_bcast(infodata%glcocn_present,          mpicom)
    call shr_mpi_bcast(infodata%glcice_present,          mpicom)
    call shr_mpi_bcast(infodata%glc_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%glc_coupled_fluxes,      mpicom)
    call shr_mpi_bcast(infodata%wav_present,             mpicom)
    call shr_mpi_bcast(infodata%wav_prognostic,          mpicom)
    call shr_mpi_bcast(infodata%esp_present,             mpicom)
    call shr_mpi_bcast(infodata%esp_prognostic,          mpicom)

    call shr_mpi_bcast(infodata%atm_nx,                  mpicom)
    call shr_mpi_bcast(infodata%atm_ny,                  mpicom)
    call shr_mpi_bcast(infodata%lnd_nx,                  mpicom)
    call shr_mpi_bcast(infodata%lnd_ny,                  mpicom)
    call shr_mpi_bcast(infodata%rof_nx,                  mpicom)
    call shr_mpi_bcast(infodata%rof_ny,                  mpicom)
    call shr_mpi_bcast(infodata%ice_nx,                  mpicom)
    call shr_mpi_bcast(infodata%ice_ny,                  mpicom)
    call shr_mpi_bcast(infodata%ocn_nx,                  mpicom)
    call shr_mpi_bcast(infodata%ocn_ny,                  mpicom)
    call shr_mpi_bcast(infodata%glc_nx,                  mpicom)
    call shr_mpi_bcast(infodata%glc_ny,                  mpicom)
    call shr_mpi_bcast(infodata%wav_nx,                  mpicom)
    call shr_mpi_bcast(infodata%wav_ny,                  mpicom)

    call shr_mpi_bcast(infodata%nextsw_cday,             mpicom)
    call shr_mpi_bcast(infodata%precip_fact,             mpicom)

    call shr_mpi_bcast(infodata%atm_phase,               mpicom)
    call shr_mpi_bcast(infodata%lnd_phase,               mpicom)
    call shr_mpi_bcast(infodata%ice_phase,               mpicom)
    call shr_mpi_bcast(infodata%ocn_phase,               mpicom)
    call shr_mpi_bcast(infodata%glc_phase,               mpicom)
    call shr_mpi_bcast(infodata%rof_phase,               mpicom)
    call shr_mpi_bcast(infodata%wav_phase,               mpicom)

    call shr_mpi_bcast(infodata%atm_aero,                mpicom)
    call shr_mpi_bcast(infodata%glc_g2lupdate,           mpicom)
    call shr_mpi_bcast(infodata%glc_valid_input,         mpicom)

    call seq_infodata_pauseresume_bcast(infodata,        mpicom)

  end subroutine seq_infodata_bcast

END MODULE seq_infodata_mod
