module MED

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to
  ! do so.
  !-----------------------------------------------------------------------------

  use mpi
  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    mediator_routine_SS             => SetServices, &
    mediator_routine_Run            => routine_Run, &
    mediator_label_DataInitialize   => label_DataInitialize, &
    mediator_label_Advance          => label_Advance, &
    mediator_label_CheckImport      => label_CheckImport, &
    mediator_label_TimestampExport  => label_TimestampExport, &
    mediator_label_SetRunClock      => label_SetRunClock, &
    NUOPC_MediatorGet

  use seq_comm_mct          , only: llogunit => logunit
  use shr_kind_mod          , only: SHR_KIND_CL
  use shr_sys_mod           , only: shr_sys_flush, shr_sys_abort

  use shr_nuopc_fldList_mod , only: flds_scalar_name
  use shr_nuopc_fldList_mod , only: flds_x2a, flds_a2x
  use shr_nuopc_fldList_mod , only: flds_x2i, flds_i2x
  use shr_nuopc_fldList_mod , only: flds_x2l, flds_l2x
  use shr_nuopc_fldList_mod , only: flds_x2o, flds_o2x
  use shr_nuopc_fldList_mod , only: flds_x2r, flds_r2x
  use shr_nuopc_fldList_mod , only: flds_x2g, flds_g2x
  use shr_nuopc_fldList_mod , only: flds_x2w, flds_w2x
  use shr_nuopc_fldList_mod , only: fldListFr, fldListTo
  use shr_nuopc_fldList_mod , only: fldListXao_fluxes_a, fldListXao_fluxes_o
  use shr_nuopc_fldList_mod , only: fldListXao_ocnalb_a, fldListXao_ocnalb_o
  use shr_nuopc_fldList_mod , only: ncomps, compmed, compatm, compocn
  use shr_nuopc_fldList_mod , only: compice, complnd, comprof, compwav, compglc, compname
  use shr_nuopc_fldList_mod , only: mapbilnr, mapconsf, mapconsd, mappatch, mapfcopy, mapnames
  use shr_nuopc_fldList_mod , only: med_coupling_allowed
  use shr_nuopc_fldList_mod , only: shr_nuopc_fldList_Realize
  use shr_nuopc_fldList_mod , only: shr_nuopc_fldList_Advertise

  use shr_nuopc_methods_mod , only: shr_nuopc_methods_FB_Init
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_FB_Reset
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_FB_Copy
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_Field_GeomPrint
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_GeomPrint
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_GeomWrite
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_reset
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_getNumFields
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_clock_timeprint
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_Diagnose

  use med_infodata_mod      , only: med_infodata_CopyStateToInfodata
  use med_infodata_mod      , only: infodata=>med_infodata

  use med_internalstate_mod , only: InternalState

  use med_connectors_mod    , only: med_connectors_prep_med2atm
  use med_connectors_mod    , only: med_connectors_prep_med2ocn
  use med_connectors_mod    , only: med_connectors_prep_med2ice
  use med_connectors_mod    , only: med_connectors_prep_med2lnd
  use med_connectors_mod    , only: med_connectors_prep_med2rof
  use med_connectors_mod    , only: med_connectors_prep_med2wav
  use med_connectors_mod    , only: med_connectors_prep_med2glc

  use med_connectors_mod    , only: med_connectors_post_atm2med
  use med_connectors_mod    , only: med_connectors_post_ocn2med
  use med_connectors_mod    , only: med_connectors_post_ice2med
  use med_connectors_mod    , only: med_connectors_post_lnd2med
  use med_connectors_mod    , only: med_connectors_post_rof2med
  use med_connectors_mod    , only: med_connectors_post_wav2med
  use med_connectors_mod    , only: med_connectors_post_glc2med

  use med_phases_mod        , only: med_phases_prep_atm
  use med_phases_mod        , only: med_phases_prep_ocn
  use med_phases_mod        , only: med_phases_prep_ice
  use med_phases_mod        , only: med_phases_prep_lnd
  use med_phases_mod        , only: med_phases_prep_rof
  use med_phases_mod        , only: med_phases_prep_wav
  use med_phases_mod        , only: med_phases_prep_glc
  use med_phases_mod        , only: med_phases_accum_fast
  use med_phases_mod        , only: med_phases_atmocn_init
  use med_phases_mod        , only: med_phases_atmocn_ocnalb
  use med_phases_mod        , only: med_phases_atmocn_flux

  use med_fraction_mod      , only: fraclist
  use med_fraction_mod      , only: med_fraction_setupflds
  use med_fraction_mod      , only: med_fraction_init
  use med_fraction_mod      , only: med_fraction_set

  use med_constants_mod     , only: med_constants_dbug_flag
  use med_constants_mod     , only: med_constants_spval_init
  use med_constants_mod     , only: med_constants_spval
  use med_constants_mod     , only: med_constants_czero
  use med_constants_mod     , only: med_constants_ispval_mask
  use med_constants_mod     , only: med_constants_spval_rhfile

  implicit none

  private

  integer            :: dbug_flag = med_constants_dbug_flag
  integer            :: dbrc
  integer            :: stat
  logical            :: isPresent
  character(len=1024):: msgString
  type(ESMF_VM)      :: vm
  integer            :: localPet
  logical            :: mastertask
  character(len=*)  , parameter :: grid_arbopt = "grid_reg"   ! grid_reg or grid_arb
  real(ESMF_KIND_R8), parameter :: spval_init  = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval       = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero       = med_constants_czero
  integer           , parameter :: ispval_mask = med_constants_ispval_mask
  character(*),parameter :: u_FILE_u = &
    __FILE__

  public SetServices

  private InitializeP0
  private InitializeIPDv03p1 ! advertise fields
  private InitializeIPDv03p3 ! realize connected Fields with transfer action "provide"
  private InitializeIPDv03p4 ! optionally modify the decomp/distr of transferred Grid/Mesh
  private InitializeIPDv03p5 ! realize all Fields with transfer action "accept"
  private DataInitialize     ! finish initialization and resolve data dependencies
  private SetRunClock

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    !------------------
    ! the NUOPC model component mediator_routine_SS will register the generic methods
    !------------------

    call NUOPC_CompDerive(gcomp, mediator_routine_SS, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! set entry point for methods that require specific implementation
    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    !------------------

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      InitializeP0, phase=0, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! IPDv03p1: advertise Fields
    !------------------

    ! Mediator advertises its import and export Fields and sets the TransferOfferGeomObject Attribute.
    ! The TransferOfferGeomObject is a String value indicating a component’s
    ! intention to transfer the underlying Grid or Mesh on which an advertised Field object is defined.
    ! The valid values are: [will provide, can provide, cannot provide]

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeIPDv03p1, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeIPDv03p3, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeIPDv03p4, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! IPDv03p5: realize all Fields with transfer action "accept"
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeIPDv03p5, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! attach specializing method for DataInitialize
    !------------------

    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! setup various mediator phases
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_accum_fast"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_accum_fast", specRoutine=med_phases_accum_fast, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_fraction_set"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_fraction_set", specRoutine=med_fraction_set, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! prep and post phases for connectors
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2atm"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2atm", specRoutine=med_connectors_prep_med2atm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_atm2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_atm2med", specRoutine=med_connectors_post_atm2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2ocn"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2ocn", specRoutine=med_connectors_prep_med2ocn, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_ocn2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_ocn2med", specRoutine=med_connectors_post_ocn2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2ice"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2ice", specRoutine=med_connectors_prep_med2ice, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_ice2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_ice2med", specRoutine=med_connectors_post_ice2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2lnd"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2lnd", specRoutine=med_connectors_prep_med2lnd, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_lnd2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_lnd2med", specRoutine=med_connectors_post_lnd2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2rof"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2rof", specRoutine=med_connectors_prep_med2rof, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_rof2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_rof2med", specRoutine=med_connectors_post_rof2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2wav"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2wav", specRoutine=med_connectors_prep_med2wav, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_wav2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_wav2med", specRoutine=med_connectors_post_wav2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_prep_med2glc"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_prep_med2glc", specRoutine=med_connectors_prep_med2glc, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_connectors_post_glc2med"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_connectors_post_glc2med", specRoutine=med_connectors_post_glc2med, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! prep routines for model imports
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_atm"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_atm", specRoutine=med_phases_prep_atm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_ocn"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_ocn", specRoutine=med_phases_prep_ocn, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_ice"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_ice", specRoutine=med_phases_prep_ice, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_lnd"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_lnd", specRoutine=med_phases_prep_lnd, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_rof"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_rof", specRoutine=med_phases_prep_rof, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_wav"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_wav", specRoutine=med_phases_prep_wav, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_prep_glc"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_prep_glc", specRoutine=med_phases_prep_glc, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! phase routines for atmocn flux computation and and ocean albedo computation
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_atmocn_ocnalb"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_atmocn_ocnalb", specRoutine=med_phases_atmocn_ocnalb_run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_atmocn_flux"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_atmocn_flux", specRoutine=med_phases_atmocn_flux_run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! attach specializing method(s)
    ! -> NUOPC specializes by default --->>> first need to remove the default
    !------------------

    call ESMF_MethodRemove(gcomp, mediator_label_CheckImport, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specRoutine=NUOPC_NoOp, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! attach specializing method(s)
    ! -> NUOPC specializes by default --->>> first need to remove the default
    !------------------

    call ESMF_MethodRemove(gcomp, mediator_label_SetRunClock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specRoutine=SetRunClock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP0)'
    character(len=128)         :: value

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    mastertask = .false.
    if (localPet == 0) mastertask=.true.

    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, defaultValue="max", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    dbug_flag = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","high"/), specialValueList=(/0,255,255/), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(A,i6)') trim(subname)//' dbug_flag = ',dbug_flag
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeP0

  !-----------------------------------------------------------------------

  subroutine InitializeIPDv03p1(gcomp, importState, exportState, clock, rc)

    ! Mediator advertises its import and export Fields and sets the
    ! TransferOfferGeomObject Attribute.

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer :: n, n1, n2
    type(InternalState)        :: is_local
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p1)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !------------------
    ! Allocate memory for the internal state and set it in the Component.
    !------------------

    allocate(is_local%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
         msg="Allocation of the internal state memory failed.", &
         line=__LINE__, &
         file=u_FILE_u)) &
         return  ! bail out

    call ESMF_GridCompSetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! add a namespace (i.e. nested state)  for each import and export component state in the mediator's InternalState
    !------------------

    ! Namespaces are implemented via nested states. This creates a nested state inside of
    ! state. The nested state is returned as nestedState. nestedStateName will be used to name the
    ! newly created nested state.

    call NUOPC_AddNamespace(importState, namespace="ATM", nestedStateName="NestedState-AtmImp", &
         nestedState=is_local%wrap%NStateImp(compatm), rc=rc)
    call NUOPC_AddNamespace(importState, namespace="OCN", nestedStateName="NestedState-OcnImp", &
         nestedState=is_local%wrap%NStateImp(compocn), rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ICE", nestedStateName="NestedState-IceImp", &
         nestedState=is_local%wrap%NStateImp(compice), rc=rc)
    call NUOPC_AddNamespace(importState, namespace="LND", nestedStateName="NestedState-LndImp", &
         nestedState=is_local%wrap%NStateImp(complnd), rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ROF", nestedStateName="NestedState-RofImp", &
         nestedState=is_local%wrap%NStateImp(comprof), rc=rc)
    call NUOPC_AddNamespace(importState, namespace="WAV", nestedStateName="NestedState-WavImp", &
         nestedState=is_local%wrap%NStateImp(compwav), rc=rc)
    call NUOPC_AddNamespace(importState, namespace="GLC", nestedStateName="NestedState-GlcImp", &
         nestedState=is_local%wrap%NStateImp(compglc), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ATM", nestedStateName="NestedState-AtmExp", &
         nestedState=is_local%wrap%NStateExp(compatm), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="OCN", nestedStateName="NestedState-OcnExp", &
         nestedState=is_local%wrap%NStateExp(compocn), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ICE", nestedStateName="NestedState-IceExp", &
         nestedState=is_local%wrap%NStateExp(compice), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="LND", nestedStateName="NestedState-LndExp", &
         nestedState=is_local%wrap%NStateExp(complnd), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ROF", nestedStateName="NestedState-RofExp", &
         nestedState=is_local%wrap%NStateExp(comprof), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="WAV", nestedStateName="NestedState-WavExp", &
         nestedState=is_local%wrap%NStateExp(compwav), rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="GLC", nestedStateName="NestedState-GlcExp", &
         nestedState=is_local%wrap%NStateExp(compglc), rc=rc)

    !------------------
    ! Advertise mediator field names
    !------------------

    do ncomp = 1,ncomps
       do n = 1,size(fldListFr(ncomp)%flds)
          if (trim(fldListFr(ncomp)%flds(n)%shortname) == flds_scalar_name) then
             transferOffer = 'will provide'
          else
             transferOffer = 'cannot provide'
          end if
          call shr_nuopc_flds_Advertise(is_local%wrap%NStateImp(ncomp), fldListFr(ncomp)%flds(n), &
               transferOffer='cannot provide', subname//':Fr'//trim(compname(ncomp)), rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end do

       do n = 1,size(fldListTo(ncomp)%flds)
          if (trim(fldListTo(ncomp)%flds(n)%shortname) == flds_scalar_name) then
             transferOffer = 'will provide'
          else
             transferOffer = 'cannot provide'
          end if
          call shr_nuopc_flds_Advertise(is_local%wrap%NStateImp(ncomp), fldListTo(ncomp)%flds(n), &
               transferOffer=trim(transferOffer), subname//':Fr'//trim(compname(ncomp)), rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end do
    end do

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeIPDv03p1

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p3(gcomp, importState, exportState, clock, rc)

    ! Realize connected Fields with transfer action "provide"

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                         :: i, j
    real(kind=ESMF_KIND_R8),pointer :: lonPtr(:), latPtr(:)
    type(InternalState)             :: is_local
    integer                         :: lmpicom
    real(ESMF_KIND_R8)              :: intervalSec
    type(ESMF_TimeInterval)         :: timeStep
    ! tcx XGrid
    ! type(ESMF_Field)              :: fieldX, fieldA, fieldO
    ! type(ESMF_XGrid)              :: xgrid
    integer                         :: n, n1, n2
    character(SHR_KIND_CL)          :: cvalue
    logical                         :: connected
    character(len=*),parameter      :: subname='(module_MEDIATOR:InitializeIPDv03p3)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize the internal state members
    call ESMF_VMGet(vm, mpiCommunicator=lmpicom, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call MPI_Comm_Dup(lmpicom, is_local%wrap%mpicom, stat)

    !------------------
    ! Realize States
    !------------------

    do n1 = 1,ncomps
      if (ESMF_StateIsCreated(is_local%wrap%NStateImp(n1), rc=rc)) then
         call shr_nuopc_fldList_Realize(is_local%wrap%NStateImp(n1), fldListFr=fldListFr(n1), &
              tag=subname//':Fr'//trim(compname(n1)), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
      if (ESMF_StateIsCreated(is_local%wrap%NStateExp(n1), rc=rc)) then
         call shr_nuopc_fldList_Realize(is_local%wrap%NStateExp(n1), fldListTo=fldListTo(n1), &
              tag=subname//':To'//trim(compname(n1)), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    enddo

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeIPDv03p3

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p4(gcomp, importState, exportState, clock, rc)

    ! Otionally modify the decomp/distr of transferred Grid/Mesh

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    integer :: n1,n2
    !    type(ESMF_Field)              :: field
    !    type(ESMF_Grid)               :: grid
    !    integer                       :: localDeCount
    !    type(ESMF_DistGrid)           :: distgrid
    !    integer                       :: dimCount, tileCount, petCount
    !    integer                       :: deCountPTile, extraDEs
    !    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
    !    integer, allocatable          :: regDecompPTile(:,:)
    !    integer                       :: i, j, n, n1

    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p4)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    rc = ESMF_SUCCESS

    ! Get the internal state from the mediator gridded component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Recieve Grids
    !------------------

    do n1 = 1,ncomps
      if (ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc)) then
        call realizeConnectedGrid(is_local%wrap%NStateImp(n1), trim(compname(n1))//'Imp', rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
      if (ESMF_StateIsCreated(is_local%wrap%NStateExp(n1),rc=rc)) then
        call realizeConnectedGrid(is_local%wrap%NStateExp(n1), trim(compname(n1))//'Exp', rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    enddo

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeConnectedGrid(State,string,rc)

      type(ESMF_State)   , intent(inout) :: State
      character(len=*)   , intent(in)    :: string
      integer            , intent(out)   :: rc

      ! local variables
      type(ESMF_Field)              :: field
      type(ESMF_Grid)               :: grid
      integer                       :: localDeCount

      type(ESMF_DistGrid)           :: distgrid
      type(ESMF_DistGridConnection), allocatable :: connectionList(:)
      integer                       :: arbDimCount
      integer                       :: dimCount, tileCount, petCount
      integer                       :: connectionCount
      integer                       :: deCountPTile, extraDEs
      integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable          :: regDecompPTile(:,:)
      integer                       :: i, j, n, n1, fieldCount, nxg, i1, i2
      type(ESMF_GeomType_Flag)      :: geomtype
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      type(ESMF_FieldStatus_Flag)   :: fieldStatus
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedGrid)'

      !NOTE: All fo the Fields that set their TransferOfferGeomObject Attribute
      !NOTE: to "cannot provide" should now have the accepted Grid available.
      !NOTE: Go and pull out this Grid for one of a representative Field and
      !NOTE: modify the decomposition and distribution of the Grid to match the
      !NOTE: Mediator PETs.

      !TODO: quick implementation, do it for each field one by one
      !TODO: commented out below are application to other fields

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      do n=1, min(fieldCount,1)

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then

          ! while this is still an empty field, it does now hold a Grid with DistGrid
          call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (geomtype == ESMF_GEOMTYPE_GRID) then

            call shr_nuopc_methods_Field_GeomPrint(field,trim(fieldNameList(n))//'_orig',rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

            call ESMF_AttributeGet(field, name="ArbDimCount", value=arbDimCount, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

            ! make decision on whether the incoming Grid is arbDistr or not
            if (arbDimCount>0) then
              ! The provider defined an arbDistr grid
              !
              ! Need to make a choice here to either represent the grid as a
              ! regDecomp grid on the acceptor side, or to stay with arbDistr grid:
              !
              ! Setting the PRECIP_REGDECOMP macro will set up a regDecomp grid on the
              ! acceptor side.
              !
              ! Not setting the PRECIP_REGDECOMP macro will default into keeping the
              ! original arbDistr Grid.

              if (grid_arbopt == "grid_reg") then

                if (dbug_flag > 1) then
                   call ESMF_LogWrite(trim(subname)//trim(string)//": accept arb2reg grid for "//trim(fieldNameList(n)), &
                        ESMF_LOGMSG_INFO, rc=dbrc)
                endif
                ! Use a regDecomp representation for the grid
                ! first get tile min/max, only single tile supported for arbDistr Grid
                allocate(minIndexPTile(arbDimCount,1),maxIndexPTile(arbDimCount,1))
                call ESMF_AttributeGet(field, name="MinIndex", &
                  valueList=minIndexPTile(:,1), &
                  convention="NUOPC", purpose="Instance", rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                call ESMF_AttributeGet(field, name="MaxIndex", &
                  valueList=maxIndexPTile(:,1), &
                  convention="NUOPC", purpose="Instance", rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                ! create default regDecomp DistGrid
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                 maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                ! Create default regDecomp Grid
                grid = ESMF_GridCreate(distgrid, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                ! swap out the transferred grid for the newly created one
                call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                do i1 = 1,arbDimCount
                  write(msgString,'(A,3i8)') trim(subname)//':PTile =',i1,minIndexPTile(i1,1),maxIndexPTile(i1,1)
                  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
                enddo
                deallocate(minIndexPTile,maxIndexPTile)

              elseif (grid_arbopt == "grid_arb") then

                ! Stick with the arbDistr representation of the grid:
                ! There is nothing to do here if the same number of DEs is kept on the
                ! acceptor side. Alternatively, the acceptor side could set up a more
                ! natural number of DEs (maybe same number as acceptor PETs), and then
                ! redistribute the arbSeqIndexList. Here simply keep the DEs of the
                ! provider Grid.
                if (dbug_flag > 1) then
                   call ESMF_LogWrite(trim(subname)//trim(string)//": accept arb2arb grid for "//trim(fieldNameList(n)), &
                        ESMF_LOGMSG_INFO, rc=dbrc)
                endif

              else   ! grid_arbopt

                 call ESMF_LogWrite(trim(subname)//trim(string)//": ERROR grid_arbopt setting = "//trim(grid_arbopt), &
                      ESMF_LOGMSG_INFO, rc=rc)
                rc = ESMF_FAILURE
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              endif  ! grid_arbopt


            else   ! arbdimcount <= 0

              ! The provider defined as non arb grid

              ! access localDeCount to show this is a real Grid
              if (dbug_flag > 1) then
                 call ESMF_LogWrite(trim(subname)//trim(string)//": accept reg2reg grid for "//trim(fieldNameList(n)), &
                      ESMF_LOGMSG_INFO, rc=dbrc)
              endif

              call ESMF_FieldGet(field, grid=grid, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              ! Create a custom DistGrid, based on the minIndex, maxIndex of the
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.

              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
                connectionCount=connectionCount, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                       maxIndexPTile(dimCount, tileCount))
              allocate(connectionList(connectionCount))

              ! get minIndex and maxIndex arrays, and connectionList
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              ! construct a default regDecompPTile -> TODO: move this into ESMF as default
              call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              allocate(regDecompPTile(dimCount, tileCount))
              deCountPTile = petCount/tileCount
              extraDEs = max(0, petCount-deCountPTile)
              do i=1, tileCount
                if (i<=extraDEs) then
                  regDecompPTile(1, i) = deCountPTile + 1
                else
                  regDecompPTile(1, i) = deCountPTile
                endif
                do j=2, dimCount
                  regDecompPTile(j, i) = 1
                enddo
              enddo

              do i2 = 1,tileCount
              do i1 = 1,dimCount
                 write(msgString,'(A,5i8)') trim(subname)//':PTile =',i2,i1,minIndexPTile(i1,i2),&
                      maxIndexPTile(i1,i2),regDecompPTile(i1,i2)
                call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
              enddo
              enddo

              !--- tcraig, hardwire i direction wraparound, temporary
              !--- tcraig, now getting info from model distgrid, see above
              !              allocate(connectionList(1))
              !              nxg = maxIndexPTile(1,1) - minIndexPTile(1,1) + 1
              !              write(msgstring,*) trim(subname)//trim(string),': connlist nxg = ',nxg
              !              call ESMF_LogWrite(trim(msgstring), ESMF_LOGMSG_INFO, rc=rc)
              !              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
              !              call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
              !                tileIndexB=1, positionVector=(/nxg, 0/), rc=rc)
              !              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

              ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
              ! but with a default regDecompPTile
              ! tcraig, force connectionlist and gridEdge arguments to fix wraparound
              ! need ESMF fixes to implement properly.
              if (dimcount == 2) then
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, &
                  connectionList=connectionList, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                if (dbug_flag > 1) then
                  call ESMF_LogWrite(trim(subname)//trim(string)//': distgrid with dimcount=2', ESMF_LOGMSG_INFO, rc=rc)
                  if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                endif
                ! Create a new Grid on the new DistGrid and swap it in the Field
                grid = ESMF_GridCreate(distgrid, &
                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
              else
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                if (dbug_flag > 1) then
                  call ESMF_LogWrite(trim(subname)//trim(string)//': distgrid with dimcount=1', ESMF_LOGMSG_INFO, rc=rc)
                  if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                endif
                ! Create a new Grid on the new DistGrid and swap it in the Field
                grid = ESMF_GridCreate(distgrid, &
                  gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
              endif

              ! local clean-up
              deallocate(connectionList)
              deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)

            endif  ! arbdimCount

          ! Swap all the Grids in the State

!            do n1=n,n
            do n1=1, fieldCount
              ! access a field in the State and set the Grid
              call ESMF_StateGet(State, field=field, itemName=fieldNameList(n1), rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
              if (dbug_flag > 1) then
                 call ESMF_LogWrite(trim(subname)//trim(string)//": attach grid for "//trim(fieldNameList(n1)), &
                      ESMF_LOGMSG_INFO, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
              endif
              call shr_nuopc_methods_Field_GeomPrint(field,trim(fieldNameList(n))//'_new',rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
            enddo

          elseif (geomtype == ESMF_GEOMTYPE_MESH) then

            call shr_nuopc_methods_Field_GeomPrint(field,trim(fieldNameList(n))//'_orig',rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          else  ! geomtype

            call ESMF_LogWrite(trim(subname)//": ERROR geomtype not supported ", ESMF_LOGMSG_INFO, rc=rc)
            rc=ESMF_FAILURE
            return

          endif ! geomtype

        elseif (fieldStatus==ESMF_FIELDSTATUS_EMPTY) then

          call ESMF_LogWrite(trim(subname)//trim(string)//": provide grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

        elseif (fieldStatus==ESMF_FIELDSTATUS_COMPLETE) then

          call ESMF_LogWrite(trim(subname)//trim(string)//": no grid provided for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

        else

          call ESMF_LogWrite(trim(subname)//": ERROR fieldStatus not supported ", ESMF_LOGMSG_INFO, rc=rc)
          rc=ESMF_FAILURE
          return

        endif   ! fieldStatus

      enddo   ! n fld loop

      deallocate(fieldNameList)

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine realizeConnectedGrid

  end subroutine InitializeIPDv03p4

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p5(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    integer             :: n1,n2
    character(len=*),parameter  :: subname='(module_MEDIATOR:InitializeIPDv03p5)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    rc = ESMF_SUCCESS

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !----------------------------------------------------------
    !--- Finish initializing the State Fields
    !--- Write out grid information
    !----------------------------------------------------------

    do n1 = 1,ncomps
      if (ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc)) then
        call completeFieldInitialization(is_local%wrap%NStateImp(n1), rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        call shr_nuopc_methods_State_reset(is_local%wrap%NStateImp(n1), value=spval_init, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif

      if (ESMF_StateIsCreated(is_local%wrap%NStateExp(n1),rc=rc)) then
        call completeFieldInitialization(is_local%wrap%NStateExp(n1), rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        call shr_nuopc_methods_State_reset(is_local%wrap%NStateExp(n1), value=spval_init, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NStateExp(n1),'gridExp'//trim(compname(n1)),rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NStateExp(n1), 'grid_med_'//trim(compname(n1)), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    enddo

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine completeFieldInitialization(State,rc)

      type(ESMF_State)   , intent(inout) :: State
      integer            , intent(out)   :: rc

      integer                     :: n, fieldCount
      character(ESMF_MAXSTR)      :: fieldName
      type(ESMF_Field),pointer    :: fieldList(:)
      type(ESMF_FieldStatus_Flag) :: fieldStatus
      character(len=*),parameter  :: subname='(module_MEDIATOR:completeFieldInitialization)'

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      rc = ESMF_Success

      call shr_nuopc_methods_State_GetNumFields(State, fieldCount, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      if (fieldCount > 0) then
        nullify(fieldList)
        call NUOPC_getStateMemberLists(State, fieldList=fieldList, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        do n=1, fieldCount

          call ESMF_FieldGet(fieldList(n), status=fieldStatus, name=fieldName, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then
            if (dbug_flag > 1) then
              call ESMF_LogWrite(subname//" is allocating field memory for field "//trim(fieldName), &
                   ESMF_LOGMSG_INFO, rc=rc)
              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
            endif

            call ESMF_FieldEmptyComplete(fieldList(n), typekind=ESMF_TYPEKIND_R8, rc=rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif   ! fieldStatus

          call shr_nuopc_methods_Field_GeomPrint(fieldList(n), trim(subname)//':'//trim(fieldName), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        enddo
        deallocate(fieldList)
      endif

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine completeFieldInitialization

  end subroutine InitializeIPDv03p5

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: time
    type(ESMF_Field)            :: field
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: atCorrectTime, allDone, connected
    type(InternalState)         :: is_local
    integer                     :: n1,n2,cntn1,cntn2
    character(len=512)          :: fmapfile, smapfile, dmapfile, pmapfile
    character(len=512)          :: vmapfile, rmapfile, rimapfile, rlmapfile
    character(len=128)          :: value, rhname, rhname_file
    integer                     :: SrcMaskValue, DstMaskValue
    integer                     :: n, fieldCount
    integer                     :: len
    integer                     :: ierr
    logical                     :: first_call = .true.
    character(ESMF_MAXSTR),allocatable    :: fieldNameList(:)
    character(MPI_MAX_ERROR_STRING)       :: lstring
    integer                               :: n1, n2
    type(ESMF_PoleMethod_Flag), parameter :: polemethod=ESMF_POLEMETHOD_ALLAVG
    type(ESMF_Field)                      :: fldsrc, flddst
    integer                               :: lsrcMaskValue, ldstMaskValue
    real(ESMF_KIND_R8), pointer           :: factorList(:)
    character(len=*), parameter :: subname='(module_MEDIATOR:DataInitialize)'
    !-----------------------------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    rc = ESMF_SUCCESS

    ! the MED needs valid ATM export Fields to initialize its internal state

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    ! Beginning  of first_call block
    !---------------------------------------

    if (first_call) then

      ! initialize the present flags in the mediator
      if (dbug_flag > 1) then
        call ESMF_LogWrite("Starting to initialize present flags", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush()
      endif

      !----------------------------------------------------------
      !--- Check present flags
      !----------------------------------------------------------

      do n1 = 1,ncomps
        call ESMF_AttributeGet(gcomp, name=trim(compname(n1))//"_present", value=value, defaultValue="false", &
             convention="NUOPC", purpose="Instance", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        is_local%wrap%comp_present(n1) = (value == "true")
        write(msgString,'(A,L4)') trim(subname)//' comp_present(comp'//trim(compname(n1))//') = ',&
             is_local%wrap%comp_present(n1)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
      enddo

      !----------------------------------------------------------
      !--- Check for active coupling interactions
      !    must be allowed, bundles created, and both sides have some fields
      !----------------------------------------------------------

      if (dbug_flag > 1) then
        call ESMF_LogWrite("Starting to initialize active flags", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush()
      endif

      ! initialize med_coupling_active
      is_local%wrap%med_coupling_active(:,:) = .false.

      do n1 = 1,ncomps
        if (is_local%wrap%comp_present(n1) .and. ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc)) then
          call shr_nuopc_methods_State_GetNumFields(is_local%wrap%NStateImp(n1), cntn1, rc=rc) ! Import Field Count
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          if (cntn1 > 0) then
             do n2 = 1,ncomps
                if (is_local%wrap%comp_present(n2) .and. ESMF_StateIsCreated(is_local%wrap%NStateExp(n2),rc=rc) &
                     .and. med_coupling_allowed(n1,n2)) then
                   call shr_nuopc_methods_State_GetNumFields(is_local%wrap%NStateExp(n2), cntn2, rc=rc) ! Import Field Count
                   if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                   if (cntn2 > 0) then
                      is_local%wrap%med_coupling_active(n1,n2) = .true.
                   endif
                endif
             enddo
          end if
        endif
      enddo

      ! create tables of output
      if (mastertask) then
         if (dbug_flag > 5) then
            write(llogunit,*) ' '
            write(llogunit,'(A)') subname//' Allowed coupling flags'
            write(llogunit,'(2x,A10,20(A5))') '|from to->',(compname(n2),n2=1,ncomps)
            do n1 = 1,ncomps
               write(msgString,'(2x,a1,A,5x,20(L5))') '|',trim(compname(n1)),(med_coupling_allowed(n1,n2),n2=1,ncomps)
               do n2 = 1,len_trim(msgString)
                  if (msgString(n2:n2) == 'F') msgString(n2:n2)='-'
               enddo
               write(llogunit,'(A)') trim(msgString)
            enddo
            write(llogunit,*) ' '
            call shr_sys_flush(llogunit)
         endif

         if (dbug_flag >= 0) then
            write(llogunit,*) ' '
            write(llogunit,'(A)') subname//' Active coupling flags'
            write(llogunit,'(2x,A10,20(A5))') '|from to->',(compname(n2),n2=1,ncomps)
            do n1 = 1,ncomps
               write(msgString,'(2x,a1,A,5x,20(L5))') '|',trim(compname(n1)),&
                    (is_local%wrap%med_coupling_active(n1,n2),n2=1,ncomps)
               do n2 = 1,len_trim(msgString)
                  if (msgString(n2:n2) == 'F') msgString(n2:n2)='-'
               enddo
               write(llogunit,'(A)') trim(msgString)
            enddo
            write(llogunit,*) ' '
            call shr_sys_flush(llogunit)
         endif
      endif

      !----------------------------------------------------------
      ! NOW allocate other Mediator data
      !----------------------------------------------------------

      if (dbug_flag > 1) then
        call ESMF_LogWrite("Starting to initialize FBs", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush()
      endif

      is_local%wrap%conn_prep_cnt(:) = 0
      is_local%wrap%conn_post_cnt(:) = 0

      call med_fraction_setupflds(rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !----------------------------------------------------------
      ! Allocate various FBs, FBImp, FBExp, FBImpAccum, FBExpAccum, FBfrac
      !----------------------------------------------------------

      do n1 = 1,ncomps
        if (is_local%wrap%comp_present(n1) .and. &
            ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc) .and. &
            ESMF_StateIsCreated(is_local%wrap%NStateExp(n1),rc=rc)) then

          if (mastertask) write(llogunit,*) subname,' initializing FBs for '//trim(compname(n1))

          call shr_nuopc_methods_FB_init(is_local%wrap%FBImp(n1,n1), STgeom=is_local%wrap%NStateImp(n1), &
            STflds=is_local%wrap%NStateImp(n1), name='FBImp'//trim(compname(n1)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_init(is_local%wrap%FBExp(n1), STgeom=is_local%wrap%NStateExp(n1), &
            STflds=is_local%wrap%NStateExp(n1), name='FBExp'//trim(compname(n1)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_init(is_local%wrap%FBImpAccum(n1), STgeom=is_local%wrap%NStateImp(n1), &
            STflds=is_local%wrap%NStateImp(n1), name='FBImpAccum'//trim(compname(n1)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_init(is_local%wrap%FBExpAccum(n1), STgeom=is_local%wrap%NStateExp(n1), &
            STflds=is_local%wrap%NStateExp(n1), name='FBExpAccum'//trim(compname(n1)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Initialize FBfrac for component n1 - the field names will be fraclist(:,n1)
          ! Note - must use import state here - since export state might not contain anything other
          ! than scalar data if the component is not prognostic

          call shr_nuopc_methods_FB_init(is_local%wrap%FBfrac(n1), STgeom=is_local%wrap%NStateImp(n1), &
            fieldNameList=fraclist(:,n1), name='FBfrac'//trim(compname(n1)), rc=rc)

        endif
        if (mastertask) call shr_sys_flush(llogunit)

        ! These are the FBImp mapped to different grids, FBImp(n1,n1) is handled above
        do n2 = 1,ncomps
           if (n1 /= n2 .and. &
                is_local%wrap%med_coupling_active(n1,n2) .and. &
                ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc) .and. &
                ESMF_StateIsCreated(is_local%wrap%NStateExp(n2),rc=rc)) then
            if (mastertask) write(llogunit,*) subname,' initializing FBs for '//trim(compname(n1))//'_'//trim(compname(n2))

            ! TODO:
            ! Important Note - the NStateImp(n2) should be used here rather than NStateExp(n2), since
            ! the export state might only contain control data and no grid information if
            ! if the target component (n2) is not prognostic only receives control data back
            ! But if STgeom=is_local%wrap%NStateImp(n2) is substituted for STgeom=is_local%wrap%NStateExp(n2) - then an error
            ! occurs as follows
            ! PET00 (med_fraction_init): called
            ! PET00 (shr_nuopc_methods_FB_FieldRegrid) field not found: afrac,afrac
            ! PET00 (med_fraction_set): called
            ! PET00 (shr_nuopc_methods_FB_FieldRegrid) field not found: ifrac,ifrac

            call shr_nuopc_methods_FB_init(          &
                 is_local%wrap%FBImp(n1,n2),         &
                 STgeom=is_local%wrap%NStateExp(n2), &
                 STflds=is_local%wrap%NStateImp(n1), &
                 name='FBImp'//trim(compname(n1))//'_'//trim(compname(n2)), rc=rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
        enddo
      enddo
      if (mastertask) call shr_sys_flush(llogunit)

      !----------------------------------------------------------
      ! Initialize Mediator field bundles
      !----------------------------------------------------------

      ! FBs for ocean albedo and ocn/atm flux calculations

      if (is_local%wrap%comp_present(compatm) .and. is_local%wrap%comp_present(compocn)) then
         ! NOTE: the NStateImp(compocn) or NStateImp(compatm) used here
         ! rather than NStateExp(n2), since the export state might only
         ! contain control data and no grid information if if the target
         ! component (n2) is not prognostic only receives control data back

         call shr_nuopc_methods_FB_init(is_local%wrap%FBXao_ocnalb_a, &
              STgeom=is_local%wrap%NStateImp(compatm), fieldnamelist=flds_xao_ocnalb, name='FBMed_ocnalb_a', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         call shr_nuopc_methods_FB_init(is_local%wrap%FBXao_ocnalb_o, &
              STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=flds_xao_ocnalb, name='FBMed_ocnalb_o', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         call shr_nuopc_methods_FB_init(is_local%wrap%FBXao_fluxes_a, &
              STgeom=is_local%wrap%NStateImp(compatm), fieldnamelist=flds_xao_fluxes, name='FBMed_ocnatm_fluxes_a', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         call shr_nuopc_methods_FB_init(is_local%wrap%FBXao_fluxes_o, &
              STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=flds_xao_fluxes, name='FBMed_ocnatm_fluxes_o', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_accum_ocnatm_fluxes_o, &
              STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=flds_xao_fluxes, name='FBMed_ocnatm_fluxes_o_accum', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      ! FBs for lnd <-> glc accumulation and elevation class downscaling

      if (is_local%wrap%comp_present(complnd) .and. is_local%wrap%comp_present(compglc)) then
         call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_l2g_l_accum, &
              STgeom=is_local%wrap%NStateImp(complnd), fieldnamelist=flds_l2x_to_glc, name='FBMed_l2g_l_accum', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_g2x_to_lnd, &
              STgeom=is_local%wrap%NStateImp(complnd), fieldnamelist=flds_g2x_to_lnd, name='FBMed_g2x_to_lnd', rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      !----------------------------------------------------------
      !--- Initialize route handles
      !----------------------------------------------------------

      if (dbug_flag > 1) then
        call ESMF_LogWrite("Starting to initialize RHs", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush()
      endif

      if (mastertask) write(llogunit,*) ' '
      do n1 = 1, ncomps
         do n2 = 1, ncomps

            dstMaskValue = ispval_mask
            srcMaskValue = ispval_mask
            if (n1 == compocn .or. n1 == compice) srcMaskValue = 0
            if (n2 == compocn .or. n2 == compice) dstMaskValue = 0

            !--- get single fields from bundles
            !--- 1) ASSUMES all fields in the bundle are on identical grids
            !--- 2) MULTIPLE route handles are going to be generated for
            !---    given field bundle source and destination grids

            call shr_nuopc_methods_FB_getFieldN(FBsrc, 1, fldsrc, rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

            call shr_nuopc_methods_FB_getFieldN(FBdst, 1, flddst, rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

            if (n1 /= n2) then
               if (is_local%wrap%med_coupling_active(n1,n2)) then ! If coupling is active between n1 and n2

                  ! Determine route handle names
                  rhname = trim(compname(n1))//"2"//trim(compname(n2))
                  if (mastertask) write(llogunit,*) subname,' initialize RH for '//trim(rhname)

                  ! Loop over fields in
                  do n = 1,size(FldsFr(n1)%flds)
                     mapindex = trim(FldsFr(n1)%flds(n)%mapindex(n2))

                     ! Create route handle for target mapindex if route handle is required
                     ! (i.e. mapindex /= mapunset) and route handle has not already been created
                     if (mapindex /= mapunset .and. &
                          .not. ESMF_RouteHandleIsCreated(is_local%wrap%RH(n1,n2,mapindex), rc=rc)) then
                        mapname  = trim(mapnames(mapindex))
                        mapfile  = trim(FldsFr(n1)%flds(n)%mapfile(n2))
                        string   = trim(rhname)//'_weights'

                        if (mapindex == mapfcopy) then
                           if (lmastertask) write(llogunit,'(3A)') subname,trim(string),' RH redist '
                           call ESMF_LogWrite(trim(subname) // trim(string) // ' RH redist ', ESMF_LOGMSG_INFO, rc=dbrc)
                           call ESMF_FieldRedistStore(fldsrc, flddst, &
                                routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                ignoreUnmatchedIndices = .true., rc=rc)
                           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                        else if (mapfile /= 'unset') then
                           if (mastertask) then
                              write(llogunit,'(4A)') subname,trim(string),&
                                   ' RH '//trim(mapname)//' via input file ',trim(mapfile)
                           end if
                           call ESMF_LogWrite(subname // trim(string) //&
                                ' RH '//trim(mapname)//' via input file ',trim(mapfile), ESMF_LOGMSG_INFO, rc=dbrc)
                           call ESMF_FieldSMMStore(fldsrc, flddst, &
                                trim(filename), &
                                routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                ignoreUnmatchedIndices=.true., &
                                srcTermProcessing=srcTermProcessing_Value, rc=rc)
                           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                        else
                           if (lmastertask) write(llogunit,'(3A)') subname,trim(string),' RH consf regrid computed on the fly'
                           call ESMF_LogWrite(subname // trim(string) //' RH consf regrid computed on the fly', ESMF_LOGMSG_INFO, rc=dbrc)
                           if (mapindex == mapbilnr) then
                              srcTermProcessing_Value = 0
                              call ESMF_FieldRegridStore(fldsrc, flddst, &
                                   routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                   srcMaskValues=(/lsrcMaskValue/), &
                                   dstMaskValues=(/ldstMaskValue/), &
                                   regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                                   polemethod=polemethod, &
                                   srcTermProcessing=srcTermProcessing_Value, &
                                   factorList=factorList, &
                                   ignoreDegenerate=.true., &
                                   unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                           else if (mapindex == mapconsf) then
                              call ESMF_FieldRegridStore(fldsrc, flddst, &
                                   routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                   srcMaskValues=(/lsrcMaskValue/), &
                                   dstMaskValues=(/ldstMaskValue/), &
                                   regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                                   normType=ESMF_NORMTYPE_FRACAREA, &
                                   srcTermProcessing=srcTermProcessing_Value, &
                                   factorList=factorList, &
                                   ignoreDegenerate=.true., &
                                   unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                           else if (mapindex == mapconsd) then
                              call ESMF_FieldRegridStore(fldsrc, flddst, &
                                   routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                   srcMaskValues=(/lsrcMaskValue/), &
                                   dstMaskValues=(/ldstMaskValue/), &
                                   regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                                   normType=ESMF_NORMTYPE_DSTAREA, &
                                   srcTermProcessing=srcTermProcessing_Value, &
                                   factorList=factorList, &
                                   ignoreDegenerate=.true., &
                                   unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                           else if (mapindex == mappatch) then
                              call ESMF_FieldRegridStore(fldsrc, flddst, &
                                   routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                   srcMaskValues=(/lsrcMaskValue/), &
                                   dstMaskValues=(/ldstMaskValue/), &
                                   regridmethod=ESMF_REGRIDMETHOD_PATCH, &
                                   polemethod=polemethod, &
                                   srcTermProcessing=srcTermProcessing_Value, &
                                   factorList=factorList, &
                                   ignoreDegenerate=.true., &
                                   unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                           end if
                           if (rhprint_flag) then
                              call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_consf.nc", rc)
                              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                              call ESMF_RouteHandlePrint(consfmap, rc=rc)
                              if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                           endif
                           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                        end if
                        ! Check that a valid route handle has been created
                        if (ESMF_RouteHandleIsCreated(, rc=rc)) then
                           call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH "//trim(mapname), &
                                ESMF_LOGMSG_INFO, rc=dbrc)
                        else
                           call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH "//trim(mapname), &
                                ESMF_LOGMSG_INFO, rc=dbrc)
                        endif
                     end if  !end of route handle needs to be created

               elseif (is_local%wrap%comp_present(n1) .and. is_local%wrap%comp_present(n2)) then

                  ! If coupling is not active between n1 and n2 - but the two components are present

                  call NUOPC_CompAttributeGet(gcomp, name=trim(rhname)//"_fmapname", value=fmapfile, rc=rc)
                  if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
                     call ESMF_LogWrite(trim(rhname)//"_fmapname = "//trim(fmapfile), ESMF_LOGMSG_INFO)

                     call shr_nuopc_methods_RH_Init(&
                          FBsrc=is_local%wrap%FBfrac(n1), &
                          FBdst=is_local%wrap%FBfrac(n2), &
                          consfmap=is_local%wrap%RH(n1,n2,mapconsf), &
                          srcMaskValue=srcMaskValue, &
                          dstMaskValue=dstMaskValue, &
                          string=trim(rhname)//'_weights_for_fraction', &
                          consffn=trim(fmapfile), &
                          spvalfn=med_constants_spval_rhfile, &
                          mastertask=mastertask, &
                          rc=rc)
                     if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                  endif

               endif
            endif
         enddo
      enddo
      if (mastertask) call shr_sys_flush(llogunit)

      !---------------------------------------
      ! Initialize infodata, Accums (to zero), and FBImp (from NStateImp)
      !---------------------------------------

      do n1 = 1,ncomps
        is_local%wrap%FBImpAccumCnt(n1) = 0
        is_local%wrap%FBExpAccumCnt(n1) = 0
        if (is_local%wrap%comp_present(n1) .and. ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc)) then
           call med_infodata_CopyStateToInfodata(is_local%wrap%NStateImp(n1), infodata, trim(compname(n1))//'2cpli', &
                is_local%wrap%mpicom, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImpAccum(n1), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_reset(is_local%wrap%FBExpAccum(n1), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_copy(is_local%wrap%FBImp(n1,n1), is_local%wrap%NStateImp(n1), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        endif
      enddo

      !---------------------------------------
      ! Initialize fractions
      !---------------------------------------

      call med_fraction_init(gcomp,rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !---------------------------------------
      ! Initialize field bundles for normalization
      !---------------------------------------
      ! Also do the mapping for unity normalization up front

      fldname = 'one'
      do n1 = 1,ncomps
         do n2 = 1,ncomps
            if (n1 /= n2 .and. is_local%wrap%med_coupling_active(n1,n2)) then
               do m = 1,nmappers
                  if (ESMF_RouteHandleIsCreated(is_local%wrap%RH(n1,n2,m), rc=rc)) then
                     call shr_nuopc_methods_FB_init(FBout=is_local%wrap%FBNormOne(n1,n2,m), &
                          FBgeom=is_local%wrap%FBImp(n1,n2), fieldNameList=/trim(fldname)/, rc=rc)
                     if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

                     call shr_nuopc_methods_FB_reset(is_local%wrap%FBNormOne(n1,n2,m), value=czero, rc=rc)
                     if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

                     call shr_nuopc_methods_FB_init( FBout=is_local%wrap%FBTmp, &
                          STgeom=is_local%wrap%NStateImp(n1), fieldNameList=/trim(fldname)/, rc=rc)
                     if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

                     call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBTmp, trim(fldname), fldptr1=dataPtr1, rc=rc)
                     if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                     dataptr1(:) = 1.0_ESMF_KIND_R8

                     call shr_nuopc_methods_FB_FieldRegrid(&
                          is_local%warp%FBTmp             , fldname, &
                          is_local%wrap%FBNormOne(n1,n2,m), fldname, &
                          RH(n1,n2,m), rc)
                     if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

                     call shr_nuopc_methods_FB_clean(FBTmp, rc=rc)
                     if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
                  end if
               end do
            end if
         end do
      end do

#if (1 == 0)
      !---------------------------------------
      ! read mediator restarts
      !---------------------------------------

      !---tcraig, turn if on to force no mediator restarts for testing
      !if (.not.coldstart) then
        call Mediator_restart(gcomp,'read','mediator',rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      !endif

      ! default initialize s_surf to work around limitations of current initialization sequence
      call ESMF_StateGet(is_local%wrap%NStateExp(compice), itemName='s_surf', itemType=itemType, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        if (NUOPC_IsConnected(is_local%wrap%NStateExp(compice),'s_surf',rc=rc)) then
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call State_SetFldPtr(is_local%wrap%NStateExp(compice), 's_surf', 34.0_ESMF_KIND_R8, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        endif

      endif
#endif

      !---------------------------------------
      ! Carry out atmocn init if appropriate
      !---------------------------------------

      if (is_local%wrap%med_coupling_active(compocn,compatm) .and. &
          is_local%wrap%med_coupling_active(compatm,compocn)) then

         ! Initialize the atm/ocean fluxes and compute the ocean albedos
         call ESMF_LogWrite("MED - initialize atm/ocn fluxes and compute ocean albedo", ESMF_LOGMSG_INFO, rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         ! Initialize atm/ocn fluxes and ocean albedo
         call med_phases_atmocn_init(gcomp, rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

         ! Compute ocean albedoes
         ! This will update the relevant module arrays in med_atmocn_mod.F90
         ! since they are simply pointers into the FBAtmOcn, FBAtm and FBOcn field bundles
         call med_phases_atmocn_ocnalb(gcomp, rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

      !---------------------------------------
      ! If atmosphere is not present, than data initialization is complete
      !---------------------------------------

      if (.not. is_local%wrap%comp_present(compatm)) then

         ! here if atm component is not present - so initialization is complete
         ! -> set InitializeDataComplete Component Attribute to "true", indicating
         ! to the driver that this Component has fully initialized its data
         call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      end if

    endif  ! end first_call if-block
    first_call = .false.

    !---------------------------------------
    ! End of first_call block - reset first_call to .false.
    !---------------------------------------

    !---------------------------------------
    ! Carry out data dependency for atm initialization if needed
    !---------------------------------------

    if (is_local%wrap%comp_present(compatm)) then

       allDone = .true.  ! reset if an item is found that is not done

       call ESMF_StateGet(is_local%wrap%NStateImp(compatm), itemCount=fieldCount, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       allocate(fieldNameList(fieldCount))
       call ESMF_StateGet(is_local%wrap%NStateImp(compatm), itemNameList=fieldNameList, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       do n=1, fieldCount
          call ESMF_StateGet(is_local%wrap%NStateImp(compatm), itemName=fieldNameList(n), field=field, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          atCorrectTime = NUOPC_IsAtTime(field, time, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (.not. atCorrectTime) then
             ! If any atm import fields are not time stamped correctly, then dependency is not satisified - must return to atm
             call ESMF_LogWrite("MED - Initialize-Data-Dependency from ATM NOT YET SATISFIED!!!", ESMF_LOGMSG_INFO, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
             allDone = .false.
             exit  ! break out of the loop when first not satisfied found
          endif
       enddo
       deallocate(fieldNameList)

       if (.not. allDone) then  ! allDone is not true

          ! initialize fractions
          call med_fraction_set(gcomp, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          ! do the merge to the atmospheric component
          call med_phases_prep_atm(gcomp, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          ! copy the FBExp(compatm) to NstatExp(compatm)
          call med_connectors_prep_med2atm(gcomp, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          ! change 'Updated' attribute to true for ALL exportState fields
          call ESMF_StateGet(is_local%wrap%NStateExp(compatm), itemCount=fieldCount, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          allocate(fieldNameList(fieldCount))
          call ESMF_StateGet(is_local%wrap%NStateExp(compatm), itemNameList=fieldNameList, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          do n=1, fieldCount
             call ESMF_StateGet(is_local%wrap%NStateExp(compatm), itemName=fieldNameList(n), field=field, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
             call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          end do
          deallocate(fieldNameList)

          ! Connectors will be automatically called between the mediator and atm until allDone is true
          call ESMF_LogWrite("MED - Initialize-Data-Dependency Sending Data to ATM", ESMF_LOGMSG_INFO, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       else ! allDone is true

          ! Copy the NstateImp(compatm) to FBImp(compatm)
          call med_connectors_post_atm2med(gcomp, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          ! set InitializeDataComplete Component Attribute to "true", indicating
          ! to the driver that this Component has fully initialized its data
          call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call ESMF_LogWrite("MED - Initialize-Data-Dependency from ATM is SATISFIED!!!", ESMF_LOGMSG_INFO, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       end if

    end if

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)           :: mediatorClock, driverClock
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeStep
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock)'

    rc = ESMF_SUCCESS

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(gcomp, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
       call shr_nuopc_methods_Clock_TimePrint(driverClock  ,trim(subname)//'driver clock1',rc)
       call shr_nuopc_methods_Clock_TimePrint(mediatorClock,trim(subname)//'mediat clock1',rc)
    endif

    ! set the mediatorClock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_ClockSet(mediatorClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
       call shr_nuopc_methods_Clock_TimePrint(driverClock  ,trim(subname)//'driver clock2',rc)
       call shr_nuopc_methods_Clock_TimePrint(mediatorClock,trim(subname)//'mediat clock2',rc)
    endif

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunClock

  !-----------------------------------------------------------------------------
#if (1 == 0)

  subroutine Mediator_restart(gcomp,mode,bfname,rc)
    !
    ! read/write mediator restart file
    !
    type(ESMF_GridComp)  :: gcomp
    character(len=*), intent(in)    :: mode
    character(len=*), intent(in)    :: bfname
    integer         , intent(inout) :: rc

    type(InternalState)  :: is_local
    character(len=1280)  :: fname
    integer              :: funit
    logical              :: fexists
    character(len=*),parameter :: subname='(module_MEDIATOR:Mediator_restart)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (mode /= 'write' .and. mode /= 'read') then
       call ESMF_LogWrite(trim(subname)//": ERROR mode not allowed "//trim(mode), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(compatm)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(compatm),read_rest_FBaccum(compatm),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(compocn)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(compocn),read_rest_FBaccum(compocn),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(compice)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(compice),read_rest_FBaccum(compice),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(complnd)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(complnd),read_rest_FBaccum(complnd),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(comprof)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(comprof),read_rest_FBaccum(comprof),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(compwav)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(compwav),read_rest_FBaccum(compwav),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccum(compglc)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccum(compglc),read_rest_FBaccum(compglc),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBaccumAtmOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtmOcn,read_rest_FBaccumAtmOcn,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    fname = trim(bfname)//'_FBAtm_a_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(compatm,compatm),read_rest_FBAtm_a,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(compatm), is_local%wrap%FBImp(compatm,compatm), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBImp(compice,compice)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(compice,compice),read_rest_FBImp(compice,compice),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(compice), is_local%wrap%FBImp(compice,compice), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBImp(compocn,compocn)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(compocn,compocn),read_rest_FBImp(compocn,compocn),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(compocn), is_local%wrap%FBImp(compocn,compocn), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBImp(complnd,complnd)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(complnd,complnd),read_rest_FBImp(complnd,complnd),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(complnd), is_local%wrap%FBImp(complnd,complnd), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBImp(comprof,comprof)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(comprof,comprof),read_rest_FBImp(comprof,comprof),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(comprof), is_local%wrap%FBImp(comprof,comprof), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBImp(compwav,comprof)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(compwav,comprof),read_rest_FBImp(compwav,comprof),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(compwav), is_local%wrap%FBImp(compwav,comprof), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBImp(compglc,comprof)_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBImp(compglc,comprof),read_rest_FBImp(compglc,comprof),rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NStateImp(compglc), is_local%wrap%FBImp(compglc,comprof), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    fname = trim(bfname)//'_FBAtmOcn_o_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBAtmOcn_o,read_rest_FBAtmOcn_o,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    funit = 1101
    fname = trim(bfname)//'_scalars_restart.txt'
    if (mode == 'write') then
      call ESMF_LogWrite(trim(subname)//": write "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
      open(funit,file=fname,form='formatted')
      write(funit,*) is_local%wrap%FBaccumcnt(compatm)
      write(funit,*) is_local%wrap%FBaccumcnt(compocn)
      write(funit,*) is_local%wrap%FBaccumcnt(compice)
      write(funit,*) is_local%wrap%FBaccumcntAtmOcn
      write(funit,*) is_local%wrap%FBaccumcnt(complnd)
      write(funit,*) is_local%wrap%FBaccumcnt(comprof)
      write(funit,*) is_local%wrap%FBaccumcnt(compwav)
      write(funit,*) is_local%wrap%FBaccumcnt(compglc)
      close(funit)
    elseif (mode == 'read') then
      inquire(file=fname,exist=fexists)
      if (fexists) then
        call ESMF_LogWrite(trim(subname)//": read "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
        open(funit,file=fname,form='formatted')
        ! DCR - temporary skip reading Lnd and Rof until components are added to test case
        !       restart files
        is_local%wrap%FBaccumcnt(compatm)=0
        is_local%wrap%FBaccumcnt(compocn)=0
        is_local%wrap%FBaccumcnt(compice)=0
        is_local%wrap%FBaccumcntAtmOcn=0
        is_local%wrap%FBaccumcnt(complnd)=0
        is_local%wrap%FBaccumcnt(comprof)=0
        is_local%wrap%FBaccumcnt(compwav)=0
        is_local%wrap%FBaccumcnt(compglc)=0
        read (funit,*) is_local%wrap%FBaccumcnt(compatm)
        read (funit,*) is_local%wrap%FBaccumcnt(compocn)
        read (funit,*) is_local%wrap%FBaccumcnt(compice)
        read (funit,*) is_local%wrap%FBaccumcntAtmOcn
        read (funit,*) is_local%wrap%FBaccumcnt(complnd)
        read (funit,*) is_local%wrap%FBaccumcnt(comprof)
        read (funit,*) is_local%wrap%FBaccumcnt(compwav)
        read (funit,*) is_local%wrap%FBaccumcnt(compglc)
        close(funit)
      else
        read_rest_FBaccum(compatm) = .false.
        read_rest_FBaccum(compocn) = .false.
        read_rest_FBaccum(compice) = .false.
        read_rest_FBaccum(complnd) = .false.
        read_rest_FBaccum(comprof) = .false.
        read_rest_FBaccum(compwav) = .false.
        read_rest_FBaccum(compglc) = .false.
        read_rest_FBaccumAtmOcn    = .false.
      endif
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Mediator_restart
#endif

  !-----------------------------------------------------------------------------

end module MED
