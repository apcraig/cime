module MED

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to 
  ! do so.
  !-----------------------------------------------------------------------------

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
  use shr_nuopc_fldList_mod
  use med_method_mod
  use seq_flds_mod
 
  implicit none
  
  private
  
  ! private internal state to keep instance data
  type InternalStateStruct
    integer               :: slowcntr
    integer               :: fastcntr
    integer               :: accumcntAtm ! accumulator counter
    integer               :: accumcntOcn ! accumulator counter
    integer               :: accumcntIce ! accumulator counter
    integer               :: accumcntLnd ! accumulator counter
    integer               :: accumcntRof ! accumulator counter
    integer               :: accumcntAtmOcn ! accumulator counter
    type(ESMF_FieldBundle):: FBaccumAtm  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBaccumOcn  ! accumulator of ocn export data
    type(ESMF_FieldBundle):: FBaccumIce  ! accumulator of ice export data
    type(ESMF_FieldBundle):: FBaccumLnd  ! accumulator of lnd export data
    type(ESMF_FieldBundle):: FBaccumRof  ! accumulator of lnd export data
    type(ESMF_FieldBundle):: FBaccumAtmOcn  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBAtm_a     ! Atm export data on atm grid
    type(ESMF_FieldBundle):: FBAtm_o     ! Atm export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBAtm_i     ! Atm export data mapped to ice grid
    type(ESMF_FieldBundle):: FBAtm_l     ! Atm export data mapped to lnd grid
    type(ESMF_FieldBundle):: FBAtm_h     ! Atm export data mapped to rof grid
    type(ESMF_FieldBundle):: FBOcn_a     ! Ocn export data mapped to atm grid
    type(ESMF_FieldBundle):: FBOcn_o     ! Ocn export data on ocn grid
    type(ESMF_FieldBundle):: FBOcn_i     ! Ocn export data mapped to ice grid
    type(ESMF_FieldBundle):: FBIce_a     ! Ice export data mapped to atm grid
    type(ESMF_FieldBundle):: FBIce_o     ! Ice export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBIce_i     ! Ice export data on ice grid
    type(ESMF_FieldBundle):: FBIce_if    ! Ice export data on ice grid multiplied by ice fraction
    type(ESMF_FieldBundle):: FBLnd_a     ! Lnd export data mapped to atm grid
    type(ESMF_FieldBundle):: FBLnd_l     ! Lnd export on lnd grid
    type(ESMF_FieldBundle):: FBLnd_h     ! Lnd export data mapped to rof grid
    type(ESMF_FieldBundle):: FBRof_l     ! Rof export data mapped to lnd grid
    type(ESMF_FieldBundle):: FBRof_a     ! Rof export data mapped to atm grid
    type(ESMF_FieldBundle):: FBRof_h     ! Rof export on rof grid
    type(ESMF_FieldBundle):: FBAtmOcn_o  ! Atm/Ocn flux fields on ocn grid
    type(ESMF_FieldBundle):: FBAtmOcn_a  ! Atm/Ocn flux fields on atm grid
    type(ESMF_FieldBundle):: FBforAtm    ! data storage for atm import
    type(ESMF_FieldBundle):: FBforOcn    ! data storage for ocn import
    type(ESMF_FieldBundle):: FBforIce    ! data storage for ice import
    type(ESMF_FieldBundle):: FBforLnd    ! data storage for lnd import
    type(ESMF_FieldBundle):: FBforRof    ! data storage for rof import
    type(ESMF_RouteHandle):: RH_a2o_bilnr  ! atm to ocn bilinear
    type(ESMF_RouteHandle):: RH_o2a_bilnr  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_bilnr  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_bilnr  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_bilnr  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_bilnr  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_bilnr  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_bilnr  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_bilnr  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_bilnr  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_bilnr  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_bilnr  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_consf  ! atm to ocn conservative fracarea
    type(ESMF_RouteHandle):: RH_o2a_consf  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consf  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consf  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_consf  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_consf  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_consf  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_consf  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_consf  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consf  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_consf  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_consf  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_consd  ! atm to ocn conservative dstarea
    type(ESMF_RouteHandle):: RH_o2a_consd  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consd  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consd  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_consd  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_consd  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_consd  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_consd  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_consd  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consd  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_consd  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_consd  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_patch  ! atm to ocn patch
    type(ESMF_RouteHandle):: RH_o2a_patch  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_patch  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_patch  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_patch  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_patch  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_patch  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_patch  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_patch  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_patch  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_patch  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_patch  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_fcopy  ! atm to ocn fcopy
    type(ESMF_RouteHandle):: RH_o2a_fcopy  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_fcopy  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_fcopy  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_fcopy  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_fcopy  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_fcopy  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_fcopy  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_fcopy  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_fcopy  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_fcopy  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_fcopy  ! rof to lnd
    logical               :: a2o_active
    logical               :: o2a_active
    logical               :: a2i_active
    logical               :: i2a_active
    logical               :: a2l_active
    logical               :: l2a_active
    logical               :: a2h_active
    logical               :: h2a_active
    logical               :: o2i_active
    logical               :: i2o_active
    logical               :: l2h_active
    logical               :: h2l_active
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  ! local variables
  type(ESMF_State)     :: NState_AtmImp   ! Atm Import nested state
  type(ESMF_State)     :: NState_AtmExp   ! Atm Export nested state
  type(ESMF_State)     :: NState_OcnImp   ! Ocn Import nested state
  type(ESMF_State)     :: NState_OcnExp   ! Ocn Export nested state
  type(ESMF_State)     :: NState_IceImp   ! Ice Import nested state
  type(ESMF_State)     :: NState_IceExp   ! Ice Export nested state
  type(ESMF_State)     :: NState_LndImp   ! Lnd Import nested state
  type(ESMF_State)     :: NState_LndExp   ! Lnd Export nested state
  type(ESMF_State)     :: NState_RofImp   ! Rof Import nested state
  type(ESMF_State)     :: NState_RofExp   ! Rof Export nested state
  type (shr_nuopc_fldList_Type) :: fldsToAtm
  type (shr_nuopc_fldList_Type) :: fldsFrAtm
  type (shr_nuopc_fldList_Type) :: fldsToOcn
  type (shr_nuopc_fldList_Type) :: fldsFrOcn
  type (shr_nuopc_fldList_Type) :: fldsToIce
  type (shr_nuopc_fldList_Type) :: fldsFrIce
  type (shr_nuopc_fldList_Type) :: fldsToLnd
  type (shr_nuopc_fldList_Type) :: fldsFrLnd
  type (shr_nuopc_fldList_Type) :: fldsToRof
  type (shr_nuopc_fldList_Type) :: fldsFrRof
  type (shr_nuopc_fldList_Type) :: fldsAtmOcn

  type(ESMF_Grid)    :: gridAtm, gridOcn, gridIce, gridLnd, gridRof, gridMed
  integer            :: dbug_flag = 5
  logical            :: statewrite_flag = .true.
  integer            :: dbrc
  logical            :: isPresent
  character(len=1024):: msgString
  character(len=*)  , parameter :: grid_arbopt = "grid_reg"   ! grid_reg or grid_arb
  real(ESMF_KIND_R8), parameter :: spval_init = 0.0_ESMF_KIND_R8  ! spval for initialization
  real(ESMF_KIND_R8), parameter :: spval = 0.0_ESMF_KIND_R8  ! spval
  real(ESMF_KIND_R8), parameter :: czero = 0.0_ESMF_KIND_R8  ! spval
  integer           , parameter :: ispval_mask = -987987     ! spval for RH mask values

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, mediator_routine_SS, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! set entry point for methods that require specific implementation
#if (1 == 0)
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
#else
    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      InitializeP0, phase=0, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeIPDv03p1, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeIPDv03p3, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeIPDv03p4, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeIPDv03p5, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

#endif

#if (1 == 0)    
    ! slow Mediation phase with OCN (use the default "RunPhase1" for slow)
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="RunPhase1", specRoutine=SetRunClock_slow, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="RunPhase1", specRoutine=MediatorAdvance_slow, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
#endif
    
#if (1 == 0)    
    ! fast Mediation phase with ATM: before
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhaseFastBefore"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=SetRunClock_fast_before, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=CheckImport_fast_before, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=TimestampExport_fast_before, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=MediatorAdvance_fast_before, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
#endif
    
#if (1 == 0)    
    ! fast Mediation phase with ATM: after
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhaseFastAfter"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=SetRunClock_fast_after, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=CheckImport_fast_after, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=TimestampExport_fast_after, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=MediatorAdvance_fast_after, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
#endif

    ! set entry point for Run( phase = accum_fast ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_accum_fast"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_accum_fast", specRoutine=MedPhase_accum_fast, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! prep_atm
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_atm"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_atm", specRoutine=MedPhase_prep_atm, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! prep_ocn
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_ocn"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_ocn", specRoutine=MedPhase_prep_ocn, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! attach specializing method(s)
    ! -> NUOPC specializes by default --->>> first need to remove the default
    call ESMF_MethodRemove(gcomp, mediator_label_CheckImport, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specRoutine=NUOPC_NoOp, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! attach specializing method(s)
    ! -> NUOPC specializes by default --->>> first need to remove the default
    call ESMF_MethodRemove(gcomp, mediator_label_SetRunClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specRoutine=SetRunClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

  end subroutine SetServices
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

  end subroutine InitializeP1
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    type(InternalState)     :: is
    integer                 :: stat
    type(ESMF_TimeInterval) :: timeStep

    rc = ESMF_SUCCESS
    
    ! create a Grid object for Fields
   gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/20, 100/), &
      minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
      maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! importable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! importable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! exportable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! The Fields in the exportState are going to be correctly timestamped by
    ! the generic code. However, the Fields in the importState are not 
    ! automatically time stamped. Instead it is expected that the customized
    ! initialization takes care of this aspect. Here just "blindly" time stamp
    ! the Fields in the importState, indicating to the first Run method that
    ! all is good.
    
    call NUOPC_UpdateTimestamp(importState, clock=clock, rc=rc) 
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! Allocate memory for the internal state and set it in the Component.
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(gcomp, is, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
#if (1 == 0)
    ! Initialize the internal clocks: both slow and fast start as copies of in
    is%wrap%clockSlow = ESMF_ClockCreate(clock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    is%wrap%clockFast = ESMF_ClockCreate(clock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    ! set the slow clock to 60min steps
    call ESMF_TimeIntervalSet(timeStep, m=60, rc=rc) ! 60 min steps
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_ClockSet(is%wrap%clockSlow, timestep=timeStep, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    ! set the fast clock to 30min steps
    call ESMF_TimeIntervalSet(timeStep, m=30, rc=rc) ! 30 min steps
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_ClockSet(is%wrap%clockFast, timestep=timeStep, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
#endif

  end subroutine InitializeP2

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(6)
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP0)'
    character(len=10)                         :: value

    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, defaultValue="max", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    dbug_flag = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","high"/), specialValueList=(/0,255,255/), rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    write(msgString,'(A,i6)') trim(subname)//' dbug_flag = ',dbug_flag
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeP0

  !-----------------------------------------------------------------------

  subroutine InitializeIPDv03p1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    integer :: n
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p1)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! importable fields:

    ! add a namespace
    call NUOPC_AddNamespace(importState, namespace="ATM", nestedStateName="NestedState-AtmImp", nestedState=NState_AtmImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="OCN", nestedStateName="NestedState-OcnImp", nestedState=NState_OcnImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ICE", nestedStateName="NestedState-IceImp", nestedState=NState_IceImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="LND", nestedStateName="NestedState-LndImp", nestedState=NState_LndImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ROF", nestedStateName="NestedState-RofImp", nestedState=NState_RofImp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ATM", nestedStateName="NestedState-AtmExp", nestedState=NState_AtmExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="OCN", nestedStateName="NestedState-OcnExp", nestedState=NState_OcnExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ICE", nestedStateName="NestedState-IceExp", nestedState=NState_IceExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="LND", nestedStateName="NestedState-LndExp", nestedState=NState_LndExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ROF", nestedStateName="NestedState-RofExp", nestedState=NState_RofExp, rc=rc)

    call shr_nuopc_fldList_Zero(fldsFrAtm, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToAtm, rc=rc)
    call shr_nuopc_fldList_Zero(fldsFrOcn, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToOcn, rc=rc)
    call shr_nuopc_fldList_Zero(fldsFrIce, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToIce, rc=rc)
    call shr_nuopc_fldList_Zero(fldsFrLnd, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToLnd, rc=rc)
    call shr_nuopc_fldList_Zero(fldsFrRof, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToRof, rc=rc)
    call shr_nuopc_fldList_Zero(fldsAtmOcn, rc=rc)

!    call shr_nuopc_fldList_Add(fldsToAtm, "sea_surface_temperature"  ,"cannot provide",subname//':fldsToAtm',mapping='bilinear',rc=rc)

!    call shr_nuopc_fldList_Add(fldsFrAtm, "air_pressure_at_sea_level","cannot provide",subname//':fldsFrAtm',mapping='bilinear',rc=rc)
!    call shr_nuopc_fldList_Add(fldsFrAtm, "surface_net_downward_shortwave_flux","cannot provide",subname//':fldsFrAtm',mapping='bilinear',rc=rc)

    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_states, "cannot provide", subname//":seq_flds_x2a_states", "bilinear", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_fluxes, "cannot provide", subname//":seq_flds_x2a_fluxes", "conservefrac", rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_fluxes, "cannot provide", subname//":seq_flds_x2a_fluxes", "bilinear", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_states, "cannot provide", subname//":seq_flds_a2x_states", "bilinear", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_fluxes, "cannot provide", subname//":seq_flds_a2x_fluxes", "conservefrac", rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_fluxes, "cannot provide", subname//":seq_flds_a2x_fluxes", "bilinear", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!    call shr_nuopc_fldList_Add(fldsFrOcn, "sea_surface_temperature"  ,"will provide",subname//':fldsFrOcn',mapping='bilinear',rc=rc)

!    call shr_nuopc_fldList_Add(fldsToOcn, "air_pressure_at_sea_level","will provide",subname//':fldsToOcn',mapping='bilinear',rc=rc)
!    call shr_nuopc_fldList_Add(fldsToOcn, "surface_net_downward_shortwave_flux","will provide",subname//':fldsToOcn',mapping='bilinear',rc=rc)

    call shr_nuopc_fldList_Add(fldsFrOcn, "sea_surface_temperature"  ,"cannot provide",subname//':fldsFrOcn',mapping='bilinear',rc=rc)

    call shr_nuopc_fldList_Add(fldsToOcn, "air_pressure_at_sea_level","cannot provide",subname//':fldsToOcn',mapping='bilinear',rc=rc)
    call shr_nuopc_fldList_Add(fldsToOcn, "surface_net_downward_shortwave_flux","cannot provide",subname//':fldsToOcn',mapping='bilinear',rc=rc)

    call shr_nuopc_fldList_Advertise(NState_AtmImp, fldsFrAtm, subname//':FrAtm', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_LndImp, fldsFrLnd, subname//':FrLnd', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_RofImp, fldsFrRof, subname//':FrRof', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_OcnImp, fldsFrOcn, subname//':FrOcn', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_IceImp, fldsFrIce, subname//':FrIce', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call shr_nuopc_fldList_Advertise(NState_AtmExp, fldsToAtm, subname//':ToAtm', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_LndExp, fldsToLnd, subname//':ToLnd', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_RofExp, fldsToRof, subname//':ToRof', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_OcnExp, fldsToOcn, subname//':ToOcn', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Advertise(NState_IceExp, fldsToIce, subname//':ToIce', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

#if (1 == 0)
    do n = 1,fldsFrAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrAtm Advertise "// &
          trim(fldsFrAtm%stdname(n))//":"// &
          trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_AtmImp, &
        StandardName = trim(fldsFrAtm%stdname(n)), &
        name=fldsFrAtm%shortname(n), &
        TransferOfferGeomObject=fldsFrAtm%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsFrOcn%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrOcn Advertise "// &
          trim(fldsFrOcn%stdname(n))//":"// &
          trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_OcnImp, &
        StandardName = fldsFrOcn%stdname(n), &
        name = fldsFrOcn%shortname(n), &
        TransferOfferGeomObject=fldsFrOcn%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsFrIce%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrIce Advertise "// &
          trim(fldsFrIce%stdname(n))//":"// &
          trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_IceImp, &
        StandardName = fldsFrIce%stdname(n), &
        name = fldsFrIce%shortname(n), &
        TransferOfferGeomObject=fldsFrIce%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsFrLnd%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrLnd Advertise "// &
          trim(fldsFrLnd%stdname(n))//":"// &
          trim(fldsFrLnd%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_LndImp, &
        StandardName = fldsFrLnd%stdname(n), &
        name = fldsFrLnd%shortname(n), &
        TransferOfferGeomObject=fldsFrLnd%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsFrRof%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrRof Advertise "// &
          trim(fldsFrRof%stdname(n))//":"// &
          trim(fldsFrRof%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_RofImp, &
        StandardName = fldsFrRof%stdname(n), &
        name = fldsFrRof%shortname(n), &
        TransferOfferGeomObject=fldsFrRof%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo
      
    ! exportable fields:

    do n = 1,fldsToAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToAtm Advertise "// &
          trim(fldsToAtm%stdname(n))//":"// &
          trim(fldsToAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_AtmExp, &
        StandardName = fldsToAtm%stdname(n), &
        name = fldsToAtm%shortname(n), &
        TransferOfferGeomObject=fldsToAtm%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsToOcn%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToOcn Advertise "// &
          trim(fldsToOcn%stdname(n))//":"// &
          trim(fldsToOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_OcnExp, &
        StandardName = fldsToOcn%stdname(n), &
        name = fldsToOcn%shortname(n), &
        TransferOfferGeomObject=fldsToOcn%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsToIce%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToIce Advertise "// &
          trim(fldsToIce%stdname(n))//":"// &
          trim(fldsToIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_IceExp, &
        StandardName = fldsToIce%stdname(n), &
        name = fldsToIce%shortname(n), &
        TransferOfferGeomObject=fldsToIce%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsToLnd%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToLnd Advertise "// &
          trim(fldsToLnd%stdname(n))//":"// &
          trim(fldsToLnd%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_LndExp, &
        StandardName = fldsToLnd%stdname(n), &
        name = fldsToLnd%shortname(n), &
        TransferOfferGeomObject=fldsToLnd%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    do n = 1,fldsToRof%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToRof Advertise "// &
          trim(fldsToRof%stdname(n))//":"// &
          trim(fldsToRof%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_RofExp, &
        StandardName = fldsToRof%stdname(n), &
        name = fldsToRof%shortname(n), &
        TransferOfferGeomObject=fldsToRof%transferOffer(n), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo
#endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeIPDv03p1
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer                     :: i, j
    real(kind=ESMF_KIND_R8),pointer :: lonPtr(:,:), latPtr(:,:)
    type(InternalState)         :: is_local
    integer                     :: stat
    real(ESMF_KIND_R8)          :: intervalSec
    type(ESMF_TimeInterval)     :: timeStep
    character(ESMF_MAXSTR)      :: transferAction
! tcx XGrid
!    type(ESMF_Field)            :: fieldX, fieldA, fieldO
!    type(ESMF_XGrid)            :: xgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p3)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! Allocate memory for the internal state and set it in the Component.
    allocate(is_local%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    ! Initialize the internal state members
    is_local%wrap%fastcntr = 1
    is_local%wrap%slowcntr = 1

! tcraig hardwire 1 degree grid as backup option
    gridMed = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360,180/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -90._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!    gridLnd = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!    gridRof = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Generate RouteHandles
! tcx Xgrid
! what needs to be in the grids to create an XGrid (corners?)
! add error checking code

!    xgrid = ESMF_XGridCreate(sideAGrid=(/gridatm/), sideBGrid=(/gridocn/), rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
!    fieldX = ESMF_FieldCreate(xgrid  , typekind=ESMF_TYPEKIND_R8, rc=rc)
!    fieldA = ESMF_FieldCreate(gridAtm, typekind=ESMF_TYPEKIND_R8, rc=rc)
!    fieldO = ESMF_FieldCreate(gridAtm, typekind=ESMF_TYPEKIND_R8, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldA, fieldX, routehandle=is_local%wrap%RHa2x, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldO, fieldX, routehandle=is_local%wrap%RHo2x, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldX, fieldA, routehandle=is_local%wrap%RHx2a, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldX, fieldO, routehandle=is_local%wrap%RHx2o, rc=rc)
!    call ESMF_FieldDestroy(fieldX, rc=rc)
!    call ESMF_FieldDestroy(fieldA, rc=rc)
!    call ESMF_FieldDestroy(fieldO, rc=rc)
!    call ESMF_XGridDestroy(xgrid, rc=rc)

#if (1 == 0)
    !--- Importable fields from atm:

!gjt: import fields from ATM are now marked as "cannot provide" thus accept Grid
!gjt: -> eventually comment out the following lines...
    call realizeConnectedFields(NState_AtmImp, &
      fieldNameList=fldsFrAtm%shortname(1:fldsFrAtm%num), &
      string='AtmImp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Exportable fields to atm:

    call realizeConnectedFields(NState_AtmExp, &
      fieldNameList=fldsToAtm%shortname(1:fldsToAtm%num), &
      string='AtmExp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Importable fields from ocn:

    call realizeConnectedFields(NState_OcnImp, &
      fieldNameList=fldsFrOcn%shortname(1:fldsFrOcn%num), &
      string='OcnImp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Exportable fields to ocn:

    call realizeConnectedFields(NState_OcnExp, &
      fieldNameList=fldsToOcn%shortname(1:fldsToOcn%num), &
      string='OcnExp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Importable fields from ice:

    call realizeConnectedFields(NState_IceImp, &
      fieldNameList=fldsFrIce%shortname(1:fldsFrIce%num), &
      string='IceImp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Exportable fields to ice:

    call realizeConnectedFields(NState_IceExp, &
      fieldNameList=fldsToIce%shortname(1:fldsToIce%num), &
      string='IceExp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Importable fields from lnd:

    call realizeConnectedFields(NState_LndImp, &
      fieldNameList=fldsFrLnd%shortname(1:fldsFrLnd%num), &
      string='LndImp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Exportable fields to lnd:

    call realizeConnectedFields(NState_LndExp, &
      fieldNameList=fldsToLnd%shortname(1:fldsToLnd%num), &
      string='LndExp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Importable fields from rof:

    call realizeConnectedFields(NState_RofImp, &
      fieldNameList=fldsFrRof%shortname(1:fldsFrRof%num), &
      string='RofImp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- Exportable fields to rof:

    call realizeConnectedFields(NState_RofExp, &
      fieldNameList=fldsToRof%shortname(1:fldsToRof%num), &
      string='RofExp',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! Clean Up

!    call ESMF_GridDestroy(gridAtm, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
!    call ESMF_GridDestroy(gridOcn, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
#endif

    call shr_nuopc_fldList_Realize(NState_AtmImp, fldlist=fldsFrAtm, tag=subname//':FrAtm', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_LndImp, fldlist=fldsFrLnd, tag=subname//':FrLnd', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_RofImp, fldlist=fldsFrRof, tag=subname//':FrRof', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_OcnImp, fldlist=fldsFrOcn, tag=subname//':FrOcn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_IceImp, fldlist=fldsFrIce, tag=subname//':FrIce', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call shr_nuopc_fldList_Realize(NState_AtmExp, fldlist=fldsToAtm, tag=subname//':ToAtm', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_LndExp, fldlist=fldsToLnd, tag=subname//':ToLnd', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_RofExp, fldlist=fldsToRof, tag=subname//':ToRof', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_OcnExp, fldlist=fldsToOcn, tag=subname//':ToOcn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call shr_nuopc_fldList_Realize(NState_IceExp, fldlist=fldsToIce, tag=subname//':ToIce', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

#if (1 == 0)
  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
    subroutine realizeConnectedFields(state, fieldNameList, grid, string, rc)
      type(ESMF_State)                :: state
      character(len=*)                :: fieldNameList(:)
      type(ESMF_Grid),optional        :: grid
      character(len=*)                :: string
      integer, intent(out)            :: rc

      integer                         :: n
      type(ESMF_Field)                :: field
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedFields)'

      if (dbug_flag > 5) then
        write(msgString,*) size(fieldNameList)
        call ESMF_LogWrite(trim(subname)//trim(string)//": called "//trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      rc = ESMF_SUCCESS
      
      do n=1, size(fieldNameList)
        if (NUOPC_IsConnected(state, fieldName=fieldNameList(n))) then

          call ESMF_StateGet(state, field=field, itemName=fieldNameList(n), rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          if (trim(transferAction) == "accept") then
            call ESMF_LogWrite(trim(subname)//trim(string)//" field+grid connected "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

          else   ! provide

            ! realize the connected Field using the internal coupling Field
            if (.not.present(grid)) then
              call ESMF_LogWrite(trim(subname)//trim(string)//": ERROR grid expected "//trim(transferAction), ESMF_LOGMSG_INFO, rc=rc)
              rc = ESMF_FAILURE
              return
            endif
            field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n),rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
            call NUOPC_Realize(state, field=field, rc=rc)
            call ESMF_LogWrite(trim(subname)//trim(string)//" field connected      "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

          endif   ! transferAction

        else   ! StateIsFieldConnected

          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(n)/), rc=rc)
          call ESMF_LogWrite(trim(subname)//trim(string)//" field NOT connected  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        endif
      enddo
      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//trim(string)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine realizeConnectedFields
#endif

  end subroutine InitializeIPDv03p3
  
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p4(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
!    type(ESMF_Field)              :: field
!    type(ESMF_Grid)               :: grid
!    integer                       :: localDeCount

!    type(ESMF_DistGrid)           :: distgrid
!    integer                       :: dimCount, tileCount, petCount
!    integer                       :: deCountPTile, extraDEs
!    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
!    integer, allocatable          :: regDecompPTile(:,:)
!    integer                       :: i, j, n, n1
!    character(ESMF_MAXSTR)        :: transferAction
    
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p4)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

    call realizeConnectedGrid(NState_atmImp, 'AtmImp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_atmExp, 'AtmExp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_ocnImp, 'OcnImp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_ocnExp, 'OcnExp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_iceImp, 'IceImp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_iceExp, 'IceExp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_lndImp, 'LndImp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_lndExp, 'LndExp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_rofImp, 'RofImp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call realizeConnectedGrid(NState_rofExp, 'RofExp', rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

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
      type(ESMF_Mesh)               :: mesh
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
      character(ESMF_MAXSTR)        :: transferAction
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
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!      do n=1, fieldCount
      do n=1, min(fieldCount,1)

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
          value=transferAction, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (trim(transferAction) == "accept") then
          ! while this is still an empty field, it does now hold a Grid with DistGrid
          call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          if (geomtype == ESMF_GEOMTYPE_GRID) then

            call ESMF_FieldGet(field, grid=grid, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

            call med_method_Grid_Print(grid,trim(fieldNameList(n))//'_orig',rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

            call ESMF_AttributeGet(field, name="ArbDimCount", value=arbDimCount, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

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
                  call ESMF_LogWrite(trim(subname)//trim(string)//": accept arb2reg grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
                endif
                ! Use a regDecomp representation for the grid
                ! first get tile min/max, only single tile supported for arbDistr Grid
                allocate(minIndexPTile(arbDimCount,1),maxIndexPTile(arbDimCount,1))
                call ESMF_AttributeGet(field, name="MinIndex", &
                  valueList=minIndexPTile(:,1), &
                  convention="NUOPC", purpose="Instance", rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                call ESMF_AttributeGet(field, name="MaxIndex", &
                  valueList=maxIndexPTile(:,1), &
                  convention="NUOPC", purpose="Instance", rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                ! create default regDecomp DistGrid
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                 maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                ! Create default regDecomp Grid
                grid = ESMF_GridCreate(distgrid, rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                ! swap out the transferred grid for the newly created one
                call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
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
                  call ESMF_LogWrite(trim(subname)//trim(string)//": accept arb2arb grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
                endif

              else   ! grid_arbopt

                call ESMF_LogWrite(trim(subname)//trim(string)//": ERROR grid_arbopt setting = "//trim(grid_arbopt), ESMF_LOGMSG_INFO, rc=rc)
                rc = ESMF_FAILURE
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

              endif  ! grid_arbopt


            else   ! arbdimcount <= 0

              ! The provider defined as non arb grid

              ! access localDeCount to show this is a real Grid
              if (dbug_flag > 1) then
                call ESMF_LogWrite(trim(subname)//trim(string)//": accept reg2reg grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
              endif
              call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

              ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.

              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
                connectionCount=connectionCount, rc=rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                       maxIndexPTile(dimCount, tileCount))
              allocate(connectionList(connectionCount))
    
              ! get minIndex and maxIndex arrays, and connectionList
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

              ! construct a default regDecompPTile -> TODO: move this into ESMF as default
              call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

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
                write(msgString,'(A,5i8)') trim(subname)//':PTile =',i2,i1,minIndexPTile(i1,i2),maxIndexPTile(i1,i2),regDecompPTile(i1,i2)
                call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
              enddo
              enddo

!--- tcraig, hardwire i direction wraparound, temporary
!--- tcraig, now getting info from model distgrid, see above
!              allocate(connectionList(1))
!              nxg = maxIndexPTile(1,1) - minIndexPTile(1,1) + 1
!              write(msgstring,*) trim(subname)//trim(string),': connlist nxg = ',nxg
!              call ESMF_LogWrite(trim(msgstring), ESMF_LOGMSG_INFO, rc=rc)
!              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
!              call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
!                tileIndexB=1, positionVector=(/nxg, 0/), rc=rc)
!              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

              ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
              ! but with a default regDecompPTile
              ! tcraig, force connectionlist and gridEdge arguments to fix wraparound
              ! need ESMF fixes to implement properly.
              if (dimcount == 2) then
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, &
                  connectionList=connectionList, rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                if (dbug_flag > 1) then
                  call ESMF_LogWrite(trim(subname)//trim(string)//': distgrid with dimcount=2', ESMF_LOGMSG_INFO, rc=rc)
                  if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                endif
                ! Create a new Grid on the new DistGrid and swap it in the Field
                grid = ESMF_GridCreate(distgrid, &
                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
              else
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                if (dbug_flag > 1) then
                  call ESMF_LogWrite(trim(subname)//trim(string)//': distgrid with dimcount=1', ESMF_LOGMSG_INFO, rc=rc)
                  if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
                endif
                ! Create a new Grid on the new DistGrid and swap it in the Field
                grid = ESMF_GridCreate(distgrid, &
                  gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
              endif

              ! local clean-up
              deallocate(connectionList)
              deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)

            endif  ! arbdimCount

          ! Swap all the Grids in the State

!            do n1=n,n
            do n1=1, fieldCount
              ! access a field in the State and set the Grid
              call ESMF_StateGet(State, field=field, &
                itemName=fieldNameList(n1), rc=rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
              if (dbug_flag > 1) then
                call ESMF_LogWrite(trim(subname)//trim(string)//": attach grid for "//trim(fieldNameList(n1)), ESMF_LOGMSG_INFO, rc=rc)
                if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
              endif
              call med_method_Grid_Print(grid,trim(fieldNameList(n))//'_new',rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
            enddo

          elseif (geomtype == ESMF_GEOMTYPE_MESH) then

            call ESMF_FieldGet(field, mesh=mesh, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

            call med_method_Mesh_Print(mesh,trim(fieldNameList(n))//'_orig',rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          else  ! geomtype

            call ESMF_LogWrite(trim(subname)//": ERROR geomtype not supported ", ESMF_LOGMSG_INFO, rc=rc)
            rc=ESMF_FAILURE
            return

          endif ! geomtype

        else    ! accept

          call ESMF_LogWrite(trim(subname)//trim(string)//": provide grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

        endif   ! accept

      enddo   ! nflds

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
    type(ESMF_Field)            :: field, field1, field2
    type(ESMF_Field)            :: fieldArea
    type(ESMF_Grid)             :: grid
    type(InternalState)         :: is_local
    integer                     :: fieldCount
    real(ESMF_KIND_R8), pointer :: factorList(:)
    character(ESMF_MAXSTR)      :: name
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    integer                     :: i,j
    type(ESMF_Field)            :: fieldAtm, fieldOcn
    type(ESMF_Array)            :: arrayOcn, arrayIce
    type(ESMF_RouteHandle)      :: RH_mapmask  ! unmasked conservative remapping 
    type(ESMF_Grid)             :: gridAtmCoord, gridOcnCoord
    integer(ESMF_KIND_I4), pointer :: dataPtr_arrayOcn(:,:), dataPtr_arrayIce(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fieldOcn(:,:), dataPtr_fieldAtm(:,:)
    logical                     :: isPresentOcn, isPresentIce
    character(len=*),parameter  :: subname='(module_MEDIATOR:InitializeIPDv03p5)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

    !----------------------------------------------------------
    !--- Finish initializing the State Fields
    !----------------------------------------------------------

    call completeFieldInitialization(NState_atmImp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_atmImp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_atmExp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_atmExp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_ocnImp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_ocnImp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_ocnExp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_ocnExp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_iceImp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_iceImp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_iceExp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_iceExp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_lndImp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_lndImp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_lndExp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_lndExp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_rofImp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_rofImp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call completeFieldInitialization(NState_rofExp, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call med_method_State_reset(NState_rofExp, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !----------------------------------------------------------
    !--- Set the model grids using first field in each model's import state
    !----------------------------------------------------------

!tcraig old version
!    call ESMF_StateGet(NState_atmImp, field=field, itemName=fldsFrAtm%shortname(1), rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!    call ESMF_FieldGet(field, grid=gridAtm, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!    call ESMF_StateGet(NState_ocnImp, field=field, itemName=fldsFrOcn%shortname(1), rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

!    call ESMF_FieldGet(field, grid=gridOcn, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_StateGet(NState_atmImp, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write(msgString,*) fieldcount
    call ESMF_LogWrite(trim(subname)//": tcx fieldcount = " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_atmImp, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_StateGet(NState_atmImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_FieldGet(field, grid=gridAtm, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      deallocate(fieldNameList)
    else
      gridAtm = gridMed
    endif

    call ESMF_StateGet(NState_ocnImp, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_ocnImp, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_StateGet(NState_ocnImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_FieldGet(field, grid=gridOcn, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      deallocate(fieldNameList)
    else
      gridOcn = gridMed
    endif

    call ESMF_StateGet(NState_iceImp, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_iceImp, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_StateGet(NState_iceImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_FieldGet(field, grid=gridIce, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      deallocate(fieldNameList)
    else
      gridIce = gridMed
    endif

! Land will pick up the grid from the first field exported to Land

    call ESMF_StateGet(NState_lndExp, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_lndExp, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_StateGet(NState_lndExp, field=field, itemName=fieldNameList(1), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_FieldGet(field, grid=gridLnd, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call ESMF_GridGetCoord(gridLnd, staggerloc=ESMF_STAGGERLOC_CENTER, &
        isPresent=isPresent,rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (.NOT. isPresent) then
        call med_method_Grid_CopyCoord(gridcomp=gcomp, gridSrc=gridAtm, gridDst=gridLnd, &
          staggerloc=(/ESMF_STAGGERLOC_CENTER/), invert=(/2/), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_LogWrite(trim(subname)// &
          ": Copied gridATM center coordinates to gridLnd", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      call ESMF_GridGetCoord(gridLnd, staggerloc=ESMF_STAGGERLOC_CORNER, &
        isPresent=isPresent,rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (.NOT. isPresent) then
        call med_method_Grid_CopyCoord(gridcomp=gcomp, gridSrc=gridAtm, gridDst=gridLnd, &
          staggerloc=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), invert=(/2/), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_LogWrite(trim(subname)// &
          ": Copied gridATM center and corner coordinates to gridLnd", ESMF_LOGMSG_INFO, rc=dbrc)
        call med_method_Grid_CopyItem(gridcomp=gcomp, gridSrc=gridAtm, gridDst=gridLnd, &
          item=(/ESMF_GRIDITEM_AREA/), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_LogWrite(trim(subname)// &
          ": Copied gridATM areas to gridLnd", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      deallocate(fieldNameList)
    else
      gridLnd = gridMed
    endif

! Rof will pick up the grid from the first field exported to Rof

    call ESMF_StateGet(NState_rofExp, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_rofExp, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_StateGet(NState_rofExp, field=field, itemName=fieldNameList(1), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_FieldGet(field, grid=gridRof, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      deallocate(fieldNameList)
    else
      gridRof = gridMed
    endif

    !----------------------------------------------------------
    !--- Diagnose Grid Info
    !----------------------------------------------------------

    call med_method_Grid_Print(gridAtm,'gridAtm',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Print(gridOcn,'gridOcn',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Print(gridIce,'gridIce',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Print(gridLnd,'gridLnd',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Print(gridRof,'gridRof',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Print(gridMed,'gridMed',rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

#if 1
    !----------------------------------------------------------
    ! dump the Grid coordinate arrays for reference      
    !----------------------------------------------------------

    call med_method_Grid_Write(gridAtm, 'array_med_atm', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Write(gridOcn, 'array_med_ocn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Write(gridIce, 'array_med_ice', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Write(gridLnd, 'array_med_lnd', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Write(gridRof, 'array_med_rof', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Grid_Write(gridMed, 'array_med_med', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

#endif

    !----------------------------------------------------------
    ! NOW allocate other Mediator datatypes
    !----------------------------------------------------------

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !----------------------------------------------------------
    ! Initialize FB for each model import states on each grid
    !----------------------------------------------------------

    !--- atm

    call med_method_FB_init(is_local%wrap%FBAtm_a, grid=gridAtm, &
      state=NState_AtmImp, name='FBAtm_a', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBAtm_o, grid=gridOcn, &
      state=NState_AtmImp, name='FBAtm_o', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBAtm_i, grid=gridIce, &
      state=NState_AtmImp, name='FBAtm_i', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBAtm_l, grid=gridLnd, &
      state=NState_AtmImp, name='FBAtm_l', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBAtm_h, grid=gridRof, &
      state=NState_AtmImp, name='FBAtm_h', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- ocn

    call med_method_FB_init(is_local%wrap%FBOcn_a, grid=gridAtm, &
      state=NState_OcnImp, name='FBOcn_a', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBOcn_o, grid=gridOcn, &
      state=NState_OcnImp, name='FBOcn_o', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBOcn_i, grid=gridIce, &
      state=NState_OcnImp, name='FBOcn_i', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- ice

    call med_method_FB_init(is_local%wrap%FBIce_a, grid=gridAtm, &
      state=NState_IceImp, name='FBIce_a', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBIce_o, grid=gridOcn, &
      state=NState_IceImp, name='FBIce_o', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBIce_i, grid=gridIce, &
      state=NState_IceImp, name='FBIce_i', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBIce_if, grid=gridIce, &
      state=NState_IceImp, name='FBIce_if', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- lnd

    call med_method_FB_init(is_local%wrap%FBLnd_a, grid=gridAtm, &
      state=NState_LndImp, name='FBLnd_a', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBLnd_l, grid=gridLnd, &
      state=NState_LndImp, name='FBLnd_l', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBLnd_h, grid=gridRof, &
      state=NState_LndImp, name='FBLnd_h', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !--- rof

    call med_method_FB_init(is_local%wrap%FBRof_l, grid=gridLnd, &
      state=NState_RofImp, name='FBRof_l', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBRof_a, grid=gridAtm, &
      state=NState_RofImp, name='FBRof_a', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBRof_h, grid=gridRof, &
      state=NState_RofImp, name='FBRof_h', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !----------------------------------------------------------
    ! Initialize Accumulators
    !----------------------------------------------------------

    call med_method_FB_init(is_local%wrap%FBaccumAtm, grid=gridAtm, &
      state=NState_AtmImp, name='FBaccumAtm', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBaccumOcn, grid=gridOcn, &
      state=NState_OcnImp, name='FBaccumOcn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBaccumIce, grid=gridIce, &
      state=NState_IceImp, name='FBaccumIce', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBaccumLnd, grid=gridLnd, &
      state=NState_LndImp, name='FBaccumLnd', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBaccumRof, grid=gridRof, &
      state=NState_RofImp, name='FBaccumRof', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !----------------------------------------------------------
    ! Initialize AtmOcn FBs
    !----------------------------------------------------------

    call med_method_FB_init(is_local%wrap%FBAtmOcn_o, grid=gridOcn, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBAtmOcn_o', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBAtmOcn_a, grid=gridAtm, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBAtmOcn_a', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBaccumAtmOcn, grid=gridOcn, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBaccumAtmOcn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !----------------------------------------------------------
    ! Initialize FB for export to models
    !----------------------------------------------------------

    call med_method_FB_init(is_local%wrap%FBforAtm, &
      state=NState_AtmExp, name='FBforAtm', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBforOcn, &
      state=NState_OcnExp, name='FBforOcn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBforIce, &
      state=NState_IceExp, name='FBforIce', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBforLnd, &
      state=NState_LndExp, name='FBforLnd', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_init(is_local%wrap%FBforRof, &
      state=NState_RofExp, name='FBforRof', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !----------------------------------------------------------
    !--- Check for active regrid directions
    !----------------------------------------------------------
    
    ! initialize
    is_local%wrap%a2o_active = .false.
    is_local%wrap%a2i_active = .false.
    is_local%wrap%a2l_active = .false.
    is_local%wrap%a2h_active = .false.
    is_local%wrap%o2a_active = .false.
    is_local%wrap%o2i_active = .false.
    is_local%wrap%i2a_active = .false.
    is_local%wrap%i2o_active = .false.
    is_local%wrap%l2a_active = .false.
    is_local%wrap%l2h_active = .false.
    is_local%wrap%h2l_active = .false.
    is_local%wrap%h2a_active = .false.

    ! a2o, a2i, a2l, a2h
    call ESMF_FieldBundleGet(is_local%wrap%FBAtm_a, fieldCount=fieldCount, rc=rc) ! Atmosphere Export Field Count
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforOcn, fieldCount=fieldCount, rc=rc) ! Ocean Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2o_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforIce, fieldCount=fieldCount, rc=rc) ! Ice Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2i_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforLnd, fieldCount=fieldCount, rc=rc) ! Land Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2l_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforRof, fieldCount=fieldCount, rc=rc) ! Rof Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2h_active = .true.
      endif
    endif
    
    ! o2a, o2i
    call ESMF_FieldBundleGet(is_local%wrap%FBOcn_o, fieldCount=fieldCount, rc=rc) ! Ocean Export Field Count
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%o2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforIce, fieldCount=fieldCount, rc=rc) ! Ice Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%o2i_active = .true.
      endif
    endif
    
    ! i2a, i2o
    call ESMF_FieldBundleGet(is_local%wrap%FBIce_i, fieldCount=fieldCount, rc=rc) ! Ice Export Field Count
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%i2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforOcn, fieldCount=fieldCount, rc=rc) ! Ocean Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%i2o_active = .true.
      endif
    endif
    
    ! l2a, l2h
    call ESMF_FieldBundleGet(is_local%wrap%FBLnd_l, fieldCount=fieldCount, rc=rc) ! Land Export Field Count
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%l2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforRof, fieldCount=fieldCount, rc=rc) ! Rof Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%l2h_active = .true.
      endif
    endif

    ! h2l, h2a
    call ESMF_FieldBundleGet(is_local%wrap%FBRof_h, fieldCount=fieldCount, rc=rc) ! Rof Export Field Count
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforLnd, fieldCount=fieldCount, rc=rc) ! Land Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%h2l_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (fieldCount > 0) then
        is_local%wrap%h2a_active = .true.
      endif
    endif

    write(msgString,*) is_local%wrap%a2o_active
    call ESMF_LogWrite(trim(subname)//": a2o active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2i_active
    call ESMF_LogWrite(trim(subname)//": a2i active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2l_active
    call ESMF_LogWrite(trim(subname)//": a2l active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2h_active
    call ESMF_LogWrite(trim(subname)//": a2h active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%o2a_active
    call ESMF_LogWrite(trim(subname)//": o2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%o2i_active
    call ESMF_LogWrite(trim(subname)//": o2i active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%i2a_active
    call ESMF_LogWrite(trim(subname)//": i2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%i2o_active
    call ESMF_LogWrite(trim(subname)//": i2o active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%l2a_active
    call ESMF_LogWrite(trim(subname)//": l2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%l2h_active
    call ESMF_LogWrite(trim(subname)//": l2h active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%h2l_active
    call ESMF_LogWrite(trim(subname)//": h2l active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%h2a_active
    call ESMF_LogWrite(trim(subname)//": h2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------
    !--- Initialize route handles
    !----------------------------------------------------------

    if (dbug_flag > 1) then
      call ESMF_LogWrite("Starting to initialize RHs", ESMF_LOGMSG_INFO)
      call ESMF_LogFlush()
    endif

    if (is_local%wrap%a2o_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_o, &
        bilnrmap=is_local%wrap%RH_a2o_bilnr, &
        consfmap=is_local%wrap%RH_a2o_consf, &
        consdmap=is_local%wrap%RH_a2o_consd, &
        patchmap=is_local%wrap%RH_a2o_patch, &
        fcopymap=is_local%wrap%RH_a2o_fcopy, &
        dstMaskValue=0, &
        fldlist1=FldsFrAtm, string='a2o_weights', &
        bilnrfn="/glade/p/cesmdata/cseg/inputdata/cpl/cpl6/map_T31_to_gx3v7_patch_090903.nc", &
        consffn="/glade/p/cesmdata/cseg/inputdata/cpl/cpl6/map_T31_to_gx3v7_aave_da_090903.nc", &
        rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%a2i_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_i, &
        bilnrmap=is_local%wrap%RH_a2i_bilnr, &
        consfmap=is_local%wrap%RH_a2i_consf, &
        consdmap=is_local%wrap%RH_a2i_consd, &
        patchmap=is_local%wrap%RH_a2i_patch, &
        fcopymap=is_local%wrap%RH_a2i_fcopy, &
        dstMaskValue=0, &
        fldlist1=FldsFrAtm, string='a2i_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%a2l_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_l, &
        bilnrmap=is_local%wrap%RH_a2l_bilnr, &
        consfmap=is_local%wrap%RH_a2l_consf, &
        consdmap=is_local%wrap%RH_a2l_consd, &
        patchmap=is_local%wrap%RH_a2l_patch, &
        fcopymap=is_local%wrap%RH_a2l_fcopy, &
        fldlist1=FldsFrAtm, string='a2l_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%a2h_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_h, &
        bilnrmap=is_local%wrap%RH_a2h_bilnr, &
        consfmap=is_local%wrap%RH_a2h_consf, &
        consdmap=is_local%wrap%RH_a2h_consd, &
        patchmap=is_local%wrap%RH_a2h_patch, &
        fcopymap=is_local%wrap%RH_a2h_fcopy, &
        fldlist1=FldsFrAtm, string='a2h_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%o2a_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBOcn_o, FBdst=is_local%wrap%FBOcn_a, &
        bilnrmap=is_local%wrap%RH_o2a_bilnr, &
        consfmap=is_local%wrap%RH_o2a_consf, &
        consdmap=is_local%wrap%RH_o2a_consd, &
        patchmap=is_local%wrap%RH_o2a_patch, &
        fcopymap=is_local%wrap%RH_o2a_fcopy, &
        srcMaskValue=0, &
        fldlist1=FldsFrOcn, fldlist2=FldsAtmOcn, string='o2a_weights', &
        bilnrfn="/glade/p/cesmdata/cseg/inputdata/cpl/cpl6/map_gx3v7_to_T31_aave_da_090903.nc", &
        consffn="/glade/p/cesmdata/cseg/inputdata/cpl/cpl6/map_gx3v7_to_T31_aave_da_090903.nc", &
        rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%o2i_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBOcn_o, FBdst=is_local%wrap%FBOcn_i, &
        bilnrmap=is_local%wrap%RH_o2i_bilnr, &
        consfmap=is_local%wrap%RH_o2i_consf, &
        consdmap=is_local%wrap%RH_o2i_consd, &
        patchmap=is_local%wrap%RH_o2i_patch, &
        fcopymap=is_local%wrap%RH_o2i_fcopy, &
        srcMaskValue=0, dstMaskValue=0, &
        fldlist1=FldsFrOcn, string='o2i_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%i2a_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBIce_i, FBdst=is_local%wrap%FBIce_a, &
        bilnrmap=is_local%wrap%RH_i2a_bilnr, &
        consfmap=is_local%wrap%RH_i2a_consf, &
        consdmap=is_local%wrap%RH_i2a_consd, &
        patchmap=is_local%wrap%RH_i2a_patch, &
        fcopymap=is_local%wrap%RH_i2a_fcopy, &
        srcMaskValue=0, &
        fldlist1=FldsFrIce, string='i2a_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%i2o_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBIce_i, FBdst=is_local%wrap%FBIce_o, &
        bilnrmap=is_local%wrap%RH_i2o_bilnr, &
        consfmap=is_local%wrap%RH_i2o_consf, &
        consdmap=is_local%wrap%RH_i2o_consd, &
        patchmap=is_local%wrap%RH_i2o_patch, &
        fcopymap=is_local%wrap%RH_i2o_fcopy, &
        srcMaskValue=0, dstMaskValue=0, &
        fldlist1=FldsFrIce, string='i2o_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%l2a_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBLnd_l, FBdst=is_local%wrap%FBLnd_a, &
        bilnrmap=is_local%wrap%RH_l2a_bilnr, &
        consfmap=is_local%wrap%RH_l2a_consf, &
        consdmap=is_local%wrap%RH_l2a_consd, &
        patchmap=is_local%wrap%RH_l2a_patch, &
        fcopymap=is_local%wrap%RH_l2a_fcopy, &
        fldlist1=FldsFrLnd, string='l2a_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%l2h_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBLnd_l, FBdst=is_local%wrap%FBLnd_h, &
        bilnrmap=is_local%wrap%RH_l2h_bilnr, &
        consfmap=is_local%wrap%RH_l2h_consf, &
        consdmap=is_local%wrap%RH_l2h_consd, &
        patchmap=is_local%wrap%RH_l2h_patch, &
        fcopymap=is_local%wrap%RH_l2h_fcopy, &
        fldlist1=FldsFrLnd, string='l2h_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%h2a_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBRof_h, FBdst=is_local%wrap%FBRof_a, &
        bilnrmap=is_local%wrap%RH_h2a_bilnr, &
        consfmap=is_local%wrap%RH_h2a_consf, &
        consdmap=is_local%wrap%RH_h2a_consd, &
        patchmap=is_local%wrap%RH_h2a_patch, &
        fcopymap=is_local%wrap%RH_h2a_fcopy, &
        fldlist1=FldsFrRof, string='h2a_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (is_local%wrap%h2l_active) then
      call med_method_RH_Init(FBsrc=is_local%wrap%FBRof_h, FBdst=is_local%wrap%FBRof_l, &
        bilnrmap=is_local%wrap%RH_h2l_bilnr, &
        consfmap=is_local%wrap%RH_h2l_consf, &
        consdmap=is_local%wrap%RH_h2l_consd, &
        patchmap=is_local%wrap%RH_h2l_patch, &
        fcopymap=is_local%wrap%RH_h2l_fcopy, &
        fldlist1=FldsFrRof, string='h2l_weights', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

#if (1 == 0)
    !--- land mask
    if (generate_landmask) then

      call ESMF_GridGetItem(gridOcn, itemflag=ESMF_GRIDITEM_MASK, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresentOcn, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_GridGetItem(gridIce, itemflag=ESMF_GRIDITEM_MASK, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresentIce, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      if (isPresentOcn .or. isPresentIce) then

        if (isPresentOcn .and. isPresentIce) then

          ! ocn mask from ocn grid

          call ESMF_GridGetItem(gridOcn, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, array=arrayOcn, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_GridGetItem(gridOcn, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dataPtr_arrayOcn, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArraySet(arrayOcn, name="ocean_mask", rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          write (msgString,*) trim(subname)//"ocn_mask raw = ",minval(dataPtr_arrayOcn),maxval(dataPtr_arrayOcn)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          call ESMF_ArrayWrite(arrayOcn, 'field_med_ocn_a_ocean_mask.nc', overwrite=.true., rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          ! ice mask from ice grid

          call ESMF_GridGetItem(gridIce, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, array=arrayIce, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_GridGetItem(gridIce, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dataPtr_arrayIce, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArraySet(arrayIce, name="ice_mask", rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          write (msgString,*) trim(subname)//"ice_mask raw = ",minval(dataPtr_arrayIce),maxval(dataPtr_arrayIce)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          call ESMF_ArrayWrite(arrayIce, 'field_med_ocn_a_ice_mask.nc', overwrite=.true., rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          ! generate ocn grid with just coords, no mask or area
          ! create ocn/ice mask field on ocn grid, coords only

          call Grid_CreateCoords(gridOcnCoord, gridOcn, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          fieldOcn = ESMF_FieldCreate(gridOcnCoord, ESMF_TYPEKIND_R8, name='ocnice_mask', rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_FieldGet(fieldOcn, farrayPtr=dataPtr_fieldOcn, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          ! generate atm grid with just coords, no mask or area
          ! create land mask field on atm grid, coords only

          call Grid_CreateCoords(gridAtmCoord, gridAtm, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          fieldAtm = ESMF_FieldCreate(gridAtmCoord, ESMF_TYPEKIND_R8, name='land_mask', rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_FieldGet(fieldAtm, farrayPtr=dataPtr_FieldAtm, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          ! Here, the ocean/ice mask is the intersection of ocean and ice masks, which are integer fields of 0 or 1
          ! Convert to real and make sure values are only 0 or 1.

          do j = lbound(dataPtr_fieldOcn,2),ubound(dataPtr_fieldOcn,2)
          do i = lbound(dataPtr_fieldOcn,1),ubound(dataPtr_fieldOcn,1)
            dataPtr_fieldOcn(i,j) = min(dataPtr_arrayIce(i,j),dataPtr_arrayOcn(i,j))
            if (dataPtr_fieldOcn(i,j) < 0.50_ESMF_KIND_R8) then
              dataPtr_fieldOcn(i,j) = 0.0_ESMF_KIND_R8
            else
              dataPtr_fieldOcn(i,j) = 1.0_ESMF_KIND_R8
            endif
          enddo
          enddo

          ! generate a new RH from Atm and Ocn coords, no masks, no areas.  Should not use o2a_consd mapping
          ! because it has masks and area corrections.

          call ESMF_FieldRegridStore(fieldOcn, fieldAtm, routehandle=RH_mapmask, &
            regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
            srcTermProcessing=srcTermProcessing_Value, &
            ignoreDegenerate=.true., &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          ! regrid ocean mask from ocn to atm grid using unmasked conservative mapping

          if (ESMF_RouteHandleIsCreated(RH_mapmask, rc=rc)) then
            dataPtr_fieldAtm = 0.0_ESMF_KIND_R8
            call ESMF_FieldRegrid(fieldOcn, fieldAtm, routehandle=RH_mapmask, &
              termorderflag=ESMF_TERMORDER_SRCSEQ, zeroregion=ESMF_REGION_TOTAL, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
            call ESMF_FieldRegridRelease(RH_mapmask, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          else
            call ESMF_LogWrite(trim(subname)//": ERROR RH_mapmask not created", ESMF_LOGMSG_INFO, rc=rc)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          endif

          ! convert from ocean mask to land mask
          ! check min/max
          ! also fill "land_mask" array and save it for later

          allocate(land_mask(lbound(dataPtr_fieldAtm,1):ubound(dataPtr_fieldAtm,1),lbound(dataPtr_fieldAtm,2):ubound(dataPtr_fieldAtm,2)))
          do j = lbound(dataPtr_fieldAtm,2),ubound(dataPtr_fieldAtm,2)
          do i = lbound(dataPtr_fieldAtm,1),ubound(dataPtr_fieldAtm,1)
            dataPtr_fieldAtm(i,j) = 1.0_ESMF_KIND_R8 - dataPtr_fieldAtm(i,j)
            if (dataPtr_fieldAtm(i,j) > 1.0_ESMF_KIND_R8) dataPtr_fieldAtm(i,j) = 1.0_ESMF_KIND_R8
            if (dataPtr_fieldAtm(i,j) < 1.0e-6_ESMF_KIND_R8) dataPtr_fieldAtm(i,j) = 0.0_ESMF_KIND_R8
            land_mask(i,j) = dataPtr_fieldAtm(i,j)
          enddo
          enddo

          ! write out masks

          call ESMF_FieldWrite(fieldOcn,'field_med_ocn_a_ocnice_mask.nc',overwrite=.true.,rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          write (msgString,*) trim(subname)//"ocean_mask = ",minval(dataPtr_fieldOcn),maxval(dataPtr_fieldOcn)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

          call ESMF_FieldWrite(fieldAtm,'field_med_atm_a_land_mask.nc',overwrite=.true.,rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          write (msgString,*) trim(subname)//"land_mask = ",minval(dataPtr_fieldAtm),maxval(dataPtr_fieldAtm)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

          ! clean up

          call ESMF_GridDestroy(gridAtmCoord,rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_FieldDestroy(fieldAtm,rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_GridDestroy(gridOcnCoord,rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_FieldDestroy(fieldOcn,rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        else  ! isPresentOcn .and. isPresentIce
          call ESMF_LogWrite(trim(subname)//": ABORT more support needed for Ocn or Ice mask", ESMF_LOGMSG_INFO, rc=rc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif

      endif    ! isPresentOcn .or. isPresentIce

    endif  ! generate_landmask
#endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine completeFieldInitialization(State,rc)

      type(ESMF_State)   , intent(inout) :: State
      integer            , intent(out)   :: rc
    
      integer                     :: n, fieldCount
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      character(ESMF_MAXSTR)      :: transferAction
      type(ESMF_GeomType_Flag)    :: geomtype
      character(len=*),parameter  :: subname='(module_MEDIATOR:completeFieldInitialization)'

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      do n=1, fieldCount

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
          value=transferAction, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (trim(transferAction) == "accept") then
          if (dbug_flag > 1) then
            call ESMF_LogWrite(subname//" is accepting grid for field "//trim(fieldNameList(n)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          endif
          ! the transferred Grid is already set, allocate field data memory
          ! DCR - The WRFROFRO soil fields have an ungridded 3rd dimension.
          ! The ESMF_FieldEmptyComplete is not allocating memory for this 3rd dimension
          call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        endif   ! accept

        call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (geomtype == ESMF_GEOMTYPE_GRID) then
          call med_method_Field_GridPrint(field,fieldNameList(n),rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        elseif (geomtype == ESMF_GEOMTYPE_MESH) then
          call med_method_Field_MeshPrint(field,fieldNameList(n),rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        else
          call ESMF_LogWrite(trim(subname)//": ERROR in geom type ", ESMF_LOGMSG_INFO, rc=rc)
          rc=ESMF_FAILURE
          return
        endif

      enddo

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
    character(len=*), parameter :: subname='(module_MEDIATOR:DataInitialize)'
    integer                     :: n

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    rc = ESMF_SUCCESS

    ! the MED needs valid ATM export Fields to initialize its internal state

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! initialze cumulative flag
    allDone = .true.  ! reset if an item is found that is not done
    
    ! check that all imported fields from ATM show correct timestamp
    do n = 1,fldsFrAtm%num
      call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), itemType=itemType, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        connected = NUOPC_IsConnected(NState_AtmImp, fieldName=fldsFrAtm%shortname(n), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (connected) then
          call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), field=field, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          atCorrectTime = NUOPC_IsAtTime(field, time, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          if (.not.atCorrectTime) then
            call ESMF_LogWrite("MED - Initialize-Data-Dependency NOT YET SATISFIED!!!", ESMF_LOGMSG_INFO, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
            allDone = .false.
            exit  ! break out of the loop when first not satisfied found
          else
            call ESMF_LogWrite("MED - Initialize-Data-Dependency SATISFIED!!!", ESMF_LOGMSG_INFO, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          endif
        endif
      endif
    enddo

    !TODO: need to loop through fields from all of the components from which
    !TODO: valid field data is expected at this time!!

    if (allDone) then
      ! -> set InitializeDataComplete Component Attribute to "true", indicating
      ! to the driver that this Component has fully initialized its data
      call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      ! gjt: The above code ensures that the MED has initial conditions from ATM.
    
      ! TODO - For the real case this should probably use the fields from the
      ! importState and do something with it as a sensible starting point
      ! for the accumulation field so that the OCN receives a meaningful
      ! fields during its first time step. However, here for testing
      ! I simply initialize to zero.
          
      call med_method_State_reset(NState_atmImp, value=spval_init, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call med_method_State_reset(NState_ocnImp, value=spval_init, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call med_method_State_reset(NState_iceImp, value=spval_init, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call med_method_State_reset(NState_lndImp, value=spval_init, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call med_method_State_reset(NState_rofImp, value=spval_init, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call med_method_FB_reset(is_local%wrap%FBaccumAtm, value=czero, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      is_local%wrap%accumcntAtm = 0

      call med_method_FB_reset(is_local%wrap%FBaccumOcn, value=czero, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      is_local%wrap%accumcntOcn = 0

      call med_method_FB_reset(is_local%wrap%FBaccumIce, value=czero, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      is_local%wrap%accumcntIce = 0

      call med_method_FB_reset(is_local%wrap%FBaccumLnd, value=czero, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      is_local%wrap%accumcntLnd = 0

      call med_method_FB_reset(is_local%wrap%FBaccumRof, value=czero, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      is_local%wrap%accumcntRof = 0

#if (1 == 0)
      !---------------------------------------
      ! read mediator restarts
      !---------------------------------------

      !---tcraig, turn if on to force no mediator restarts for testing
      !if (.not.coldstart) then
        call Mediator_restart(gcomp,'read','mediator',rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      !endif

      ! default initialize s_surf to work around limitations of current initialization sequence
      call ESMF_StateGet(NState_IceExp, itemName='s_surf', itemType=itemType, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        if (NUOPC_IsConnected(NState_IceExp,'s_surf',rc=rc)) then
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call State_SetFldPtr(NState_IceExp, 's_surf', 34.0_ESMF_KIND_R8, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        endif

      endif
#endif

    endif

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
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 1) then
       call med_method_Clock_TimePrint(driverClock  ,trim(subname)//'driver clock1',rc)
       call med_method_Clock_TimePrint(mediatorClock,trim(subname)//'mediat clock1',rc)
    endif

    ! set the mediatorClock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_ClockSet(mediatorClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 1) then
       call med_method_Clock_TimePrint(driverClock  ,trim(subname)//'driver clock2',rc)
       call med_method_Clock_TimePrint(mediatorClock,trim(subname)//'mediat clock2',rc)
    endif

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunClock

  !-----------------------------------------------------------------------------
#if (1 == 0)
  subroutine SetRunClock_slow(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(InternalState)     :: is
    type(ESMF_Clock)        :: driverClock

    rc = ESMF_SUCCESS
    
    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! set clockSlow to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is%wrap%clockSlow, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! query component for the driver Clock
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
  end subroutine SetRunClock_slow

  !-----------------------------------------------------------------------------
  
  subroutine SetRunClock_fast_before(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(InternalState)     :: is
    type(ESMF_Clock)        :: driverClock

    rc = ESMF_SUCCESS
    
    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! set clockFast to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is%wrap%clockFast, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! query component for the driver Clock
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
  end subroutine SetRunClock_fast_before

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_fast_after(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(InternalState)     :: is
    type(ESMF_Clock)        :: driverClock

    rc = ESMF_SUCCESS
    
    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! set clockFast to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is%wrap%clockFast, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! query component for the driver Clock
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
  end subroutine SetRunClock_fast_after

  !-----------------------------------------------------------------------------

  subroutine CheckImport_fast_before(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that ensures that the import Fields come in with
    ! the correct time stamps during the "fast" cycle: 
    ! -> Fields from the ATM are not used by the "before" phase and need not 
    !    be checked.
    ! -> Fields from the OCN must be at the startTime of the parent driver 
    !    Clock
    
    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: startTime
    type(ESMF_State)        :: importState
    type(ESMF_Field)        :: field
    logical                 :: atCorrectTime

    rc = ESMF_SUCCESS
    
    ! query the Component for its importState
    call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! query the Component for its driverClock
    call NUOPC_MediatorGet(gcomp, driverClock=clock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! get the start time out of the driver Clock
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! check fields from OCN to be at startTime of the driver Clock
    if (NUOPC_IsConnected(importState, fieldName="sst")) then
      call ESMF_StateGet(importState, itemName="sst", field=field, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      atCorrectTime = NUOPC_IsAtTime(field, startTime, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (.not.atCorrectTime) then
        !TODO: introduce and use INCOMPATIBILITY return codes!!!!
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at correct time", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif

  end subroutine CheckImport_fast_before

  !-----------------------------------------------------------------------------
  
  subroutine CheckImport_fast_after(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that ensures that the import Fields come in with
    ! the correct time stamps during the "fast" cycle: 
    ! -> Fields from the ATM must be at stopTime because this mediator phase
    !    runs _after_ the ATM runs.
    ! -> Fields from the OCN are not used by the "after" phase and need not 
    !    be checked.
    
    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: stopTime
    type(ESMF_State)        :: importState
    type(ESMF_Field)        :: field
    logical                 :: atCorrectTime

    rc = ESMF_SUCCESS
    
    ! query the Component for its Clock and importState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! get the stop time out of the Clock
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! check fields from ATM to be at stopTime
    if (NUOPC_IsConnected(importState, fieldName="pmsl")) then
      call ESMF_StateGet(importState, itemName="pmsl", field=field, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      atCorrectTime = NUOPC_IsAtTime(field, stopTime, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (.not.atCorrectTime) then
        !TODO: introduce and use INCOMPATIBILITY return codes!!!!
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at correct time", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif

  end subroutine CheckImport_fast_after

  !-----------------------------------------------------------------------------
  
  subroutine TimestampExport_fast_before(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that executes _after_ the "fast_before" mediator 
    ! phase has been run. Timestamping does not need to be adjusted here,
    ! but the Clock needs to be stepped back because the "fast_after" phase
    ! will be updating the same Clock during the same driver cylce.

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeStep

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! get the timeStep out of Clock
    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! step the Clock back one timestep
    call ESMF_ClockAdvance(clock, timeStep= -timeStep, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
  end subroutine TimestampExport_fast_before

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_fast_after(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that applies the time stamp on the export Fields
    ! during the "fast" cycle: 
    ! -> By default the MED Run method time stamps the export Fields with the
    !    current time at the beginning of the advance step, however here,
    !    because the "fast" cycle runs after the ATM model, the correct time
    !    stamp is the currTime _after_ the MED advance step.

    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: exportState

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, clock=clock, exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! update timestamp on export Fields
    call NUOPC_UpdateTimestamp(exportState, clock, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
  end subroutine TimestampExport_fast_after

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance_slow(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->MED Advance_slow() mediating for: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="----------------> model time step to: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
     
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance_fast_before(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->MED Advance_fast_before() mediating for: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="----------------> model time step to: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance_fast_after(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->MED Advance_fast_after() mediating for: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="----------------> model time step to: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
     
  end subroutine
#endif
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_atm(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase runs before ATM and ICE are being called and
    ! prepares the ATM and ICE import Fields.
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:),dataPtr4(:,:)
    real(ESMF_KIND_R8), pointer :: ifrac_i(:,:)                   ! ice fraction on ice grid
    real(ESMF_KIND_R8), pointer :: ifrac_af(:,:), ifrac_afr(:,:)  ! ice fraction on atm grid consf map
    real(ESMF_KIND_R8), pointer :: ifrac_ad(:,:), ifrac_adr(:,:)  ! ice fraction on atm grid consd map
    real(ESMF_KIND_R8), pointer :: ifrac_ab(:,:), ifrac_abr(:,:)  ! ice fraction on atm grid bilnr map
    real(ESMF_KIND_R8), pointer :: ifrac_ap(:,:), ifrac_apr(:,:)  ! ice fraction on atm grid patch map
    real(ESMF_KIND_R8), pointer :: ocnwgt(:,:),icewgt(:,:),customwgt(:,:)
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_prep_atm)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    call ESMF_TimeGet(time,timestring=timestr)
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    !---------------------------------------
    !--- this is fast, no accumulator needed
    !---------------------------------------

    if (dbug_flag > 1) then
      call med_method_FB_reset(is_local%wrap%FBAtm_a, value=czero, rc=rc)
      call med_method_FB_reset(is_local%wrap%FBOcn_o, value=czero, rc=rc)
      call med_method_FB_reset(is_local%wrap%FBIce_i, value=czero, rc=rc)
      call med_method_FB_reset(is_local%wrap%FBLnd_l, value=czero, rc=rc)
      call med_method_FB_reset(is_local%wrap%FBRof_h, value=czero, rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a zero', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o zero', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i zero', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l zero', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBRof_h, trim(subname)//' FBrof_h zero', rc=rc)
      call med_method_State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call med_method_State_diagnose(NState_OcnImp, trim(subname)//' OcnImp ', rc=rc)
      call med_method_State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call med_method_State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call med_method_State_diagnose(NState_RofImp, trim(subname)//' RofImp ', rc=rc)
    endif

    call med_method_FB_copy(is_local%wrap%FBAtm_a, NState_AtmImp, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBOcn_o, NState_OcnImp, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBIce_i, NState_IceImp, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBLnd_l, NState_LndImp, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBRof_h, NState_RofImp, rc=rc)

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBRof_h, trim(subname)//' FBRof_h ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call med_method_FB_reset(is_local%wrap%FBOcn_a, value=czero, rc=rc)
    call med_method_FB_reset(is_local%wrap%FBIce_a, value=czero, rc=rc)
    call med_method_FB_reset(is_local%wrap%FBIce_if, value=czero, rc=rc)
    call med_method_FB_reset(is_local%wrap%FBLnd_a, value=czero, rc=rc)
    call med_method_FB_reset(is_local%wrap%FBRof_a, value=czero, rc=rc)
    call med_method_FB_reset(is_local%wrap%FBAtmOcn_a, value=czero, rc=rc)

    if (is_local%wrap%o2a_active) then
      call med_method_FB_Regrid(fldsFrOcn, is_local%wrap%FBOcn_o, is_local%wrap%FBOcn_a, &
         consfmap=is_local%wrap%RH_o2a_consf, &
         consdmap=is_local%wrap%RH_o2a_consd, &
         bilnrmap=is_local%wrap%RH_o2a_bilnr, &
         patchmap=is_local%wrap%RH_o2a_patch, &
         string='o2a', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

      call med_method_FB_Regrid(fldsAtmOcn, is_local%wrap%FBAtmOcn_o, is_local%wrap%FBAtmOcn_a, &
         consfmap=is_local%wrap%RH_o2a_consf, &
         consdmap=is_local%wrap%RH_o2a_consd, &
         bilnrmap=is_local%wrap%RH_o2a_bilnr, &
         patchmap=is_local%wrap%RH_o2a_patch, &
         string='o2aatmocn', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    if (is_local%wrap%i2a_active) then
      if (med_method_FB_FldChk(is_local%wrap%FBIce_i, 'ice_fraction', rc=rc) .and. &
          med_method_FB_FldChk(is_local%wrap%FBIce_a, 'ice_fraction', rc=rc)) then
        !--- tcraig, need to weight the ice2atm regrid by the ice fraction
        !--- need to compute weight by the frac mapped with the correct mapping
        !--- first compute the ice fraction on the atm grid for all active mappings

        call med_method_FB_GetFldPtr(is_local%wrap%FBIce_i, 'ice_fraction', dataPtr1, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
        allocate(ifrac_i (lbound(dataPtr1,1):ubound(dataPtr1,1),lbound(dataPtr1,2):ubound(dataPtr1,2)))

        !--- conservative frac
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consf, rc=rc)) then
          call med_method_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_consf, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          allocate(ifrac_afr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_af (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_af(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_afr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_afr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- conservative dst
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consd, rc=rc)) then
          call med_method_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_consd, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          allocate(ifrac_adr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_ad (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ad(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_adr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_adr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- bilinear
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_bilnr, rc=rc)) then
          call med_method_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_bilnr, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          allocate(ifrac_abr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_ab (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ab(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_abr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_abr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- patch
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_patch, rc=rc)) then
          call med_method_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_patch, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

          allocate(ifrac_apr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_ap (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ap(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_apr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_apr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- multiply FBIce_i by ifrac_i

        do n = 1,fldsFrIce%num
          if (med_method_FB_FldChk(is_local%wrap%FBIce_i, fldsFrIce%shortname(n), rc=rc) .and. &
              med_method_FB_FldChk(is_local%wrap%FBIce_if,fldsFrIce%shortname(n), rc=rc)) then
            call med_method_FB_GetFldPtr(is_local%wrap%FBIce_i , fldsFrIce%shortname(n), dataPtr3, rc=rc)
            call med_method_FB_GetFldPtr(is_local%wrap%FBIce_if, fldsFrIce%shortname(n), dataPtr4, rc=rc)
            do j=lbound(dataptr3,2),ubound(dataptr3,2)
            do i=lbound(dataptr3,1),ubound(dataptr3,1)
              dataPtr4(i,j) = dataPtr3(i,j) * ifrac_i(i,j)
            enddo
            enddo
          endif
        enddo

        !--- regrid FBIce_if, fields with fraction multiplied

        call med_method_FB_Regrid(fldsFrIce, is_local%wrap%FBIce_if, is_local%wrap%FBIce_a, &
           consfmap=is_local%wrap%RH_i2a_consf, &
           consdmap=is_local%wrap%RH_i2a_consd, &
           bilnrmap=is_local%wrap%RH_i2a_bilnr, &
           patchmap=is_local%wrap%RH_i2a_patch, &
           string='i2a', rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

        !--- divide FBIce_a by ifrac_a, interpolated ice fraction
        !--- actually multiply by reciprocal of ifrac_a, ifrac_ar

        do n = 1,fldsFrIce%num
          if (med_method_FB_FldChk(is_local%wrap%FBIce_a, fldsFrIce%shortname(n), rc=rc)) then
            call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, fldsFrIce%shortname(n), dataPtr3, rc=rc)
            if (fldsFrIce%mapping(n) == "conservefrac") then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_afr(i,j)
              enddo
              enddo
            elseif (fldsFrIce%mapping(n) == "conservedst") then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_adr(i,j)
              enddo
              enddo
            elseif (fldsFrIce%mapping(n) == 'bilinear') then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_abr(i,j)
              enddo
              enddo
            elseif (fldsFrIce%mapping(n) == 'patch') then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_apr(i,j)
              enddo
              enddo
            else
              call ESMF_LogWrite(trim(subname)//": mapping name error "//trim(fldsFrIce%mapping(n)), ESMF_LOGMSG_INFO, rc=rc)
              rc=ESMF_FAILURE
              return
            endif
          endif
        enddo
        !--- make sure ifrac_a in the mapped bundle is correct
        call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr3, rc=rc)
        do j=lbound(dataptr3,2),ubound(dataptr3,2)
        do i=lbound(dataptr3,1),ubound(dataptr3,1)
          dataPtr3(i,j) = ifrac_af(i,j)
        enddo
        enddo

        deallocate(ifrac_i)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consf, rc=rc)) &
          deallocate(ifrac_af, ifrac_afr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consd, rc=rc)) &
          deallocate(ifrac_ad, ifrac_adr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_bilnr, rc=rc)) &
          deallocate(ifrac_ab, ifrac_abr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_patch, rc=rc)) &
          deallocate(ifrac_ap, ifrac_apr)

      else
        call med_method_FB_Regrid(fldsFrIce, is_local%wrap%FBIce_i, is_local%wrap%FBIce_a, &
           consfmap=is_local%wrap%RH_i2a_consf, &
           consdmap=is_local%wrap%RH_i2a_consd, &
           bilnrmap=is_local%wrap%RH_i2a_bilnr, &
           patchmap=is_local%wrap%RH_i2a_patch, &
           string='i2a', rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      endif
    endif

    if (is_local%wrap%l2a_active) then
      call med_method_FB_Regrid(fldsFrLnd, is_local%wrap%FBLnd_l, is_local%wrap%FBLnd_a, &
         consfmap=is_local%wrap%RH_l2a_consf, &
         consdmap=is_local%wrap%RH_l2a_consd, &
         bilnrmap=is_local%wrap%RH_l2a_bilnr, &
         patchmap=is_local%wrap%RH_l2a_patch, &
         string='l2a', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    if (is_local%wrap%h2a_active) then
      call med_method_FB_Regrid(fldsFrRof, is_local%wrap%FBRof_h, is_local%wrap%FBRof_a, &
         consfmap=is_local%wrap%RH_h2a_consf, &
         consdmap=is_local%wrap%RH_h2a_consd, &
         bilnrmap=is_local%wrap%RH_h2a_bilnr, &
         patchmap=is_local%wrap%RH_h2a_patch, &
         string='h2a', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBOcn_a, trim(subname)//' FBOcn_a ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBIce_a, trim(subname)//' FBIce_a ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBLnd_a, trim(subname)//' FBLnd_a ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBRof_a, trim(subname)//' FBRof_a ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBAtmOcn_a, trim(subname)//' FBAtmOcn_a ', rc=rc)
    endif

    call med_method_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBOcn_a, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBIce_a, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBLnd_a, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBRof_a, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBAtmOcn_a, rc=rc)

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBforAtm, trim(subname)//' FBforAtm ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      if (is_local%wrap%o2a_active) then
        call ESMF_FieldBundleWrite(is_local%wrap%FBOcn_a, 'fields_med_ocn_a.nc', &
          singleFile=.true., overwrite=.true., timeslice=is_local%wrap%fastcntr, &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      endif

      if (is_local%wrap%i2a_active) then
        call ESMF_FieldBundleWrite(is_local%wrap%FBIce_a, 'fields_med_ice_a.nc', &
          singleFile=.true., overwrite=.true., timeslice=is_local%wrap%fastcntr, &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      endif
    endif

    !---------------------------------------
    !--- custom calculations to atm
    !---------------------------------------

#if (1 == 0)
    !---  ocn and ice fraction for merges

    call med_method_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    allocate(ocnwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
    do i=lbound(icewgt,1),ubound(icewgt,1)
      ocnwgt = 1.0_ESMF_KIND_R8 - icewgt
    enddo
    enddo

    !--- fill land mask every coupling from initial computation

    if (generate_landmask) then
      call med_method_FB_GetFldPtr(is_local%wrap%FBforAtm, 'land_mask', dataPtr3, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      do j=lbound(dataPtr3,2),ubound(dataPtr3,2)
      do i=lbound(dataPtr3,1),ubound(dataPtr3,1)
        dataPtr3(i,j) = land_mask(i,j)
      enddo
      enddo
    else
      call ESMF_LogWrite(trim(subname)//": ERROR generate_landmask must be true ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    !--- merges

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'surface_temperature' , & 
                                  is_local%wrap%FBOcn_a   ,'sea_surface_temperature',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'sea_ice_temperature',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_sensi_heat_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_sensi_heat_flx_atm_into_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_sensi_heat_flx_atm_into_ice',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_laten_heat_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_laten_heat_flx_atm_into_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_laten_heat_flx_atm_into_ice',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_up_lw_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_up_lw_flx_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_up_lw_flx_ice',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_evap_rate' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_evap_rate_atm_into_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_evap_rate_atm_into_ice',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_zonal_moment_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'stress_on_air_ocn_zonal',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'stress_on_air_ice_zonal',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_merid_moment_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'stress_on_air_ocn_merid',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'stress_on_air_ice_merid',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    deallocate(ocnwgt)
#endif

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call med_method_State_reset(NState_AtmExp, value=spval, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (dbug_flag > 1) then
      call med_method_State_diagnose(NState_AtmExp, trim(subname)//' AtmExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call med_method_FB_copy(NState_AtmExp, is_local%wrap%FBforAtm, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (dbug_flag > 1) then
      call med_method_State_diagnose(NState_AtmExp, trim(subname)//' AtmExp_final ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to atm to file
      call NUOPC_Write(NState_AtmExp, &
        fldsToAtm%shortname(1:fldsToAtm%num), &
        "field_med_to_atm_", timeslice=is_local%wrap%fastcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif
    
    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine MedPhase_prep_atm

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_ocn(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_StateItem_Flag)   :: itemType
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(ESMF_MAXSTR)      :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    real(ESMF_KIND_R8), pointer :: atmwgt(:,:),icewgt(:,:),customwgt(:,:)
    logical                     :: checkOK, checkOK1, checkOK2
    character(len=*),parameter  :: subname='(module_MEDIATOR:MedPhase_prep_ocn)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    call ESMF_TimeGet(time,timestring=timestr)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      call NUOPC_Write(NState_OcnImp, &
        fldsFrOcn%shortname(1:fldsFrOcn%num), &
        "field_med_from_ocn_", timeslice=is_local%wrap%slowcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    !---------------------------------------
    !--- average atm, ice, lnd accumulators
    !---------------------------------------

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccA_B4avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccI_B4avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccL_B4avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccH_B4avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAO_B4avg ', rc=rc)
    endif

    call med_method_FB_average(is_local%wrap%FBaccumAtm, is_local%wrap%accumcntAtm, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_average(is_local%wrap%FBaccumIce, is_local%wrap%accumcntIce, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_average(is_local%wrap%FBaccumLnd, is_local%wrap%accumcntLnd, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_average(is_local%wrap%FBaccumRof, is_local%wrap%accumcntRof, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    
    call med_method_FB_average(is_local%wrap%FBaccumAtmOcn, is_local%wrap%accumcntAtmOcn, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccA_avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccI_avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccL_avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccH_avg ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAO_avg ', rc=rc)
    endif

    !---------------------------------------
    !--- regrid average atm+ice+lnd+rof fields to ocean grid
    !---------------------------------------

    if (is_local%wrap%a2o_active) then
      call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumAtm to FBAtm_o', ESMF_LOGMSG_INFO, rc=rc)
      call med_method_FB_Regrid(fldsFrAtm, is_local%wrap%FBaccumAtm, is_local%wrap%FBAtm_o, &
        consfmap=is_local%wrap%RH_a2o_consf, &
        consdmap=is_local%wrap%RH_a2o_consd, &
        bilnrmap=is_local%wrap%RH_a2o_bilnr, &
        patchmap=is_local%wrap%RH_a2o_patch, &
        string='a2o', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    if (is_local%wrap%i2o_active) then
      call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumIce to FBIce_o', ESMF_LOGMSG_INFO, rc=rc)
      call med_method_FB_Regrid(fldsFrIce, is_local%wrap%FBaccumIce, is_local%wrap%FBIce_o, &
        consfmap=is_local%wrap%RH_i2o_consf, &
        consdmap=is_local%wrap%RH_i2o_consd, &
        bilnrmap=is_local%wrap%RH_i2o_bilnr, &
        patchmap=is_local%wrap%RH_i2o_patch, &
        fcopymap=is_local%wrap%RH_i2o_fcopy, &
        string='i2o', rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBAtm_o, trim(subname)//' FBAtm_o ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBIce_o, trim(subname)//' FBIce_o ', rc=rc)
    endif

! tcx Xgrid
    ! XGrid intermediary required? instantiate FBXgrid FieldBundle?
    ! call ESMF_FieldBundleRegrid(is_local%wrap%FBaccumAtm, FBXgrid, is_local%wrap%RHa2x, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! call ESMF_FieldBundleRegrid(FBXgrid, is_local%wrap%FBforOcn  , is_local%wrap%RHx2o, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! tcraig temporarily copy
    
    call med_method_FB_copy(is_local%wrap%FBforOcn, is_local%wrap%FBAtm_o, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBforOcn, is_local%wrap%FBIce_o, rc=rc)
    call med_method_FB_copy(is_local%wrap%FBforOcn, is_local%wrap%FBAccumAtmOcn, rc=rc)

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFregrid ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to ocn
    !---------------------------------------

!    if (dbug_flag > 1) then
!      call med_method_FB_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFcc ', rc=rc)
!    endif

    !---------------------------------------
    !--- merges to ocn
    !---------------------------------------

#if (1 == 0)

    ! atm and ice fraction
    call med_method_FB_GetFldPtr(is_local%wrap%FBIce_o, 'ice_fraction', icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    allocate(atmwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(customwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
    do i=lbound(icewgt,1),ubound(icewgt,1)
      atmwgt = 1.0_ESMF_KIND_R8 - icewgt
    enddo
    enddo

    !-------------
    ! mean_evap_rate = mean_laten_heat_flux * (1-ice_fraction)/const_lhvap
    !-------------

!    customwgt = atmwgt / const_lhvap
!    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_evap_rate' , & 
!                                  is_local%wrap%FBAtm_o, 'mean_laten_heat_flux' ,customwgt, rc=rc)
!    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction)
    !-------------

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_fprec_rate' , & 
                                  is_local%wrap%FBAtm_o, 'mean_fprec_rate' ,atmwgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_down_lw_flx' , & 
                                  is_local%wrap%FBAtm_o, 'mean_down_lw_flx' ,atmwgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_evap_rate' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_evap_rate_atm_into_ocn' ,atmwgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_laten_heat_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_laten_heat_flx_atm_into_ocn' ,atmwgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_net_lw_flx' , & 
                                  is_local%wrap%FBAtm_o      ,'mean_down_lw_flx  ' ,atmwgt, &
                                  is_local%wrap%FBAccumAtmOcn,'mean_up_lw_flx_ocn' ,atmwgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_up_lw_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_up_lw_flx_ocn' ,atmwgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction) + field_from_ice * (ice_fraction)
    !-------------

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_prec_rate' , & 
                                  is_local%wrap%FBAtm_o, 'mean_prec_rate' ,atmwgt, &
                                  is_local%wrap%FBIce_o, 'mean_fresh_water_to_ocean_rate', icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_sensi_heat_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_sensi_heat_flx_atm_into_ocn' ,atmwgt, &
                                  is_local%wrap%FBIce_o      ,'net_heat_flx_to_ocn', icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn  ,'mean_zonal_moment_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'stress_on_air_ocn_zonal',atmwgt, &
                                  is_local%wrap%FBIce_o   ,'stress_on_ocn_ice_zonal',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn  ,'mean_merid_moment_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'stress_on_air_ocn_merid',atmwgt, &
                                  is_local%wrap%FBIce_o   ,'stress_on_ocn_ice_merid',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    !-------------
    ! netsw_for_ocn = downsw_from_atm * (1-ocn_albedo) * (1-ice_fraction) + pensw_from_ice * (ice_fraction)
    !-------------

    customwgt = atmwgt * (1.0 - 0.06)
!    customwgt = (1.0 - 0.06)
    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_vis_dir_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_vis_dir_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_vis_dir_flx' ,icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_vis_dif_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_vis_dif_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_vis_dif_flx',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_ir_dir_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_ir_dir_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_ir_dir_flx',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call med_method_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_ir_dif_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_ir_dif_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_ir_dif_flx',icewgt, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    !-------------
    ! End merges
    !-------------

    deallocate(atmwgt,customwgt)

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFmrg ', rc=rc)
    endif
    
#endif

    !---------------------------------------
    !--- zero accumulator
    !---------------------------------------

    is_local%wrap%accumcntAtm = 0
    call med_method_FB_reset(is_local%wrap%FBaccumAtm, value=czero, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    is_local%wrap%accumcntIce = 0
    call med_method_FB_reset(is_local%wrap%FBaccumIce, value=czero, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    is_local%wrap%accumcntLnd = 0
    call med_method_FB_reset(is_local%wrap%FBaccumLnd, value=czero, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    is_local%wrap%accumcntRof = 0
    call med_method_FB_reset(is_local%wrap%FBaccumRof, value=czero, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    is_local%wrap%accumcntAtmOcn = 0
    call med_method_FB_reset(is_local%wrap%FBaccumAtmOcn, value=czero, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (dbug_flag > 1) then
!tcx      call med_method_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call med_method_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBacc_AFzero ', rc=rc)
!dcr      call med_method_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBacc_AFzero ', rc=rc)
!dcr      call med_method_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call med_method_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBacc_AFzero ', rc=rc)
    endif

    !--- set export State to special value for testing

    call med_method_State_reset(NState_OcnExp, value=spval, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (dbug_flag > 1) then
      call med_method_State_diagnose(NState_OcnExp, trim(subname)//' es_AF99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call med_method_FB_copy(NState_OcnExp, is_local%wrap%FBforOcn, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (dbug_flag > 1) then
      call med_method_State_diagnose(NState_OcnExp, trim(subname)//' es_AFcp ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to ocn to file
      call NUOPC_Write(NState_OcnExp, &
        fldsToOcn%shortname(1:fldsToOcn%num), &
        "field_med_to_ocn_", timeslice=is_local%wrap%slowcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    !---------------------------------------

    is_local%wrap%slowcntr = is_local%wrap%slowcntr + 1

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine MedPhase_prep_ocn

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine MedPhase_accum_fast(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_accum_fast)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    call ESMF_TimeGet(time,timestring=timestr)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (statewrite_flag) then
      ! write the fields imported from atm to file
      call NUOPC_Write(NState_AtmImp, &
        fldsFrAtm%shortname(1:fldsFrAtm%num), &
        "field_med_from_atm_", timeslice=is_local%wrap%fastcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

      ! write the fields imported from ice to file
      call NUOPC_Write(NState_IceImp, &
        fldsFrIce%shortname(1:fldsFrIce%num), &
        "field_med_from_ice_", timeslice=is_local%wrap%fastcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

      ! write the fields imported from lnd to file
      call NUOPC_Write(NState_LndImp, &
        fieldNameList=fldsFrLnd%shortname(1:fldsFrLnd%num), &
        fileNamePrefix="field_med_from_lnd_", timeslice=is_local%wrap%fastcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

      ! write the fields imported from rof to file
      call NUOPC_Write(NState_RofImp, &
        fieldNameList=fldsFrRof%shortname(1:fldsFrRof%num), &
        fileNamePrefix="field_med_from_rof_", timeslice=is_local%wrap%fastcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    endif

    !---------------------------------------
    !--- atm, ice, lnd, rof accumulator for ocean
    !---------------------------------------

    if (dbug_flag > 1) then
      call med_method_State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call med_method_State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call med_method_State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call med_method_State_diagnose(NState_RofImp, trim(subname)//' RofImp ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_B4accum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccIce_B4accum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccLnd_B4accum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccRof_B4accum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAtmOcn_B4accum ', rc=rc)
    endif

    call med_method_FB_accum(is_local%wrap%FBaccumAtm, NState_AtmImp, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    is_local%wrap%accumcntAtm = is_local%wrap%accumcntAtm + 1

    call med_method_FB_accum(is_local%wrap%FBaccumIce, NState_IceImp, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    is_local%wrap%accumcntIce = is_local%wrap%accumcntIce + 1

    call med_method_FB_accum(is_local%wrap%FBaccumLnd, NState_LndImp, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    is_local%wrap%accumcntLnd = is_local%wrap%accumcntLnd + 1

    call med_method_FB_accum(is_local%wrap%FBaccumRof, NState_RofImp, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    is_local%wrap%accumcntRof = is_local%wrap%accumcntRof + 1

    call med_method_FB_accum(is_local%wrap%FBaccumAtmOcn, is_local%wrap%FBAtmOcn_o, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    is_local%wrap%accumcntAtmOcn = is_local%wrap%accumcntAtmOcn + 1

    if (dbug_flag > 1) then
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_AFaccum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccIce_AFaccum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccLnd_AFaccum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccRof_AFaccum ', rc=rc)
      call med_method_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAtmOcn_AFaccum ', rc=rc)
    endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    !---------------------------------------

    is_local%wrap%fastcntr = is_local%wrap%fastcntr + 1

    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine MedPhase_accum_fast

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
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
      call ESMF_LogWrite(trim(subname)//": ERROR mode not allowed "//trim(mode), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBaccumAtm_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtm,read_rest_FBaccumAtm,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBaccumOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumOcn,read_rest_FBaccumOcn,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBaccumIce_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumIce,read_rest_FBaccumIce,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBaccumLnd_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumLnd,read_rest_FBaccumLnd,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBaccumRof_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumRof,read_rest_FBaccumRof,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBaccumAtmOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtmOcn,read_rest_FBaccumAtmOcn,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fname = trim(bfname)//'_FBAtm_a_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBAtm_a,read_rest_FBAtm_a,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (mode == 'read') then
      call med_method_FB_copy(NState_AtmImp, is_local%wrap%FBAtm_a, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    fname = trim(bfname)//'_FBIce_i_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBIce_i,read_rest_FBIce_i,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (mode == 'read') then
      call med_method_FB_copy(NState_IceImp, is_local%wrap%FBIce_i, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    fname = trim(bfname)//'_FBOcn_o_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBOcn_o,read_rest_FBOCN_o,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (mode == 'read') then
      call med_method_FB_copy(NState_OcnImp, is_local%wrap%FBOcn_o, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    fname = trim(bfname)//'_FBLnd_l_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBLnd_l,read_rest_FBLnd_l,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (mode == 'read') then
      call med_method_FB_copy(NState_LndImp, is_local%wrap%FBLnd_l, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    fname = trim(bfname)//'_FBRof_h_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBRof_h,read_rest_FBRof_h,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (mode == 'read') then
      call med_method_FB_copy(NState_RofImp, is_local%wrap%FBRof_h, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    fname = trim(bfname)//'_FBAtmOcn_o_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBAtmOcn_o,read_rest_FBAtmOcn_o,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    funit = 1101
    fname = trim(bfname)//'_scalars_restart.txt'
    if (mode == 'write') then
      call ESMF_LogWrite(trim(subname)//": write "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
      open(funit,file=fname,form='formatted')
      write(funit,*) is_local%wrap%accumcntAtm
      write(funit,*) is_local%wrap%accumcntOcn
      write(funit,*) is_local%wrap%accumcntIce
      write(funit,*) is_local%wrap%accumcntAtmOcn
      write(funit,*) is_local%wrap%accumcntLnd
      write(funit,*) is_local%wrap%accumcntRof
      close(funit)
    elseif (mode == 'read') then
      inquire(file=fname,exist=fexists)
      if (fexists) then
        call ESMF_LogWrite(trim(subname)//": read "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
        open(funit,file=fname,form='formatted')
        ! DCR - temporary skip reading Lnd and Rof until components are added to test case
        !       restart files
        is_local%wrap%accumcntAtm=0
        is_local%wrap%accumcntOcn=0
        is_local%wrap%accumcntIce=0
        is_local%wrap%accumcntAtmOcn=0
        is_local%wrap%accumcntLnd=0
        is_local%wrap%accumcntRof=0
        read (funit,*) is_local%wrap%accumcntAtm
        read (funit,*) is_local%wrap%accumcntOcn
        read (funit,*) is_local%wrap%accumcntIce
        read (funit,*) is_local%wrap%accumcntAtmOcn
!        read (funit,*) is_local%wrap%accumcntLnd
!        read (funit,*) is_local%wrap%accumcntRof
        close(funit)
      else
        read_rest_FBaccumAtm = .false.
        read_rest_FBaccumOcn = .false.
        read_rest_FBaccumIce = .false.
        read_rest_FBaccumLnd = .false.
        read_rest_FBaccumRof = .false.
        read_rest_FBaccumAtmOcn = .false.
      endif
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Mediator_restart
#endif

  !-----------------------------------------------------------------------------

end module MED

