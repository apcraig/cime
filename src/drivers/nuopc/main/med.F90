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
  use shr_nuopc_methods_mod
  use seq_flds_mod
  use seq_infodata_mod, only: infodata=>seq_infodata_infodata
  use med_constants_mod
  use med_internalstate_mod
  use med_connectors_mod
  use med_phases_mod

  implicit none
  
  private
  
  integer            :: dbug_flag = med_constants_dbug_flag
  integer            :: dbrc
  integer            :: stat
  logical            :: isPresent
  character(len=1024):: msgString
  character(len=*)  , parameter :: grid_arbopt = "grid_reg"   ! grid_reg or grid_arb
  real(ESMF_KIND_R8), parameter :: spval_init  = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval       = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero       = med_constants_czero
  integer           , parameter :: ispval_mask = med_constants_ispval_mask
  character(*),parameter :: u_FILE_u = &
    __FILE__

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    !------------------
    ! the NUOPC model component will register the generic methods
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
    ! set mediator phase for fast accumulate
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"med_phases_accum_fast"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="med_phases_accum_fast", specRoutine=med_phases_accum_fast, rc=rc)
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
    character(len=10)          :: value

    rc = ESMF_SUCCESS

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
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    integer :: n
    type(InternalState)        :: is_local
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p1)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! Allocate memory for the internal state and set it in the Component.
    allocate(is_local%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      
    !------------------
    ! add a namespace
    !------------------

    call NUOPC_AddNamespace(importState, namespace="ATM", nestedStateName="NestedState-AtmImp", nestedState=is_local%wrap%NState_AtmImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="OCN", nestedStateName="NestedState-OcnImp", nestedState=is_local%wrap%NState_OcnImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ICE", nestedStateName="NestedState-IceImp", nestedState=is_local%wrap%NState_IceImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="LND", nestedStateName="NestedState-LndImp", nestedState=is_local%wrap%NState_LndImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ROF", nestedStateName="NestedState-RofImp", nestedState=is_local%wrap%NState_RofImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="WAV", nestedStateName="NestedState-WavImp", nestedState=is_local%wrap%NState_WavImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="GLC", nestedStateName="NestedState-GlcImp", nestedState=is_local%wrap%NState_GlcImp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ATM", nestedStateName="NestedState-AtmExp", nestedState=is_local%wrap%NState_AtmExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="OCN", nestedStateName="NestedState-OcnExp", nestedState=is_local%wrap%NState_OcnExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ICE", nestedStateName="NestedState-IceExp", nestedState=is_local%wrap%NState_IceExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="LND", nestedStateName="NestedState-LndExp", nestedState=is_local%wrap%NState_LndExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ROF", nestedStateName="NestedState-RofExp", nestedState=is_local%wrap%NState_RofExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="WAV", nestedStateName="NestedState-WavExp", nestedState=is_local%wrap%NState_WavExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="GLC", nestedStateName="NestedState-GlcExp", nestedState=is_local%wrap%NState_GlcExp, rc=rc)

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
    call shr_nuopc_fldList_Zero(fldsFrWav, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToWav, rc=rc)
    call shr_nuopc_fldList_Zero(fldsFrGlc, rc=rc)
    call shr_nuopc_fldList_Zero(fldsToGlc, rc=rc)
    call shr_nuopc_fldList_Zero(fldsAtmOcn, rc=rc)

    !------------------
    ! fldsToAtm
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_states, "cannot provide", subname//":seq_flds_x2a_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_fluxes, "cannot provide", subname//":seq_flds_x2a_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_fluxes, "cannot provide", subname//":seq_flds_x2a_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToAtm, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrAtm
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_states, "cannot provide", subname//":seq_flds_a2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_fluxes, "cannot provide", subname//":seq_flds_a2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_fluxes, "cannot provide", subname//":seq_flds_a2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrAtm, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsToOcn
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToOcn, seq_flds_x2o_states, "cannot provide", subname//":seq_flds_x2o_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToOcn, seq_flds_x2o_fluxes, "cannot provide", subname//":seq_flds_x2o_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToOcn, seq_flds_x2o_fluxes, "cannot provide", subname//":seq_flds_x2o_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToOcn, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrOcn
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrOcn, seq_flds_o2x_states, "cannot provide", subname//":seq_flds_o2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrOcn, seq_flds_o2x_fluxes, "cannot provide", subname//":seq_flds_o2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrOcn, seq_flds_o2x_fluxes, "cannot provide", subname//":seq_flds_o2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrOcn, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsToIce
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToIce, seq_flds_x2i_states, "cannot provide", subname//":seq_flds_x2i_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToIce, seq_flds_x2i_fluxes, "cannot provide", subname//":seq_flds_x2i_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToIce, seq_flds_x2i_fluxes, "cannot provide", subname//":seq_flds_x2i_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToIce, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrIce
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrIce, seq_flds_i2x_states, "cannot provide", subname//":seq_flds_i2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrIce, seq_flds_i2x_fluxes, "cannot provide", subname//":seq_flds_i2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrIce, seq_flds_i2x_fluxes, "cannot provide", subname//":seq_flds_i2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrIce, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsToLnd
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToLnd, seq_flds_x2l_states, "cannot provide", subname//":seq_flds_x2l_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToLnd, seq_flds_x2l_fluxes, "cannot provide", subname//":seq_flds_x2l_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToLnd, seq_flds_x2l_fluxes, "cannot provide", subname//":seq_flds_x2l_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToLnd, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrLnd
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrLnd, seq_flds_l2x_states, "cannot provide", subname//":seq_flds_l2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrLnd, seq_flds_l2x_fluxes, "cannot provide", subname//":seq_flds_l2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrLnd, seq_flds_l2x_fluxes, "cannot provide", subname//":seq_flds_l2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrLnd, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsToRof
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToRof, seq_flds_x2r_states, "cannot provide", subname//":seq_flds_x2r_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToRof, seq_flds_x2r_fluxes, "cannot provide", subname//":seq_flds_x2r_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToRof, seq_flds_x2r_fluxes, "cannot provide", subname//":seq_flds_x2r_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToRof, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrRof
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrRof, seq_flds_r2x_states, "cannot provide", subname//":seq_flds_r2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrRof, seq_flds_r2x_fluxes, "cannot provide", subname//":seq_flds_r2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrRof, seq_flds_r2x_fluxes, "cannot provide", subname//":seq_flds_r2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrRof, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsToWav
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToWav, seq_flds_x2r_states, "cannot provide", subname//":seq_flds_x2r_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToWav, seq_flds_x2r_fluxes, "cannot provide", subname//":seq_flds_x2r_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToWav, seq_flds_x2r_fluxes, "cannot provide", subname//":seq_flds_x2r_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToWav, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrWav
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrWav, seq_flds_r2x_states, "cannot provide", subname//":seq_flds_r2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrWav, seq_flds_r2x_fluxes, "cannot provide", subname//":seq_flds_r2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrWav, seq_flds_r2x_fluxes, "cannot provide", subname//":seq_flds_r2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrWav, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsToGlc
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsToGlc, seq_flds_x2r_states, "cannot provide", subname//":seq_flds_x2r_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsToGlc, seq_flds_x2r_fluxes, "cannot provide", subname//":seq_flds_x2r_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsToGlc, seq_flds_x2r_fluxes, "cannot provide", subname//":seq_flds_x2r_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsToGlc, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! fldsFrGlc
    !------------------

    call shr_nuopc_fldList_fromseqflds(fldsFrGlc, seq_flds_r2x_states, "cannot provide", subname//":seq_flds_r2x_states", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
!    call shr_nuopc_fldList_fromseqflds(fldsFrGlc, seq_flds_r2x_fluxes, "cannot provide", subname//":seq_flds_r2x_fluxes", "conservefrac", rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_fromseqflds(fldsFrGlc, seq_flds_r2x_fluxes, "cannot provide", subname//":seq_flds_r2x_fluxes", "bilinear", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Add(fldsFrGlc, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !------------------
    ! Advertise
    !------------------

    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_AtmImp, fldsFrAtm, subname//':FrAtm', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_LndImp, fldsFrLnd, subname//':FrLnd', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_OcnImp, fldsFrOcn, subname//':FrOcn', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_IceImp, fldsFrIce, subname//':FrIce', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_RofImp, fldsFrRof, subname//':FrRof', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_WavImp, fldsFrWav, subname//':FrWav', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_GlcImp, fldsFrGlc, subname//':FrGlc', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_AtmExp, fldsToAtm, subname//':ToAtm', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_LndExp, fldsToLnd, subname//':ToLnd', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_OcnExp, fldsToOcn, subname//':ToOcn', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_IceExp, fldsToIce, subname//':ToIce', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_RofExp, fldsToRof, subname//':ToRof', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_WavExp, fldsToWav, subname//':ToWav', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Advertise(is_local%wrap%NState_GlcExp, fldsToGlc, subname//':ToGlc', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

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
    integer                    :: i, j
    real(kind=ESMF_KIND_R8),pointer :: lonPtr(:), latPtr(:)
    type(ESMF_VM)              :: vm
    type(InternalState)        :: is_local
    integer                    :: lmpicom
    real(ESMF_KIND_R8)         :: intervalSec
    type(ESMF_TimeInterval)    :: timeStep
! tcx XGrid
!    type(ESMF_Field)            :: fieldX, fieldA, fieldO
!    type(ESMF_XGrid)            :: xgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p3)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! Initialize the internal state members
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call ESMF_VMGet(vm, mpiCommunicator=lmpicom, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call MPI_Comm_Dup(lmpicom, is_local%wrap%mpicom, stat)

    is_local%wrap%atmcntr = 0
    is_local%wrap%ocncntr = 0
    is_local%wrap%icecntr = 0
    is_local%wrap%lndcntr = 0
    is_local%wrap%rofcntr = 0
    is_local%wrap%wavcntr = 0
    is_local%wrap%glccntr = 0

! tcraig hardwire 1 degree grid as backup option
!    gridMed = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360,180/), &
!      minCornerCoord=(/0._ESMF_KIND_R8, -90._ESMF_KIND_R8/), &
!      maxCornerCoord=(/360._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
!      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

!    gridLnd = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

!    gridRof = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !--- Generate RouteHandles
! tcx Xgrid
! what needs to be in the grids to create an XGrid (corners?)
! add error checking code

!    xgrid = ESMF_XGridCreate(sideAGrid=(/gridatm/), sideBGrid=(/gridocn/), rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
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

    call shr_nuopc_fldList_Realize(is_local%wrap%NState_AtmImp, fldlist=fldsFrAtm, tag=subname//':FrAtm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_LndImp, fldlist=fldsFrLnd, tag=subname//':FrLnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_OcnImp, fldlist=fldsFrOcn, tag=subname//':FrOcn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_IceImp, fldlist=fldsFrIce, tag=subname//':FrIce', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_RofImp, fldlist=fldsFrRof, tag=subname//':FrRof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_WavImp, fldlist=fldsFrWav, tag=subname//':FrWav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_GlcImp, fldlist=fldsFrGlc, tag=subname//':FrGlc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_fldList_Realize(is_local%wrap%NState_AtmExp, fldlist=fldsToAtm, tag=subname//':ToAtm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_LndExp, fldlist=fldsToLnd, tag=subname//':ToLnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_OcnExp, fldlist=fldsToOcn, tag=subname//':ToOcn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_IceExp, fldlist=fldsToIce, tag=subname//':ToIce', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_RofExp, fldlist=fldsToRof, tag=subname//':ToRof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_WavExp, fldlist=fldsToWav, tag=subname//':ToWav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_fldList_Realize(is_local%wrap%NState_GlcExp, fldlist=fldsToGlc, tag=subname//':ToGlc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeIPDv03p3
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p4(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(InternalState)        :: is_local
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

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call realizeConnectedGrid(is_local%wrap%NState_atmImp, 'AtmImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_atmExp, 'AtmExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_ocnImp, 'OcnImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_ocnExp, 'OcnExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_iceImp, 'IceImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_iceExp, 'IceExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_lndImp, 'LndImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_lndExp, 'LndExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_rofImp, 'RofImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_rofExp, 'RofExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_wavImp, 'WavImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_wavExp, 'WavExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_glcImp, 'GlcImp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call realizeConnectedGrid(is_local%wrap%NState_glcExp, 'GlcExp', rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

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

!      do n=1, fieldCount
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
                  call ESMF_LogWrite(trim(subname)//trim(string)//": accept arb2reg grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
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
                  call ESMF_LogWrite(trim(subname)//trim(string)//": accept arb2arb grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
                endif

              else   ! grid_arbopt

                call ESMF_LogWrite(trim(subname)//trim(string)//": ERROR grid_arbopt setting = "//trim(grid_arbopt), ESMF_LOGMSG_INFO, rc=rc)
                rc = ESMF_FAILURE
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

              endif  ! grid_arbopt


            else   ! arbdimcount <= 0

              ! The provider defined as non arb grid

              ! access localDeCount to show this is a real Grid
              if (dbug_flag > 1) then
                call ESMF_LogWrite(trim(subname)//trim(string)//": accept reg2reg grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
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
                call ESMF_LogWrite(trim(subname)//trim(string)//": attach grid for "//trim(fieldNameList(n1)), ESMF_LOGMSG_INFO, rc=rc)
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

        else

          call ESMF_LogWrite(trim(subname)//": ERROR fieldStatus not supported ", ESMF_LOGMSG_INFO, rc=rc)
          rc=ESMF_FAILURE
          return

        endif   ! fieldStatus

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
    integer(ESMF_KIND_I4), pointer :: dataPtr_arrayOcn(:), dataPtr_arrayIce(:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fieldOcn(:), dataPtr_fieldAtm(:)
    logical                     :: isPresentOcn, isPresentIce
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
    !----------------------------------------------------------

    call completeFieldInitialization(is_local%wrap%NState_atmImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_atmImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_atmExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_atmExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_ocnImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_ocnImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_ocnExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_ocnExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_iceImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_iceImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_iceExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_iceExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_lndImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_lndImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_lndExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_lndExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_rofImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_rofImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_rofExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_rofExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_wavImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_wavImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_wavExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_wavExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_glcImp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_glcImp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call completeFieldInitialization(is_local%wrap%NState_glcExp, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    call shr_nuopc_methods_State_reset(is_local%wrap%NState_glcExp, value=spval_init, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    !--- Diagnose Grid Info
    !----------------------------------------------------------

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_atmExp,'gridAtmExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_ocnExp,'gridOcnExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_iceExp,'gridIceExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_lndExp,'gridLndExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_rofExp,'gridRofExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_wavExp,'gridWavExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomPrint(is_local%wrap%NState_glcExp,'gridGlcExp',rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    ! dump the Grid coordinate arrays for reference      
    !----------------------------------------------------------

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_AtmExp, 'array_med_atm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_OcnExp, 'array_med_ocn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_IceExp, 'array_med_ice', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_LndExp, 'array_med_lnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_RofExp, 'array_med_rof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_WavExp, 'array_med_wav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_State_GeomWrite(is_local%wrap%NState_GlcExp, 'array_med_glc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    ! NOW allocate other Mediator datatypes
    !----------------------------------------------------------

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    ! Initialize FB for each model import states on each grid
    !----------------------------------------------------------

    !--- atm

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtm_a, STgeom=is_local%wrap%NState_AtmImp, &
      STflds=is_local%wrap%NState_AtmImp, name='FBAtm_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtm_o, STgeom=is_local%wrap%NState_OcnExp, &
      STflds=is_local%wrap%NState_AtmImp, name='FBAtm_o', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtm_i, STgeom=is_local%wrap%NState_IceExp, &
      STflds=is_local%wrap%NState_AtmImp, name='FBAtm_i', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtm_l, STgeom=is_local%wrap%NState_LndExp, &
      STflds=is_local%wrap%NState_AtmImp, name='FBAtm_l', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtm_r, STgeom=is_local%wrap%NState_RofExp, &
      STflds=is_local%wrap%NState_AtmImp, name='FBAtm_r', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !--- ocn

    call shr_nuopc_methods_FB_init(is_local%wrap%FBOcn_a, STgeom=is_local%wrap%NState_AtmExp, &
      STflds=is_local%wrap%NState_OcnImp, name='FBOcn_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBOcn_o, STgeom=is_local%wrap%NState_OcnImp, &
      STflds=is_local%wrap%NState_OcnImp, name='FBOcn_o', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBOcn_i, STgeom=is_local%wrap%NState_IceExp, &
      STflds=is_local%wrap%NState_OcnImp, name='FBOcn_i', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !--- ice

    call shr_nuopc_methods_FB_init(is_local%wrap%FBIce_a, STgeom=is_local%wrap%NState_AtmExp, &
      STflds=is_local%wrap%NState_IceImp, name='FBIce_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBIce_o, STgeom=is_local%wrap%NState_OcnExp, &
      STflds=is_local%wrap%NState_IceImp, name='FBIce_o', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBIce_i, STgeom=is_local%wrap%NState_IceImp, &
      STflds=is_local%wrap%NState_IceImp, name='FBIce_i', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBIce_if, STgeom=is_local%wrap%NState_IceImp, &
      STflds=is_local%wrap%NState_IceImp, name='FBIce_if', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !--- lnd

    call shr_nuopc_methods_FB_init(is_local%wrap%FBLnd_a, STgeom=is_local%wrap%NState_AtmExp, &
      STflds=is_local%wrap%NState_LndImp, name='FBLnd_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBLnd_l, STgeom=is_local%wrap%NState_LndImp, &
      STflds=is_local%wrap%NState_LndImp, name='FBLnd_l', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBLnd_r, STgeom=is_local%wrap%NState_RofExp, &
      STflds=is_local%wrap%NState_LndImp, name='FBLnd_r', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !--- rof

    call shr_nuopc_methods_FB_init(is_local%wrap%FBRof_l, STgeom=is_local%wrap%NState_LndExp, &
      STflds=is_local%wrap%NState_RofImp, name='FBRof_l', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBRof_a, STgeom=is_local%wrap%NState_AtmExp, &
      STflds=is_local%wrap%NState_RofImp, name='FBRof_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBRof_r, STgeom=is_local%wrap%NState_RofImp, &
      STflds=is_local%wrap%NState_RofImp, name='FBRof_r', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    ! Initialize Accumulators
    !----------------------------------------------------------

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumAtm, STgeom=is_local%wrap%NState_AtmImp, &
      STflds=is_local%wrap%NState_AtmImp, name='FBaccumAtm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumOcn, STgeom=is_local%wrap%NState_OcnImp, &
      STflds=is_local%wrap%NState_OcnImp, name='FBaccumOcn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumIce, STgeom=is_local%wrap%NState_IceImp, &
      STflds=is_local%wrap%NState_IceImp, name='FBaccumIce', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumLnd, STgeom=is_local%wrap%NState_LndImp, &
      STflds=is_local%wrap%NState_LndImp, name='FBaccumLnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumRof, STgeom=is_local%wrap%NState_RofImp, &
      STflds=is_local%wrap%NState_RofImp, name='FBaccumRof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumWav, STgeom=is_local%wrap%NState_WavImp, &
      STflds=is_local%wrap%NState_WavImp, name='FBaccumWav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumGlc, STgeom=is_local%wrap%NState_GlcImp, &
      STflds=is_local%wrap%NState_GlcImp, name='FBaccumGlc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    ! Initialize AtmOcn FBs
    !----------------------------------------------------------

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtmOcn_o, STgeom=is_local%wrap%NState_OcnExp, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBAtmOcn_o', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBAtmOcn_a, STgeom=is_local%wrap%NState_AtmExp, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBAtmOcn_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBaccumAtmOcn, STgeom=is_local%wrap%NState_OcnExp, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBaccumAtmOcn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    ! Initialize FB for export to models
    !----------------------------------------------------------

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforAtm, &
      STgeom=is_local%wrap%NState_AtmExp, name='FBforAtm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforOcn, &
      STgeom=is_local%wrap%NState_OcnExp, name='FBforOcn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforIce, &
      STgeom=is_local%wrap%NState_IceExp, name='FBforIce', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforLnd, &
      STgeom=is_local%wrap%NState_LndExp, name='FBforLnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforRof, &
      STgeom=is_local%wrap%NState_RofExp, name='FBforRof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforWav, &
      STgeom=is_local%wrap%NState_WavExp, name='FBforWav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    call shr_nuopc_methods_FB_init(is_local%wrap%FBforGlc, &
      STgeom=is_local%wrap%NState_GlcExp, name='FBforGlc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    !----------------------------------------------------------
    !--- Check for active regrid directions
    !----------------------------------------------------------
    
    ! initialize
    is_local%wrap%a2o_active = .false.
    is_local%wrap%a2i_active = .false.
    is_local%wrap%a2l_active = .false.
    is_local%wrap%a2r_active = .false.
    is_local%wrap%o2a_active = .false.
    is_local%wrap%o2i_active = .false.
    is_local%wrap%i2a_active = .false.
    is_local%wrap%i2o_active = .false.
    is_local%wrap%l2a_active = .false.
    is_local%wrap%l2r_active = .false.
    is_local%wrap%r2l_active = .false.
    is_local%wrap%r2a_active = .false.

    ! a2o, a2i, a2l, a2r
    call ESMF_FieldBundleGet(is_local%wrap%FBAtm_a, fieldCount=fieldCount, rc=rc) ! Atmosphere Export Field Count
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforOcn, fieldCount=fieldCount, rc=rc) ! Ocean Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2o_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforIce, fieldCount=fieldCount, rc=rc) ! Ice Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2i_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforLnd, fieldCount=fieldCount, rc=rc) ! Land Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2l_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforRof, fieldCount=fieldCount, rc=rc) ! Rof Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%a2r_active = .true.
      endif
    endif
    
    ! o2a, o2i
    call ESMF_FieldBundleGet(is_local%wrap%FBOcn_o, fieldCount=fieldCount, rc=rc) ! Ocean Export Field Count
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%o2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforIce, fieldCount=fieldCount, rc=rc) ! Ice Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%o2i_active = .true.
      endif
    endif
    
    ! i2a, i2o
    call ESMF_FieldBundleGet(is_local%wrap%FBIce_i, fieldCount=fieldCount, rc=rc) ! Ice Export Field Count
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%i2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforOcn, fieldCount=fieldCount, rc=rc) ! Ocean Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%i2o_active = .true.
      endif
    endif
    
    ! l2a, l2r
    call ESMF_FieldBundleGet(is_local%wrap%FBLnd_l, fieldCount=fieldCount, rc=rc) ! Land Export Field Count
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%l2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforRof, fieldCount=fieldCount, rc=rc) ! Rof Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%l2r_active = .true.
      endif
    endif

    ! r2l, r2a
    call ESMF_FieldBundleGet(is_local%wrap%FBRof_r, fieldCount=fieldCount, rc=rc) ! Rof Export Field Count
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforLnd, fieldCount=fieldCount, rc=rc) ! Land Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%r2l_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (fieldCount > 0) then
        is_local%wrap%r2a_active = .true.
      endif
    endif

    write(msgString,*) is_local%wrap%a2o_active
    call ESMF_LogWrite(trim(subname)//": a2o active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2i_active
    call ESMF_LogWrite(trim(subname)//": a2i active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2l_active
    call ESMF_LogWrite(trim(subname)//": a2l active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2r_active
    call ESMF_LogWrite(trim(subname)//": a2r active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

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
    write(msgString,*) is_local%wrap%l2r_active
    call ESMF_LogWrite(trim(subname)//": l2r active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%r2l_active
    call ESMF_LogWrite(trim(subname)//": r2l active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%r2a_active
    call ESMF_LogWrite(trim(subname)//": r2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------
    !--- Initialize route handles
    !----------------------------------------------------------

    if (dbug_flag > 1) then
      call ESMF_LogWrite("Starting to initialize RHs", ESMF_LOGMSG_INFO)
      call ESMF_LogFlush()
    endif

    if (is_local%wrap%a2o_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_o, &
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
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%a2i_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_i, &
        bilnrmap=is_local%wrap%RH_a2i_bilnr, &
        consfmap=is_local%wrap%RH_a2i_consf, &
        consdmap=is_local%wrap%RH_a2i_consd, &
        patchmap=is_local%wrap%RH_a2i_patch, &
        fcopymap=is_local%wrap%RH_a2i_fcopy, &
        dstMaskValue=0, &
        fldlist1=FldsFrAtm, string='a2i_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%a2l_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_l, &
        bilnrmap=is_local%wrap%RH_a2l_bilnr, &
        consfmap=is_local%wrap%RH_a2l_consf, &
        consdmap=is_local%wrap%RH_a2l_consd, &
        patchmap=is_local%wrap%RH_a2l_patch, &
        fcopymap=is_local%wrap%RH_a2l_fcopy, &
        fldlist1=FldsFrAtm, string='a2l_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%a2r_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_r, &
        bilnrmap=is_local%wrap%RH_a2r_bilnr, &
        consfmap=is_local%wrap%RH_a2r_consf, &
        consdmap=is_local%wrap%RH_a2r_consd, &
        patchmap=is_local%wrap%RH_a2r_patch, &
        fcopymap=is_local%wrap%RH_a2r_fcopy, &
        fldlist1=FldsFrAtm, string='a2r_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%o2a_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBOcn_o, FBdst=is_local%wrap%FBOcn_a, &
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
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%o2i_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBOcn_o, FBdst=is_local%wrap%FBOcn_i, &
        bilnrmap=is_local%wrap%RH_o2i_bilnr, &
        consfmap=is_local%wrap%RH_o2i_consf, &
        consdmap=is_local%wrap%RH_o2i_consd, &
        patchmap=is_local%wrap%RH_o2i_patch, &
        fcopymap=is_local%wrap%RH_o2i_fcopy, &
        srcMaskValue=0, dstMaskValue=0, &
        fldlist1=FldsFrOcn, string='o2i_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%i2a_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBIce_i, FBdst=is_local%wrap%FBIce_a, &
        bilnrmap=is_local%wrap%RH_i2a_bilnr, &
        consfmap=is_local%wrap%RH_i2a_consf, &
        consdmap=is_local%wrap%RH_i2a_consd, &
        patchmap=is_local%wrap%RH_i2a_patch, &
        fcopymap=is_local%wrap%RH_i2a_fcopy, &
        srcMaskValue=0, &
        fldlist1=FldsFrIce, string='i2a_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%i2o_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBIce_i, FBdst=is_local%wrap%FBIce_o, &
        bilnrmap=is_local%wrap%RH_i2o_bilnr, &
        consfmap=is_local%wrap%RH_i2o_consf, &
        consdmap=is_local%wrap%RH_i2o_consd, &
        patchmap=is_local%wrap%RH_i2o_patch, &
        fcopymap=is_local%wrap%RH_i2o_fcopy, &
        srcMaskValue=0, dstMaskValue=0, &
        fldlist1=FldsFrIce, string='i2o_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%l2a_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBLnd_l, FBdst=is_local%wrap%FBLnd_a, &
        bilnrmap=is_local%wrap%RH_l2a_bilnr, &
        consfmap=is_local%wrap%RH_l2a_consf, &
        consdmap=is_local%wrap%RH_l2a_consd, &
        patchmap=is_local%wrap%RH_l2a_patch, &
        fcopymap=is_local%wrap%RH_l2a_fcopy, &
        fldlist1=FldsFrLnd, string='l2a_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%l2r_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBLnd_l, FBdst=is_local%wrap%FBLnd_r, &
        bilnrmap=is_local%wrap%RH_l2r_bilnr, &
        consfmap=is_local%wrap%RH_l2r_consf, &
        consdmap=is_local%wrap%RH_l2r_consd, &
        patchmap=is_local%wrap%RH_l2r_patch, &
        fcopymap=is_local%wrap%RH_l2r_fcopy, &
        fldlist1=FldsFrLnd, string='l2r_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%r2a_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBRof_r, FBdst=is_local%wrap%FBRof_a, &
        bilnrmap=is_local%wrap%RH_r2a_bilnr, &
        consfmap=is_local%wrap%RH_r2a_consf, &
        consdmap=is_local%wrap%RH_r2a_consd, &
        patchmap=is_local%wrap%RH_r2a_patch, &
        fcopymap=is_local%wrap%RH_r2a_fcopy, &
        fldlist1=FldsFrRof, string='r2a_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (is_local%wrap%r2l_active) then
      call shr_nuopc_methods_RH_Init(FBsrc=is_local%wrap%FBRof_r, FBdst=is_local%wrap%FBRof_l, &
        bilnrmap=is_local%wrap%RH_r2l_bilnr, &
        consfmap=is_local%wrap%RH_r2l_consf, &
        consdmap=is_local%wrap%RH_r2l_consd, &
        patchmap=is_local%wrap%RH_r2l_patch, &
        fcopymap=is_local%wrap%RH_r2l_fcopy, &
        fldlist1=FldsFrRof, string='r2l_weights', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine completeFieldInitialization(State,rc)

      type(ESMF_State)   , intent(inout) :: State
      integer            , intent(out)   :: rc
    
      integer                     :: n, fieldCount
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      type(ESMF_FieldStatus_Flag) :: fieldStatus 
      character(len=*),parameter  :: subname='(module_MEDIATOR:completeFieldInitialization)'

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      do n=1, fieldCount

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
        call ESMF_FieldGet(field, status=fieldStatus, rc=rc) 
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

        if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then 
          if (dbug_flag > 1) then
            call ESMF_LogWrite(subname//" is allocating field memory for field "//trim(fieldNameList(n)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
          endif
          call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
        endif   ! fieldStatus

        call shr_nuopc_methods_Field_GeomPrint(field, trim(subname)//':'//trim(fieldNameList(n)), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

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
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    
    ! initialze cumulative flag
    allDone = .true.  ! reset if an item is found that is not done

    ! check that all imported fields from ATM show correct timestamp
    do n = 1,fldsFrAtm%num
      call ESMF_StateGet(is_local%wrap%NState_AtmImp, itemName=fldsFrAtm%shortname(n), itemType=itemType, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        connected = NUOPC_IsConnected(is_local%wrap%NState_AtmImp, fieldName=fldsFrAtm%shortname(n), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
        if (connected) then
          call ESMF_StateGet(is_local%wrap%NState_AtmImp, itemName=fldsFrAtm%shortname(n), field=field, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
          atCorrectTime = NUOPC_IsAtTime(field, time, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
          if (.not.atCorrectTime) then
            call ESMF_LogWrite("MED - Initialize-Data-Dependency NOT YET SATISFIED!!!", ESMF_LOGMSG_INFO, rc=rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
            allDone = .false.
            exit  ! break out of the loop when first not satisfied found
          else
            call ESMF_LogWrite("MED - Initialize-Data-Dependency SATISFIED!!!", ESMF_LOGMSG_INFO, rc=rc)
            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
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
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      ! gjt: The above code ensures that the MED has initial conditions from ATM.
    
      ! TODO - For the real case this should probably use the fields from the
      ! importState and do something with it as a sensible starting point
      ! for the accumulation field so that the OCN receives a meaningful
      ! fields during its first time step. However, here for testing
      ! I simply initialize to zero.

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_atmImp,infodata,'atm2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_atmImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_ocnImp,infodata,'ocn2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_ocnImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_iceImp,infodata,'ice2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_iceImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_lndImp,infodata,'lnd2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_lndImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_rofImp,infodata,'rof2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_rofImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_wavImp,infodata,'wav2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_wavImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_glcImp,infodata,'glc2cpli',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_State_reset(is_local%wrap%NState_glcImp, value=spval_init, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumAtm, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntAtm = 0

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumOcn, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntOcn = 0

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumIce, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntIce = 0

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumLnd, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntLnd = 0

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumRof, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntRof = 0

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumWav, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntWav = 0

      call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumGlc, value=czero, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      is_local%wrap%accumcntGlc = 0

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
      call ESMF_StateGet(is_local%wrap%NState_IceExp, itemName='s_surf', itemType=itemType, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        if (NUOPC_IsConnected(is_local%wrap%NState_IceExp,'s_surf',rc=rc)) then
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
          call State_SetFldPtr(is_local%wrap%NState_IceExp, 's_surf', 34.0_ESMF_KIND_R8, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
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
      call ESMF_LogWrite(trim(subname)//": ERROR mode not allowed "//trim(mode), ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumAtm_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtm,read_rest_FBaccumAtm,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumOcn,read_rest_FBaccumOcn,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumIce_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumIce,read_rest_FBaccumIce,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumLnd_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumLnd,read_rest_FBaccumLnd,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumRof_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumRof,read_rest_FBaccumRof,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumWav_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumWav,read_rest_FBaccumWav,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumGlc_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumGlc,read_rest_FBaccumGlc,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBaccumAtmOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtmOcn,read_rest_FBaccumAtmOcn,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 

    fname = trim(bfname)//'_FBAtm_a_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBAtm_a,read_rest_FBAtm_a,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_AtmImp, is_local%wrap%FBAtm_a, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    fname = trim(bfname)//'_FBIce_i_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBIce_i,read_rest_FBIce_i,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_IceImp, is_local%wrap%FBIce_i, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    fname = trim(bfname)//'_FBOcn_o_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBOcn_o,read_rest_FBOCN_o,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_OcnImp, is_local%wrap%FBOcn_o, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    fname = trim(bfname)//'_FBLnd_l_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBLnd_l,read_rest_FBLnd_l,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_LndImp, is_local%wrap%FBLnd_l, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    fname = trim(bfname)//'_FBRof_r_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBRof_r,read_rest_FBRof_r,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_RofImp, is_local%wrap%FBRof_r, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    fname = trim(bfname)//'_FBWav_r_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBWav_r,read_rest_FBWav_r,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_WavImp, is_local%wrap%FBWav_r, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    endif

    fname = trim(bfname)//'_FBGlc_r_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBGlc_r,read_rest_FBGlc_r,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return 
    if (mode == 'read') then
      call shr_nuopc_methods_FB_copy(is_local%wrap%NState_GlcImp, is_local%wrap%FBGlc_r, rc=rc)
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
      write(funit,*) is_local%wrap%accumcntAtm
      write(funit,*) is_local%wrap%accumcntOcn
      write(funit,*) is_local%wrap%accumcntIce
      write(funit,*) is_local%wrap%accumcntAtmOcn
      write(funit,*) is_local%wrap%accumcntLnd
      write(funit,*) is_local%wrap%accumcntRof
      write(funit,*) is_local%wrap%accumcntWav
      write(funit,*) is_local%wrap%accumcntGlc
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
        is_local%wrap%accumcntWav=0
        is_local%wrap%accumcntGlc=0
        read (funit,*) is_local%wrap%accumcntAtm
        read (funit,*) is_local%wrap%accumcntOcn
        read (funit,*) is_local%wrap%accumcntIce
        read (funit,*) is_local%wrap%accumcntAtmOcn
        read (funit,*) is_local%wrap%accumcntLnd
        read (funit,*) is_local%wrap%accumcntRof
        read (funit,*) is_local%wrap%accumcntWav
        read (funit,*) is_local%wrap%accumcntGlc
        close(funit)
      else
        read_rest_FBaccumAtm = .false.
        read_rest_FBaccumOcn = .false.
        read_rest_FBaccumIce = .false.
        read_rest_FBaccumLnd = .false.
        read_rest_FBaccumRof = .false.
        read_rest_FBaccumWav = .false.
        read_rest_FBaccumGlc = .false.
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

