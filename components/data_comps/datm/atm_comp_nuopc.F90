module atm_comp_nuopc

#ifdef NUOPC_INTERFACE 
  use shr_kind_mod, only:  R8=>SHR_KIND_R8, IN=>SHR_KIND_IN, &
       CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_sys_mod   ! shared system calls

  use seq_flds_mod
  use seq_cdata_mod
  use seq_infodata_mod
  use seq_timemgr_mod, only: seq_timemgr_EClockGetData, seq_timemgr_alarm_restart
  use shr_nuopc_fldList_mod
  use shr_nuopc_data_mod, only: n_ATMID, n_infodata
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_ClockTimePrint
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_dmodelgridinit
  use shr_file_mod,  only : shr_file_getlogunit, shr_file_getloglevel, shr_file_setloglevel

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance, &
    model_label_SetRunClock => label_SetRunClock, &
    model_label_Finalize  => label_Finalize

  use datm_comp_mod
  use perf_mod
  use mct_mod
  use med_method_mod

  implicit none

  public :: SetServices

  private ! except

  type (shr_nuopc_fldList_Type) :: fldsToAtm
  type (shr_nuopc_fldList_Type) :: fldsFrAtm

  type(seq_cdata)         :: cdata
  type(seq_infodata_type) :: infodata
  type(mct_gsMap)         :: gsmap
  type(mct_gGrid)         :: ggrid
  type(mct_aVect)         :: x2d
  type(mct_aVect)         :: d2x
  integer                 :: mpicom, iam
  integer                 :: dbrc
  character(len=1024)     :: tmpstr
  character(len=*),parameter :: grid_option = "mesh"  ! grid_de, grid_arb, grid_reg, mesh
  integer, parameter      :: dbug = 2

  !----- formats -----
  character(*),parameter :: modName =  "(atm_comp_nuopc)"

  !===============================================================================
  contains
  !===============================================================================

  subroutine SetServices(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
#if (1 == 0)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_MethodRemove(gcomp, label=model_label_SetRunClock, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, &
      specRoutine=ModelSetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    character(len=10)     :: value
    type(ESMF_VM)         :: vm
    integer               :: lpet

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitializeP0
  
  !===============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(len=*),parameter  :: subname=trim(modName)//':(InitializeAdvertise) '
    
    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! create import fields list
    call shr_nuopc_fldList_Zero(fldsToAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_states, "will provide", subname//":seq_flds_x2a_states", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_fluxes, "will provide", subname//":seq_flds_x2a_fluxes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_Add(fldsToAtm, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    ! create export fields list
    call shr_nuopc_fldList_Zero(fldsFrAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_states, "will provide", subname//":seq_flds_a2x_states", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_fluxes, "will provide", subname//":seq_flds_a2x_fluxes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_Add(fldsFrAtm, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    ! advertise import and export

    call shr_nuopc_fldList_Advertise(importState, fldsToAtm, subname//':datmImport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_Advertise(exportState, fldsFrAtm, subname//':datmExport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine InitializeAdvertise
  
  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer(IN)      :: MYID
    character(CL)    :: NLFilename
    integer(IN)      :: phase
    character(ESMF_MAXSTR) :: convCIM, purpComp
    type(ESMF_Grid)  :: Egrid
    type(ESMF_Mesh)  :: Emesh
    integer          :: nx_global, ny_global
    real(r8)         :: nextsw_cday
    character(len=*),parameter :: subname=trim(modName)//':(InitializeRealize) '

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    call shr_file_getLogUnit (logunit)

    NLFilename = 'unused'
    MYID = n_ATMID(1)
    phase = 1

    if (phase == 1) then
       call seq_cdata_init(cdata,MYID,ggrid,gsmap,n_infodata,'datm')
       call seq_cdata_setptrs(cdata,mpicom=mpicom)
       call MPI_COMM_RANK(mpicom, iam, rc)
    else
       call state2avect(importState,x2d,rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    call datm_comp_init(clock, cdata, x2d, d2x, NLFilename)

    call seq_infodata_GetData(n_infodata, atm_nx=nx_global, atm_ny=ny_global)
    call shr_nuopc_stuff_dmodelgridinit(nx_global,ny_global,mpicom,gsMap,ggrid,grid_option,EGrid,Emesh,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (grid_option == 'mesh') then

      call shr_nuopc_fldList_Realize(importState, mesh=Emesh, fldlist=fldsToAtm, tag=subname//':datmImport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call shr_nuopc_fldList_Realize(exportState, mesh=Emesh, fldlist=fldsFrAtm, tag=subname//':datmExport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    else

      call shr_nuopc_fldList_Realize(importState, grid=Egrid, fldlist=fldsToAtm, tag=subname//':datmImport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call shr_nuopc_fldList_Realize(exportState, grid=Egrid, fldlist=fldsFrAtm, tag=subname//':datmExport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    endif

    ! Pack export state

    call avect2state(d2x,exportState,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    call med_method_State_SetScalar(1.0_r8,         seq_flds_scalar_index_present, exportState, mpicom, rc)
    call med_method_State_SetScalar(0.0_r8,         seq_flds_scalar_index_prognostic, exportState, mpicom, rc)
    call med_method_State_SetScalar(dble(ny_global),seq_flds_scalar_index_nx, exportState, mpicom, rc)
    call med_method_State_SetScalar(dble(nx_global),seq_flds_scalar_index_ny, exportState, mpicom, rc)
    call med_method_State_SetScalar(0.0_r8,         seq_flds_scalar_index_dead_comps, exportState, mpicom, rc)
    call seq_infodata_GetData(n_infodata, nextsw_cday=nextsw_cday)
    call med_method_State_SetScalar(nextsw_cday,    seq_flds_scalar_index_nextsw_cday, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    ! diagnostics
    call mct_aVect_info(2, d2x, istr=subname//':AV')
    call med_method_State_diagnose(exportState,subname//':ES',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

#ifdef USE_ESMF_METADATA
    convCIM  = "CIM"
    purpComp = "Model Component Simulation Description"

    call ESMF_AttributeAdd(comp,  &
         convention=convCIM, purpose=purpComp, rc=rc)

    call ESMF_AttributeSet(comp, "ShortName", "DATM", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "LongName", &
         "Climatological Atmosphere Data Model", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "Description", &
         "The CESM data models perform the basic function of " // &
         "reading external data, modifying that data, and then " // &
         "sending it to the driver via standard CESM coupling " // &
         "interfaces. The driver and other models have no " // &
         "fundamental knowledge of whether another component " // &
         "is fully active or just a data model.  In some cases, " // &
         "data models are prognostic and also receive and use " // &
         "some data sent by the driver to the data model.  But " // &
         "in most cases, the data models are not running " // &
         "prognostically and have no need to receive any data " // &
         "from the driver.", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ReleaseDate", "2010", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ModelType", "Atmosphere", &
         convention=convCIM, purpose=purpComp, rc=rc)

    !   call ESMF_AttributeSet(comp, "Name", "Cecile Hannay", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
    !   call ESMF_AttributeSet(comp, "EmailAddress", &
    !                          "hannay@ucar.edu", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
    !   call ESMF_AttributeSet(comp, "ResponsiblePartyRole", "contact", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
#endif

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine InitializeRealize
  
  !===============================================================================

#if (1 == 0)
  subroutine SetClock(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: mclock,dclock
    type(ESMF_Time)             :: dcurrtime, dstarttime, dstoptime
    type(ESMF_TimeInterval)     :: dtimestep
    character(len=*),parameter  :: subname=trim(modName)//':(SetClock) '

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(dclock, currtime=dcurrtime, starttime=dstarttime, &
       stoptime=dstoptime, timestep=dtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockSet(mclock, currtime=dcurrtime, starttime=dstarttime, &
       stoptime=dstoptime, timestep=dtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSetClock(gcomp, mclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
 
    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine SetClock
#endif
  !===============================================================================

  subroutine ModelAdvance(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: time
    type(ESMF_State)              :: importState, exportState
    integer  :: CurrentYMD, CurrentTOD, yy, mm, dd, stepno, idt
    integer  :: logunit, shrloglev
    real(r8) :: value
    real(r8) :: nextsw_cday
    character(len=128) :: calendar
    character(len=*),parameter  :: subname=trim(modName)//':(ModelAdvance) '

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    call shr_file_getLogUnit (logunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogLevel(max(shrloglev,1))

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!    call shr_nuopc_stuff_ClockTimePrint(n_EClock_a,subname//'n_EClock_a',rc=rc)
    call shr_nuopc_stuff_ClockTimePrint(clock,subname//'clock',rc=rc)

!    call seq_timemgr_EClockGetData( n_EClock_a, curr_ymd=CurrentYMD, curr_tod=CurrentTOD)
!    call seq_timemgr_EClockGetData( n_EClock_a, curr_yr=yy, curr_mon=mm, curr_day=dd)
!    call seq_timemgr_EClockGetData( n_EClock_a, stepno=stepno, dtime=idt)
!    call seq_timemgr_EClockGetData( n_EClock_a, calendar=calendar)
!    write(logunit,*) subname,' c currymd,tod = ',CurrentYMD,CurrentTOD
!    write(logunit,*) subname,' c  yy,mm,dd = ',yy,mm,dd
!    write(logunit,*) subname,' c stepno, idt = ',stepno,idt
!    write(logunit,*) subname,' c calendar = ',trim(calendar)
    call seq_timemgr_EClockGetData( clock, curr_ymd=CurrentYMD, curr_tod=CurrentTOD)
    call seq_timemgr_EClockGetData( clock, curr_yr=yy, curr_mon=mm, curr_day=dd)
    call seq_timemgr_EClockGetData( clock, stepno=stepno, dtime=idt)
    call seq_timemgr_EClockGetData( clock, calendar=calendar)
    write(logunit,*) subname,' n currymd,tod = ',CurrentYMD,CurrentTOD
    write(logunit,*) subname,' n yy,mm,dd = ',yy,mm,dd
    write(logunit,*) subname,' n stepno, idt = ',stepno,idt
    write(logunit,*) subname,' n calendar = ',trim(calendar)

    ! Unpack export state

    call state2avect(importState,x2d,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    ! Run model

    call datm_comp_run(clock, cdata, x2d, d2x)

    ! Pack export state

    call avect2state(d2x,exportState,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out
    call seq_infodata_GetData(n_infodata, nextsw_cday=nextsw_cday)
    call med_method_State_SetScalar(nextsw_cday,    seq_flds_scalar_index_nextsw_cday, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    ! diagnostics
    call mct_aVect_info(2, d2x, istr=subname//':AV')
    call med_method_State_diagnose(exportState,subname//':ES',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call shr_file_setLogLevel(shrloglev)

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelAdvance

  !===============================================================================

  subroutine ModelSetRunClock(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)         :: mclock, dclock
    type(ESMF_Time)          :: mcurrtime, dcurrtime
    type(ESMF_Time)          :: mstoptime
    type(ESMF_TimeInterval)  :: mtimestep, dtimestep
    character(len=128)       :: mtimestring, dtimestring
    type(ESMF_Alarm),pointer :: alarmList(:)
    type(ESMF_Alarm)         :: dalarm
    integer                  :: alarmcount, n
    character(len=*),parameter :: subname=trim(modName)//':(ModelSetRunClock) '

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(dclock, currTime=dcurrtime, timeStep=dtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockGet(mclock, currTime=mcurrtime, timeStep=mtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out

    !--- check that the current time in the model and driver are the same

    if (mcurrtime /= dcurrtime) then
      call ESMF_TimeGet(dcurrtime, timeString=dtimestring, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_TimeGet(mcurrtime, timeString=mtimestring, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      rc=ESMF_Failure
      call ESMF_LogWrite(subname//" ERROR in time consistency; "//trim(dtimestring)//" ne "//trim(mtimestring),  ESMF_LOGMSG_ERROR, rc=dbrc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    endif

    !--- force the driver timestep into the model clock for consistency
    !--- by default, the model timestep is probably the slowest timestep in the system
    !--- while the driver timestep will be the timestep for this NUOPC slot
    !--- also update the model stop time for this timestep

    mstoptime = mcurrtime + dtimestep

    call ESMF_ClockSet(mclock, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    !---- copy alarms from driver to model clock if model clock has no alarms (do this only once!)
    if (alarmCount == 0) then
      call ESMF_ClockGetAlarmList(dclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      allocate(alarmList(alarmCount))
      call ESMF_ClockGetAlarmList(dclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmList=alarmList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do n = 1, alarmCount
!         call ESMF_AlarmPrint(alarmList(n), rc=rc)
!         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
         dalarm = ESMF_AlarmCreate(alarmList(n), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
         call ESMF_AlarmSet(dalarm, clock=mclock, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      enddo

      deallocate(alarmList)
    endif

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelSetRunClock

  !===============================================================================

  subroutine ModelFinalize(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    character(len=*),parameter  :: subname=trim(modName)//':(ModelFinalize) '

    !----------------------------------------------------------------------------
    ! Finalize routine 
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    call datm_comp_final()

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelFinalize

  !===============================================================================

  subroutine avect2state(avect, state, rc)

    ! copy avect data to state fields

    implicit none

    !----- arguments -----
    type(mct_aVect)              :: avect
    type(ESMF_State)             :: state
    integer, intent(out)         :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: farray1(:)
    real(ESMF_KIND_R8), pointer :: favect(:)

    character(*),parameter :: subName = trim(modName)//":(avect2state) "
    !----------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    nflds = mct_avect_nRattr(avect)
    lsize = mct_avect_lsize(avect)
    rList = " "
    if (nflds > 0) rList = mct_aVect_ExportRList2c(avect)
    call med_method_State_reset(state,value = -9999._R8, rc=rc)
    allocate(favect(lsize))

    do nf = 1,nflds

      rc = ESMF_SUCCESS
      call shr_string_listGetName(rList, nf, fldname, dbrc)
      call ESMF_StateGet(state, itemName=trim(fldname), field=lfield, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" not found on state", ESMF_LOGMSG_INFO, rc=dbrc)
      elseif (grid_option == "grid_de") then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        call mct_aVect_exportRAttr(avect, trim(fldname), favect, lsize)
        do n = 1,lsize
          DE = n-1
!         write(tmpstr,'(a,3i8)') subname//' n,DE,lsize ',n,DE,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, localDE=DE, farrayPtr=farray2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          farray2(1,1) = favect(n)
        enddo
      elseif (grid_option == 'mesh' .or. grid_option == 'arb') then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        call mct_aVect_exportRAttr(avect, trim(fldname), favect, lsize)
        do n = 1,lsize
!         write(tmpstr,'(a,3i8)') subname//' n,lsize ',n,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, farrayPtr=farray1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          farray1(n) = favect(n)
        enddo
        write(tmpstr,'(a,3g13.6)') trim(subname)//":"//trim(fldname)//"=",minval(farray1),maxval(farray1),sum(farray1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      else
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)
!        rc=ESMF_Failure
!        call ESMF_LogWrite(subname//" ERROR for grid_option = "//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif

    enddo

    deallocate(favect)

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine avect2state

  !===============================================================================

  subroutine state2avect(state, avect, rc)

    ! copy state fields to avect data

    implicit none

    !----- arguments -----
    type(ESMF_State)             :: state
    type(mct_aVect)              :: avect
    integer, intent(out)         :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: farray1(:)
    real(ESMF_KIND_R8), pointer :: favect(:)

    character(*),parameter :: subName = trim(modName)//":(state2avect) "
    !----------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 1) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    nflds = mct_avect_nRattr(avect)
    lsize = mct_avect_lsize(avect)
    rList = " "
    if (nflds > 0) rList = mct_aVect_ExportRList2c(avect)
    call mct_avect_zero(avect)
    allocate(favect(lsize))

    do nf = 1,nflds

      rc = ESMF_SUCCESS
      call shr_string_listGetName(rList, nf, fldname, dbrc)
      call ESMF_StateGet(state, itemName=trim(fldname), field=lfield, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" not found on state", ESMF_LOGMSG_INFO, rc=dbrc)
      elseif (grid_option == "grid_de") then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        do n = 1,lsize
          DE = n-1
!         write(tmpstr,'(a,3i8)') subname//' n,DE,lsize ',n,DE,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, localDE=DE, farrayPtr=farray2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          favect(n) = farray2(1,1)
        enddo
        call mct_aVect_importRAttr(avect, trim(fldname), favect, lsize)
      elseif (grid_option == 'mesh' .or. grid_option == 'arb') then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        do n = 1,lsize
!         write(tmpstr,'(a,3i8)') subname//' n,lsize ',n,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, farrayPtr=farray1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          favect(n) = farray1(n)
        enddo
        call mct_aVect_importRAttr(avect, trim(fldname), favect, lsize)
        write(tmpstr,'(a,3g13.6)') trim(subname)//":"//trim(fldname)//"=",minval(farray1),maxval(farray1),sum(farray1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      else
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)
!        rc=ESMF_Failure
!        call ESMF_LogWrite(subname//" ERROR for grid_option = "//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif

    enddo

    deallocate(favect)

    if (dbug > 1) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine state2avect

  !===============================================================================

#endif

end module atm_comp_nuopc
