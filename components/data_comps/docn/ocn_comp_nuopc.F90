module ocn_comp_nuopc

#ifdef NUOPC_INTERFACE 
  use shr_kind_mod, only:  R8=>SHR_KIND_R8, IN=>SHR_KIND_IN, &
       CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_sys_mod   ! shared system calls

  use seq_flds_mod
  use seq_cdata_mod, only: seq_cdata
  use seq_infodata_mod, only: seq_infodata_type, seq_infodata_GetData, seq_infodata_PutData
  use shr_nuopc_fldList_mod
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_ClockTimePrint
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_dmodelgridinit
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_dmodelAttrCopyToInfodata
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_dmodelAvectToState
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_dmodelStateToAvect
  use shr_file_mod,  only : shr_file_getlogunit, shr_file_setlogunit, &
       shr_file_getloglevel, shr_file_setloglevel

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance, &
    model_label_SetRunClock => label_SetRunClock, &
    model_label_Finalize  => label_Finalize

  use docn_comp_mod, only: docn_comp_init, docn_comp_run, docn_comp_final
  use docn_comp_mod, only: logunit
  use perf_mod
  use mct_mod
  use med_method_mod, only: med_method_State_SetScalar, med_method_State_Diagnose

  implicit none

  public :: SetServices

  private ! except

  type (shr_nuopc_fldList_Type) :: fldsToOcn
  type (shr_nuopc_fldList_Type) :: fldsFrOcn

  type(seq_cdata)         :: cdata
  type(seq_infodata_type),target :: infodata
  type(mct_gsMap)        ,target :: gsmap
  type(mct_gGrid)        ,target :: ggrid
  type(mct_aVect)         :: x2d
  type(mct_aVect)         :: d2x
  integer                 :: mpicom, iam
  integer                 :: dbrc
  character(len=1024)     :: tmpstr
  character(len=*),parameter :: grid_option = "mesh"  ! grid_de, grid_arb, grid_reg, mesh
  integer, parameter      :: dbug = 2

  !----- formats -----
  character(*),parameter :: modName =  "(ocn_comp_nuopc)"
  character(*),parameter :: u_FILE_u = &
    __FILE__

  !===============================================================================
  contains
  !===============================================================================

  subroutine SetServices(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    ! attach specializing method(s)
#if (1 == 0)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out
#endif

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_MethodRemove(gcomp, label=model_label_SetRunClock, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, &
      specRoutine=ModelSetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

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
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
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
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! create import fields list
    call shr_nuopc_fldList_Zero(fldsToOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_fromseqflds(fldsToOcn, seq_flds_x2o_states, "will provide", subname//":seq_flds_x2o_states", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_fromseqflds(fldsToOcn, seq_flds_x2o_fluxes, "will provide", subname//":seq_flds_x2o_fluxes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_Add(fldsToOcn, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    ! create export fields list
    call shr_nuopc_fldList_Zero(fldsFrOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_fromseqflds(fldsFrOcn, seq_flds_o2x_states, "will provide", subname//":seq_flds_o2x_states", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_fromseqflds(fldsFrOcn, seq_flds_o2x_fluxes, "will provide", subname//":seq_flds_o2x_fluxes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_Add(fldsFrOcn, trim(seq_flds_scalar_name), "will provide", subname//":seq_flds_scalar_name", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    ! advertise import and export

    call shr_nuopc_fldList_Advertise(importState, fldsToOcn, subname//':docnImport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call shr_nuopc_fldList_Advertise(exportState, fldsFrOcn, subname//':docnExport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine InitializeAdvertise
  
  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer(IN)      :: MCTID
    character(CL)    :: NLFilename, cvalue
    integer(IN)      :: phase, lmpicom, ierr
    character(ESMF_MAXSTR) :: convCIM, purpComp
    type(ESMF_Grid)  :: Egrid
    type(ESMF_Mesh)  :: Emesh
    integer          :: nx_global, ny_global
    real(r8)         :: nextsw_cday
    integer          :: shrlogunit, shrloglev
    type(ESMF_VM)    :: vm
    character(len=*),parameter :: subname=trim(modName)//':(InitializeRealize) '

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    NLFilename = 'unused'

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, mpiCommunicator=lmpicom, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call MPI_Comm_Dup(lmpicom, mpicom, ierr)

    call NUOPC_CompAttributeGet(gcomp, name='MCTID', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    read(cvalue,*) MCTID  ! convert from string to integer

    phase = 1

    if (phase == 1) then
       !--- don't use seq_cdata_init as it grabs stuff from seq_comm
       !--- initialize cleanly on data model side
!       call seq_cdata_init(cdata,MCTID,ggrid,gsmap,infodata,'docn')
!       call seq_cdata_setptrs(cdata,mpicom=mpicom)
       cdata%name     =  'docn'
       cdata%ID       =  MCTID
       cdata%mpicom   =  mpicom
       cdata%dom      => ggrid
       cdata%gsMap    => gsMap
       cdata%infodata => infodata
       !-------------------------
       call MPI_COMM_RANK(mpicom, iam, rc)
       call shr_nuopc_stuff_dmodelAttrCopyToInfodata(gcomp, infodata, rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
       call seq_infodata_PutData(infodata,ocn_phase=phase)
    else
       call shr_nuopc_stuff_dmodelStateToAvect(importState, x2d, grid_option, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    endif

    call docn_comp_init(clock, cdata, x2d, d2x, NLFilename)

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (logunit)

    call seq_infodata_GetData(infodata, ocn_nx=nx_global, ocn_ny=ny_global)
    call shr_nuopc_stuff_dmodelgridinit(nx_global,ny_global,mpicom,gsMap,ggrid,grid_option,EGrid,Emesh,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    if (grid_option == 'mesh') then

      call shr_nuopc_fldList_Realize(importState, mesh=Emesh, fldlist=fldsToOcn, tag=subname//':docnImport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      call shr_nuopc_fldList_Realize(exportState, mesh=Emesh, fldlist=fldsFrOcn, tag=subname//':docnExport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    else

      call shr_nuopc_fldList_Realize(importState, grid=Egrid, fldlist=fldsToOcn, tag=subname//':docnImport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      call shr_nuopc_fldList_Realize(exportState, grid=Egrid, fldlist=fldsFrOcn, tag=subname//':docnExport', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    endif

    ! Pack export state

    call shr_nuopc_stuff_dmodelAvectToState(d2x, exportState, grid_option, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    call med_method_State_SetScalar(1.0_r8,         seq_flds_scalar_index_present, exportState, mpicom, rc)
    call med_method_State_SetScalar(0.0_r8,         seq_flds_scalar_index_prognostic, exportState, mpicom, rc)
    call med_method_State_SetScalar(dble(ny_global),seq_flds_scalar_index_nx, exportState, mpicom, rc)
    call med_method_State_SetScalar(dble(nx_global),seq_flds_scalar_index_ny, exportState, mpicom, rc)
    call med_method_State_SetScalar(0.0_r8,         seq_flds_scalar_index_dead_comps, exportState, mpicom, rc)
    call seq_infodata_GetData(infodata, nextsw_cday=nextsw_cday)
    call med_method_State_SetScalar(nextsw_cday,    seq_flds_scalar_index_nextsw_cday, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    ! diagnostics
    if (dbug > 1) then
      call mct_aVect_info(2, d2x, istr=subname//':AV')
      call med_method_State_diagnose(exportState,subname//':ES',rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    endif

#ifdef USE_ESMF_METADATA
    convCIM  = "CIM"
    purpComp = "Model Component Simulation Description"

    call ESMF_AttributeAdd(comp,  &
         convention=convCIM, purpose=purpComp, rc=rc)

    call ESMF_AttributeSet(comp, "ShortName", "DOCN", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "LongName", &
         "Climatological Ocnosphere Data Model", &
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
    call ESMF_AttributeSet(comp, "ModelType", "Ocnosphere", &
         convention=convCIM, purpose=purpComp, rc=rc)

    !   call ESMF_AttributeSet(comp, "Name", "Cecile Hannay", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
    !   call ESMF_AttributeSet(comp, "EmailAddress", &
    !                          "hannay@ucar.edu", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
    !   call ESMF_AttributeSet(comp, "ResponsiblePartyRole", "contact", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
#endif

    call shr_file_setLogLevel(shrloglev)
    call shr_file_setLogUnit (shrlogunit)

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

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
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_ClockGet(dclock, currtime=dcurrtime, starttime=dstarttime, &
       stoptime=dstoptime, timestep=dtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_ClockSet(mclock, currtime=dcurrtime, starttime=dstarttime, &
       stoptime=dstoptime, timestep=dtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call NUOPC_CompSetClock(gcomp, mclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out
 
    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

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
    integer  :: shrlogunit, shrloglev
    real(r8) :: value
    real(r8) :: nextsw_cday
    character(len=128) :: calendar
    character(len=*),parameter  :: subname=trim(modName)//':(ModelAdvance) '

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogLevel(max(shrloglev,1))
    call shr_file_setLogUnit (logunit)

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    if (dbug > 1) then
      call shr_nuopc_stuff_ClockTimePrint(clock,subname//'clock',rc=rc)
    endif

    ! Unpack export state

    call shr_nuopc_stuff_dmodelStateToAvect(importState, x2d, grid_option, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    ! Run model

    call docn_comp_run(clock, cdata, x2d, d2x)

    ! Pack export state

    call shr_nuopc_stuff_dmodelAvectToState(d2x, exportState, grid_option, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    call seq_infodata_GetData(infodata, nextsw_cday=nextsw_cday)
    call med_method_State_SetScalar(nextsw_cday,    seq_flds_scalar_index_nextsw_cday, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    ! diagnostics
    if (dbug > 1) then
      call mct_aVect_info(2, d2x, istr=subname//':AV')
      call med_method_State_diagnose(exportState,subname//':ES',rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    endif

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call shr_file_setLogLevel(shrloglev)
    call shr_file_setLogUnit (shrlogunit)

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

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
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    call ESMF_ClockGet(dclock, currTime=dcurrtime, timeStep=dtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    call ESMF_ClockGet(mclock, currTime=mcurrtime, timeStep=mtimestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    !--- check that the current time in the model and driver are the same

    if (mcurrtime /= dcurrtime) then
      call ESMF_TimeGet(dcurrtime, timeString=dtimestring, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      call ESMF_TimeGet(mcurrtime, timeString=mtimestring, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      rc=ESMF_Failure
      call ESMF_LogWrite(subname//" ERROR in time consistency; "//trim(dtimestring)//" ne "//trim(mtimestring),  ESMF_LOGMSG_ERROR, rc=dbrc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    endif

    !--- force the driver timestep into the model clock for consistency
    !--- by default, the model timestep is probably the slowest timestep in the system
    !--- while the driver timestep will be the timestep for this NUOPC slot
    !--- also update the model stop time for this timestep

    mstoptime = mcurrtime + dtimestep

    call ESMF_ClockSet(mclock, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    !---- copy alarms from driver to model clock if model clock has no alarms (do this only once!)
    if (alarmCount == 0) then
      call ESMF_ClockGetAlarmList(dclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      allocate(alarmList(alarmCount))
      call ESMF_ClockGetAlarmList(dclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmList=alarmList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      do n = 1, alarmCount
!         call ESMF_AlarmPrint(alarmList(n), rc=rc)
!         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
         dalarm = ESMF_AlarmCreate(alarmList(n), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
         call ESMF_AlarmSet(dalarm, clock=mclock, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      enddo

      deallocate(alarmList)
    endif

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

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
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    call docn_comp_final()

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelFinalize

  !===============================================================================

#endif

end module ocn_comp_nuopc
