module xocn_comp_nuopc

  !----------------------------------------------------------------------------
  ! This is the NUOPC cap for XOCN
  !----------------------------------------------------------------------------

  use shr_kind_mod, only:  R8=>SHR_KIND_R8, IN=>SHR_KIND_IN
  use shr_kind_mod, only:  CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_sys_mod   ! shared system calls

  use seq_comm_mct          , only: seq_comm_inst, seq_comm_name, seq_comm_suffix
  use seq_timemgr_mod       , only: seq_timemgr_EClockGetData

  use shr_nuopc_fldList_mod
  use shr_nuopc_flds_mod    , only: flds_o2x, flds_o2x_map, flds_x2o, flds_x2o_map
  use shr_nuopc_flds_mod    , only: flds_scalar_name, flds_scalar_index_dead_comps
  use shr_nuopc_flds_mod    , only: flds_scalar_index_nx, flds_scalar_index_ny
  use shr_nuopc_flds_mod    , only: flds_scalar_index_ocnrof_prognostic
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_Clock_TimePrint, shr_nuopc_methods_chkerr
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_SetScalar, shr_nuopc_methods_State_Diagnose
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_Print_FieldExchInfo
  use shr_nuopc_grid_mod    , only: shr_nuopc_grid_Meshinit
  use shr_nuopc_grid_mod    , only: shr_nuopc_grid_ArrayToState
  use shr_nuopc_grid_mod    , only: shr_nuopc_grid_StateToArray
  use shr_file_mod          , only: shr_file_getlogunit, shr_file_setlogunit
  use shr_file_mod          , only: shr_file_getloglevel, shr_file_setloglevel
  use shr_file_mod          , only: shr_file_setIO, shr_file_getUnit
  use shr_string_mod        , only: shr_string_listGetNum

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance, &
    model_label_SetRunClock => label_SetRunClock, &
    model_label_Finalize  => label_Finalize

  use dead_data_mod , only : dead_grid_lat, dead_grid_lon, dead_grid_index
  use dead_nuopc_mod, only : dead_init_nuopc, dead_run_nuopc, dead_final_nuopc

  implicit none

  public :: SetServices

  private ! except

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  real(r8), pointer          :: gbuf(:,:)            ! model info
  real(r8), pointer          :: lat(:)
  real(r8), pointer          :: lon(:)
  integer , allocatable      :: gindex(:)
  real(r8), allocatable      :: x2d(:,:)
  real(r8), allocatable      :: d2x(:,:)
  integer                    :: nflds_d2x
  integer                    :: nflds_x2d
  integer(IN)                :: nxg                  ! global dim i-direction
  integer(IN)                :: nyg                  ! global dim j-direction
  integer(IN)                :: mpicom               ! mpi communicator
  integer(IN)                :: my_task              ! my task in mpi communicator mpicom
  integer                    :: inst_index           ! number of current instance (ie. 1)
  character(len=16)          :: inst_name            ! fullname of current instance (ie. "ocn_0001")
  character(len=16)          :: inst_suffix = ""     ! char string associated with instance (ie. "_0001" or "")
  integer(IN)                :: logunit              ! logging unit number
  integer(IN),parameter      :: master_task=0        ! task number of master task
  character(len=*),parameter :: grid_option = "mesh" ! grid_de, grid_arb, grid_reg, mesh
  integer, parameter         :: dbug = 10
  integer                    :: dbrc
  logical                    :: ocn_prognostic
  logical                    :: ocnrof_prognostic
  integer(IN)                :: compid               ! component id

  type (shr_nuopc_fldList_Type) :: fldsToOcn
  type (shr_nuopc_fldList_Type) :: fldsFrOcn

  !----- formats -----
  character(*),parameter :: modName =  "(xocn_comp_nuopc)"
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
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries

    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
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
    type(ESMF_VM) :: vm
    integer(IN)   :: lmpicom
    character(CL) :: cvalue
    logical       :: exists
    integer(IN)   :: lsize       ! local array size
    integer(IN)   :: ierr        ! error code
    integer(IN)   :: shrlogunit  ! original log unit
    integer(IN)   :: shrloglev   ! original log level
    logical       :: ocn_present ! if true, component is present
    character(len=*),parameter :: subname=trim(modName)//':(InitializeAdvertise) '
    integer :: n
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------------------------
    ! generate local mpi comm
    !----------------------------------------------------------------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call ESMF_VMGet(vm, mpiCommunicator=lmpicom, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    call mpi_comm_dup(lmpicom, mpicom, ierr)
    call mpi_comm_rank(mpicom, my_task, ierr)

    !----------------------------------------------------------------------------
    ! get compid
    !----------------------------------------------------------------------------

    call NUOPC_CompAttributeGet(gcomp, name='MCTID', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    read(cvalue,*) compid  ! convert from string to integer

    !----------------------------------------------------------------------------
    ! determine instance information
    !----------------------------------------------------------------------------

    inst_name   = seq_comm_name(compid)
    inst_index  = seq_comm_inst(compid)
    inst_suffix = seq_comm_suffix(compid)

    !----------------------------------------------------------------------------
    ! set logunit
    !----------------------------------------------------------------------------

    if (my_task == master_task) then
       inquire(FILE='ocn_modelio.nml'//trim(inst_suffix), exist=exists)
       if (exists) then
         logUnit = shr_file_getUnit()
         call shr_file_setIO('ocn_modelio.nml'//trim(inst_suffix),logUnit)
       end if
    endif

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (logunit)

    !----------------------------------------------------------------------------
    ! Initialize xocn
    !----------------------------------------------------------------------------

    call dead_init_nuopc('ocn', mpicom, my_task, master_task, &
         inst_index, inst_suffix, inst_name, logunit, lsize, gbuf, nxg, nyg)

    nflds_d2x = shr_string_listGetNum(flds_o2x)
    nflds_x2d = shr_string_listGetNum(flds_x2o)

    allocate(gindex(lsize))
    allocate(lon(lsize))
    allocate(lat(lsize))
    allocate(d2x(nflds_d2x,lsize))
    allocate(x2d(nflds_x2d,lsize))

    gindex(:) = gbuf(:,dead_grid_index)
    lat(:)    = gbuf(:,dead_grid_lat)
    lon(:)    = gbuf(:,dead_grid_lon)
    d2x(:,:)  = 0._r8
    x2d(:,:)  = 0._r8

    if (nxg == 0 .and. nyg == 0) then
       ocn_present = .false.
       ocn_prognostic = .false.
       ocnrof_prognostic = .false.
    else
       ocn_present = .true.
       ocn_prognostic = .true.
       ocnrof_prognostic = .true.
    end if

    !--------------------------------
    ! create import fields list
    !--------------------------------

    call shr_nuopc_fldList_Zero(fldsToOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    if (ocn_present) then
       call shr_nuopc_fldList_fromflds(fldsToOcn, flds_x2o, flds_x2o_map, "will provide", subname//":flds_x2o", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

       call shr_nuopc_fldList_Add(fldsToOcn, trim(flds_scalar_name), "will provide", subname//":flds_scalar_name", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    end if

    !--------------------------------
    ! create export fields list
    !--------------------------------

    call shr_nuopc_fldList_Zero(fldsFrOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    if (ocn_present) then
       call shr_nuopc_fldList_fromflds(fldsFrOcn, flds_o2x, flds_o2x_map, "will provide", subname//":flds_o2x", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

       call shr_nuopc_fldList_Add(fldsFrOcn, trim(flds_scalar_name), "will provide", subname//":flds_scalar_name", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    end if

    !--------------------------------
    ! advertise import and export fields
    !--------------------------------

    call shr_nuopc_fldList_Advertise(importState, fldsToOcn, subname//':docnImport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call shr_nuopc_fldList_Advertise(exportState, fldsFrOcn, subname//':docnExport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------------------------
    ! Reset shr logging to original values
    !----------------------------------------------------------------------------

    call shr_file_setLogLevel(shrloglev)
    call shr_file_setLogUnit (shrlogunit)

  end subroutine InitializeAdvertise

  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(CL)          :: NLFilename, cvalue
    integer(IN)            :: phase, lmpicom, ierr
    character(ESMF_MAXSTR) :: convCIM, purpComp
    type(ESMF_Grid)        :: Egrid
    type(ESMF_Mesh)        :: Emesh
    integer(IN)            :: shrlogunit                ! original log unit
    integer(IN)            :: shrloglev                 ! original log level
    type(ESMF_VM)          :: vm
    integer(IN)            :: n
    logical                :: connected                 ! is field connected?
    real(R8)               :: scalar                    ! temporary
    character(len=*),parameter :: subname=trim(modName)//':(InitializeRealize: xocn) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (logunit)

    !--------------------------------
    ! generate the mesh
    !--------------------------------

    call shr_nuopc_grid_MeshInit(gcomp, nxg, nyg, mpicom, compid, gindex, lon, lat, Emesh, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    !--------------------------------
    ! realize the actively coupled fields
    !--------------------------------

    call shr_nuopc_fldList_Realize(importState, mesh=Emesh, fldlist=fldsToOcn, tag=subname//':docnImport', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call shr_nuopc_fldList_Realize(exportState, mesh=Emesh, fldlist=fldsFrOcn, tag=subname//':docnExport', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    !--------------------------------
    ! Pack export state
    ! Copy from d2x to exportState
    ! Set the coupling scalars
    !--------------------------------

    call shr_nuopc_grid_ArrayToState(d2x, flds_o2x, exportState, grid_option, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    call shr_nuopc_methods_State_SetScalar(dble(nyg),flds_scalar_index_nx, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    call shr_nuopc_methods_State_SetScalar(dble(nxg),flds_scalar_index_ny, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    call shr_nuopc_methods_State_SetScalar(0.0_r8, flds_scalar_index_dead_comps, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    if (ocnrof_prognostic) then
       scalar = 1.0_r8
    else
       scalar = 0.0_r8
    end if
    call shr_nuopc_methods_State_SetScalar(1.0_r8, flds_scalar_index_ocnrof_prognostic, exportState, mpicom, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    !--------------------------------
    ! diagnostics
    !--------------------------------

    if (dbug > 1) then
       if (my_task == master_task) then
          call shr_nuopc_methods_Print_FieldExchInfo(flag=2, values=d2x, logunit=logunit, &
               fldlist=flds_o2x, nflds=nflds_d2x, istr="InitializeRealize: ocn->mediator")
       end if
       call shr_nuopc_methods_State_diagnose(exportState,subname//':ES',rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    endif

#ifdef USE_ESMF_METADATA
    !--------------------------------
    ! CIM
    !--------------------------------

    convCIM  = "CIM"
    purpComp = "Model Component Simulation Description"

    call ESMF_AttributeAdd(comp,  &
         convention=convCIM, purpose=purpComp, rc=rc)

    call ESMF_AttributeSet(comp, "ShortName", "XOCN", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "LongName", &
         "Ocnosphere Dead Model", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "Description", &
         "The CESM dead models stand in as test model for active " // &
         "components.  Coupling data is artificially generated ", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ReleaseDate", "2017", &
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
    type(ESMF_Clock)         :: clock
    type(ESMF_Time)          :: time
    type(ESMF_State)         :: importState, exportState
    integer                  :: CurrentYMD, CurrentTOD
    integer(IN)              :: shrlogunit     ! original log unit
    integer(IN)              :: shrloglev      ! original log level
    character(CL)            :: case_name      ! case name
    character(len=128)       :: calendar
    character(len=*),parameter  :: subname=trim(modName)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogLevel(max(shrloglev,1))
    call shr_file_setLogUnit (logunit)

    !--------------------------------
    ! query the Component for its clock, importState and exportState
    !--------------------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=u_FILE_u)) &
      return  ! bail out

    if (dbug > 1) then
      call shr_nuopc_methods_Clock_TimePrint(clock,subname//'clock',rc=rc)
    endif

    !--------------------------------
    ! Unpack export state
    !--------------------------------

    call shr_nuopc_grid_StateToArray(importState, x2d, flds_x2o, grid_option, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    !--------------------------------
    ! Run model
    !--------------------------------

    call dead_run_nuopc('ocn', clock, x2d, d2x, gbuf, flds_o2x, my_task, master_task, logunit)

    !--------------------------------
    ! Pack export state
    !--------------------------------

    call shr_nuopc_grid_ArrayToState(d2x, flds_o2x, exportState, grid_option, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    !--------------------------------
    ! diagnostics
    !--------------------------------

    if (dbug > 1) then
       if (my_task == master_task) then
          call shr_nuopc_methods_Print_FieldExchInfo(flag=2, values=d2x, logunit=logunit, &
               fldlist=flds_o2x, nflds=nflds_d2x, istr="ModelAdvance: ocn->mediator")
       end if
       call shr_nuopc_methods_State_diagnose(exportState,subname//':ES',rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out
    endif

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

    !--------------------------------
    ! check that the current time in the model and driver are the same
    !--------------------------------

    if (mcurrtime /= dcurrtime) then
      call ESMF_TimeGet(dcurrtime, timeString=dtimestring, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      call ESMF_TimeGet(mcurrtime, timeString=mtimestring, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      rc=ESMF_Failure
      call ESMF_LogWrite(subname//" ERROR in time consistency; "//trim(dtimestring)//" ne "//trim(mtimestring),  ESMF_LOGMSG_ERROR, rc=dbrc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    endif

    !--------------------------------
    ! force the driver timestep into the model clock for consistency
    ! by default, the model timestep is probably the slowest timestep in the system
    ! while the driver timestep will be the timestep for this NUOPC slot
    ! also update the model stop time for this timestep
    !--------------------------------

    mstoptime = mcurrtime + dtimestep

    call ESMF_ClockSet(mclock, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    !--------------------------------
    ! copy alarms from driver to model clock if model clock has no alarms (do this only once!)
    !--------------------------------

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
    !-------------------------------------------------------------------------------

    !--------------------------------
    ! Finalize routine
    !--------------------------------

    rc = ESMF_SUCCESS
    if (dbug > 5) call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    call dead_final_nuopc('ocn', my_task, master_task, logunit)

    if (dbug > 5) call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelFinalize

  !===============================================================================

end module xocn_comp_nuopc
