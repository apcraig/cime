module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance
  use med_method_mod
  
  implicit none
  
  private
  
  public SetServices

  integer :: dbrc
  character(len=1024) :: infostr
  integer :: tcnt
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    character(len=*), parameter :: subname = "(ocn.F90:setServices)"

    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(len=*), parameter :: subname = "(ocn.F90:initializeP1)"
    
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)

    tcnt = 0

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer :: i,j,nxg,nyg
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    type(ESMF_Array)        :: array
    type(ESMF_VM)           :: vm
    integer :: clbnd(ESMF_MAXDIM), cubnd(ESMF_MAXDIM)
    integer :: petCount, localPet
    real(ESMF_KIND_R8),pointer :: farraylon(:,:),farraylat(:,:)
    real(ESMF_KIND_R8)      :: lon,lat
    character(len=*), parameter :: subname = "(ocn.F90:initializeP2)"
    
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)

    nxg = 100
    nyg = 116
    
    ! create a Grid object for Fields
#if (1 == 1) 
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/nxg, nyg/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -90._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return  ! bail out
#else

    call ESMF_GridCompGet(model, petCount=petCount, vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    gridIn = ESMF_GridCreate1PeriDim(maxIndex=(/nxg, nyg/), &
      coordSys=ESMF_COORDSYS_CART, periodicDim=1, regDecomp=(/petCount,1/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), indexflag=ESMF_INDEX_GLOBAL, &
      rc=rc)

    call ESMF_GridAddCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridGetCoord(gridIn, coordDim=1, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=clbnd, computationalUBound=cubnd, &
      farrayPtr=farraylon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    write(infostr,'(4i8)') clbnd(1:2),cubnd(1:2)
    call ESMF_LogWrite(subname//' comp bnd = '//trim(infostr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_GridGetCoord(gridIn, coordDim=2, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=clbnd, computationalUBound=cubnd, &
      farrayPtr=farraylat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    ! cbnd uses global indexing in the grid above, so can use that to set a lon and lat

    do j = clbnd(2),cubnd(2)
    do i = clbnd(1),cubnd(1)
!       lon = dble(localPet)/dble(petCount)*360._ESMF_KIND_R8 + (360._ESMF_KIND_R8)/dble(nxg) * dble(i-clbnd(1)+0.5_ESMF_KIND_R8)
!       lat =                               -90._ESMF_KIND_R8 + (180._ESMF_KIND_R8)/dble(nyg) * dble(j-clbnd(2)+0.5_ESMF_KIND_R8)
       lon =   0.0_ESMF_KIND_R8 + (dble(i-1)+0.5_ESMF_KIND_R8)/dble(nxg) * 360._ESMF_KIND_R8
       lat = -90.0_ESMF_KIND_R8 + (dble(j-1)+0.5_ESMF_KIND_R8)/dble(nyg) * 180._ESMF_KIND_R8
       farraylon(i,j) = lon
       farraylat(i,j) = lat
    enddo
    enddo
#endif

    gridOut = gridIn ! for now out same as in

    call med_method_Grid_Write(gridIn, 'array_ocn_ocn', rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    ! importable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call med_method_State_reset(exportState, value=-99.0_ESMF_KIND_R8, rc=rc)

    call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep
    character(len=*), parameter :: subname = "(ocn.F90:setClock)"

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=10, rc=rc) ! 10 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep
    integer                       :: i,j,n,fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*), parameter :: subname = "(ocn.F90:ModelAdvance)"

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
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
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    tcnt = tcnt + 1

    call ESMF_StateGet(exportState, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(exportState, itemNameList=fieldNameList, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    do n = 1, fieldCount
      call med_method_State_GetFldPtr(exportState, fieldNameList(n), fldptr2=dataPtr, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = tcnt*1000 + n*100 + cos(i/10.) + sin(j/10.)
      enddo
      enddo

    enddo
    deallocate(fieldNameList)

    call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine

end module
