module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_SetRunSequence   => label_SetRunSequence
  
  use ATM, only: atmSS => SetServices
  use mom_cap_mod, only: ocnSS => SetServices
  use LND, only: lndSS => SetServices
  use ICE, only: iceSS => SetServices
  use MED, only: medSS => SetServices
  
  use NUOPC_Connector, only: cplSS => SetServices
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    type(ESMF_Config)           :: config

    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! create, open and set the config
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigLoadFile(config, "esmApp.runconfig", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_Config)             :: config
    type(NUOPC_FreeFormat)        :: attrFF
    
    rc = ESMF_SUCCESS
    
    ! read free format driver attributes
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    attrFF = NUOPC_FreeFormatCreate(config, label="driverAttributes::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
#if 0
    print *, "-- start FreeFormatPrint(attrFF) of read in attributes --------"
    call NUOPC_FreeFormatPrint(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, "---- end FreeFormatPrint(attrFF) of read in attributes --------"
#endif

    ! ingest FreeFormat driver attributes
    call NUOPC_CompAttributeIngest(driver, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
#if 0

    ! pull out all the Attributes defined on the Driver in FreeFormat
    call NUOPC_CompAttributeEgest(driver, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *, "-- start FreeFormatPrint(attrFF) of egested attributes --------"
    call NUOPC_FreeFormatPrint(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, "---- end FreeFormatPrint(attrFF) of egested attributes --------"

    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#endif

    ! SetServices for ATM
    call NUOPC_DriverAddComp(driver, "ATM", atmSS, comp=child, &
      petlist=(/0,1,2,3/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set default ATM attributes
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! read ATM attributes from config file into FreeFormat
    attrFF = NUOPC_FreeFormatCreate(config, label="atmAttributes::", &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ingest FreeFormat atm attributes
    call NUOPC_CompAttributeIngest(child, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for OCN
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, comp=child, &
      petlist=(/4,5,6,7/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set default OCN attributes
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! read OCN attributes from config file into FreeFormat
    attrFF = NUOPC_FreeFormatCreate(config, label="ocnAttributes::", &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ingest FreeFormat ocn attributes
    call NUOPC_CompAttributeIngest(child, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for LND
    call NUOPC_DriverAddComp(driver, "LND", lndSS, comp=child, &
      petlist=(/0,1,2,3/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set default LND attributes
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! read LND attributes from config file into FreeFormat
    attrFF = NUOPC_FreeFormatCreate(config, label="lndAttributes::", &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ingest FreeFormat lnd attributes
    call NUOPC_CompAttributeIngest(child, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for ICE
    call NUOPC_DriverAddComp(driver, "ICE", iceSS, comp=child, &
      petlist=(/0,1,2,3/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set default ICE attributes
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! read ICE attributes from config file into FreeFormat
    attrFF = NUOPC_FreeFormatCreate(config, label="lndAttributes::", &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ingest FreeFormat lnd attributes
    call NUOPC_CompAttributeIngest(child, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for MED
    call NUOPC_DriverAddComp(driver, "MED", medSS, comp=child, &
      petlist=(/0,1,2,3/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set default MED attributes
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! read MED attributes from config file into FreeFormat
    attrFF = NUOPC_FreeFormatCreate(config, label="medAttributes::", &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! ingest FreeFormat med attributes
    call NUOPC_CompAttributeIngest(child, attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! clean-up
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Disabling the following macro, e.g. renaming to WITHCONNECTORS_disable,
    ! will result in a driver that does not call connectors between the model
    ! components. This mode can be used if all model components are driven 
    ! as independent models. However, even for independent models the
    ! connectors can be set here, but will turn into no-ops.
#define WITHCONNECTORS
#ifdef WITHCONNECTORS
!    ! SetServices for atm2ocn
!    call NUOPC_DriverAddComp(driver, srcCompLabel="ATM", dstCompLabel="OCN", &
!      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
!      rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    
!    ! SetServices for ocn2atm
!    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="ATM", &
!      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
!      rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    
!    ! SetServices for atm2lnd
!    call NUOPC_DriverAddComp(driver, srcCompLabel="ATM", dstCompLabel="LND", &
!      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
!      rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! SetServices for lnd2atm
!    call NUOPC_DriverAddComp(driver, srcCompLabel="LND", dstCompLabel="ATM", &
!      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
!      rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
    ! SetServices for ocn2med
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="MED", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! SetServices for atm2med
    call NUOPC_DriverAddComp(driver, srcCompLabel="ATM", dstCompLabel="MED", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!    ! SetServices for lnd2med
!    call NUOPC_DriverAddComp(driver, srcCompLabel="LND", dstCompLabel="MED", &
!      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
!      rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    ! SetServices for med2ocn
    call NUOPC_DriverAddComp(driver, srcCompLabel="MED", dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! SetServices for med2atm
    call NUOPC_DriverAddComp(driver, srcCompLabel="MED", dstCompLabel="ATM", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! SetServices for med2lnd
    call NUOPC_DriverAddComp(driver, srcCompLabel="MED", dstCompLabel="LND", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! SetServices for med2ice
    call NUOPC_DriverAddComp(driver, srcCompLabel="MED", dstCompLabel="ICE", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! SetServices for ocn2ice
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="ICE", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! SetServices for ice2ocn
    call NUOPC_DriverAddComp(driver, srcCompLabel="ICE", dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
      
    ! set the model clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute default step
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeSet(startTime, yy=1, mm=1, dd=1, h=0, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeSet(stopTime, yy=1, mm=1, dd=2, h=0, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call LoadDictionary(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)              :: name
    type(ESMF_Config)                   :: config
    type(NUOPC_FreeFormat)              :: runSeqFF

    rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! read free format run sequence from config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
#if 0
    call NUOPC_FreeFormatPrint(runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    ! Diagnostic output
    call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine LoadDictionary(rc)

    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "air_density_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="air_density_height_lowest", &
        canonicalUnits="kg m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_zonal_moment_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_zonal_moment_flx", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_merid_moment_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_merid_moment_flx", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_sensi_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_sensi_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_sensi_heat_flx_atm_into_ice")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_sensi_heat_flx_atm_into_ice", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_sensi_heat_flx_atm_into_ocn")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_sensi_heat_flx_atm_into_ocn", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_laten_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_laten_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_laten_heat_flx_atm_into_ice")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_laten_heat_flx_atm_into_ice", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_laten_heat_flx_atm_into_ocn")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_laten_heat_flx_atm_into_ocn", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_down_lw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_down_lw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_down_sw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_down_sw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_fprec_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_fprec_rate", &
        canonicalUnits="kg s m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_prec_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_prec_rate", &
        canonicalUnits="kg s m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_evap_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_evap_rate", &
        canonicalUnits="kg s m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_evap_rate_atm_into_ice")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_evap_rate_atm_into_ice", &
        canonicalUnits="kg s m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_evap_rate_atm_into_ocn")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_evap_rate_atm_into_ocn", &
        canonicalUnits="kg s m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_zonal_moment_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_zonal_moment_flx", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_merid_moment_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_merid_moment_flx", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_sensi_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_sensi_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_laten_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_laten_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_down_lw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_down_lw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_down_sw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_down_sw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_temp_height2m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_temp_height2m", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_spec_humid_height2m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_spec_humid_height2m", &
        canonicalUnits="kg kg-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_u_wind_height10m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_u_wind_height10m", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_v_wind_height10m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_v_wind_height10m", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_zonal_wind_height10m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_zonal_wind_height10m", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_merid_wind_height10m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_merid_wind_height10m", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_temp_height_surface")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_temp_height_surface", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_pres_height_surface")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_pres_height_surface", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_surface_height")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_surface_height", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    ! -> Additional fields identified as needed by MOM5 and others...
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_down_sw_vis_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_down_sw_vis_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_down_sw_vis_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_down_sw_vis_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_down_sw_ir_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_down_sw_ir_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_down_sw_ir_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_down_sw_ir_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_down_sw_vis_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_down_sw_vis_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_down_sw_vis_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_down_sw_vis_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_down_sw_ir_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_down_sw_ir_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_down_sw_ir_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_down_sw_ir_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_net_sw_vis_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_net_sw_vis_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_net_sw_vis_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_net_sw_vis_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_net_sw_ir_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_net_sw_ir_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_net_sw_ir_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_net_sw_ir_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_net_sw_vis_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_net_sw_vis_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_net_sw_vis_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_net_sw_vis_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_net_sw_ir_dir_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_net_sw_ir_dir_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_net_sw_ir_dif_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_net_sw_ir_dif_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_salt_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_salt_rate", &
        canonicalUnits="kg psu m-2 s", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_runoff_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_runoff_rate", &
        canonicalUnits="kg m-2 s", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_calving_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_calving_rate", &
        canonicalUnits="kg m-2 s", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_runoff_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_runoff_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry(  &
      "mean_calving_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_calving_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ice_fraction")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ice_fraction", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_sw_pen_to_ocn")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_sw_pen_to_ocn", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_up_lw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_up_lw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mass_of_overlying_sea_ice")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mass_of_overlying_sea_ice", &
        canonicalUnits="kg", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "s_surf")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="s_surf", &
        canonicalUnits="psu", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "freezing_melting_potential")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="freezing_melting_potential", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "u_surf")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="u_surf", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "v_surf")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="v_surf", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_lev")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_lev", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "wind_stress_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="wind_stress_zonal", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "wind_stress_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="wind_stress_merid", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ocn_current_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ocn_current_zonal", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ocn_current_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ocn_current_merid", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ocn_current_idir")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ocn_current_idir", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ocn_current_jdir")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ocn_current_jdir", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_surface_slope_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_surface_slope_zonal", &
        canonicalUnits="m m-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_surface_slope_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_surface_slope_merid", &
        canonicalUnits="m m-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_surface_slope_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_surface_slope_zonal", &
        canonicalUnits="m m-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_surface_slope_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_surface_slope_merid", &
        canonicalUnits="m m-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_air_ice_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_air_ice_zonal", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_air_ice_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_air_ice_merid", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_air_ocn_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_air_ocn_zonal", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_air_ocn_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_air_ocn_merid", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_ocn_ice_zonal")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_ocn_ice_zonal", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_ocn_ice_merid")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_ocn_ice_merid", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_ocn_ice_idir")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_ocn_ice_idir", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "stress_on_ocn_ice_jdir")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="stress_on_ocn_ice_jdir", &
        canonicalUnits="N m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mixed_layer_depth")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mixed_layer_depth", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_net_lw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_net_lw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_net_sw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_net_sw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_up_lw_flx_ice")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_up_lw_flx_ice", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_up_lw_flx_ocn")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_up_lw_flx_ocn", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_net_lw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_net_lw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_net_sw_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_net_sw_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ir_dir_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ir_dir_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ir_dif_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ir_dif_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_vis_dir_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_vis_dir_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_vis_dif_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_vis_dif_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ocn_ir_dir_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ocn_ir_dir_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ocn_ir_dif_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ocn_ir_dif_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ocn_vis_dir_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ocn_vis_dir_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ocn_vis_dif_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ocn_vis_dif_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ice_ir_dir_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ice_ir_dir_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ice_ir_dif_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ice_ir_dif_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ice_vis_dir_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ice_vis_dir_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_ice_vis_dif_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_ice_vis_dif_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_land_sea_mask")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_land_sea_mask", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_temp_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_temp_height_lowest", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_spec_humid_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_spec_humid_height_lowest", &
        canonicalUnits="kg kg-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "humidity_2m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="humidity_2m", &
        canonicalUnits="kg kg-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_zonal_wind_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_zonal_wind_height_lowest", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_merid_wind_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_merid_wind_height_lowest", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_pres_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_pres_height_lowest", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "inst_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_height_lowest", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ocean_mask")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ocean_mask", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "ice_mask")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="ice_mask", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "land_mask")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="land_mask", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    ! special HYCOM exports
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "surface_downward_eastward_stress")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_downward_eastward_stress", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "surface_downward_northward_stress")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_downward_northward_stress", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "wind_speed_height10m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="wind_speed_height10m", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "wind_speed_squared_10m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="wind_speed_squared_10m", &
        canonicalUnits="m2 s-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "friction_speed")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="friction_speed", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_lat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_lat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_sens_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_sens_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "water_flux_into_sea_water")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="water_flux_into_sea_water", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "frozen_water_flux_into_sea_water")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="frozen_water_flux_into_sea_water", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif 
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "surface_temperature")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_temperature", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "air_surface_temperature")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="air_surface_temperature", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "temperature_2m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temperature_2m", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "upward_sea_ice_basal_available_heat_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="upward_sea_ice_basal_available_heat_flux", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    ! special HYCOM imports
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_ice_area_fraction")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_ice_area_fraction", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "downward_x_stress_at_sea_ice_base")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="downward_x_stress_at_sea_ice_base", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "downward_y_stress_at_sea_ice_base")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="downward_y_stress_at_sea_ice_base", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "downward_sea_ice_basal_solar_heat_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="downward_sea_ice_basal_solar_heat_flux", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "upward_sea_ice_basal_heat_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="upward_sea_ice_basal_heat_flux", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "downward_sea_ice_basal_salt_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="downward_sea_ice_basal_salt_flux", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "downward_sea_ice_basal_water_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="downward_sea_ice_basal_water_flux", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_ice_temperature")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_ice_temperature", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_ice_thickness")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_ice_thickness", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_ice_x_velocity")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_ice_x_velocity", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "sea_ice_y_velocity")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="sea_ice_y_velocity", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "net_heat_flx_to_ocn")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="net_heat_flx_to_ocn", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_fresh_water_to_ocean_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_fresh_water_to_ocean_rate", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_ice_volume")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_ice_volume", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "mean_snow_volume")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_snow_volume", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
    ! Synonyms for HYCOM fields
    call NUOPC_FieldDictionarySetSyno( &
      standardNames = (/"surface_downward_eastward_stress",&
                        "mean_zonal_moment_flx           "/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldDictionarySetSyno( &
      standardNames = (/"surface_downward_northward_stress",&
                        "mean_merid_moment_flx            "/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldDictionarySetSyno( &
      standardNames = (/"mean_lat_flx       ",&
                        "mean_laten_heat_flx"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldDictionarySetSyno( &
      standardNames = (/"mean_sens_flx      ",&
                        "mean_sensi_heat_flx"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! DCR - Fields added for Regional Application
    ! ATM-OCN-ICE-LND-HYD
    ! List of exisitng fields
    ! ice_mask, inst_down_lw_flx, inst_down_sw_flx, inst_height_lowest,
    ! inst_merid_wind_height_lowest, inst_pres_height_lowest,
    ! inst_pres_height_surface, inst_spec_humid_height_lowest,
    ! inst_temp_height_lowest, inst_temp_height_surface,
    ! inst_zonal_wind_height_lowest, mean_down_lw_flx, mean_down_sw_flx,
    ! mean_fprec_rate, mean_laten_heat_flx, mean_net_lw_flx, mean_net_sw_flx,
    ! mean_prec_rate, mean_sensi_heat_flx

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "aerodynamic_roughness_length")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="aerodynamic_roughness_length", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "canopy_moisture_storage")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="canopy_moisture_storage", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "carbon_dioxide")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="carbon_dioxide", &
        canonicalUnits="ppmv", & ! Units must be clarified
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "cosine_zenith_angle")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="cosine_zenith_angle", &
        canonicalUnits="degree", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "exchange_coefficient_heat")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="exchange_coefficient_heat", &
        canonicalUnits="W m-2 K-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "exchange_coefficient_heat_height2m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="exchange_coefficient_heat_height2m", &
        canonicalUnits="W m-2 K-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "exchange_coefficient_moisture_height2m")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="exchange_coefficient_moisture_height2m", &
        canonicalUnits="kg m-2 s-1 Pa-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "inst_wind_speed_height_lowest")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="inst_wind_speed_height_lowest", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_cprec_rate")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_cprec_rate", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_grnd_sensi_heat_flx")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_grnd_sensi_heat_flx", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_laten_heat_flx_kinematic")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_laten_heat_flx_kinematic", &
        canonicalUnits="Kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_surface_albedo")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_surface_albedo", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_surface_skin_temp")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_surface_skin_temp", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mixing_ratio_surface")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mixing_ratio_surface", &
        canonicalUnits="kg kg-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "root_moisture")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="root_moisture", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "saturated_mixing_ratio")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="saturated_mixing_ratio", &
        canonicalUnits="kg kg-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "surface_snow_area_fraction")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_snow_area_fraction", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "surface_snow_thickness")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_snow_thickness", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "surface_snow_melt_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_snow_melt_flux", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "liquid_water_content_of_surface_snow")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="liquid_water_content_of_surface_snow", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "soil_depth")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="soil_depth", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "soil_hydraulic_conductivity_at_saturation")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="soil_hydraulic_conductivity_at_saturation", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "moisture_content_of_soil_layer")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="moisture_content_of_soil_layer", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "moisture_content_of_soil_layer_1")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="moisture_content_of_soil_layer_1", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "moisture_content_of_soil_layer_2")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="moisture_content_of_soil_layer_2", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "moisture_content_of_soil_layer_3")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="moisture_content_of_soil_layer_3", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "moisture_content_of_soil_layer_4")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="moisture_content_of_soil_layer_4", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "soil_porosity")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="soil_porosity", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "temperature_of_soil_layer")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temperature_of_soil_layer", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "temperature_of_soil_layer_1")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temperature_of_soil_layer_1", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "temperature_of_soil_layer_2")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temperature_of_soil_layer_2", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "temperature_of_soil_layer_3")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temperature_of_soil_layer_3", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "temperature_of_soil_layer_4")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temperature_of_soil_layer_4", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "soil_temperature_bottom")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="soil_temperature_bottom", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "soil_type")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="soil_type", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "soil_moisture_content")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="soil_moisture_content", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "subsurface_basin_mask")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="subsurface_basin_mask", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "subsurface_runoff_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="subsurface_runoff_flux", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "surface_microwave_emissivity")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_microwave_emissivity", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "surface_runoff_flux")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="surface_runoff_flux", &
        canonicalUnits="kg m-2 s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "vegetation_type")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="vegetation_type", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "volume_fraction_of_frozen_water_in_soil")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="volume_fraction_of_frozen_water_in_soil", &
        canonicalUnits="m3 m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "liquid_water_content_of_soil_layer")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="liquid_water_content_of_soil_layer", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "liquid_water_content_of_soil_layer_1")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="liquid_water_content_of_soil_layer_1", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "liquid_water_content_of_soil_layer_2")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="liquid_water_content_of_soil_layer_2", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "liquid_water_content_of_soil_layer_3")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="liquid_water_content_of_soil_layer_3", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "liquid_water_content_of_soil_layer_4")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="liquid_water_content_of_soil_layer_4", &
        canonicalUnits="kg m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "volume_fraction_of_total_water_in_soil")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="volume_fraction_of_total_water_in_soil", &
        canonicalUnits="m3 m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "volume_fraction_of_total_water_in_soil_at_critical_point")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="volume_fraction_of_total_water_in_soil_at_critical_point", &
        canonicalUnits="m3 m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "volume_fraction_of_total_water_in_soil_at_field_capacity")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="volume_fraction_of_total_water_in_soil_at_field_capacity", &
        canonicalUnits="m3 m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "volume_fraction_of_total_water_in_soil_at_wilting_point")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="volume_fraction_of_total_water_in_soil_at_wilting_point", &
        canonicalUnits="m3 m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "water_surface_height_above_reference_datum")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="water_surface_height_above_reference_datum", &
        canonicalUnits="m", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_sensi_heat_flx_atm_into_lnd")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_sensi_heat_flx_atm_into_lnd", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "mean_laten_heat_flx_atm_into_lnd")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="mean_laten_heat_flx_atm_into_lnd", &
        canonicalUnits="W m-2", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Fields from and to WW3

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "eastward_wind_at_10m_height")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="eastward_wind_at_10m_height", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    call NUOPC_FieldDictionarySetSyno( &
      standardNames = (/"eastward_wind_at_10m_height",&
                        "inst_zonal_wind_height10m  "/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "northward_wind_at_10m_height")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="northward_wind_at_10m_height", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    call NUOPC_FieldDictionarySetSyno( &
      standardNames = (/"northward_wind_at_10m_height",&
                        "inst_merid_wind_height10m   "/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "eastward_stokes_drift_current")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="eastward_stokes_drift_current", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "northward_stokes_drift_current")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="northward_stokes_drift_current", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "eastward_wave_bottom_current")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="eastward_wave_bottom_current", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "northward_wave_bottom_current")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="northward_wave_bottom_current", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "wave_bottom_current_radian_frequency")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="wave_bottom_current_radian_frequency", &
        canonicalUnits="rad s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "eastward_wave_radiation_stress_gradient")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="eastward_wave_radiation_stress_gradient", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "northward_wave_radiation_stress_gradient")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="northward_wave_radiation_stress_gradient", &
        canonicalUnits="Pa", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "wave_induced_charnock_parameter")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="wave_induced_charnock_parameter", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Fields from WAM to IPE

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "northward_wind_neutral")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="northward_wind_neutral", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "eastward_wind_neutral")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="eastward_wind_neutral", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "upward_wind_neutral")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="upward_wind_neutral", &
        canonicalUnits="m s-1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "temp_neutral")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="temp_neutral", &
        canonicalUnits="K", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "O_Density")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="O_Density", &
        canonicalUnits="m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "O2_Density")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="O2_Density", &
        canonicalUnits="m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "N2_Density")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="N2_Density", &
        canonicalUnits="m-3", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    if (.not.NUOPC_FieldDictionaryHasEntry( &
      "height")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="height", &
        canonicalUnits="km", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Dummy fields

    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "dummyfield")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="dummyfield", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "dummyfield1")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="dummyfield1", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    if (.not. NUOPC_FieldDictionaryHasEntry( &
      "dummyfield2")) then
      call NUOPC_FieldDictionaryAddEntry( &
        standardName="dummyfield2", &
        canonicalUnits="1", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine LoadDictionary

end module
