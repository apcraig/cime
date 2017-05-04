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

  use atm_comp_nuopc, only: datm_SS => SetServices
  use ocn_comp_nuopc, only: docn_SS => SetServices
  use ice_comp_nuopc, only: dice_SS => SetServices
  use rof_comp_nuopc, only: drof_SS => SetServices
#if (1 == 3)
  use lnd_comp_nuopc, only: dlnd_SS => SetServices
  use   datm_comp_nuopc, only:   datm_SS => SetServices
  use   docn_comp_nuopc, only:   docn_SS => SetServices
  use   dice_comp_nuopc, only:   dice_SS => SetServices
  use   dlnd_comp_nuopc, only:   dlnd_SS => SetServices
  use   drof_comp_nuopc, only:   drof_SS => SetServices
  use    cam_comp_nuopc, only:    cam_SS => SetServices
  use    pop_comp_nuopc, only:   pop2_SS => SetServices
  use   mom6_comp_nuopc, only:   mom6_SS => SetServices
  use   cice_comp_nuopc, only:  cice5_SS => SetServices
  use    clm_comp_nuopc, only:    clm_SS => SetServices
  use    rtm_comp_nuopc, only:    rtm_SS => SetServices
  use mosart_comp_nuopc, only: mosart_SS => SetServices
#endif
  use MED            , only:  med_SS => SetServices
  use NUOPC_Connector, only:  cpl_SS => SetServices

  use shr_kind_mod,only : r8 => SHR_KIND_R8
  use shr_kind_mod,only : CL => SHR_KIND_CL

  use seq_comm_mct , only: CPLID, GLOID, logunit, loglevel
  use seq_comm_mct , only: ATMID, LNDID, OCNID, ICEID, GLCID, ROFID, WAVID, ESPID
#if (1 == 0)
  use seq_comm_mct , only: num_inst_atm, num_inst_lnd, num_inst_rof
  use seq_comm_mct , only: num_inst_ocn, num_inst_ice, num_inst_glc
  use seq_comm_mct , only: num_inst_wav, num_inst_esp, num_inst_total
  use seq_comm_mct , only: seq_comm_init
#endif
  use seq_comm_mct , only: seq_comm_petlist 
  use seq_infodata_mod, only: infodata=>seq_infodata_infodata
  use seq_infodata_mod, only: seq_infodata_putData, seq_infodata_GetData
  use cesm_comp_mod, only: cesm_pre_init1, cesm_pre_init2
  use seq_timemgr_mod, only: seq_timemgr_EClock_d, seq_timemgr_EClock_a, seq_timemgr_EClock_o
  use shr_nuopc_fldList_mod, only: shr_nuopc_fldList_setDict_fromseqflds
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_Clock_TimePrint
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_ChkErr

  implicit none

  include 'mpif.h'          
  integer, parameter :: dbug_flag = 10
  character(len=512) :: msgstr    
  integer :: dbrc
  character(*), parameter :: NLFileName = "drv_in"  ! input namelist filename
  character(*), parameter :: runseq_filename = "cesm.runconfig"
  character(*), parameter :: u_FILE_u = &
    __FILE__

  private

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    type(ESMF_Config)           :: config
    type(NUOPC_FreeFormat)      :: attrFF
    character(len=*), parameter :: subname = "(esm.F90:SetServices)"
    
    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! register an internal initialization method
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p2"/), userRoutine=ModifyCplLists, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    
    ! Create, open and set the config
    config = ESMF_ConfigCreate(rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_ConfigLoadFile(config, runseq_filename, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_GridCompSet(driver, config=config, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                     :: localrc
    type(ESMF_Time)             :: startTime
    type(ESMF_Time)             :: stopTime
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    type(ESMF_Clock)            :: internalClock
    type(ESMF_GridComp)         :: child
#if (1 == 0)
    integer                     :: petCount, i
    integer, allocatable        :: petList(:)
#endif
    integer                     :: n, stat
    integer                     :: GlobalComm
    integer, pointer            :: petList(:)
    type(ESMF_Config)           :: config
    character(len=32), allocatable  :: compLabels(:)
    character(len=10)           :: value
    character(len=20)           :: model, prefix
    integer                     :: componentCount
    type(NUOPC_FreeFormat)      :: attrFF
    character(len=*), parameter :: subname = "(esm.F90:SetModelServices)"

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    !-------------------------------------------    
    ! Initialize mct and pets and cesm stuff
    !-------------------------------------------    

    call cesm_pre_init1()
    call cesm_pre_init2()
    call shr_nuopc_fldList_setDict_fromseqflds(rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------------------------------------    
    ! Read components from config file
    !-------------------------------------------    

    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! determine the generic component labels
    componentCount = ESMF_ConfigGetLen(config,label="CESM_component_list:", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(compLabels(componentCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg="Allocation of compLabels failed.", &
          line=__LINE__, file=u_FILE_u, rcToReturn=rc)) return
    call ESMF_ConfigGetAttribute(config, valueList=compLabels, label="CESM_component_list:", count=componentCount, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    attrFF = NUOPC_FreeFormatCreate(config, label="driverAttributes::", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompAttributeIngest(driver, attrFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------
    ! determine information for each component and add to the driver
    !--------

    do n=1, componentCount

      !--- construct component prefix
      prefix=trim(compLabels(n))

      !--- read in model instance name
      call ESMF_ConfigGetAttribute(config, model, &
        label=trim(prefix)//"_model:", default="none", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !--- check that there was a model instance specified
      if (trim(model) == "none") then
        ! Error condition: no model was specified
        write (msgstr, *) "No model was specified for component: ",trim(prefix)
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
 
#if (1 == 0)
      ! read in petList bounds
      call ESMF_ConfigGetAttribute(config, petListBounds, &
        label=trim(prefix)//"_petlist_bounds:", default=-1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      ! handle the default situation
      if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
        petListBounds(1) = 0
        petListBounds(2) = petCount - 1
      endif
      ! set petList for this component
      allocate(petList(petListBounds(2)-petListBounds(1)+1))
      do j=petListBounds(1), petListBounds(2)
        petList(j-petListBounds(1)+1) = j ! PETs are 0 based
      enddo
#endif

      !--------
      ! ATM
      !--------

      if (trim(prefix) == "ATM") then

        call seq_comm_petlist(ATMID(1),petList)
        if (trim(model) == "datm") then
          call NUOPC_DriverAddComp(driver, "ATM", datm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#if (1 == 3)
        elseif (trim(model) == "cam") then
          call NUOPC_DriverAddComp(driver, "ATM",  cam_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, ATMID(1), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read ATM attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", &
          relaxedflag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !--------
      ! OCN
      !--------

      elseif (trim(prefix) == "OCN") then

        call seq_comm_petlist(OCNID(1),petList)
        if (trim(model) == "docn") then
          call NUOPC_DriverAddComp(driver, "OCN", docn_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#if (1 == 3)
        elseif (trim(model) == "pop2") then
          call NUOPC_DriverAddComp(driver, "OCN", pop2_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        elseif (trim(model) == "mom6") then
          call NUOPC_DriverAddComp(driver, "OCN", mom6_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, OCNID(1), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read OCN attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", &
          relaxedflag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !--------
      ! ICE
      !--------

      elseif (trim(prefix) == "ICE") then

        call seq_comm_petlist(ICEID(1),petList)
        if (trim(model) == "dice") then
          call NUOPC_DriverAddComp(driver, "ICE",  dice_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#if (1 == 3)
        elseif (trim(model) == "cice") then
          call NUOPC_DriverAddComp(driver, "ICE", cice5_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, ICEID(1), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read ICE attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", &
          relaxedflag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !--------
      ! LND
      !--------

      elseif (trim(prefix) == "LND") then

        call seq_comm_petlist(LNDID(1),petList)
        if (trim(model) == "dlnd") then
#if (1 == 3)
          call NUOPC_DriverAddComp(driver, "LND", dlnd_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        elseif (trim(model) == "clm") then
          call NUOPC_DriverAddComp(driver, "LND",  clm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, LNDID(1), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read LND attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", &
          relaxedflag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !--------
      ! ROF
      !--------

      elseif (trim(prefix) == "ROF") then

        call seq_comm_petlist(ROFID(1),petList)
        if (trim(model) == "drof") then
          call NUOPC_DriverAddComp(driver, "ROF", drof_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#if (1 == 3)
        elseif (trim(model) == "rtm") then
          call NUOPC_DriverAddComp(driver, "ROF", rtm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        elseif (trim(model) == "mosart") then
          call NUOPC_DriverAddComp(driver, "ROF", mosart_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, ROFID(1), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read ROF attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", &
          relaxedflag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      !--------
      ! MED
      !--------

      elseif (trim(prefix) == "MED") then

        call seq_comm_petlist(CPLID,petList)
        if (trim(model) == "cesm") then
          call NUOPC_DriverAddComp(driver, "MED", med_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, CPLID, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read MED attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", &
          relaxedflag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      else

        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg=subname//' invalid component = '//trim(prefix), &
          line=__LINE__, file=u_FILE_u, rcToReturn=rc)
        return  ! bail out

      endif

    enddo

    !--------
    ! clean-up
    !--------
    deallocate(compLabels)

    !--------
    ! Set baseline clock
    !--------

    call ESMF_GridCompSet(driver, clock=seq_timemgr_Eclock_o, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_Clock_TimePrint(seq_timemgr_Eclock_o,subname//'EClock_base',rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetModelServices

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_Config)             :: config
    type(NUOPC_FreeFormat)        :: runSeqFF
    character(len=*), parameter :: subname = "(esm.F90:SetRunSequence)"

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    !--------
    ! Run Sequence and Connectors
    !--------

    ! read free format run sequence from config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_FreeFormatPrint(runSeqFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, autoAddConnectors=.true., rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------
    ! Update Clocks
    !--------

    call NUOPC_DriverSetRunSequence(driver, slot=1, clock=seq_timemgr_EClock_o, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_DriverSetRunSequence(driver, slot=2, clock=seq_timemgr_EClock_a, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_Clock_TimePrint(seq_timemgr_Eclock_o,subname//'EClock_o',rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_Clock_TimePrint(seq_timemgr_Eclock_a,subname//'EClock_a',rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Diagnostic
    call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunSequence

  !-----------------------------------------------------------------------------

  recursive subroutine ModifyCplLists(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_CplComp), pointer     :: connectorList(:)
    integer                         :: i, j, cplListSize
    character(len=160), allocatable :: cplList(:)
    character(len=160)              :: tempString
    character(len=*), parameter :: subname = "(esm.F90:ModifyCplLists)"

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    
    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    write (msgstr,*) "Found ", size(connectorList), " Connectors."// &
      " Modifying CplList Attribute...."
    call ESMF_LogWrite(trim(msgstr), ESMF_LOGMSG_INFO, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    do i=1, size(connectorList)
      ! query the cplList for connector i
      call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
          valueList=cplList, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        ! go through all of the entries in the cplList and add options
        do j=1, cplListSize
!          tempString = trim(cplList(j))//":REMAPMETHOD=bilinear"//&
!         ":SrcTermProcessing=1:DUMPWEIGHTS=true:TermOrder=SrcSeq"
          tempString = trim(cplList(j))//":remapmethod=redist"
          cplList(j) = trim(tempString)
        enddo
        ! store the modified cplList in CplList attribute of connector i
        call NUOPC_CompAttributeSet(connectorList(i), &
          name="CplList", valueList=cplList, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        deallocate(cplList)
      endif
    enddo
      
    deallocate(connectorList)
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine ModifyCplLists

  !-----------------------------------------------------------------------------

  subroutine esm_AddAttributes(gcomp, MCTID, rc)
    ! Add specific set of attributes to gcomp from infodata
    type(ESMF_GridComp),intent(inout) :: gcomp
    integer            ,intent(in)    :: MCTID
    integer            ,intent(inout) :: rc

    ! locals
    character(len=CL) :: cvalue
    integer  :: ivalue, n
    real(r8) :: rvalue
    logical  :: lvalue
    integer, parameter :: nattrlist = 21
    character(len=*), parameter, dimension(nattrlist) :: attrList = &
      (/ "case_name     ", "single_column ", "scmlat        ", "scmlon         ", &
         "orb_eccen     ", "orb_obliqr    ", "orb_lambm0    ", "orb_mvelpp     ", &
         "read_restart  ", "start_type    ", "tfreeze_option", "model_version  ", &
         "info_debug    ", "atm_aero      ", &
         "aqua_planet   ", "brnch_rcase   ", "perpetual     ", "perpetual_ymd  ", &
         "hostname      ", "username      ", "MCTID         " /)
    character(len=*), parameter :: subname = "(esm.F90:esm_AddAttributes)"

    rc = ESMF_Success

    call NUOPC_CompAttributeAdd(gcomp, attrList=attrList, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,nattrlist

      select case(trim(attrList(n)))

      case("case_name")
        call seq_infodata_GetData(infodata, case_name=cvalue)

      case("single_column")
        call seq_infodata_GetData(infodata, single_column=lvalue)
        cvalue = "false"
        if (lvalue) cvalue = "true"

      case ("scmlat")
        call seq_infodata_GetData(infodata, scmlat=rvalue)
        write(cvalue,'(g26.17)') rvalue

      case ("scmlon")
        call seq_infodata_GetData(infodata, scmlon=rvalue)
        write(cvalue,'(g26.17)') rvalue

      case("orb_eccen")
        call seq_infodata_GetData(infodata, orb_eccen=rvalue)
        write(cvalue,'(g26.17)') rvalue

      case("orb_obliqr")
        call seq_infodata_GetData(infodata, orb_obliqr=rvalue)
        write(cvalue,'(g26.17)') rvalue

      case("orb_lambm0")
        call seq_infodata_GetData(infodata, orb_lambm0=rvalue)
        write(cvalue,'(g26.17)') rvalue

      case("orb_mvelpp")
        call seq_infodata_GetData(infodata, orb_mvelpp=rvalue)
        write(cvalue,'(g26.17)') rvalue

      case("read_restart")
        call seq_infodata_GetData(infodata, read_restart=lvalue)
        cvalue = "false"
        if (lvalue) cvalue = "true"

      case("start_type")
        call seq_infodata_GetData(infodata, start_type=cvalue)

      case("tfreeze_option")
        call seq_infodata_GetData(infodata, tfreeze_option=cvalue)

      case("model_version")
        call seq_infodata_GetData(infodata, model_version=cvalue)

      case("info_debug")
        call seq_infodata_GetData(infodata, info_debug=ivalue)
        write(cvalue,'(i16)') ivalue

      case("atm_aero")
        call seq_infodata_GetData(infodata, atm_aero=lvalue)
        cvalue = "false"
        if (lvalue) cvalue = "true"

      case("aqua_planet")
        call seq_infodata_GetData(infodata, aqua_planet=lvalue)
        cvalue = "false"
        if (lvalue) cvalue = "true"

      case("brnch_rcase")
        call seq_infodata_GetData(infodata, brnch_retain_casename=lvalue)
        cvalue = "false"
        if (lvalue) cvalue = "true"

      case("perpetual")
        call seq_infodata_GetData(infodata, perpetual=lvalue)
        cvalue = "false"
        if (lvalue) cvalue = "true"

      case("perpetual_ymd")
        call seq_infodata_GetData(infodata, perpetual_ymd=ivalue)
        write(cvalue,'(i16)') ivalue

      case("hostname")
        call seq_infodata_GetData(infodata, hostname=cvalue)

      case("username")
        call seq_infodata_GetData(infodata, username=cvalue)

      case("MCTID")
        write(cvalue,'(i16)') MCTID
 
      case default
        rc = ESMF_Failure
        call ESMF_LogWrite(trim(subname)//": unknown attrlist = "//trim(attrList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      end select

      call NUOPC_CompAttributeSet(gcomp, name=trim(attrList(n)), value=trim(cvalue), rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    enddo

  end subroutine esm_AddAttributes

  !-----------------------------------------------------------------------------
end module ESM
