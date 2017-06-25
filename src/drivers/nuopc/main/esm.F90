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

#ifdef ESMFUSE_NOTYET_cam
  use  cam_comp_nuopc, only:   cam_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_pop2
  use pop2_comp_nuopc, only:   pop2_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_cice
  use cice_comp_nuopc, only:   cice_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_clm
  use  clm_comp_nuopc, only:   clm_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_rtm
  use  rtm_comp_nuopc, only:   rtm_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_mosart
  use mosart_comp_nuopc, only: mosart_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_mom6
  use mom6_comp_nuopc, only:   mom6_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_ww3
  use  ww3_comp_nuopc, only:   ww3_SS => SetServices
#endif


#ifdef ESMFUSE_datm
  use datm_comp_nuopc, only: datm_SS => SetServices
#endif
#ifdef ESMFUSE_docn
  use docn_comp_nuopc, only: docn_SS => SetServices
#endif
#ifdef ESMFUSE_dice
  use dice_comp_nuopc, only: dice_SS => SetServices
#endif
#ifdef ESMFUSE_dlnd
  use dlnd_comp_nuopc, only: dlnd_SS => SetServices
#endif
#ifdef ESMFUSE_drof
  use drof_comp_nuopc, only: drof_SS => SetServices
#endif
#ifdef ESMFUSE_dwav
  use dwav_comp_nuopc, only: dwav_SS => SetServices
#endif
#ifdef ESMFUSE_dglc
  use dglc_comp_nuopc, only: dglc_SS => SetServices
#endif
#ifdef ESMFUSE_desp
  use desp_comp_nuopc, only: desp_SS => SetServices
#endif

#ifdef ESMFUSE_xatm
  use xatm_comp_nuopc, only: xatm_SS => SetServices
#endif
#ifdef ESMFUSE_xocn
  use xocn_comp_nuopc, only: xocn_SS => SetServices
#endif
#ifdef ESMFUSE_xice
  use xice_comp_nuopc, only: xice_SS => SetServices
#endif
#ifdef ESMFUSE_xlnd
  use xlnd_comp_nuopc, only: xlnd_SS => SetServices
#endif
#ifdef ESMFUSE_xrof
  use xrof_comp_nuopc, only: xrof_SS => SetServices
#endif
#ifdef ESMFUSE_xwav
  use xwav_comp_nuopc, only: xwav_SS => SetServices
#endif
#ifdef ESMFUSE_xglc
  use xglc_comp_nuopc, only: xglc_SS => SetServices
#endif
#ifdef ESMFUSE_NOTYET_xesp
  use xesp_comp_nuopc, only: xesp_SS => SetServices
#endif

#ifdef ESMFUSE_satm
  use satm_comp_nuopc, only: satm_SS => SetServices
#endif
#ifdef ESMFUSE_socn
  use socn_comp_nuopc, only: socn_SS => SetServices
#endif
#ifdef ESMFUSE_sice
  use sice_comp_nuopc, only: sice_SS => SetServices
#endif
#ifdef ESMFUSE_slnd
  use slnd_comp_nuopc, only: slnd_SS => SetServices
#endif
#ifdef ESMFUSE_srof
  use srof_comp_nuopc, only: srof_SS => SetServices
#endif
#ifdef ESMFUSE_swav
  use swav_comp_nuopc, only: swav_SS => SetServices
#endif
#ifdef ESMFUSE_sglc
  use sglc_comp_nuopc, only: sglc_SS => SetServices
#endif
#ifdef ESMFUSE_sesp
  use sesp_comp_nuopc, only: sesp_SS => SetServices
#endif


  use MED                   , only: med_SS => SetServices
  use NUOPC_Connector       , only: cpl_SS => SetServices

  use shr_kind_mod          , only: r8 => SHR_KIND_R8
  use shr_kind_mod          , only: CL => SHR_KIND_CL

  use seq_comm_mct          , only: CPLID, GLOID, logunit, loglevel
  use seq_comm_mct          , only: ATMID, LNDID, OCNID, ICEID, GLCID, ROFID, WAVID, ESPID
#if (1 == 0)
  use seq_comm_mct          , only: num_inst_atm, num_inst_lnd, num_inst_rof
  use seq_comm_mct          , only: num_inst_ocn, num_inst_ice, num_inst_glc
  use seq_comm_mct          , only: num_inst_wav, num_inst_esp, num_inst_total
  use seq_comm_mct          , only: seq_comm_init
#endif
  use seq_comm_mct          , only: seq_comm_petlist 
  use cesm_init_mod         , only: cesm_init
  use seq_timemgr_mod       , only: seq_timemgr_EClock_d, seq_timemgr_EClock_a, seq_timemgr_EClock_o
  use shr_nuopc_fldList_mod , only: shr_nuopc_fldList_setDict_fromseqflds
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_Clock_TimePrint
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_ChkErr

  implicit none

  include 'mpif.h'          
  integer, parameter      :: dbug_flag = 10
  character(len=512)      :: msgstr    
  integer                 :: dbrc
  character(*), parameter :: runseq_filename = "cesm.runconfig"
  character(*), parameter :: u_FILE_u = __FILE__

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
    !-------------------------------------------    

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

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

    attrFF = NUOPC_FreeFormatCreate(config, label="DRIVER_attributes::", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_FreeFormatPrint(attrFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    attrFF = NUOPC_FreeFormatCreate(config, label="driver_input::", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_FreeFormatPrint(attrFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------------------------------------    
    ! Initialize mct and pets and cesm stuff
    !-------------------------------------------    

    call cesm_init(driver)

    call shr_nuopc_fldList_setDict_fromseqflds(rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------
    ! determine information for each component and add to the driver
    !--------

    do n=1, componentCount

      !--- construct component prefix
      prefix=trim(compLabels(n))

      !--- read in model instance name
      call ESMF_ConfigGetAttribute(config, model, label=trim(prefix)//"_model:", default="none", rc=rc)
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
#ifdef ESMFUSE_datm
          call NUOPC_DriverAddComp(driver, "ATM", datm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "xatm") then
#ifdef ESMFUSE_xatm
          call NUOPC_DriverAddComp(driver, "ATM", xatm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "cam") then
#ifdef ESMFUSE_NOTYET_cam
          call NUOPC_DriverAddComp(driver, "ATM",  cam_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "satm") then
#ifdef ESMFUSE_satm
          call NUOPC_DriverAddComp(driver, "ATM",  satm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, driver, ATMID(1), rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        ! read ATM attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_Attributes::", relaxedflag=.true., rc=rc)
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
#ifdef ESMFUSE_docn
          call NUOPC_DriverAddComp(driver, "OCN", docn_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "xocn") then
#ifdef ESMFUSE_xocn
          call NUOPC_DriverAddComp(driver, "OCN", xocn_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "pop2") then
#ifdef ESMFUSE_NOTYET_pop2
          call NUOPC_DriverAddComp(driver, "OCN", pop2_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "mom6") then
#ifdef ESMFUSE_NOTYET_mom6
          call NUOPC_DriverAddComp(driver, "OCN", mom6_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "socn") then
#ifdef ESMFUSE_socn
          call NUOPC_DriverAddComp(driver, "OCN", socn_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, driver, OCNID(1), rc=rc)
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
#ifdef ESMFUSE_dice
          call NUOPC_DriverAddComp(driver, "ICE", dice_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "cice") then
#ifdef ESMFUSE_NOTYET_cice
          call NUOPC_DriverAddComp(driver, "ICE", cice_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "xice") then
#ifdef ESMFUSE_xice
          call NUOPC_DriverAddComp(driver, "ICE", xice_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "sice") then
#ifdef ESMFUSE_sice
          call NUOPC_DriverAddComp(driver, "ICE", sice_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, driver, ICEID(1), rc=rc)
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
#ifdef ESMFUSE_dlnd
          call NUOPC_DriverAddComp(driver, "LND", dlnd_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "clm") then
#ifdef ESMFUSE_NOTYET_clm
          call NUOPC_DriverAddComp(driver, "LND", clm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "xlnd") then
#ifdef ESMFUSE_xlnd
          call NUOPC_DriverAddComp(driver, "LND", xlnd_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "slnd") then
#ifdef ESMFUSE_slnd
          call NUOPC_DriverAddComp(driver, "LND", slnd_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, driver, LNDID(1), rc=rc)
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
#ifdef ESMFUSE_drof
          call NUOPC_DriverAddComp(driver, "ROF", drof_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "rtm") then
#ifdef ESMFUSE_NOTYET_rtm
          call NUOPC_DriverAddComp(driver, "ROF", rtm_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "mosart") then
#ifdef ESMFUSE_NOTYET_mosart
          call NUOPC_DriverAddComp(driver, "ROF", mosart_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "xrof") then
#ifdef ESMFUSE_xrof
          call NUOPC_DriverAddComp(driver, "ROF", xrof_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "srof") then
#ifdef ESMFUSE_srof
          call NUOPC_DriverAddComp(driver, "ROF", srof_SS, petList=petList, comp=child, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
#else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' model unavailable = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
#endif
        else
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=subname//' invalid model = '//trim(prefix)//':'//trim(model), &
            line=__LINE__, file=u_FILE_u, rcToReturn=rc)
          return  ! bail out
        endif
        call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call esm_AddAttributes(child, driver, ROFID(1), rc=rc)
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
        call esm_AddAttributes(child, driver, CPLID, rc=rc)
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
           !tempString = trim(cplList(j))//":REMAPMETHOD=bilinear"//&
           !":SrcTermProcessing=1:DUMPWEIGHTS=true:TermOrder=SrcSeq"
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

  subroutine esm_AddAttributes(gcomp, driver, MCTID, rc)

    ! Add specific set of attributes to gcomp from infodata
    use shr_sys_mod

    ! input/output parameters
    type(ESMF_GridComp),intent(inout) :: gcomp
    integer            ,intent(in)    :: MCTID
    type(ESMF_GridComp),intent(in)    :: driver
    integer            ,intent(inout) :: rc

    ! locals
    character(len=CL)  :: cvalue
    integer            :: n
    integer, parameter :: nattrlist = 20
    character(len=*), parameter :: attrList(nattrlist) = &
      (/ "case_name"    ,"single_column","scmlat"        ,"scmlon"               , &
         "orb_eccen"    ,"orb_obliqr"   ,"orb_lambm0"    ,"orb_mvelpp"           , &
         "read_restart" ,"start_type"   ,"tfreeze_option","model_version"        , &
         "info_debug"   ,"atm_aero"     ,"aqua_planet"   ,"brnch_retain_casename", &
         "perpetual"    ,"perpetual_ymd","hostname"      ,"username"/)
    character(len=*), parameter :: subname = "(esm.F90:esm_AddAttributes)"
    !-------------------------------------------    

    rc = ESMF_Success

    ! Add MCTID to gcomp attributes
    write(cvalue,*) MCTID
    call NUOPC_CompAttributeAdd(gcomp, attrList=(/'MCTID'/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  
    call NUOPC_CompAttributeSet(gcomp, name='MCTID', value=trim(cvalue), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  

    ! Now add all the other attributes in AttrList (which have already been added to driver attributes)
    call NUOPC_CompAttributeAdd(gcomp, attrList=attrList, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,nattrlist
       call NUOPC_CompAttributeGet(driver, name=trim(attrList(n)), value=cvalue, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call NUOPC_CompAttributeSet(gcomp, name=trim(attrList(n)), value=trim(cvalue), rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

  end subroutine esm_AddAttributes

  !-----------------------------------------------------------------------------

end module ESM
