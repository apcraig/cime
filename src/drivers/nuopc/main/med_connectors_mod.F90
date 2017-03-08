module med_connectors_mod

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to 
  ! do so.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_CopyStateToInfodata
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_CopyInfodataToState
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_State_diagnose
  use med_internalstate_mod
  use med_constants_mod
  use seq_infodata_mod, only: infodata=>seq_infodata_infodata

  implicit none
  
  private

  integer            :: dbug_flag = med_constants_dbug_flag
  logical            :: statewrite_flag = med_constants_statewrite_flag
  integer            :: dbrc
  character(*),parameter :: u_FILE_u = &
    __FILE__

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public med_connectors_prep_med2atm
  public med_connectors_prep_med2ocn
  public med_connectors_post_atm2med
  public med_connectors_post_ocn2med

!--------------------------------------------------------------------------
! Private
!--------------------------------------------------------------------------

  private med_connectors_prep_generic
  private med_connectors_post_generic
  private med_connectors_diagnose

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_generic(gcomp, type, rc)
    type(ESMF_GridComp)  :: gcomp
    character(len=*), intent(in) :: type
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_generic)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(type)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------------------
    ! diagnose export state
    ! update scalar data in Exp and Imp State
    !-------------------------

    select case (type)

    case('atm')
      call med_connectors_diagnose(is_local%wrap%NState_AtmExp, is_local%wrap%atmcntr, "med_to_atm", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_CopyInfodataToState(infodata,is_local%wrap%NState_AtmExp,'cpl2atm',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_CopyInfodataToState(infodata,is_local%wrap%NState_AtmImp,'cpl2atm',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('ocn')
      call med_connectors_diagnose(is_local%wrap%NState_OcnExp, is_local%wrap%ocncntr, "med_to_ocn", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_CopyInfodataToState(infodata,is_local%wrap%NState_OcnExp,'cpl2ocn',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_CopyInfodataToState(infodata,is_local%wrap%NState_OcnImp,'cpl2ocn',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('default')
      rc = ESMF_Failure
      call ESMF_LogWrite(trim(subname)//trim(type)//" unsupported", ESMF_LOGMSG_INFO, rc=dbrc)

    end select

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(type)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_generic

  !-----------------------------------------------------------------------------

  subroutine med_connectors_post_generic(gcomp, type, rc)
    type(ESMF_GridComp)  :: gcomp
    character(len=*), intent(in) :: type
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_generic)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(type)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------------------
    ! diagnose import state
    ! copy import state scalar data to local datatype
    !-------------------------

    select case (type)

    case('atm')
      call med_connectors_diagnose(is_local%wrap%NState_AtmImp, is_local%wrap%atmcntr, "med_from_atm", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_AtmImp,infodata,'atm2cpl',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('ocn')
      call med_connectors_diagnose(is_local%wrap%NState_OcnImp, is_local%wrap%ocncntr, "med_from_ocn", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call shr_nuopc_methods_CopyStateToInfodata(is_local%wrap%NState_OcnImp,infodata,'ocn2cpl',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('default')
      rc = ESMF_Failure
      call ESMF_LogWrite(trim(subname)//trim(type)//" unsupported", ESMF_LOGMSG_INFO, rc=dbrc)

    end select

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(type)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_generic

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2atm(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2atm)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'atm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2atm

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2ocn(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2ocn)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'ocn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2ocn


  subroutine med_connectors_post_atm2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_atm2med)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'atm', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_atm2med

  !-----------------------------------------------------------------------------

  subroutine med_connectors_post_ocn2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_ocn2med)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'ocn', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_ocn2med

  !-----------------------------------------------------------------------------

  subroutine med_connectors_diagnose(State, cntr, string, rc)
    type(ESMF_State), intent(in)    :: State
    integer         , intent(inout) :: cntr
    character(len=*), intent(in)    :: string
    integer         , intent(out)   :: rc
    
    ! local variables
    integer :: fieldCount
    character(ESMF_MAXSTR),pointer :: fieldnamelist(:)
    character(len=*),parameter :: subname='(med_connectors_diagnose)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(string)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    allocate(fieldnamelist(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldnamelist, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(State, trim(subname)//trim(string), rc=rc)
    endif

    if (cntr > 0 .and. statewrite_flag) then
      ! write the fields exported to atm to file
      call NUOPC_Write(State, &
        fieldnamelist(1:fieldCount), &
        "field_"//trim(string)//"_", timeslice=cntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    deallocate(fieldnamelist)
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(string)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_diagnose

  !-----------------------------------------------------------------------------

end module med_connectors_mod

