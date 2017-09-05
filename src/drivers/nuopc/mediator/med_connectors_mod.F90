module med_connectors_mod

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to
  ! do so.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use shr_kind_mod          , only: SHR_KIND_CL
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_State_diagnose
  use med_internalstate_mod
  ! use med_internal_state_mode, only : fldsToAtm
  ! use med_internal_state_mode, only : fldsFrAtm
  ! use med_internal_state_mode, only : fldsToOcn
  ! use med_internal_state_mode, only : fldsFrOcn
  ! use med_internal_state_mode, only : fldsToIce
  ! use med_internal_state_mode, only : fldsFrIce
  ! use med_internal_state_mode, only : fldsToLnd
  ! use med_internal_state_mode, only : fldsFrLnd
  ! use med_internal_state_mode, only : fldsToRof
  ! use med_internal_state_mode, only : fldsFrRof
  ! use med_internal_state_mode, only : fldsToWav
  ! use med_internal_state_mode, only : fldsFrWav
  ! use med_internal_state_mode, only : fldsToGlc
  ! use med_internal_state_mode, only : fldsFrGlc
  use med_constants_mod
  use med_infodata_mod      , only: med_infodata_CopyStateToInfodata
  use med_infodata_mod      , only: med_infodata_CopyInfodataToState
  use med_infodata_mod      , only: infodata=>med_infodata

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
  public med_connectors_prep_med2ice
  public med_connectors_prep_med2lnd
  public med_connectors_prep_med2rof
  public med_connectors_prep_med2wav
  public med_connectors_prep_med2glc
  public med_connectors_post_atm2med
  public med_connectors_post_ocn2med
  public med_connectors_post_ice2med
  public med_connectors_post_lnd2med
  public med_connectors_post_rof2med
  public med_connectors_post_wav2med
  public med_connectors_post_glc2med

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
    type(ESMF_Clock)           :: clock
    type(InternalState)        :: is_local
    logical                    :: diagnose
    logical                    :: connected
    integer                    :: n
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
       if (medToAtm) then
          call med_connectors_diagnose(is_local%wrap%NState_AtmExp, is_local%wrap%atmcntr, "med_to_atm", rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_AtmExp,'cpl2atm',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_AtmImp,'cpl2atm',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    case('ocn')
       if (medToOcn) then
          call med_connectors_diagnose(is_local%wrap%NState_OcnExp, is_local%wrap%ocncntr, "med_to_ocn", rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_OcnExp,'cpl2ocn',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_OcnImp,'cpl2ocn',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    case('ice')
       if (medToIce) then
          call med_connectors_diagnose(is_local%wrap%NState_IceExp, is_local%wrap%icecntr, "med_to_ice", rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_IceExp,'cpl2ice',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_IceImp,'cpl2ice',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    case('lnd')
       if (medToLnd) then
          call med_connectors_diagnose(is_local%wrap%NState_LndExp, is_local%wrap%lndcntr, "med_to_lnd", rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_LndExp,'cpl2lnd',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_LndImp,'cpl2lnd',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    case('rof')
       if (medToRof) then
          call med_connectors_diagnose(is_local%wrap%NState_RofExp, is_local%wrap%rofcntr, "med_to_rof", rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_RofExp,'cpl2rof',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call med_infodata_CopyInfodataToState(infodata,is_local%wrap%NState_RofImp,'cpl2rof',is_local%wrap%mpicom,rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
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
    !DEBUG
    character(SHR_KIND_CL)          :: cvalue

    ! local variables
    type(ESMF_Clock)           :: clock
    type(InternalState)        :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_generic)'

    ! Note: for information obtained by the mediator always write out the state
    ! if statewrite_flag is .true. This behavior is obtained by setting cntr to 1.

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
      !DEBUG
      write(cvalue,*) is_local%wrap%atmcntr_post
      call ESMF_LogWrite(trim(subname)//trim(type)//"atmcntr_post is "// cvalue, ESMF_LOGMSG_INFO, rc=dbrc)
      !
      is_local%wrap%atmcntr_post = is_local%wrap%atmcntr_post + 1
      call med_connectors_diagnose(is_local%wrap%NState_AtmImp, is_local%wrap%atmcntr_post, "med_from_atm", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call med_infodata_CopyStateToInfodata(is_local%wrap%NState_AtmImp,infodata,'atm2cpl',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('ocn')
      is_local%wrap%ocncntr_post = is_local%wrap%ocncntr_post + 1
      call med_connectors_diagnose(is_local%wrap%NState_OcnImp, is_local%wrap%ocncntr_post, "med_from_ocn", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call med_infodata_CopyStateToInfodata(is_local%wrap%NState_OcnImp,infodata,'ocn2cpl',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('ice')
      !DEBUG
      write(cvalue,*) is_local%wrap%icecntr_post
      call ESMF_LogWrite(trim(subname)//trim(type)//"icecntr_post is "// cvalue, ESMF_LOGMSG_INFO, rc=dbrc)
      !
      is_local%wrap%icecntr_post = is_local%wrap%icecntr_post + 1
      call med_connectors_diagnose(is_local%wrap%NState_IceImp, is_local%wrap%icecntr_post, "med_from_ice", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call med_infodata_CopyStateToInfodata(is_local%wrap%NState_IceImp,infodata,'ice2cpl',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('lnd')
      is_local%wrap%lndcntr_post = is_local%wrap%lndcntr_post + 1
      call med_connectors_diagnose(is_local%wrap%NState_LndImp, is_local%wrap%lndcntr_post, "med_from_lnd", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call med_infodata_CopyStateToInfodata(is_local%wrap%NState_LndImp,infodata,'lnd2cpl',is_local%wrap%mpicom,rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    case('rof')
      is_local%wrap%rofcntr_post = is_local%wrap%rofcntr_post + 1
      call med_connectors_diagnose(is_local%wrap%NState_RofImp, is_local%wrap%rofcntr_post, "med_from_rof", rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      call med_infodata_CopyStateToInfodata(is_local%wrap%NState_RofImp,infodata,'rof2cpl',is_local%wrap%mpicom,rc)
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

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2ice(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2ice)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'ice', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2ice

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2lnd(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2lnd)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'lnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2lnd

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2rof(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2rof)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'rof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2rof

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2wav(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2wav)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'wav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2wav

  !-----------------------------------------------------------------------------

  subroutine med_connectors_prep_med2glc(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_prep_med2glc)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_prep_generic(gcomp, 'glc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_prep_med2glc

  !-----------------------------------------------------------------------------

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

  subroutine med_connectors_post_ice2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_ice2med)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'ice', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_ice2med

  !-----------------------------------------------------------------------------

  subroutine med_connectors_post_lnd2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_lnd2med)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'lnd', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_lnd2med

  !-----------------------------------------------------------------------------

  subroutine med_connectors_post_rof2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_rof2med)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'rof', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_rof2med

  !-----------------------------------------------------------------------------

  subroutine med_connectors_post_wav2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_wav2med)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'wav', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_wav2med

  !-----------------------------------------------------------------------------

  subroutine med_connectors_post_glc2med(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(med_connectors_post_glc2med)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_connectors_post_generic(gcomp, 'glc', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_connectors_post_glc2med

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

    ! Obtain the field names in State - allocate memory which will be deallocated at the end
    allocate(fieldnamelist(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldnamelist, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(State, trim(subname)//trim(string), rc=rc)
    endif

    ! Write out the fields in State to netcdf files
    if (cntr > 0 .and. statewrite_flag) then
      call ESMF_LogWrite(trim(subname)//trim(string)//": writing out fields", ESMF_LOGMSG_INFO, rc=dbrc)
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
