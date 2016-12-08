!================================================================================
module shr_esmfmct_mod

#ifdef NUOPC_INTERFACE 

  use shr_kind_mod,only : R8 => SHR_KIND_R8, CXX => SHR_KIND_CXX
  use shr_kind_mod,only : CL => SHR_KIND_CL, CX => SHR_KIND_CX, CS => SHR_KIND_CS
  use shr_sys_mod, only : shr_sys_abort
  use shr_log_mod, only : loglev  => shr_log_Level
  use shr_log_mod, only : logunit => shr_log_Unit
  use ESMF
  use NUOPC
  use mct_mod
  use med_method_mod

  implicit none
  save
  private
  integer :: dbrc
  character(len=CXX) :: tmpstr

  public :: shr_esmfmct_state2avect
  public :: shr_esmfmct_avect2state

!================================================================================
contains
!================================================================================

  subroutine shr_esmfmct_state2avect(State, aVect, rc)

    implicit none
    type(ESMF_State),intent(in) :: State
    type(mct_aVect), intent(inout) :: aVect
    integer, intent(out) :: rc

    integer :: nflds, lsize, n
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(shr_esmfmct_state2avect)'

    rc = ESMF_SUCCESS

    call mct_aVect_zero(aVect)
    rList = " "
    nflds = mct_aVect_nRattr(aVect)
    lsize = mct_aVect_lsize(aVect)
    if (nflds > 0) rList = mct_aVect_ExportRList2c(AVect)

    write(tmpstr,'(A,i8)') subname//' avect lsize = ',lsize
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    do n = 1,nflds
      call shr_string_listGetName(rList, n, fldname, rc)
      call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname), ESMF_LOGMSG_INFO, rc=dbrc)
      call med_method_State_GetFldPtr(State, fldname, fldptr2=dataPtr,rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      if (n == 1) then
        write(tmpstr,'(A,i8)') subname//' dataPtr dim1 size = ',lbound(dataPtr,dim=1),ubound(dataPtr,dim=1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
        write(tmpstr,'(A,i8)') subname//' dataPtr dim2 size = ',lbound(dataPtr,dim=2),ubound(dataPtr,dim=2)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    enddo

  end subroutine shr_esmfmct_state2avect

!-----------------------------------------------------------------------------

  subroutine shr_esmfmct_avect2state(aVect, State, rc)

    implicit none
    type(mct_aVect), intent(in) :: aVect
    type(ESMF_State),intent(inout) :: State
    integer, intent(out) :: rc

    integer :: nflds, lsize, n
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(shr_esmfmct_avect2state)'

    rc = ESMF_SUCCESS

  end subroutine shr_esmfmct_avect2state

!-----------------------------------------------------------------------------

#endif

end module shr_esmfmct_mod
