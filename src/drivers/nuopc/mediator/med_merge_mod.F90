module med_merge_mod

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to 
  ! do so.
  !-----------------------------------------------------------------------------

  use ESMF
  use shr_nuopc_methods_mod
  use med_internalstate_mod
  use med_constants_mod

  implicit none
  
  private
  
  integer            :: dbrc
  integer           , parameter :: dbug_flag   = med_constants_dbug_flag
  logical           , parameter :: statewrite_flag = med_constants_statewrite_flag
  real(ESMF_KIND_R8), parameter :: spval_init  = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval       = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero       = med_constants_czero
  character(len=ESMF_MAXSTR) :: tmpstr
  character(*),parameter :: u_FILE_u = &
    __FILE__

  public med_merge_auto

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine med_merge_auto(FBout, FB1, FB1w, fldw1, FB2, FB2w, fldw2, &
                                   FB3, FB3w, fldw3, FB4, FB4w, fldw4, &
                                   FB5, FB5w, fldw5, FB6, FB6w, fldw6, &
                                   document, rc)
    type(ESMF_FieldBundle),intent(inout) :: FBout
    type(ESMF_FieldBundle),intent(in),optional :: FB1  , FB2  , FB3  , FB4  , FB5  , FB6
    type(ESMF_FieldBundle),intent(in),optional :: FB1w , FB2w , FB3w , FB4w , FB5w , FB6w
    character(len=*)      ,intent(in),optional :: fldw1, fldw2, fldw3, fldw4, fldw5, fldw6
    logical               ,intent(in),optional :: document
    integer               ,intent(out)         :: rc

    ! This subroutine initializes the fractions
    
    ! local variables
    integer                     :: cnt, n
    logical                     :: ldocument
    character(len=*),parameter  :: subname='(med_merge_auto)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    !---------------------------------------

    ldocument = .false.
    if (present(document)) ldocument=document

    call ESMF_FieldBundleGet(FBout, fieldCount=cnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,cnt

       if (present(FB1)) then
         if (present(FB1w) .and. present(fldw1)) then
           call med_merge_fbx(FBout, n, FB1, FB1w, fldw1, ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         elseif ((present(FB1w) .and. .not.present(fldw1)) .or. &
                 (.not.present(FB1w) .and. present(fldw1))) then
           call ESMF_LogWrite(trim(subname)//": error FB1w and fldw1 both required", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
           rc = ESMF_FAILURE
           return
         else
           call med_merge_fbx(FBout, n, FB1, document=ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         endif
       endif

       if (present(FB2)) then
         if (present(FB2w) .and. present(fldw2)) then
           call med_merge_fbx(FBout, n, FB2, FB2w, fldw2, ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         elseif ((present(FB2w) .and. .not.present(fldw2)) .or. &
                 (.not.present(FB2w) .and. present(fldw2))) then
           call ESMF_LogWrite(trim(subname)//": error FB2w and fldw2 both required", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
           rc = ESMF_FAILURE
           return
         else
           call med_merge_fbx(FBout, n, FB2, document=ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         endif
       endif

       if (present(FB3)) then
         if (present(FB3w) .and. present(fldw3)) then
           call med_merge_fbx(FBout, n, FB3, FB3w, fldw3, ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         elseif ((present(FB3w) .and. .not.present(fldw3)) .or. &
                 (.not.present(FB3w) .and. present(fldw3))) then
           call ESMF_LogWrite(trim(subname)//": error FB3w and fldw3 both required", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
           rc = ESMF_FAILURE
           return
         else
           call med_merge_fbx(FBout, n, FB3, document=ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         endif
       endif

       if (present(FB4)) then
         if (present(FB4w) .and. present(fldw4)) then
           call med_merge_fbx(FBout, n, FB4, FB4w, fldw4, ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         elseif ((present(FB4w) .and. .not.present(fldw4)) .or. &
                 (.not.present(FB4w) .and. present(fldw4))) then
           call ESMF_LogWrite(trim(subname)//": error FB4w and fldw4 both required", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
           rc = ESMF_FAILURE
           return
         else
           call med_merge_fbx(FBout, n, FB4, document=ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         endif
       endif

       if (present(FB5)) then
         if (present(FB5w) .and. present(fldw5)) then
           call med_merge_fbx(FBout, n, FB5, FB5w, fldw5, ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         elseif ((present(FB5w) .and. .not.present(fldw5)) .or. &
                 (.not.present(FB5w) .and. present(fldw5))) then
           call ESMF_LogWrite(trim(subname)//": error FB5w and fldw5 both required", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
           rc = ESMF_FAILURE
           return
         else
           call med_merge_fbx(FBout, n, FB5, document=ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         endif
       endif

       if (present(FB6)) then
         if (present(FB6w) .and. present(fldw6)) then
           call med_merge_fbx(FBout, n, FB6, FB6w, fldw6, ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         elseif ((present(FB6w) .and. .not.present(fldw6)) .or. &
                 (.not.present(FB6w) .and. present(fldw6))) then
           call ESMF_LogWrite(trim(subname)//": error FB6w and fldw6 both required", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
           rc = ESMF_FAILURE
           return
         else
           call med_merge_fbx(FBout, n, FB6, document=ldocument, rc=rc)
           if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
         endif
       endif

    enddo

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_merge_auto

  !-----------------------------------------------------------------------------

  subroutine med_merge_fbx(FBout, n, FB, FBw, fldw, document, rc)
    type(ESMF_FieldBundle),intent(inout) :: FBout
    integer               ,intent(in)    :: n
    type(ESMF_FieldBundle),intent(in)    :: FB
    type(ESMF_FieldBundle),intent(in),optional :: FBw
    character(len=*)      ,intent(in),optional :: fldw
    logical               ,intent(in),optional :: document
    integer               ,intent(out)         :: rc
    
    ! This subroutine initializes the fractions
    
    ! local variables
    real(ESMF_KIND_R8), pointer :: dp1 (:), dp2 (:,:)
    real(ESMF_KIND_R8), pointer :: dpf1(:), dpf2 (:,:)
    real(ESMF_KIND_R8), pointer :: dpw1(:), dpw2(:,:)
    
    integer                     :: nf
    integer                     :: cnt, cntf
    character(ESMF_MAXSTR)      :: FBoutfld, FBfld, FBoutname, FBname
    integer                     :: lrank
    logical                     :: ldocument
    character(len=*),parameter  :: subname='(med_merge_fbx)'

!    if (dbug_flag > 5) then
!      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
!    endif
    rc = ESMF_SUCCESS

    ldocument = .false.
    if (present(document)) ldocument=document

    call shr_nuopc_methods_FB_getName(FBout, n, FBoutfld, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    FBoutname = trim(FBoutfld(scan(FBoutfld,'_'):))
    call shr_nuopc_methods_FB_GetFldPtr(FBout, trim(FBoutfld), dp1, dp2, lrank, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

!    write(tmpstr,*) subname,'tcx FBoutfld=',n,trim(FBoutfld)
!    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    if (present(FBw) .and. present(fldw)) then
      call shr_nuopc_methods_FB_GetFldPtr(FBw, trim(fldw), dpw1, dpw2, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call ESMF_FieldBundleGet(FB, fieldCount=cntf, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
!    write(tmpstr,*) subname,'tcx cntf=',cntf
!    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    do nf = 1,cntf
      call shr_nuopc_methods_FB_getName(FB, nf, FBfld, rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      FBname = trim(FBfld(scan(FBfld,'_'):))

      if (trim(FBfld) == trim(FBoutfld)) then
        call shr_nuopc_methods_FB_GetFldPtr(FB, trim(FBfld), dpf1, dpf2, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        if (document) call ESMF_LogWrite(trim(subname)//":"//trim(FBoutfld)//" ="//trim(FBfld), ESMF_LOGMSG_INFO, rc=dbrc)
        if (lrank == 1) then
          dp1 = dpf1
        else
          dp2 = dpf2
        endif
      elseif (trim(FBname) == trim(FBoutname)) then
        call shr_nuopc_methods_FB_GetFldPtr(FB, trim(FBfld), dpf1, dpf2, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        if (present(FBw) .and. present(fldw)) then
          if (document) call ESMF_LogWrite(trim(subname)//":"//trim(FBoutfld)//"+="//trim(FBfld)//"*"//trim(fldw), ESMF_LOGMSG_INFO, rc=dbrc)
          if (lrank == 1) then
            dp1 = dp1 + dpf1*dpw1
          else
            dp2 = dp2 + dpf2*dpw2
          endif
        else
          if (document) call ESMF_LogWrite(trim(subname)//":"//trim(FBoutfld)//"+="//trim(FBfld), ESMF_LOGMSG_INFO, rc=dbrc)
          if (lrank == 1) then
            dp1 = dp1 + dpf1
          else
            dp2 = dp2 + dpf2
          endif
        endif
      endif
    enddo

    !---------------------------------------
    !--- clean up
    !---------------------------------------

!    if (dbug_flag > 5) then
!      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
!    endif

  end subroutine med_merge_fbx

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

end module med_merge_mod

