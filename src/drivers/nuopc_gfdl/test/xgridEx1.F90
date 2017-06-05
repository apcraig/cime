program xgridEx1

! !USES:
  use ESMF

  implicit none
  integer   :: rc

  !------------------------------------------------------------------------
  ! Initialize ESMF
  !
  call ESMF_Initialize (defaultCalKind=ESMF_CALKIND_GREGORIAN, &
    defaultlogfilename="xgridEx1.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_XGridTest1(rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Finalize()
  stop

  contains 

  subroutine ESMF_XGridTest1(rc)

    integer, intent(out)   :: rc

    type(ESMF_Mesh)        :: atmMesh, lndMesh
    type(ESMF_XGrid)       :: xgrid
    type(ESMF_Field)       :: atmField, lndField, xgridField
    type(ESMF_Routehandle) :: lnd2x, x2atm
    real(ESMF_KIND_R8), pointer :: lndPtr(:), xPtr(:)
    character(len=128)     :: tmpstr
    integer                :: i,n_iter=10

    atmMesh = ESMF_MeshCreate("ll1deg_grid.nc", ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lndMesh = ESMF_MeshCreate("ll2.5deg_grid.nc", ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgrid = ESMF_XGridCreate(sideAMesh=(/atmMesh/), sideBMesh=(/lndMesh/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    atmField = ESMF_FieldCreate(atmMesh, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lndField = ESMF_FieldCreate(lndMesh, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgridField = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(xgrid, lndField, xgridField, &
      routehandle=lnd2x, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(xgrid, xgridField, atmField, &
      routehandle=x2atm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(lndField, farrayPtr=lndPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    lndPtr = 1.0

    do i = 1, n_iter
      call ESMF_FieldRegrid(lndField, xgridField, routehandle=lnd2x, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldGet(xgridField, farrayPtr=xPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      xPtr = xPtr * i

      call ESMF_FieldRegrid(xgridField, atmField, routehandle=x2atm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  ! clean up
    call ESMF_FieldDestroy(atmField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldDestroy(lndField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldDestroy(xgridField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

end program
