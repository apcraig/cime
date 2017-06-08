program xgridEx1

#define RKIND ESMF_KIND_R4
#define RTKIND ESMF_TYPEKIND_R4
#define IKIND ESMF_KIND_I4

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
    real(RKIND), pointer   :: lndPtr(:), xPtr(:)
    character(len=128)     :: tmpstr
    integer                :: i,n_iter=1
    integer(IKIND)         :: chksum

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

    atmField = ESMF_FieldCreate(atmMesh, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lndField = ESMF_FieldCreate(lndMesh, typekind=RTKIND, rc=rc)
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

    xgridField = ESMF_FieldCreate(xgrid, typekind=RTKIND, rc=rc)
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

    chksum = checksum(lndPtr, rc)
    if(I_AM_PET(0, rc)) print *, 'global checksum = ', chksum

    call ESMF_FieldWrite(atmField, filename="atmField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldWrite(lndField, filename="lndField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

  function checksum(fptr, rc)
    real(RKIND), pointer                 :: fptr(:)
    integer, intent(out)                 :: rc
    integer(IKIND)                       :: checksum

    integer(IKIND)                       :: ivar(size(fptr))
    integer(IKIND)                       :: mold(1)
    integer                              :: nsize
    integer(IKIND), allocatable          :: send(:), recv(:)
    type(ESMF_VM)                        :: vm
    
    
    ivar = transfer(fptr, mold)

    checksum = sum( INT(ivar))

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(send(1), recv(1))
    send(1) = checksum
    nsize = 1
    call ESMF_VMReduce(vm, sendData=send, recvData=recv, &
      count=nsize, reduceFlag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    checksum = recv(1)
    deallocate(send, recv)

  end function

  function i_am_pet(pet, rc)
    integer, intent(in)               :: pet
    integer, intent(out)              :: rc
    logical                           :: i_am_pet

    type(ESMF_VM)                     :: vm
    integer                           :: lpet
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localpet = lpet, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    i_am_pet = .false.
    if(pet == lpet) i_am_pet = .true.

  end function

end program
