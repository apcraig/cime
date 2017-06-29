program xgridEx1

#define RKIND ESMF_KIND_R8
#define RTKIND ESMF_TYPEKIND_R8
#define IKIND ESMF_KIND_I8

! !USES:
  use ESMF

  implicit none
  include "mpif.h"
  integer   :: rc

  !------------------------------------------------------------------------
  ! Initialize ESMF
  !
  call ESMF_Initialize (defaultCalKind=ESMF_CALKIND_GREGORIAN, &
    defaultlogfilename="xgridEx1.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! 2D test case to show the use of XGrid with Mesh and Mesh
  call ESMF_XGridTest1(rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! 2D test case to show the use of XGrid with Mosaic Grid and Mesh
  call ESMF_XGridTest2(rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! 3D test case to show the use of XGrid to transform from 2D+1 lnd to 2D atm
  call ESMF_XGridTest3(rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! 2D test case to show the use of XGrid with Mosaic Grid and tripolar Grid with realistic SST and masking
  call ESMF_XGridTest4(rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !! 2D test case to show the use of XGrid with Mosaic Grid and regular lat-lon Grid with realistic SST and masking
  call ESMF_XGridTest5(rc)
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
    integer                :: i,n_iter=100
    integer(IKIND)         :: chksum
    real(RKIND)            :: i_time, f_time, delta_time

    i_time = MPI_WTime()
    atmMesh = ESMF_MeshCreate("data/ll1deg_grid.nc", ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lndMesh = ESMF_MeshCreate("data/ll2.5deg_grid.nc", ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgrid = ESMF_XGridCreate(sideAMesh=(/atmMesh/), sideBMesh=(/lndMesh/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    f_time = MPI_WTime()
    call time_analysis(f_time-i_time, 'Constructing mesh and xgrid', rc)

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

    i_time = MPI_Wtime()

    do i = 1, n_iter
      call ESMF_FieldRegrid(lndField, xgridField, routehandle=lnd2x, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
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

      call ESMF_FieldRegrid(xgridField, atmField, routehandle=x2atm, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    f_time = MPI_WTime()

    call time_analysis(f_time-i_time, 'Regridding 100 times', rc)

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

  subroutine ESMF_XGridTest2(rc)

    integer, intent(out)   :: rc

    type(ESMF_Grid)        :: atmGrid
    type(ESMF_Mesh)        :: lndMesh
    type(ESMF_XGrid)       :: xgrid
    type(ESMF_Field)       :: atmField, lndField, xgridField
    type(ESMF_Routehandle) :: lnd2x, x2atm
    real(RKIND), pointer   :: lndPtr(:), xPtr(:)
    character(len=128)     :: tmpstr
    integer                :: i,n_iter=100
    integer(IKIND)         :: chksum
    real(RKIND)            :: i_time, f_time, delta_time
    integer                :: decomptile(2,6)

    i_time = MPI_WTime()
    ! Create Src Grid
    ! Set up decomposition for src Grid, this is optional but can be used to 
    ! map to the model internal data representation.
    decomptile(:,1)=(/2,2/)
    decomptile(:,2)=(/2,2/)
    decomptile(:,3)=(/2,2/)
    decomptile(:,4)=(/2,2/)
    decomptile(:,5)=(/2,2/)
    decomptile(:,6)=(/2,2/)

    atmGrid=ESMF_GridCreateMosaic(filename=trim("data/C48_mosaic.nc"), &
         tileFilePath="./data/", regDecompPTile=decomptile, &
         indexflag=ESMF_INDEX_GLOBAL, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lndMesh = ESMF_MeshCreate("ll2.5deg_grid.nc", ESMF_FILEFORMAT_SCRIP, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgrid = ESMF_XGridCreate(sideAGrid=(/atmGrid/), sideBMesh=(/lndMesh/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    f_time = MPI_WTime()
    call time_analysis(f_time-i_time, 'Constructing mesh and xgrid', rc)

    atmField = ESMF_FieldCreate(atmGrid, typekind=RTKIND, rc=rc)
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

    i_time = MPI_Wtime()

    do i = 1, n_iter
      call ESMF_FieldRegrid(lndField, xgridField, routehandle=lnd2x, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
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

      call ESMF_FieldRegrid(xgridField, atmField, routehandle=x2atm, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    f_time = MPI_WTime()

    call time_analysis(f_time-i_time, 'Regridding 100 times', rc)

    chksum = checksum(lndPtr, rc)
    if(I_AM_PET(0, rc)) print *, 'global checksum = ', chksum

#ifdef TEST_MOSAIC_FIELDWRITE
    call ESMF_FieldWrite(atmField, filename="atmField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

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

  subroutine ESMF_XGridTest3(rc)

    integer, intent(out)   :: rc

    type(ESMF_Mesh)        :: atmMesh, lndMesh
    type(ESMF_XGrid)       :: xgrid
    type(ESMF_Field)       :: atmField, lndField, xgridField, xgridFieldSummed
    type(ESMF_Routehandle) :: lnd2x, x2atm
    real(RKIND), pointer   :: lndPtr(:,:), xPtr(:,:), atmPtr(:), xPtrSummed(:)
    character(len=128)     :: tmpstr
    integer                :: i,j,n_iter=100
    integer(IKIND)         :: chksum
    real(RKIND)            :: i_time, f_time, delta_time

    i_time = MPI_WTime()
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
    f_time = MPI_WTime()
    call time_analysis(f_time-i_time, 'Constructing mesh and xgrid', rc)

    atmField = ESMF_FieldCreate(atmMesh, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(atmField, farrayPtr=atmPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    lndField = ESMF_FieldCreate(lndMesh, typekind=RTKIND, &
      ungriddedLbound=(/1/), ungriddedUBound=(/10/), &
      rc=rc)
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

    xgridField = ESMF_FieldCreate(xgrid, typekind=RTKIND, &
      ungriddedLbound=(/1/), ungriddedUBound=(/10/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgridFieldSummed = ESMF_FieldCreate(xgrid, typekind=RTKIND, &
      rc=rc)
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

    call ESMF_FieldRegridStore(xgrid, xgridFieldSummed, atmField, &
      routehandle=x2atm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    i_time = MPI_Wtime()

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

      call ESMF_FieldGet(xgridFieldSummed, farrayPtr=xPtrSummed, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do j = 1, size(xPtrSummed)
        xPtrSummed(j) = sum(xPtr(j,:))
      enddo

      call ESMF_FieldRegrid(xgridFieldSummed, atmField, routehandle=x2atm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    f_time = MPI_WTime()

    call time_analysis(f_time-i_time, 'Regridding 100 times', rc)

    chksum = checksum(atmPtr, rc)
    if(I_AM_PET(0, rc)) print *, 'global checksum = ', chksum

    call ESMF_FieldWrite(atmField, filename="atmField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef TEST_2DPLUS1_FIELDWRITE
    call ESMF_FieldWrite(lndField, filename="lndField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    call ESMF_FieldWrite(xgridFieldSummed, filename="xgridFieldSummed.nc", &
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

    checksum = sum( INT(ivar, 8))
    !print *, 'per pet checksum = ', checksum, sum(fptr), sum(ivar)

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

  function checksum2D(fptr, rc)
    real(RKIND), pointer                 :: fptr(:,:)
    integer, intent(out)                 :: rc
    integer(IKIND)                       :: checksum2D

    integer(IKIND)                       :: ivar(size(fptr))
    integer(IKIND)                       :: mold(1)
    integer                              :: nsize
    integer(IKIND), allocatable          :: send(:), recv(:)
    type(ESMF_VM)                        :: vm
    
    
    ivar = transfer(fptr, mold)

    checksum2D = sum( INT(ivar, 8))
    !print *, 'per pet checksum = ', checksum2D, sum(fptr), sum(ivar)

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(send(1), recv(1))
    send(1) = checksum2D
    nsize = 1
    call ESMF_VMReduce(vm, sendData=send, recvData=recv, &
      count=nsize, reduceFlag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    checksum2D = recv(1)
    deallocate(send, recv)

  end function

  function i_am_pet(pet, rc)
    integer, intent(in)               :: pet
    integer, intent(out), optional    :: rc
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

  function petnum(rc)
    integer                           :: petnum
    integer, intent(out), optional    :: rc

    type(ESMF_VM)                     :: vm
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localpet = petnum, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end function

  subroutine time_analysis(deltaT, tag, rc)
    real(RKIND), intent(in)              :: deltaT
    character(len=*), intent(in)         :: tag
    integer, intent(out)                 :: rc

    integer                              :: nsize, npet, lpet
    real(RKIND), allocatable             :: send(:), recv(:)
    real(RKIND)                          :: mean, stddev
    type(ESMF_VM)                        :: vm

    rc = ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !print *, 'petNo=', lpet, 'deltaT=', deltaT

    allocate(send(1), recv(npet))
    send(1) = deltaT
    nsize = 1
    call ESMF_VMGather(vm, sendData=send, recvData=recv, &
      count=nsize, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !if(I_AM_PET(0)) print *, trim(tag)//":", recv

    if(I_AM_PET(0)) then
      mean = SUM(recv(1:npet)) / npet
      if(npet .gt. 1) stddev = SQRT (SUM((recv(1:npet)-mean)**2) / (npet - 1)) 
      print *, trim(tag)//": min=", minval(recv)
      print *, trim(tag)//": max=", maxval(recv)
      print *, trim(tag)//": mean=", mean
      if(npet .gt. 1) print *, trim(tag)//": stddev=", stddev
    endif

    deallocate(send, recv)

  end subroutine

  function tripolar_grid(rc)

    type(ESMF_Grid)        :: tripolar_grid
    integer, intent(out)   :: rc

    type(ESMF_Grid)        :: ocnGrid
    integer                :: nx, ny
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    type(ESMF_DistGrid)    :: dg
    type(ESMF_Array)       :: array_plon, array_plat, array_qlon, array_qlat, array_msk, array_area
    integer                :: minIdx(2,1), maxIdx(2,1)

    ! Create a 0.5 degree tripolar ocn grid from netCDF files
    nx = 720
    ny = 410
    ! prepare a single connection for periodicity along i-axis (longitude)
    allocate(connectionList(1))
    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
      tileIndexB=1, positionVector=(/nx, 0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! ready to create the HYCOM DistGrid from deBlockList with periodic connect.
    dg = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/nx,ny/), &
      connectionList=connectionList, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    deallocate(connectionList)

    ocnGrid = ESMF_GridCreate(dg, &
      gridEdgeUWidth=(/0,1/), &
      coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Add the center stagger longitude coordinate Array
    call ESMF_GridAddCoord(ocnGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Add the corner stagger longitude coordinate Array
    call ESMF_GridAddCoord(ocnGrid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Set up the coordinates
    call ESMF_GridGetCoord(ocnGrid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
      array=array_qlon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(array_qlon, minIndexPTile=minidx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(array_qlon, maxIndexPTile=maxidx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, petnum(rc), 'min=', minidx, 'max=', maxidx
    call ESMF_ArrayRead(array_qlon, filename="corner_lon_0.5.nc", &
      variablename="lon_corner", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridSetCoord(ocnGrid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
      array=array_qlon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridGetCoord(ocnGrid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
      array=array_qlat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(array_qlat, minIndexPTile=minidx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(array_qlat, maxIndexPTile=maxidx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, petnum(rc), 'min=', minidx, 'max=', maxidx
    call ESMF_ArrayRead(array_qlat, filename="corner_lat_0.5.nc", &
      variablename="lat_corner", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridWriteVTK(ocnGrid, staggerloc=ESMF_STAGGERLOC_CORNER, filename="ocnTripolarGrid", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Set masking information on the ocean Grid
    array_msk = ESMF_ArrayCreate(dg, typekind=ESMF_TYPEKIND_I4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayRead(array_msk, filename="mask_0.5.nc", &
      variablename="mask", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridSetItem(ocnGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_MASK, array=array_msk, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    tripolar_grid = ocnGrid

  end function

  ! realistic examples using SST with masking
  subroutine ESMF_XGridTest4(rc)

    integer, intent(out)   :: rc

    type(ESMF_Grid)        :: atmGrid
    type(ESMF_Grid)        :: ocnGrid
    type(ESMF_XGrid)       :: xgrid
    type(ESMF_Field)       :: atmField, ocnField, xgridField
    type(ESMF_Routehandle) :: ocn2x, x2atm, atm2x, x2ocn
    real(RKIND), pointer   :: ocnPtr(:,:), xPtr(:)
    character(len=128)     :: tmpstr
    integer                :: i,n_iter=100
    integer(IKIND)         :: chksum
    real(RKIND)            :: i_time, f_time, delta_time
    integer                :: decomptile(2,6)

    i_time = MPI_WTime()
    ! Create Src Grid
    ! Set up decomposition for src Grid, this is optional but can be used to 
    ! map to the model internal data representation.
    decomptile(:,1)=(/2,2/)
    decomptile(:,2)=(/2,2/)
    decomptile(:,3)=(/2,2/)
    decomptile(:,4)=(/2,2/)
    decomptile(:,5)=(/2,2/)
    decomptile(:,6)=(/2,2/)

    atmGrid=ESMF_GridCreateMosaic(filename=trim("data/C48_mosaic.nc"), &
         tileFilePath="./data/", regDecompPTile=decomptile, &
         indexflag=ESMF_INDEX_GLOBAL, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ocnGrid = tripolar_grid(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgrid = ESMF_XGridCreate(sideAGrid=(/atmGrid/), sideBGrid=(/ocnGrid/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    f_time = MPI_WTime()
    call time_analysis(f_time-i_time, 'Constructing mesh and xgrid', rc)

    atmField = ESMF_FieldCreate(atmGrid, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ocnField = ESMF_FieldCreate(ocnGrid, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRead(ocnField, filename="sst_0.5.nc", &
      variablename="sea_surface_temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgridField = ESMF_FieldCreate(xgrid, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(xgrid, ocnField, xgridField, &
      routehandle=ocn2x, rc=rc)
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

    call ESMF_FieldRegridStore(xgrid, atmField, xgridField, &
      routehandle=atm2x, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(xgrid, xgridField, ocnField, &
      routehandle=x2ocn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    i_time = MPI_Wtime()

    do i = 1, n_iter
      call ESMF_FieldRegrid(ocnField, xgridField, routehandle=ocn2x, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldGet(xgridField, farrayPtr=xPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldRegrid(xgridField, atmField, routehandle=x2atm, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    f_time = MPI_WTime()

    call time_analysis(f_time-i_time, 'Regridding 100 times', rc)

    ! regrid back to ocn grid and dump out result
    call ESMF_FieldRegrid(atmField, xgridField, routehandle=atm2x, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldRegrid(xgridField, ocnField, routehandle=x2ocn, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !call ESMF_FieldWrite(ocnField, filename='ocnField_regridded.nc',  &
    !  overwrite=.true., &
    !  variableName="sea_surface_temperature", rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    !chksum = checksum2D(ocnPtr, rc)
    !if(I_AM_PET(0, rc)) print *, 'global checksum = ', chksum

#ifdef TEST_MOSAIC_FIELDWRITE
    call ESMF_FieldWrite(atmField, filename="atmField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    call ESMF_FieldWrite(ocnField, filename="ocnField.nc", &
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

    call ESMF_FieldDestroy(ocnField, rc=rc)
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

  ! Regrid from tripolar grid to regular lat-lon grid
  subroutine ESMF_XGridTest5(rc)

    integer, intent(out)   :: rc

    type(ESMF_Grid)        :: atmGrid
    type(ESMF_Grid)        :: ocnGrid
    type(ESMF_XGrid)       :: xgrid
    type(ESMF_Field)       :: atmField, ocnField, xgridField
    type(ESMF_Routehandle) :: ocn2x, x2atm
    real(RKIND), pointer   :: ocnPtr(:,:), xPtr(:)
    character(len=128)     :: tmpstr
    integer                :: i,n_iter=100
    integer(IKIND)         :: chksum
    real(RKIND)            :: i_time, f_time, delta_time
    type(ESMF_Mesh)        :: xmesh

    i_time = MPI_WTime()
    ! Create Src Grid

    atmGrid=ESMF_GridCreate1PeriDimUfrm(&
       !maxIndex=(/360, 200/), &
       maxIndex=(/180, 100/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-81.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CORNER/), &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ocnGrid=tripolar_grid(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Ocn side masking: 0 => land, 1 => ocean
    xgrid = ESMF_XGridCreate(sideAGrid=(/atmGrid/), sideBGrid=(/ocnGrid/), &
      sideBMaskValues=(/0/), storeOverlay=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    f_time = MPI_WTime()
    call time_analysis(f_time-i_time, 'Constructing mesh and xgrid', rc)

    call ESMF_XGridGet(xgrid, mesh=xmesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_MeshWrite(xmesh, "xmesh", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    atmField = ESMF_FieldCreate(atmGrid, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ocnField = ESMF_FieldCreate(ocnGrid, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRead(ocnField, filename="sst_0.5.nc", &
      variablename="sea_surface_temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    xgridField = ESMF_FieldCreate(xgrid, typekind=RTKIND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(xgrid, ocnField, xgridField, &
      routehandle=ocn2x, rc=rc)
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

    i_time = MPI_Wtime()

    do i = 1, n_iter
      call ESMF_FieldRegrid(ocnField, xgridField, routehandle=ocn2x, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldGet(xgridField, farrayPtr=xPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldRegrid(xgridField, atmField, routehandle=x2atm, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    f_time = MPI_WTime()

    call time_analysis(f_time-i_time, 'Regridding 100 times', rc)

    !chksum = checksum2D(ocnPtr, rc)
    !if(I_AM_PET(0, rc)) print *, 'global checksum = ', chksum

    call ESMF_FieldWrite(atmField, filename="atmField.nc", &
      overwrite=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldWrite(ocnField, filename="ocnField.nc", &
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

    call ESMF_FieldDestroy(ocnField, rc=rc)
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

!- Add Mosaic test case
!- Add 3D Field regridding demo
!- Add mask and SST data set
!- TODO: Add ice-ocn-atm demo
