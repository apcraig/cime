!================================================================================
module shr_nuopc_grid_mod

  use shr_kind_mod          , only : r8 => SHR_KIND_r8, IN => SHR_KIND_I4, CXX => SHR_KIND_CXX
  use shr_kind_mod          , only : CL => SHR_KIND_CL, CX => SHR_KIND_CX, CS => SHR_KIND_CS
  use shr_sys_mod           , only : shr_sys_abort
  use shr_string_mod        , only : shr_string_listGetName
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_State_reset
  use ESMF
  use NUOPC

  implicit none
  save
  private

  public :: shr_nuopc_grid_ArbInit
  public :: shr_nuopc_grid_DEInit
  public :: shr_nuopc_grid_RegInit
  public :: shr_nuopc_grid_MeshInit
  public :: shr_nuopc_grid_ArrayToState
  public :: shr_nuopc_grid_StateToArray

  integer :: dbrc
  character(len=1024) :: tmpstr
  character(len=*), parameter :: u_FILE_u = &
    __FILE__

!-----------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------

  subroutine shr_nuopc_grid_ArbInit(nx_global, ny_global, mpicom, gindex, EGrid, rc)

    !-----------------------------------------
    ! create a Egrid object for Fields
    !-----------------------------------------

    integer         , intent(in)    :: nx_global
    integer         , intent(in)    :: ny_global
    integer         , intent(in)    :: mpicom
    integer         , intent(in)    :: gindex(:)
    type(ESMF_Grid) , intent(inout) :: Egrid
    integer         , intent(inout) :: rc

    !--- local ---
    integer          :: n
    integer          :: iam,ierr
    integer          :: lsize
    integer, pointer :: localArbIndex(:,:)

    character(len=*),parameter :: subname='(shr_nuopc_grid_ArbInit)'
    !--------------------------------------------------------------

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, iam, ierr)
    call ESMF_LogWrite(subname, ESMF_LOGMSG_INFO, rc=dbrc)

    lsize = size(gindex)
    allocate(localArbIndex(lsize,2))
    do n = 1,lsize
       localArbIndex(n,1) = mod(gindex(n)-1,nx_global) + 1
       localArbIndex(n,2) = (gindex(n)-1)/nx_global + 1
    enddo

    Egrid=ESMF_GridCreate1PeriDim(&
         minIndex = (/1,1/), &
         maxIndex = (/nx_global,ny_global/), &
         arbIndexCount = lsize, &
         arbIndexList = localArbIndex, &
         periodicDim = 1, &
         coordSys = ESMF_COORDSYS_SPH_DEG, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    deallocate(localArbIndex)

    call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    ! call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
         staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
         staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

  end subroutine shr_nuopc_grid_ArbInit

  !-----------------------------------------------------------------------------

  subroutine shr_nuopc_grid_DEInit(gcomp, nx_global, ny_global, mpicom, compid, gindex, Egrid, rc)

    !-----------------------------------------
    ! create a Egrid object for Fields
    !-----------------------------------------

    type(ESMF_GridComp)             :: gcomp
    integer         , intent(in)    :: nx_global
    integer         , intent(in)    :: ny_global
    integer         , intent(in)    :: mpicom
    integer         , intent(in)    :: compid
    integer         , intent(in)    :: gindex(:)
    type(ESMF_Grid) , intent(inout) :: Egrid
    integer         , intent(inout) :: rc

    !--- local ---
    integer             :: n,n1,n2,ig,jg,cnt
    integer             :: de,decount,dimcount
    integer             :: iam,ierr
    integer             :: lsize,gsize,nblocks_tot,ngseg
    integer             :: lbnd(2),ubnd(2)
    integer             :: global_index
    integer, pointer    :: indexList(:)
    integer, pointer    :: deBlockList(:,:,:)
    integer, pointer    :: petMap(:)
    real(r8),pointer    :: falon(:),falat(:)
    real(r8),pointer    :: famask(:),faarea(:)
    integer, pointer    :: iarray2(:,:)
    real(r8),pointer    :: farray2(:,:)
    type(ESMF_DELayout) :: delayout
    type(ESMF_DistGrid) :: distgrid
    integer ,pointer    :: pes_local(:)
    integer ,pointer    :: pes_global(:)
    type(ESMF_VM)       :: vm
    integer             :: petCount
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    character(len=*),parameter :: subname='(shr_nuopc_grid_DEInit)'
    !--------------------------------------------------------------

    call MPI_COMM_RANK(mpicom, iam, ierr)

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u))  return  ! bail out

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    ! 1 gridcell per DE

    lsize = size(gindex)
    gsize = nx_global * ny_global

    write(tmpstr,'(a,4i8)') subname//' nx,ny,lsize = ',nx_global,ny_global,lsize,gsize
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    nblocks_tot = gsize

    allocate(deBlockList(2,2,nblocks_tot))
    allocate(petMap(nblocks_tot))

    write(tmpstr,'(a,1i8)') subname//' nblocks = ',nblocks_tot
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    allocate(pes_local(gsize))
    allocate(pes_global(gsize))

    pes_local(:) = 0
    do n = 1,lsize
       pes_local(gindex(n)) = iam
    end do

    call ESMF_VMAllReduce(vm, sendData=pes_local, recvData=pes_global, count=nx_global*ny_global, &
         reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u))  return  ! bail out

    ! Note that below this is a global search overall all the points - not just the ones
    ! passed in my gindex(:)
    n = 0
    do global_index = 1,gsize
       ig = mod(global_index-1,nx_global) + 1
       jg = (global_index-1)/nx_global + 1
       deBlockList(1,1,n) = ig
       deBlockList(1,2,n) = ig
       deBlockList(2,1,n) = jg
       deBlockList(2,2,n) = jg
       petMap(global_index) = pes_global(global_index)
       ! write(tmpstr,'(a,2i8)') subname//' IDs  = ',n,petMap(n)
       ! call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       ! write(tmpstr,'(a,3i8)') subname//' iglo = ',n,deBlockList(1,1,n),deBlockList(1,2,n)
       ! call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       ! write(tmpstr,'(a,3i8)') subname//' jglo = ',n,deBlockList(2,1,n),deBlockList(2,2,n)
       ! call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo

    deallocate(pes_local)
    deallocate(pes_global)

    delayout = ESMF_DELayoutCreate(petMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    allocate(connectionList(1))
    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
         tileIndexB=1, positionVector=(/nx_global, 0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=u_FILE_u)) &
         return  ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/nx_global,ny_global/), &
         !          indexflag = ESMF_INDEX_DELOCAL, &
         deBlockList=deBlockList, &
         delayout=delayout, &
         connectionList=connectionList, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    deallocate(deBlockList)
    deallocate(petMap)
    deallocate(connectionList)

    call ESMF_DistGridPrint(distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=u_FILE_u)) &
         return  ! bail out

    call ESMF_DistGridGet(distgrid=distgrid, localDE=0, elementCount=cnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    allocate(indexList(cnt))
    !      write(tmpstr,'(a,i8)') subname//' distgrid cnt= ',cnt
    !      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    call ESMF_DistGridGet(distgrid=distgrid, localDE=0, seqIndexList=indexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    !      write(tmpstr,'(a,4i8)') subname//' distgrid list= ',indexList(1),indexList(cnt),minval(indexList), maxval(indexList)
    !      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    deallocate(IndexList)

    Egrid = ESMF_GridCreate(distgrid=distgrid, &
         coordSys = ESMF_COORDSYS_SPH_DEG, &
         gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
         rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    call ESMF_GridGet(Egrid, localDEcount=DEcount, dimCount=dimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    !      write(tmpstr,'(a,2i8)') subname//' localDEcount = ',DEcount,lsize
    !      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    !      write(tmpstr,'(a,2i8)') subname//' dimCount = ',dimCount
    !      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    if (DEcount /= lsize) then
       call shr_sys_abort(subname//' DEcount /= lsize')
    endif

    call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    !      call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    !      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
         staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
         staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    allocate(falon(lsize),falat(lsize),famask(lsize),faarea(lsize))

    do n = 1,lsize
       DE = n-1

       !         write(tmpstr,'(a,3i8)') subname//' n,DE,lsize ',n,DE,lsize
       !         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       !         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
       !         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

       call ESMF_GridGetCoord(Egrid, coordDim=1, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbnd, computationalUBound=ubnd, &
            farrayPtr=farray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       farray2(1,1) = falon(n)

       !         write(tmpstr,'(a,5i8)') subname//' lbnd ubnd ',DE,lbnd,ubnd
       !         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

       call ESMF_GridGetCoord(Egrid, coordDim=2, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=farray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       farray2(1,1) = falat(n)

       call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_MASK, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=iarray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       iarray2(1,1) = nint(famask(n))

       call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_AREA, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=farray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       farray2(1,1) = faarea(n)

    enddo
    deallocate(falon,falat,famask,faarea)

  end subroutine shr_nuopc_grid_DEInit

  !-----------------------------------------------------------------------------

  subroutine shr_nuopc_grid_RegInit(nx_global, ny_global, mpicom, EGrid, rc)

    !-----------------------------------------
    ! create a Grid object for Fields
    !-----------------------------------------

    integer         , intent(in)    :: nx_global
    integer         , intent(in)    :: ny_global
    integer         , intent(in)    :: mpicom
    type(ESMF_Grid) ,intent(inout)  :: Egrid
    integer         , intent(inout) :: rc

    !--- local ---
    integer :: iam,ierr
    character(len=*),parameter :: subname='(shr_nuopc_grid_RegInit)'
    !--------------------------------------------------------------

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, iam, ierr)
    call ESMF_LogWrite(subname, ESMF_LOGMSG_INFO, rc=dbrc)

    Egrid = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/nx_global, ny_global/), &
         minCornerCoord=(/0._ESMF_KIND_R8, -180._ESMF_KIND_R8/), &
         maxCornerCoord=(/360._ESMF_KIND_R8, 180._ESMF_KIND_R8/), &
         coordSys=ESMF_COORDSYS_SPH_DEG, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=u_FILE_u)) &
         return  ! bail out

  end subroutine shr_nuopc_grid_RegInit

  !-----------------------------------------------------------------------------

  subroutine shr_nuopc_grid_MeshInit(gcomp, nx_global, ny_global, mpicom, compid, gindex, lon, lat, Emesh, rc)

    !-----------------------------------------
    ! create an Emesh object for Fields
    !-----------------------------------------

    type(ESMF_GridComp)               :: gcomp
    integer           , intent(in)    :: nx_global
    integer           , intent(in)    :: ny_global
    integer           , intent(in)    :: mpicom
    integer           , intent(in)    :: compid
    integer           , intent(in)    :: gindex(:)
    real(r8), pointer , intent(in)    :: lon(:)
    real(r8), pointer , intent(in)    :: lat(:)
    type(ESMF_Mesh)   , intent(inout) :: Emesh
    integer           , intent(inout) :: rc

    !--- local ---
    integer          :: n,n1,n2,de
    integer          :: iam,ierr
    integer          :: lsize
    integer          :: numTotElems, numNodes, numConn, nodeindx
    integer          :: iur,iul,ill,ilr
    integer          :: xid, yid, xid0, yid0
    real(r8)         :: lonur, lonul, lonll, lonlr
    integer, pointer :: iurpts(:)
    integer, pointer :: elemIds(:)
    integer, pointer :: elemTypes(:)
    integer, pointer :: elemConn(:)
    real(r8),pointer :: elemCoords(:)
    integer, pointer :: nodeIds(:)
    integer, pointer :: nodeOwners(:)
    real(r8),pointer :: nodeCoords(:)
    real(r8),pointer :: latG(:)
    real(r8),pointer :: lonG(:)
    integer ,pointer :: pes_local(:)
    integer ,pointer :: pes_global(:)
    integer, pointer :: recvOffsets(:)
    integer, pointer :: recvCounts(:)
    integer          :: sendData(1)
    type(ESMF_VM)    :: vm
    integer          :: petCount
    character(len=*),parameter :: subname='(shr_nuopc_grid_MeshInit)'
    !--------------------------------------------------------------

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, iam, ierr)
    call ESMF_LogWrite(subname, ESMF_LOGMSG_INFO, rc=dbrc)

    lsize = size(gindex)

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u))  return  ! bail out

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    allocate(latG(nx_global*ny_global))
    allocate(lonG(nx_global*ny_global))

    allocate(recvoffsets(petCount))
    allocate(recvCounts(petCount))

    sendData(1) = lsize
    call ESMF_VMGather(vm, sendData=sendData, recvData=recvCounts, count=1, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    call ESMF_VMBroadCast(vm, bcstData=recvCounts, count=petCount, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return  ! bail out

    recvoffsets(1) = 0
    do n = 2,petCount
       recvoffsets(n) = recvoffsets(n-1) + recvCounts(n-1)
    end do

    call ESMF_VMAllGatherV(vm, lat, lsize, latG, recvCounts, recvOffsets, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u))  return  ! bail out

    call ESMF_VMAllGatherV(vm, lon, lsize, lonG, recvCounts, recvOffsets, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u))  return  ! bail out

    deallocate(recvoffsets)
    deallocate(recvCounts)

    ! assumes quadrilaterals for each gridcell (element)
    ! element index matches gsmap index value
    ! nodeid at lower left of each gridcell matches gsmap index value
    ! assumes wrap around in x direction but no wrap in y direction
    ! node ids need to be described in counter clockwise direction
    ! node id associated with lower left cell is assigned to local PET
    ! node ids at top of y boundary assigned to the element to the right

    numTotElems = lsize

    allocate(elemIds(numTotElems))
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/)
    allocate(elemConn(4*numTotElems))
    allocate(elemCoords(2*numTotElems))

    allocate(nodeIds(numTotElems*4))
    nodeIds = -99

    elemIds(:) = gindex(:)

    numNodes = 0
    numConn = 0

    do n = 1,numTotElems
       elemTypes(n) = ESMF_MESHELEMTYPE_QUAD
       elemCoords(2*n-1) = lon(n)
       elemCoords(2*n)   = lat(n)

       do n1 = 1,4

          numNodes = numNodes + 1
          nodeindx = numNodes
          if (n1 == 1 .or. n1 == 3) xid = mod(elemIds(n)-1,nx_global) + 1
          if (n1 == 2 .or. n1 == 4) xid = mod(elemIds(n)  ,nx_global) + 1
          if (n1 == 1 .or. n1 == 2) yid = (elemIds(n)-1)/nx_global + 1
          if (n1 == 3 .or. n1 == 4) yid = (elemIds(n)-1)/nx_global + 2
          nodeIds(numNodes) = (yid-1) * nx_global + xid
          n2 = 0
          do while (n2 < numNodes - 1 .and. nodeindx == numNodes)
             n2 = n2 + 1
             if (nodeIds(numNodes) == nodeIds(n2)) nodeindx = n2
          enddo
          if (nodeindx /= numNodes) then
             numNodes = numNodes - 1
          endif

          numConn = numConn + 1
          elemConn(numConn) = nodeindx
       enddo
    enddo

    allocate(nodeCoords(2*numNodes))
    allocate(nodeOwners(numNodes))
    allocate(iurpts(numNodes))

    do n = 1,numNodes

       xid0 = mod(nodeIds(n)-1, nx_global) + 1
       yid0 = (nodeIds(n)-1) / nx_global + 1

       xid = xid0
       yid = max(min(yid0,ny_global),1)
       iur = (yid-1) * nx_global + xid
       iurpts(n) = iur

       xid = mod(xid0 - 2 + nx_global, nx_global) + 1
       yid = max(min(yid0,ny_global),1)
       iul = (yid-1) * nx_global + xid

       xid = mod(xid0 - 2 + nx_global, nx_global) + 1
       yid = max(min(yid0-1,ny_global),1)
       ill = (yid-1) * nx_global + xid

       xid = xid0
       yid = max(min(yid0-1,ny_global),1)
       ilr = (yid-1) * nx_global + xid

       ! write(tmpstr,'(2a,8i6)') subname,' nodecoord = ',n,nodeIds(n),xid0,yid0,iur,iul,ill,ilr
       ! call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

       ! need to normalize lon values to same 360 degree setting, use lonur as reference value
       lonur = lonG(iur)
       lonul = lonG(iul)
       lonll = lonG(ill)
       lonlr = lonG(ilr)

       if (abs(lonul + 360._r8 - lonur) < abs(lonul - lonur)) lonul = lonul + 360._r8
       if (abs(lonul - 360._r8 - lonur) < abs(lonul - lonur)) lonul = lonul - 360._r8
       if (abs(lonll + 360._r8 - lonur) < abs(lonll - lonur)) lonll = lonll + 360._r8
       if (abs(lonll - 360._r8 - lonur) < abs(lonll - lonur)) lonll = lonll - 360._r8
       if (abs(lonlr + 360._r8 - lonur) < abs(lonlr - lonur)) lonlr = lonlr + 360._r8
       if (abs(lonlr - 360._r8 - lonur) < abs(lonlr - lonur)) lonlr = lonlr - 360._r8

       nodeCoords(2*n-1) = 0.25_r8 * (lonur + lonul + lonll + lonlr)
       nodeCoords(2*n)   = 0.25_r8 * (latG(iur) + latG(iul) + latG(ill) + latG(ilr))
    enddo

    deallocate(lonG)
    deallocate(latG)

    ! Determine the pes that own each index of iurpts (nodeOwners)

    allocate(pes_local(nx_global*ny_global))
    allocate(pes_global(nx_global*ny_global))
    pes_local(:) = 0
    do n = 1,lsize
       pes_local(gindex(n)) = iam
    end do

    call ESMF_VMAllReduce(vm, sendData=pes_local, recvData=pes_global, count=nx_global*ny_global, &
         reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u))  return  ! bail out

    do n = 1,numNodes
       nodeOwners(n) = pes_global(iurpts(n))
    end do
    deallocate(pes_local)
    deallocate(pes_global)

    ! do n = 1,numtotelems
    !   write(tmpstr,'(2a,2i8,2g13.6)') subname,' elemA = ',n,elemIds(n),elemCoords(2*n-1:2*n)
    !   call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    !   write(tmpstr,'(2a,6i8)') subname,' elemB = ',n,elemIds(n),nodeIds(elemConn(4*n-3)),&
    !      nodeIds(elemConn(4*n-2)),nodeIds(elemConn(4*n-1)),nodeIds(elemConn(4*n))
    !   call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    ! enddo
    ! do n = 1,numNodes
    !   write(tmpstr,'(2a,3i8,2g13.6)') subname,' nodesA = ',n,nodeIds(n),nodeOwners(n),nodeCoords(2*n-1:2*n)
    !   call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    ! enddo

    Emesh = ESMF_MeshCreate(parametricDim=2, &
         spatialDim=2, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         nodeIds=nodeIds(1:numNodes), &
         nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, &
         elementIds=elemIds,&
         elementTypes=elemTypes, &
         elementConn=elemConn, &
         elementCoords=elemCoords, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    deallocate(iurpts)
    deallocate(nodeIds, nodeCoords, nodeOwners)
    deallocate(elemIds, elemTypes, elemConn, elemCoords)

  end subroutine shr_nuopc_grid_MeshInit

  !-----------------------------------------------------------------------------

  subroutine shr_nuopc_grid_ArrayToState(array, rList, state, grid_option, rc)

    ! copy avect data to state fields

    implicit none

    !----- arguments -----
    real(r8)         , intent(inout) :: array(:,:)
    character(len=*) , intent(in)    :: rList
    type(ESMF_State) , intent(inout) :: state
    character(len=*) , intent(in)    :: grid_option
    integer          , intent(out)   :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: farray1(:)

    character(*),parameter :: subName = "(shr_nuopc_grid_ArrayToState)"
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    nflds = size(array, dim=1)
    lsize = size(array, dim=2)

    call shr_nuopc_methods_State_reset(state, value = -9999._R8, rc=rc)

    do nf = 1,nflds

      rc = ESMF_SUCCESS
      call shr_string_listGetName(rList, nf, fldname, dbrc)
      call ESMF_StateGet(state, itemName=trim(fldname), field=lfield, rc=rc)

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) then

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" not found on state", ESMF_LOGMSG_INFO, rc=dbrc)

      elseif (grid_option == "grid_de") then

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        do n = 1,lsize
          DE = n-1
          call ESMF_FieldGet(lfield, localDE=DE, farrayPtr=farray2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          farray2(1,1) = array(nf,n)
        enddo

      elseif (grid_option == 'mesh' .or. grid_option == 'arb') then

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        call ESMF_FieldGet(lfield, farrayPtr=farray1, rc=rc)
        do n = 1,lsize
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
           farray1(n) = array(nf,n)
        enddo
        write(tmpstr,'(a,3g13.6)') trim(subname)//":"//trim(fldname)//"=",minval(farray1),maxval(farray1),sum(farray1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      else

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)
     endif

    enddo

  end subroutine shr_nuopc_grid_ArrayToState

  !-----------------------------------------------------------------------------

  subroutine shr_nuopc_grid_StateToArray(state, array, rList, grid_option, rc)

    ! copy state fields to avect data

    implicit none

    !----- arguments -----
    type(ESMF_State) , intent(in)    :: state
    real(r8)         , intent(inout) :: array(:,:)
    character(len=*) , intent(in)    :: rList
    character(len=*) , intent(in)    :: grid_option
    integer          , intent(out)   :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: farray1(:)

    character(*),parameter :: subName = "(shr_nuopc_grdi_ArrayToAvect)"
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    nflds = size(array, dim=1)
    lsize = size(array, dim=2)

    do nf = 1,nflds

      rc = ESMF_SUCCESS
      call shr_string_listGetName(rList, nf, fldname, dbrc)
      call ESMF_StateGet(state, itemName=trim(fldname), field=lfield, rc=rc)

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) then

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" not found on state", ESMF_LOGMSG_INFO, rc=dbrc)

      elseif (grid_option == "grid_de") then

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        do n = 1,lsize
          DE = n-1
          call ESMF_FieldGet(lfield, localDE=DE, farrayPtr=farray2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          array(nf,n) = farray2(1,1)
        enddo

      elseif (grid_option == 'mesh' .or. grid_option == 'arb') then

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        call ESMF_FieldGet(lfield, farrayPtr=farray1, rc=rc)
        do n = 1,lsize
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          array(nf,n) = farray1(n)
        enddo
        write(tmpstr,'(a,3g13.6)') trim(subname)//":"//trim(fldname)//"=",minval(farray1),maxval(farray1),sum(farray1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      else

        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)

      endif

    enddo

  end subroutine shr_nuopc_grid_StateToArray

end module shr_nuopc_grid_mod
