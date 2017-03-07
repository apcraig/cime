!================================================================================
module shr_nuopc_dmodel_mod

#ifdef NUOPC_INTERFACE 

  use shr_kind_mod,only : r8 => SHR_KIND_r8, IN => SHR_KIND_I4, CXX => SHR_KIND_CXX
  use shr_kind_mod,only : CL => SHR_KIND_CL, CX => SHR_KIND_CX, CS => SHR_KIND_CS
  use shr_sys_mod, only : shr_sys_abort
  use seq_infodata_mod, only: seq_infodata_type, seq_infodata_PutData
  use shr_nuopc_methods_mod, only: shr_nuopc_methods_State_reset
  use ESMF
  use NUOPC
  use mct_mod

  implicit none
  save
  private
  integer :: dbrc

  public :: shr_nuopc_dmodel_GridInit
  public :: shr_nuopc_dmodel_AttrCopyToInfodata
  public :: shr_nuopc_dmodel_AvectToState
  public :: shr_nuopc_dmodel_StateToAvect

  character(len=1024) :: tmpstr
  character(len=*), parameter :: u_FILE_u = &
    __FILE__

!================================================================================
contains
!-----------------------------------------------------------------------------

  subroutine shr_nuopc_dmodel_GridInit(nx_global,ny_global,mpicom,gsMap,gGrid,grid_option,EGrid,Emesh,rc)

    ! initialize an Egrid or Emesh from gsmap

    integer, intent(in) :: nx_global
    integer, intent(in) :: ny_global
    integer, intent(in) :: mpicom
    type(mct_gsMap),intent(in) :: gsMap
    type(mct_gGrid),intent(in) :: gGrid
    character(len=*), intent(in) :: grid_option
    type(ESMF_Grid), optional, intent(inout) :: Egrid
    type(ESMF_Mesh), optional, intent(inout) :: Emesh
    integer, intent(inout) :: rc

    !--- local ---
    integer :: n,n1,n2,ig,jg,cnt,de,decount,dimcount
    integer :: iam,ierr
    integer :: lsize,gsize,nblocks_tot,ngseg
    integer :: lbnd(2),ubnd(2)
    type(ESMF_DELayout)  :: delayout
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
!GRID_ARB
    integer, pointer     :: localArbIndex(:,:),gindex1(:)
    integer, pointer     :: iarray1(:)
    real(r8),pointer     :: farray1(:)
!GRID_DE
    integer              :: peID,gindex
    integer, pointer     :: indexList(:)
    integer, pointer     :: deBlockList(:,:,:)
    integer, pointer     :: petMap(:)
    real(r8),pointer     :: falon(:),falat(:),famask(:),faarea(:)
    integer, pointer     :: iarray2(:,:)
    real(r8),pointer     :: farray2(:,:)
!MESH
    integer              :: numTotElems, numNodes, numConn, nodeindx
    integer              :: iur,iul,ill,ilr
    integer              :: xid, yid, xid0, yid0
    integer              :: klon, klat
    real(r8)             :: lonur, lonul, lonll, lonlr
    integer, pointer     :: iurpts(:)
    integer, pointer     :: elemIds(:)
    integer, pointer     :: elemTypes(:)
    integer, pointer     :: elemConn(:)
    real(r8),pointer     :: elemCoords(:)
    integer, pointer     :: nodeIds(:)
    integer, pointer     :: nodeOwners(:)
    real(r8),pointer     :: nodeCoords(:)

    type(mct_gGrid)      :: ggridG
    character(len=*),parameter :: subname='(shr_nuopc_dmodel_GridInit)'

    rc = ESMF_SUCCESS

    !-----------------------------------------
    ! create a Grid object for Fields
    !-----------------------------------------

    call MPI_COMM_RANK(mpicom, iam, ierr)
    call ESMF_LogWrite(subname//' grid_option = '//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
    if (grid_option == 'grid_arb') then
      lsize = mct_gsMap_lsize(gsMap, mpicom)
      allocate(localArbIndex(lsize,2))
      allocate(gindex1(lsize))
      call mct_gsMap_OrderedPoints(gsMap, iam, gindex1)
      do n = 1,lsize
        localArbIndex(n,1) = mod(gindex1(n)-1,nx_global) + 1
        localArbIndex(n,2) = (gindex1(n)-1)/nx_global + 1
      enddo

      Egrid=ESMF_GridCreate1PeriDim(minIndex = (/1,1/), maxIndex = (/nx_global,ny_global/), &
         arbIndexCount = lsize, arbIndexList = localArbIndex, &
         periodicDim = 1, coordSys = ESMF_COORDSYS_SPH_DEG, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      deallocate(gindex1, localArbIndex)

      DE = 0
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

      call ESMF_GridGetCoord(Egrid, coordDim=1, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, & 
        farrayPtr=farray1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      call mct_gGrid_exportRattr(ggrid,'lon',farray1,lsize)

      call ESMF_GridGetCoord(Egrid, coordDim=2, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, & 
        farrayPtr=farray1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      call mct_gGrid_exportRattr(ggrid,'lat',farray1,lsize)

!      call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_MASK, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
!        farrayPtr=iarray1, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
!      call mct_gGrid_exportRattr(ggrid,'mask',farray1,lsize)
!      iarray1 = nint(farray1)

!      call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_AREA, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
!        farrayPtr=farray1, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
!      call mct_gGrid_exportRattr(ggrid,'area',farray1,lsize)

    elseif (grid_option == 'grid_de') then

      ! 1 gridcell per DE

      lsize = mct_gsMap_lsize(gsMap, mpicom)
      gsize = mct_gsMap_gsize(gsMap)
      write(tmpstr,'(a,4i8)') subname//' nx,ny,lsize = ',nx_global,ny_global,lsize,gsize
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      nblocks_tot = gsize

      allocate(deBlockList(2,2,nblocks_tot))
      allocate(petMap(nblocks_tot))

      write(tmpstr,'(a,1i8)') subname//' nblocks = ',nblocks_tot
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      n = 0
      do n1 = 1,gsmap%ngseg
      do n2 = 1,gsmap%length(n1)
         n = n + 1
         gindex = gsmap%start(n1) + n2 - 1
         ig = mod(gindex-1,nx_global) + 1
         jg = (gindex-1)/nx_global + 1
         deBlockList(1,1,n) = ig
         deBlockList(1,2,n) = ig
         deBlockList(2,1,n) = jg
         deBlockList(2,2,n) = jg
         petMap(n) = gsmap%pe_Loc(n1)
!         write(tmpstr,'(a,2i8)') subname//' IDs  = ',n,petMap(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,3i8)') subname//' iglo = ',n,deBlockList(1,1,n),deBlockList(1,2,n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,3i8)') subname//' jglo = ',n,deBlockList(2,1,n),deBlockList(2,2,n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
      enddo
      enddo

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
      call mct_gGrid_exportRattr(ggrid,'lon',falon,lsize)
      call mct_gGrid_exportRattr(ggrid,'lat',falat,lsize)
      call mct_gGrid_exportRattr(ggrid,'mask',famask,lsize)
      call mct_gGrid_exportRattr(ggrid,'area',faarea,lsize)

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

    elseif (grid_option == 'grid_reg') then

      Egrid = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/nx_global, ny_global/), &
        minCornerCoord=(/0._ESMF_KIND_R8, -180._ESMF_KIND_R8/), &
        maxCornerCoord=(/360._ESMF_KIND_R8, 180._ESMF_KIND_R8/), &
        coordSys=ESMF_COORDSYS_SPH_DEG, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=u_FILE_u)) &
        return  ! bail out

    elseif (grid_option == 'mesh') then

      ! assumes quadrilaterals for each gridcell (element)
      ! element index matches gsmap index value
      ! nodeid at lower left of each gridcell matches gsmap index value
      ! assumes wrap around in x direction but no wrap in y direction
      ! node ids need to be described in counter clockwise direction
      ! node id associated with lower left cell is assigned to local PET
      ! node ids at top of y boundary assigned to the element to the right

      numTotElems = mct_gsMap_lsize(gsMap, mpicom)

      allocate(elemIds(numTotElems))
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/)
      allocate(elemConn(4*numTotElems))
      allocate(elemCoords(2*numTotElems))

      allocate(nodeIds(numTotElems*4))
      nodeIds = -99

      call mct_gsMap_OrderedPoints(gsMap, iam, elemIds)

      numNodes = 0
      numConn = 0
      klat = mct_aVect_indexRA(ggrid%data,'lat')
      klon = mct_aVect_indexRA(ggrid%data,'lon')

      do n = 1,numTotElems
        elemTypes(n) = ESMF_MESHELEMTYPE_QUAD
        elemCoords(2*n-1) = ggrid%data%rAttr(klon,n)
        elemCoords(2*n)   = ggrid%data%rAttr(klat,n)

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

      call mct_ggrid_gather(ggrid, ggridG, gsmap, 0, mpicom)
      call mct_ggrid_bcast(ggridG, 0, mpicom)
      klat = mct_aVect_indexRA(ggridG%data,'lat')
      klon = mct_aVect_indexRA(ggridG%data,'lon')

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

!        write(tmpstr,'(2a,8i6)') subname,' nodecoord = ',n,nodeIds(n),xid0,yid0,iur,iul,ill,ilr
!        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

        ! need to normalize lon values to same 360 degree setting, use lonur as reference value
        lonur = ggridG%data%rAttr(klon,iur)
        lonul = ggridG%data%rAttr(klon,iul)
        lonll = ggridG%data%rAttr(klon,ill)
        lonlr = ggridG%data%rAttr(klon,ilr)
        if (abs(lonul + 360._r8 - lonur) < abs(lonul - lonur)) lonul = lonul + 360._r8
        if (abs(lonul - 360._r8 - lonur) < abs(lonul - lonur)) lonul = lonul - 360._r8
        if (abs(lonll + 360._r8 - lonur) < abs(lonll - lonur)) lonll = lonll + 360._r8
        if (abs(lonll - 360._r8 - lonur) < abs(lonll - lonur)) lonll = lonll - 360._r8
        if (abs(lonlr + 360._r8 - lonur) < abs(lonlr - lonur)) lonlr = lonlr + 360._r8
        if (abs(lonlr - 360._r8 - lonur) < abs(lonlr - lonur)) lonlr = lonlr - 360._r8

        nodeCoords(2*n-1) = 0.25_r8 * (lonur + lonul + lonll + lonlr)
        nodeCoords(2*n)   = 0.25_r8 * (ggridG%data%rAttr(klat,iur) + ggridG%data%rAttr(klat,iul) + &
                                       ggridG%data%rAttr(klat,ill) + ggridG%data%rAttr(klat,ilr))

      enddo

      call mct_ggrid_clean(ggridG)

      call mct_gsmap_pelocs(gsmap,numNodes,iurpts,nodeOwners)
      deallocate(iurpts)

!      do n = 1,numtotelems
!        write(tmpstr,'(2a,2i8,2g13.6)') subname,' elemA = ',n,elemIds(n),elemCoords(2*n-1:2*n)
!        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!        write(tmpstr,'(2a,6i8)') subname,' elemB = ',n,elemIds(n),nodeIds(elemConn(4*n-3)),nodeIds(elemConn(4*n-2)),nodeIds(elemConn(4*n-1)),nodeIds(elemConn(4*n))
!        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!      enddo

!      do n = 1,numNodes
!        write(tmpstr,'(2a,3i8,2g13.6)') subname,' nodesA = ',n,nodeIds(n),nodeOwners(n),nodeCoords(2*n-1:2*n)
!        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!      enddo

      Emesh=ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           nodeIds=nodeIds(1:numNodes), nodeCoords=nodeCoords, &
           nodeOwners=nodeOwners, elementIds=elemIds,&
           elementTypes=elemTypes, elementConn=elemConn, &
           elementCoords=elemCoords, &
           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      deallocate(nodeIds, nodeCoords, nodeOwners)
      deallocate(elemIds, elemTypes, elemConn, elemCoords)

    else    ! grid_option

      call ESMF_LogWrite(subname//' ERROR: grid_option invalid = '//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
      rc = ESMF_FAILURE
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    endif   ! grid_option

  end subroutine shr_nuopc_dmodel_GridInit
  
!-----------------------------------------------------------------------------

  subroutine shr_nuopc_dmodel_AttrCopyToInfodata(gcomp, infodata, rc)
    ! Get specific set of attributes from gcomp and copy to infodata
    type(ESMF_GridComp)    ,intent(in)    :: gcomp
    type(seq_infodata_type),intent(inout) :: infodata
    integer                ,intent(inout) :: rc

    ! locals
    character(len=CL) :: cvalue
    integer  :: ivalue, n
    real(r8) :: rvalue
    logical  :: lvalue
    integer, parameter :: nattrlist = 22
    character(len=*), parameter, dimension(nattrlist) :: attrList = &
      (/ "case_name     ", "single_column ", "scmlat        ", "scmlon         ", &
         "orb_eccen     ", "orb_obliqr    ", "orb_lambm0    ", "orb_mvelpp     ", &
         "read_restart  ", "start_type    ", "tfreeze_option", "model_version  ", &
         "info_debug    ", "atm_aero      ", "atm_adiabatic ", "atm_ideal_phys ", &
         "aqua_planet   ", "brnch_rcase   ", "perpetual     ", "perpetual_ymd  ", &
         "hostname      ", "username      " /)
    character(len=*), parameter :: subname = "(shr_nuopc_dmodel_AttrCopyToInfodata)"

    rc = ESMF_Success

    do n = 1,nattrlist

      call NUOPC_CompAttributeGet(gcomp, name=trim(attrList(n)), value=cvalue, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=u_FILE_u)) &
        return  ! bail out

      select case(trim(attrList(n)))

      case("case_name")
        call seq_infodata_PutData(infodata, case_name=cvalue)

      case("single_column")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, single_column=lvalue)

      case ("scmlat")
        read(cvalue,*) rvalue
        call seq_infodata_PutData(infodata, scmlat=rvalue)

      case ("scmlon")
        read(cvalue,*) rvalue
        call seq_infodata_PutData(infodata, scmlon=rvalue)

      case("orb_eccen")
        read(cvalue,*) rvalue
        call seq_infodata_PutData(infodata, orb_eccen=rvalue)

      case("orb_obliqr")
        read(cvalue,*) rvalue
        call seq_infodata_PutData(infodata, orb_obliqr=rvalue)

      case("orb_lambm0")
        read(cvalue,*) rvalue
        call seq_infodata_PutData(infodata, orb_lambm0=rvalue)

      case("orb_mvelpp")
        read(cvalue,*) rvalue
        call seq_infodata_PutData(infodata, orb_mvelpp=rvalue)

      case("read_restart")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, read_restart=lvalue)

      case("start_type")
        call seq_infodata_PutData(infodata, start_type=cvalue)

      case("tfreeze_option")
        call seq_infodata_PutData(infodata, tfreeze_option=cvalue)

      case("model_version")
        call seq_infodata_PutData(infodata, model_version=cvalue)

      case("info_debug")
        read(cvalue,*) ivalue
        call seq_infodata_PutData(infodata, info_debug=ivalue)

      case("atm_aero")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, atm_aero=lvalue)

      case("atm_adiabatic")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, atm_adiabatic=lvalue)

      case("atm_ideal_phys")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, atm_ideal_phys=lvalue)

      case("aqua_planet")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, aqua_planet=lvalue)

      case("brnch_rcase")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, brnch_retain_casename=lvalue)

      case("perpetual")
        lvalue = (trim(cvalue) == "true")
        call seq_infodata_PutData(infodata, perpetual=lvalue)

      case("perpetual_ymd")
        read(cvalue,*) ivalue
        call seq_infodata_PutData(infodata, perpetual_ymd=ivalue)

      case("hostname")
        call seq_infodata_PutData(infodata, hostname=cvalue)

      case("username")
        call seq_infodata_PutData(infodata, username=cvalue)

      case default
        rc = ESMF_Failure
        call ESMF_LogWrite(trim(subname)//": unknown attrlist = "//trim(attrList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=u_FILE_u)) &
          return  ! bail out

      end select

    enddo


  end subroutine shr_nuopc_dmodel_AttrCopyToInfodata

!-----------------------------------------------------------------------------

  subroutine shr_nuopc_dmodel_AvectToState(avect, state, grid_option, rc)

    ! copy avect data to state fields

    implicit none

    !----- arguments -----
    type(mct_aVect) , intent(in)    :: avect
    type(ESMF_State), intent(inout) :: state
    character(len=*), intent(in)    :: grid_option
    integer, intent(out)            :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: farray1(:)
    real(ESMF_KIND_R8), pointer :: favect(:)

    character(*),parameter :: subName = "(shr_nuopc_dmodel_AvectToState)"
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    nflds = mct_avect_nRattr(avect)
    lsize = mct_avect_lsize(avect)
    rList = " "
    if (nflds > 0) rList = mct_aVect_ExportRList2c(avect)
    call shr_nuopc_methods_State_reset(state, value = -9999._R8, rc=rc)
    allocate(favect(lsize))

    do nf = 1,nflds

      rc = ESMF_SUCCESS
      call shr_string_listGetName(rList, nf, fldname, dbrc)
      call ESMF_StateGet(state, itemName=trim(fldname), field=lfield, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" not found on state", ESMF_LOGMSG_INFO, rc=dbrc)
      elseif (grid_option == "grid_de") then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        call mct_aVect_exportRAttr(avect, trim(fldname), favect, lsize)
        do n = 1,lsize
          DE = n-1
!         write(tmpstr,'(a,3i8)') subname//' n,DE,lsize ',n,DE,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, localDE=DE, farrayPtr=farray2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          farray2(1,1) = favect(n)
        enddo
      elseif (grid_option == 'mesh' .or. grid_option == 'arb') then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        call mct_aVect_exportRAttr(avect, trim(fldname), favect, lsize)
        do n = 1,lsize
!         write(tmpstr,'(a,3i8)') subname//' n,lsize ',n,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, farrayPtr=farray1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          farray1(n) = favect(n)
        enddo
        write(tmpstr,'(a,3g13.6)') trim(subname)//":"//trim(fldname)//"=",minval(farray1),maxval(farray1),sum(farray1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      else
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)
!        rc=ESMF_Failure
!        call ESMF_LogWrite(subname//" ERROR for grid_option = "//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      endif

    enddo

    deallocate(favect)

  end subroutine shr_nuopc_dmodel_AvectToState

!-----------------------------------------------------------------------------

  subroutine shr_nuopc_dmodel_StateToAvect(state, avect, grid_option, rc)

    ! copy state fields to avect data

    implicit none

    !----- arguments -----
    type(ESMF_State), intent(in)    :: state
    type(mct_aVect) , intent(inout) :: avect
    character(len=*), intent(in)    :: grid_option
    integer         , intent(out)   :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: farray1(:)
    real(ESMF_KIND_R8), pointer :: favect(:)

    character(*),parameter :: subName = "(shr_nuopc_dmodel_StateToAvect)"
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    nflds = mct_avect_nRattr(avect)
    lsize = mct_avect_lsize(avect)
    rList = " "
    if (nflds > 0) rList = mct_aVect_ExportRList2c(avect)
    call mct_avect_zero(avect)
    allocate(favect(lsize))

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
!         write(tmpstr,'(a,3i8)') subname//' n,DE,lsize ',n,DE,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, localDE=DE, farrayPtr=farray2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          favect(n) = farray2(1,1)
        enddo
        call mct_aVect_importRAttr(avect, trim(fldname), favect, lsize)
      elseif (grid_option == 'mesh' .or. grid_option == 'arb') then
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy", ESMF_LOGMSG_INFO, rc=dbrc)
        do n = 1,lsize
!         write(tmpstr,'(a,3i8)') subname//' n,lsize ',n,lsize
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!         write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!         call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_FieldGet(lfield, farrayPtr=farray1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          favect(n) = farray1(n)
        enddo
        call mct_aVect_importRAttr(avect, trim(fldname), favect, lsize)
        write(tmpstr,'(a,3g13.6)') trim(subname)//":"//trim(fldname)//"=",minval(farray1),maxval(farray1),sum(farray1)
        call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

      else
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)
!        rc=ESMF_Failure
!        call ESMF_LogWrite(subname//" ERROR for grid_option = "//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      endif

    enddo

    deallocate(favect)

  end subroutine shr_nuopc_dmodel_StateToAvect

!-----------------------------------------------------------------------------
  
#endif
  
end module shr_nuopc_dmodel_mod
  