module atm_comp_nuopc

#ifdef NUOPC_INTERFACE 
  use shr_kind_mod, only:  R8=>SHR_KIND_R8, IN=>SHR_KIND_IN, &
       CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_sys_mod   ! shared system calls

  use seq_flds_mod
  use seq_cdata_mod
  use seq_infodata_mod
  use seq_timemgr_mod, only: seq_timemgr_EClockGetData
  use shr_nuopc_fldList_mod
  use shr_nuopc_data_mod, only: n_ATMID, n_Eclock_a, n_infodata
  use shr_nuopc_stuff_mod, only: shr_nuopc_stuff_ClockTimePrint
  use shr_file_mod,  only : shr_file_getlogunit, shr_file_getloglevel, shr_file_setloglevel

  use ESMF
!  use esmfshr_mod
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance, &
    model_label_Finalize  => label_Finalize

  use datm_comp_mod
  use perf_mod
  use mct_mod
  use med_method_mod

  implicit none

  public :: SetServices

  private ! except

  integer,parameter :: fldsMax = 100
  type (shr_nuopc_fldList_Type) :: fldsToAtm
  type (shr_nuopc_fldList_Type) :: fldsFrAtm

  type(seq_cdata)         :: cdata
  type(mct_gsMap)         :: gsmap
  type(mct_gGrid)         :: ggrid
  type(mct_aVect)         :: x2d
  type(mct_aVect)         :: d2x
  integer                 :: mpicom, iam
  integer                 :: dbrc
  character(len=1024)     :: tmpstr
  character(len=*),parameter :: grid_option = "mesh"  ! grid_de, grid_arb, grid_reg, mesh

  !----- formats -----
  character(*),parameter :: modName =  "(atm_comp_nuopc)"

  !===============================================================================
  contains
  !===============================================================================

  subroutine SetServices(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if (1 == 0)
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#else
    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    character(len=10)     :: value
    type(ESMF_VM)         :: vm
    integer               :: lpet

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitializeP0
  
  !===============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(len=*),parameter  :: subname=trim(modName)//':(InitializeAdvertise) '
    
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! create import fields list
    call shr_nuopc_fldList_Zero(fldsToAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

!    call shr_nuopc_fldList_Add(fldsToAtm, "sea_surface_temperature", "will provide", 'datmImport', rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_states, "will provide", subname//":seq_flds_x2a_states", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_fromseqflds(fldsToAtm, seq_flds_x2a_fluxes, "will provide", subname//":seq_flds_x2a_fluxes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    ! create export fields list
    call shr_nuopc_fldList_Zero(fldsFrAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

!    call shr_nuopc_fldList_Add(fldsFrAtm, "air_pressure_at_sea_level", "will provide", 'datmExport', rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    call shr_nuopc_fldList_Add(fldsFrAtm, "surface_net_downward_shortwave_flux", "will provide", 'datmExport', rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_states, "will provide", subname//":seq_flds_a2x_states", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_fromseqflds(fldsFrAtm, seq_flds_a2x_fluxes, "will provide", subname//":seq_flds_a2x_fluxes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    ! advertise import and export

    call shr_nuopc_fldList_Advertise(importState, fldsToAtm, subname//':datmImport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_Advertise(exportState, fldsFrAtm, subname//':datmExport', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine InitializeAdvertise
  
  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)     :: field
    type(ESMF_Grid)      :: Egrid
    type(ESMF_Mesh)      :: Emesh
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
!
    integer              :: lsize,gsize,DE,nx_global,ny_global
    integer              :: n,n1,n2,ig,jg,cnt
    integer              :: nblocks_tot,DEcount,dimCount
    integer              :: logunit
    integer              :: lbnd(2),ubnd(2)
    character(len=*),parameter :: subname=trim(modName)//':(InitializeRealize) '

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    call shr_file_getLogUnit (logunit)

!    call atm_init_esmf(gcomp, importState, exportState, n_EClock_a, rc)
    call atm_init_esmf(gcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    !-----------------------------------------
    ! create a Grid object for Fields
    !-----------------------------------------

  call ESMF_LogWrite(subname//' grid_option = '//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
  if (grid_option == 'grid_arb') then
    call seq_infodata_GetData(n_infodata, atm_nx=nx_global, atm_ny=ny_global)
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    deallocate(gindex1, localArbIndex)

    DE = 0
    call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
       staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
       staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_GridGetCoord(Egrid, coordDim=1, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, & 
      farrayPtr=farray1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call mct_gGrid_exportRattr(ggrid,'lon',farray1,lsize)

    call ESMF_GridGetCoord(Egrid, coordDim=2, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, & 
      farrayPtr=farray1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call mct_gGrid_exportRattr(ggrid,'lat',farray1,lsize)

!    call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_MASK, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
!      farrayPtr=iarray1, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    call mct_gGrid_exportRattr(ggrid,'mask',farray1,lsize)
!    iarray1 = nint(farray1)

!    call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_AREA, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
!      farrayPtr=farray1, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    call mct_gGrid_exportRattr(ggrid,'area',farray1,lsize)

  elseif (grid_option == 'grid_de') then

    call seq_infodata_GetData(n_infodata, atm_nx=nx_global, atm_ny=ny_global)
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
!       write(tmpstr,'(a,2i8)') subname//' IDs  = ',n,petMap(n)
!       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!       write(tmpstr,'(a,3i8)') subname//' iglo = ',n,deBlockList(1,1,n),deBlockList(1,2,n)
!       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!       write(tmpstr,'(a,3i8)') subname//' jglo = ',n,deBlockList(2,1,n),deBlockList(2,2,n)
!       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    enddo

    delayout = ESMF_DELayoutCreate(petMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    allocate(connectionList(1))
    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
      tileIndexB=1, positionVector=(/nx_global, 0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/nx_global,ny_global/), &
!        indexflag = ESMF_INDEX_DELOCAL, &
        deBlockList=deBlockList, &
        delayout=delayout, &
        connectionList=connectionList, &
        rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    deallocate(deBlockList)
    deallocate(petMap)
    deallocate(connectionList)

    call ESMF_DistGridPrint(distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_DistGridGet(distgrid=distgrid, localDE=0, elementCount=cnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(indexList(cnt))
!    write(tmpstr,'(a,i8)') subname//' distgrid cnt= ',cnt
!    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    call ESMF_DistGridGet(distgrid=distgrid, localDE=0, seqIndexList=indexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    write(tmpstr,'(a,4i8)') subname//' distgrid list= ',indexList(1),indexList(cnt),minval(indexList), maxval(indexList)
!    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    deallocate(IndexList)

    Egrid = ESMF_GridCreate(distgrid=distgrid, &
       coordSys = ESMF_COORDSYS_SPH_DEG, &
       gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
       rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_GridGet(Egrid, localDEcount=DEcount, dimCount=dimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

!    write(tmpstr,'(a,2i8)') subname//' localDEcount = ',DEcount,lsize
!    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!    write(tmpstr,'(a,2i8)') subname//' dimCount = ',dimCount
!    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    if (DEcount /= lsize) then
       call shr_sys_abort(subname//' DEcount /= lsize')
    endif

    call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    call ESMF_GridAddCoord(Egrid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
       staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddItem(Egrid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
       staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    allocate(falon(lsize),falat(lsize),famask(lsize),faarea(lsize))
    call mct_gGrid_exportRattr(ggrid,'lon',falon,lsize)
    call mct_gGrid_exportRattr(ggrid,'lat',falat,lsize)
    call mct_gGrid_exportRattr(ggrid,'mask',famask,lsize)
    call mct_gGrid_exportRattr(ggrid,'area',faarea,lsize)

    do n = 1,lsize
       DE = n-1

!       write(tmpstr,'(a,3i8)') subname//' n,DE,lsize ',n,DE,lsize
!       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
!       write(tmpstr,'(a,i8,4g13.6)') subname//' grid values ',DE,falon(n),falat(n),famask(n),faarea(n)
!       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

       call ESMF_GridGetCoord(Egrid, coordDim=1, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, & 
         computationalLBound=lbnd, computationalUBound=ubnd, &
         farrayPtr=farray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       farray2(1,1) = falon(n)

!       write(tmpstr,'(a,5i8)') subname//' lbnd ubnd ',DE,lbnd,ubnd       
!       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

       call ESMF_GridGetCoord(Egrid, coordDim=2, localDE=DE, staggerLoc=ESMF_STAGGERLOC_CENTER, & 
         farrayPtr=farray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       farray2(1,1) = falat(n)

       call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_MASK, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=iarray2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        iarray2(1,1) = nint(famask(n))

       call ESMF_GridGetItem(Egrid, itemflag=ESMF_GRIDITEM_AREA, localDE=DE, staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=farray2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       farray2(1,1) = faarea(n)

    enddo
    deallocate(falon,falat,famask,faarea)

  elseif (grid_option == 'grid_reg') then

    call seq_infodata_GetData(n_infodata, atm_nx=nx_global, atm_ny=ny_global)
    Egrid = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/nx_global, ny_global/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -180._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 180._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_SPH_DEG, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  elseif (grid_option == 'mesh') then

    ! assumes quadrilaterals for each gridcell (element)
    ! element index matches gsmap index value
    ! nodeid at lower left of each gridcell matches gsmap index value
    ! assumes wrap around in x direction but no wrap in y direction
    ! node ids need to be described in counter clockwise direction
    ! node id associated with lower left cell is assigned to local PET
    ! node ids at top of y boundary assigned to the element to the right

    call seq_infodata_GetData(n_infodata, atm_nx=nx_global, atm_ny=ny_global)
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

      write(tmpstr,'(2a,8i6)') subname,' nodecoord = ',n,nodeIds(n),xid0,yid0,iur,iul,ill,ilr
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

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

    do n = 1,numtotelems
      write(tmpstr,'(2a,2i8,2g13.6)') subname,' elemA = ',n,elemIds(n),elemCoords(2*n-1:2*n)
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
      write(tmpstr,'(2a,6i8)') subname,' elemB = ',n,elemIds(n),nodeIds(elemConn(4*n-3)),nodeIds(elemConn(4*n-2)),nodeIds(elemConn(4*n-1)),nodeIds(elemConn(4*n))
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo

    do n = 1,numNodes
      write(tmpstr,'(2a,3i8,2g13.6)') subname,' nodesA = ',n,nodeIds(n),nodeOwners(n),nodeCoords(2*n-1:2*n)
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo

    Emesh=ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         nodeIds=nodeIds(1:numNodes), nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    deallocate(nodeIds, nodeCoords, nodeOwners)
    deallocate(elemIds, elemTypes, elemConn, elemCoords)

  else    ! grid_option

    call ESMF_LogWrite(subname//' ERROR: grid_option invalid = '//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
    rc = ESMF_FAILURE
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

  endif   ! grid_option

  if (grid_option == 'mesh') then

    call shr_nuopc_fldList_Realize(importState, mesh=Emesh, fldlist=fldsToAtm, tag=subname//':datmImport', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_Realize(exportState, mesh=Emesh, fldlist=fldsFrAtm, tag=subname//':datmExport', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

  else

    call shr_nuopc_fldList_Realize(importState, grid=Egrid, fldlist=fldsToAtm, tag=subname//':datmImport', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call shr_nuopc_fldList_Realize(exportState, grid=Egrid, fldlist=fldsFrAtm, tag=subname//':datmExport', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

  endif

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine InitializeRealize
  
  !===============================================================================

  subroutine SetClock(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep
    character(len=*),parameter  :: subname=trim(modName)//':(SetClock) '

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine SetClock

  !===============================================================================

  subroutine ModelAdvance(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: time
    type(ESMF_State)              :: importState, exportState
    integer :: CurrentYMD, CurrentTOD, yy, mm, dd, stepno, idt
    integer :: logunit, shrloglev
    character(len=128) :: calendar
    character(len=*),parameter  :: subname=trim(modName)//':(ModelAdvance) '

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    call shr_file_getLogUnit (logunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogLevel(max(shrloglev,1))

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!    call shr_nuopc_stuff_ClockTimePrint(n_EClock_a,rc=rc)
    call shr_nuopc_stuff_ClockTimePrint(clock,rc=rc)

!    call seq_timemgr_EClockGetData( n_EClock_a, curr_ymd=CurrentYMD, curr_tod=CurrentTOD)
!    call seq_timemgr_EClockGetData( n_EClock_a, curr_yr=yy, curr_mon=mm, curr_day=dd)
!    call seq_timemgr_EClockGetData( n_EClock_a, stepno=stepno, dtime=idt)
!    call seq_timemgr_EClockGetData( n_EClock_a, calendar=calendar)
    call seq_timemgr_EClockGetData( clock, curr_ymd=CurrentYMD, curr_tod=CurrentTOD)
    call seq_timemgr_EClockGetData( clock, curr_yr=yy, curr_mon=mm, curr_day=dd)
    call seq_timemgr_EClockGetData( clock, stepno=stepno, dtime=idt)
    call seq_timemgr_EClockGetData( clock, calendar=calendar)
    write(logunit,*) subname,' currymd,tod = ',CurrentYMD,CurrentTOD
    write(logunit,*) subname,' yy,mm,dd = ',yy,mm,dd
    write(logunit,*) subname,' stepno, idt = ',stepno,idt
    write(logunit,*) subname,' calendar = ',trim(calendar)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
!    call atm_run_esmf(gcomp, importState, exportState, n_EClock_a, rc)
    call atm_run_esmf(gcomp, importState, exportState, clock, rc)
!    call datm_comp_run(n_EClock_a, cdata, x2d, d2x)
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call shr_file_setLogLevel(shrloglev)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelAdvance

  !===============================================================================

  subroutine ModelFinalize(gcomp, rc)
    implicit none
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    character(len=*),parameter  :: subname=trim(modName)//':(ModelFinalize) '

    !----------------------------------------------------------------------------
    ! Finalize routine 
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)
    
    call datm_comp_final()

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine ModelFinalize

  !===============================================================================

  subroutine atm_init_esmf(comp, import_state, export_state, EClock, rc)
    !----------------------------------------------------------

    implicit none

    !----- arguments -----
    type(ESMF_GridComp)          :: comp
    type(ESMF_State)             :: import_state
    type(ESMF_State)             :: export_state
    type(ESMF_Clock)             :: EClock
    integer, intent(out)         :: rc

    !----- local -----
    integer(IN)      :: MYID
    character(CL)    :: NLFilename
    integer(IN)      :: phase
    character(ESMF_MAXSTR) :: convCIM, purpComp

    character(*),parameter :: subName = trim(modName)//":(atm_init_esmf) "
    !----------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    NLFilename = 'unused'

!    call ESMF_AttributeGet(export_state, name="ID", value=MYID, rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    MYID = n_ATMID(1)

!    call esmfshr_infodata_state2infodata(export_state,n_infodata, rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!    call seq_infodata_GetData(n_infodata,atm_phase=phase)
    phase = 1

    if (phase == 1) then
       call seq_cdata_init(cdata,MYID,ggrid,gsmap,n_infodata,'datm')
       call seq_cdata_setptrs(cdata,mpicom=mpicom)
       call MPI_COMM_RANK(mpicom, iam, rc)
    else
!       call shr_esmfmct_state2avect(import_state, x2d, rc=rc)
!       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    endif

    call datm_comp_init(EClock, cdata, x2d, d2x, NLFilename)

!    call esmfshr_infodata_infodata2state(n_infodata,export_state,rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!    call shr_esmfmct_avect2state(d2x,export_state,rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

#ifdef USE_ESMF_METADATA
    convCIM  = "CIM"
    purpComp = "Model Component Simulation Description"

    call ESMF_AttributeAdd(comp,  &
         convention=convCIM, purpose=purpComp, rc=rc)

    call ESMF_AttributeSet(comp, "ShortName", "DATM", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "LongName", &
         "Climatological Atmosphere Data Model", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "Description", &
         "The CESM data models perform the basic function of " // &
         "reading external data, modifying that data, and then " // &
         "sending it to the driver via standard CESM coupling " // &
         "interfaces. The driver and other models have no " // &
         "fundamental knowledge of whether another component " // &
         "is fully active or just a data model.  In some cases, " // &
         "data models are prognostic and also receive and use " // &
         "some data sent by the driver to the data model.  But " // &
         "in most cases, the data models are not running " // &
         "prognostically and have no need to receive any data " // &
         "from the driver.", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ReleaseDate", "2010", &
         convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, "ModelType", "Atmosphere", &
         convention=convCIM, purpose=purpComp, rc=rc)

    !   call ESMF_AttributeSet(comp, "Name", "Cecile Hannay", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
    !   call ESMF_AttributeSet(comp, "EmailAddress", &
    !                          "hannay@ucar.edu", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
    !   call ESMF_AttributeSet(comp, "ResponsiblePartyRole", "contact", &
    !                          convention=convCIM, purpose=purpComp, rc=rc)
#endif

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine atm_init_esmf

  !===============================================================================

  subroutine atm_run_esmf(comp, import_state, export_state, EClock, rc)

    implicit none

    !----- arguments -----
    type(ESMF_GridComp)          :: comp
    type(ESMF_State)             :: import_state
    type(ESMF_State)             :: export_state
    type(ESMF_Clock)             :: EClock
    integer, intent(out)         :: rc

    !----- local -----
    integer(IN)      :: MYID

    character(*),parameter :: subName = trim(modName)//":(atm_run_esmf) "
    !----------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    ! Unpack import state

!    call esmfshr_infodata_state2infodata(export_state,n_infodata, rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!    call shr_esmfmct_state2avect(import_state, x2d, rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    ! Run model

    call datm_comp_run(EClock, cdata, x2d, d2x)

    ! Pack export state

!    call esmfshr_infodata_infodata2state(n_infodata,export_state,rc=rc)
!    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call avect2state(d2x,export_state,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call mct_aVect_info(2, d2x, istr=subname//':AV')

    call med_method_State_diagnose(export_state,subname//':ES',rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine atm_run_esmf

  !===============================================================================

  subroutine avect2state(avect, state, rc)

    ! copy avect data to state fields

    implicit none

    !----- arguments -----
    type(mct_aVect)              :: avect
    type(ESMF_State)             :: state
    integer, intent(out)         :: rc

    !----- local -----
    integer(IN) :: nflds, lsize, n, nf, DE
    character(len=CXX) :: rList
    character(len=CS)  :: fldname
    type(ESMF_Field)   :: lfield
    real(ESMF_KIND_R8), pointer :: farray2(:,:)
    real(ESMF_KIND_R8), pointer :: favect(:)

    character(*),parameter :: subName = trim(modName)//":(avect2state) "
    !----------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO, rc=dbrc)

    nflds = mct_avect_nRattr(avect)
    lsize = mct_avect_lsize(avect)
    rList = " "
    if (nflds > 0) rList = mct_aVect_ExportRList2c(avect)
    call med_method_State_reset(state,value = -9999._R8, rc=rc)
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
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          farray2(1,1) = favect(n)
        enddo
      else
        call ESMF_LogWrite(trim(subname)//": fldname = "//trim(fldname)//" copy skipped due to grid_option", ESMF_LOGMSG_INFO, rc=dbrc)
!        rc=ESMF_Failure
!        call ESMF_LogWrite(subname//" ERROR for grid_option = "//trim(grid_option), ESMF_LOGMSG_INFO, rc=dbrc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif

    enddo

    deallocate(favect)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine avect2state

  !===============================================================================

#endif

end module atm_comp_nuopc
