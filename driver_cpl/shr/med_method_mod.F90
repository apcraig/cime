module med_method_mod

  !-----------------------------------------------------------------------------
  ! Generic operation methods used by the Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use shr_nuopc_fldList_mod, only : shr_nuopc_fldList_Type
  use seq_flds_mod, only : seq_flds_scalar_name, seq_flds_scalar_num
  use mpi
 
  implicit none
  
  private

  interface med_method_FB_accum ; module procedure &
    med_method_FB_accumFB2FB, &
    med_method_FB_accumFB2ST, &
    med_method_FB_accumST2FB
  end interface

  interface med_method_FB_copy ; module procedure &
    med_method_FB_copyFB2FB, &
    med_method_FB_copyFB2ST, &
    med_method_FB_copyST2FB
  end interface

  interface med_method_FieldPtr_compare ; module procedure &
    med_method_FieldPtr_compare1, &
    med_method_FieldPtr_compare2
  end interface

  interface med_method_UpdateTimestamp; module procedure &
    med_method_State_UpdateTimestamp, &
    med_method_Field_UpdateTimestamp
  end interface

  ! used/reused in module

  integer            :: dbug_flag = 20
  integer            :: dbrc
  logical            :: isPresent
  logical            :: rhprint_flag = .false.
  integer            :: srcTermProcessing_Value = 0
  character(len=1024):: msgString
  type(ESMF_GeomType_Flag) :: geomtype
  type(ESMF_FieldStatus_Flag) :: status
  real(ESMF_KIND_R8), parameter :: spval_init = 0.0_ESMF_KIND_R8  ! spval for initialization
  real(ESMF_KIND_R8), parameter :: spval = 0.0_ESMF_KIND_R8  ! spval
  real(ESMF_KIND_R8), parameter :: czero = 0.0_ESMF_KIND_R8  ! spval
  integer           , parameter :: ispval_mask = -987987     ! spval for RH mask values
  type(ESMF_PoleMethod_Flag), parameter :: polemethod=ESMF_POLEMETHOD_ALLAVG

  public med_method_FB_copy
  public med_method_FB_accum
  public med_method_FB_average
  public med_method_FB_init
  public med_method_FB_reset
  public med_method_FB_diagnose
  public med_method_FB_Regrid
  public med_method_FB_FldChk
  public med_method_FB_FieldRegrid
  public med_method_FB_FieldMerge
  public med_method_FB_GetFldPtr
  public med_method_State_reset
  public med_method_State_diagnose
  public med_method_State_GeomPrint
  public med_method_State_GeomWrite
  public med_method_State_GetFldPtr
  public med_method_State_SetScalar
  public med_method_State_GetScalar
  public med_method_Grid_Write
  public med_method_Field_GeomPrint
  public med_method_Clock_TimePrint
  public med_method_Grid_CopyCoord
  public med_method_Grid_CopyItem
  public med_method_CopyStateToScalar
  public med_method_CopyScalarToState
  public med_method_RH_init
  public med_method_UpdateTimestamp
  public med_method_ChkErr

  private med_method_Grid_Print
  private med_method_Mesh_Print
  private med_method_Mesh_Write
  private med_method_Field_GetFldPtr
  private med_method_Field_GeomWrite
  private med_method_FB_GeomPrint
  private med_method_FB_GeomWrite
  private med_method_FB_RWFields
  private med_method_FB_getName
  private med_method_FB_getFieldN
  private med_method_FB_getFieldName
  private med_method_FB_clean
  private med_method_FieldPtr_compare1
  private med_method_FieldPtr_compare2
  private med_method_FB_FieldCopy
  private med_method_FB_SetFldPtr
  private med_method_FB_copyFB2FB
  private med_method_FB_copyFB2ST
  private med_method_FB_copyST2FB
  private med_method_FB_accumFB2FB
  private med_method_FB_accumST2FB
  private med_method_FB_accumFB2ST
  private med_method_State_UpdateTimestamp
  private med_method_State_getName
  private med_method_State_getFieldN
  private med_method_State_getFieldName
  private med_method_Field_UpdateTimestamp
  private med_method_Distgrid_Match
  private med_method_Grid_Createcoords
  private med_method_Array_diagnose
  private med_method_State_SetFldPtr

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine med_method_FB_RWFields(mode,fname,FB,flag,rc)
    character(len=*) :: mode
    character(len=*) :: fname
    type(ESMF_FieldBundle) :: FB
    logical,optional :: flag
    integer,optional :: rc

    ! local variables
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR) :: name
    integer :: fieldcount, n
    logical :: fexists
    character(len=*),parameter :: subname='(med_method_FB_RWFields)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(fname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (mode == 'write') then
      call ESMF_LogWrite(trim(subname)//": write "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
      call ESMF_FieldBundleWrite(FB, fname, &
        singleFile=.true., status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_FB_diagnose(FB, 'write '//trim(fname), rc)
    elseif (mode == 'read') then
      inquire(file=fname,exist=fexists)
      if (fexists) then
        call ESMF_LogWrite(trim(subname)//": read "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
!-----------------------------------------------------------------------------------------------------
! tcraig, ESMF_FieldBundleRead fails if a field is not on the field bundle, but we really want to just 
! ignore that field and read the rest, so instead read each field one at a time through ESMF_FieldRead
!        call ESMF_FieldBundleRead (FB, fname, &
!          singleFile=.true., iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
!        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
!-----------------------------------------------------------------------------------------------------
        call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        do n = 1,fieldCount
          call med_method_FB_getFieldName(FB, name, field, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_FieldRead (field, fname, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_LogWrite(trim(subname)//' WARNING missing field '//trim(name),rc=dbrc)
        enddo

        call med_method_FB_diagnose(FB, 'read '//trim(fname), rc)
	if (present(flag)) flag = .true.
      endif
    else
      call ESMF_LogWrite(trim(subname)//": mode WARNING "//trim(fname)//" mode="//trim(mode), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(fname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_RWFields

  !-----------------------------------------------------------------------------

  subroutine med_method_RH_init(FBsrc, FBdst, bilnrmap, consfmap, consdmap, patchmap, fcopymap, &
                         srcMaskValue, dstMaskValue, &
                         fldlist1, fldlist2, fldlist3, fldlist4, string, &
                         bilnrfn, consffn, consdfn, patchfn, fcopyfn, rc)
    type(ESMF_FieldBundle) :: FBsrc
    type(ESMF_FieldBundle) :: FBdst
    type(ESMF_Routehandle),optional :: bilnrmap
    type(ESMF_Routehandle),optional :: consfmap
    type(ESMF_Routehandle),optional :: consdmap
    type(ESMF_Routehandle),optional :: patchmap
    type(ESMF_Routehandle),optional :: fcopymap
    integer               ,optional :: srcMaskValue
    integer               ,optional :: dstMaskValue
    type(shr_nuopc_fldList_Type),optional :: fldlist1
    type(shr_nuopc_fldList_Type),optional :: fldlist2
    type(shr_nuopc_fldList_Type),optional :: fldlist3
    type(shr_nuopc_fldList_Type),optional :: fldlist4
    character(len=*)      ,optional :: string
    character(len=*)      ,optional :: bilnrfn
    character(len=*)      ,optional :: consffn
    character(len=*)      ,optional :: consdfn
    character(len=*)      ,optional :: patchfn
    character(len=*)      ,optional :: fcopyfn
    integer               ,optional :: rc


    ! local variables
    integer :: n
    character(len=128) :: lstring
    logical :: do_consf, do_consd, do_bilnr, do_patch, do_fcopy
    integer :: lsrcMaskValue, ldstMaskValue
    type(ESMF_Field)            :: fldsrc, flddst
    real(ESMF_KIND_R8), pointer :: factorList(:)
    character(len=*),parameter :: subname='(med_method_RH_init)'

    rc = ESMF_SUCCESS

    if (present(string)) then
      lstring = trim(string)
    else
      lstring = " "
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (present(srcMaskValue)) then
      lsrcMaskValue = srcMaskValue
    else
      lsrcMaskValue = ispval_mask  ! chosen to be ignored
    endif

    if (present(dstMaskValue)) then
      ldstMaskValue = dstMaskValue
    else
      ldstMaskValue = ispval_mask  ! chosen to be ignored
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR rc expected", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    !---------------------------------------------------
    !--- decide which map files to generate.
    !--- check fldlist mapping types.
    !--- if there are no fldlists, then generate them all.
    !--- but only for mapfiles that are passed into the subroutine.
    !---------------------------------------------------

    if (.not.present(fldlist1) .and. .not.present(fldlist2) .and. &
        .not.present(fldlist3) .and. .not.present(fldlist4)) then
      do_bilnr = .true.
      do_consf = .true.
      do_consd = .true.
      do_patch = .true.
      do_fcopy = .true.
    else
      do_bilnr = .false.
      do_consf = .false.
      do_consd = .false.
      do_patch = .false.
      do_fcopy = .false.
    endif

    if (present(fldlist1)) then
      do n = 1,fldlist1%num
        if (fldlist1%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist1%mapping(n) == "conservefrac") do_consf = .true.
	if (fldlist1%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist1%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist1%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (present(fldlist2)) then
      do n = 1,fldlist2%num
        if (fldlist2%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist2%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist2%mapping(n) == "conservedst" ) do_consd = .true.
	if (fldlist2%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist2%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (present(fldlist3)) then
      do n = 1,fldlist3%num
        if (fldlist3%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist3%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist3%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist3%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist3%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (present(fldlist4)) then
      do n = 1,fldlist4%num
        if (fldlist4%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist4%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist4%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist4%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist4%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (.not.present(bilnrmap)) do_bilnr = .false.
    if (.not.present(consfmap)) do_consf = .false.
    if (.not.present(consdmap)) do_consd = .false.
    if (.not.present(patchmap)) do_patch = .false.
    if (.not.present(fcopymap)) do_fcopy = .false.

    !---------------------------------------------------
    !--- get single fields from bundles
    !--- assumes all fields in the bundle are on identical grids
    !---------------------------------------------------

    call med_method_FB_getFieldN(FBsrc, 1, fldsrc, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_FB_getFieldN(FBdst, 1, flddst, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    !---------------------------------------------------
    !--- bilinear
    !---------------------------------------------------

    if (do_bilnr) then
      if (present(bilnrfn)) then
        call ESMF_FieldSMMStore(fldsrc, flddst, bilnrfn, routehandle=bilnrmap, &
          ignoreUnmatchedIndices=.true., &
          srcTermProcessing=srcTermProcessing_Value, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      else
        call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=bilnrmap, &
          srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=polemethod, &
          srcTermProcessing=srcTermProcessing_Value, &
          factorList=factorList, ignoreDegenerate=.true., &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (rhprint_flag) then
          call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_bilnr.nc", rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_RouteHandlePrint(bilnrmap, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        endif
      endif
      if (ESMF_RouteHandleIsCreated(bilnrmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH bilnr", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH bilnr", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif
      
    !---------------------------------------------------
    !--- conservative frac
    !---------------------------------------------------

    if (do_consf) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=consfmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
        normType=ESMF_NORMTYPE_FRACAREA, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_consf.nc", rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_RouteHandlePrint(consfmap, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      endif
      if (ESMF_RouteHandleIsCreated(consfmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH consf", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH consf", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
     endif
      
    !---------------------------------------------------
    !--- conservative dst
    !---------------------------------------------------

    if (do_consd) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=consdmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
        normType=ESMF_NORMTYPE_DSTAREA, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_consd.nc", rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_RouteHandlePrint(consdmap, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      endif
      if (ESMF_RouteHandleIsCreated(consdmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH consd", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH consd", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
     endif
      
    !---------------------------------------------------
    !--- patch
    !---------------------------------------------------

    if (do_patch) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=patchmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_PATCH, &
        polemethod=polemethod, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_patch.nc", rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_RouteHandlePrint(patchmap, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      endif
      if (ESMF_RouteHandleIsCreated(patchmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH patch", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH patch", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    !---------------------------------------------------
    !--- copy
    !---------------------------------------------------

    if (do_fcopy) then
      call ESMF_FieldRedistStore(fldsrc, flddst, &
        routehandle=fcopymap, &
        ignoreUnmatchedIndices=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (rhprint_flag) then
        call ESMF_RouteHandlePrint(fcopymap, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      endif
      if (ESMF_RouteHandleIsCreated(fcopymap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH fcopy", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH fcopy", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_RH_init

  !-----------------------------------------------------------------------------
  subroutine med_method_Grid_Createcoords(gridNew,gridOld,rc)

    ! ----------------------------------------------
    ! Create FB from another FB.
    ! Zero out new FB
    ! If grid is not passed, use grid from FBin
    ! ----------------------------------------------
    type(ESMF_Grid), intent(inout) :: gridNew
    type(ESMF_Grid), intent(inout) :: gridOld
    integer        , intent(out)   :: rc

    ! local variables
    integer :: localDE, localDECount
    type(ESMF_DistGrid)        :: distgrid
    type(ESMF_CoordSys_Flag)   :: coordSys
    type(ESMF_Index_Flag)      :: indexflag
    real(ESMF_KIND_R8),pointer :: dataPtr1(:,:), dataPtr2(:,:)
    integer                    :: dimCount
    integer, pointer           :: gridEdgeLWidth(:), gridEdgeUWidth(:)
    character(len=*),parameter :: subname='(med_method_grid_createcoords)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_LogWrite(trim(subname)//": tcxA", ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_GridGet(gridold, dimCount=dimCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(gridEdgeLWidth(dimCount),gridEdgeUWidth(dimCount))
    call ESMF_GridGet(gridold,distgrid=distgrid, coordSys=coordSys, indexflag=indexflag, dimCount=dimCount, &
       gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, localDECount=localDECount, rc=rc)
!       localDECount=localDECount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_LogWrite(trim(subname)//": tcxB", ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) trim(subname)//' localDECount = ',localDECount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' dimCount = ',dimCount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' size(gELW) = ',size(gridEdgeLWidth)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' gridEdgeLWidth = ',gridEdgeLWidth
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' gridEdgeUWidth = ',gridEdgeUWidth
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_LogWrite(trim(subname)//": tcxC", ESMF_LOGMSG_INFO, rc=dbrc)

    gridnew = ESMF_GridCreate(distgrid=distgrid, coordSys=coordSys, indexflag=indexflag, &
       gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, rc=rc)
!       rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    deallocate(gridEdgeLWidth, gridEdgeUWidth)

    call ESMF_GridAddCoord(gridnew, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc) 
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_GridAddCoord(gridnew, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc) 
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    do localDE = 0,localDeCount-1

      call ESMF_GridGetCoord(gridold, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr1, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_GridGetCoord(gridnew, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr2, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      dataPtr2 = dataPtr1

      call ESMF_GridGetCoord(gridold, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr1, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_GridGetCoord(gridnew, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr2, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      dataPtr2 = dataPtr1

      call ESMF_GridGetCoord(gridold, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr1, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_GridGetCoord(gridnew, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr2, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      dataPtr2 = dataPtr1

      call ESMF_GridGetCoord(gridold, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr1, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_GridGetCoord(gridnew, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr2, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      dataPtr2 = dataPtr1

    enddo

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Grid_Createcoords

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_init(FBout, fieldNameList, FBgeom, STgeom, FBflds, STflds, name, rc)
    ! ----------------------------------------------
    ! Create FBout from fieldNameList, FBflds, STflds, FBgeom or STgeom in that order or priority
    ! Pass in FBgeom OR STgeom, get grid/mesh from that object
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in), optional :: fieldNameList(:)
    type(ESMF_FieldBundle), intent(in), optional :: FBgeom
    type(ESMF_State)      , intent(in), optional :: STgeom
    type(ESMF_FieldBundle), intent(in), optional :: FBflds
    type(ESMF_State)      , intent(in), optional :: STflds
    character(len=*)      , intent(in), optional :: name
    integer               , intent(out) :: rc

    ! local variables
    integer                    :: i,j,n,n1
    integer                    :: fieldCount,fieldCountgeom
    logical                    :: found
    character(ESMF_MAXSTR)     :: lname
    character(ESMF_MAXSTR),allocatable :: lfieldNameList(:)
    type(ESMF_Field)           :: field,lfield
    type(ESMF_Grid)            :: lgrid
    type(ESMF_Mesh)            :: lmesh
    type(ESMF_StaggerLoc)      :: staggerloc
    type(ESMF_MeshLoc)         :: meshloc
    character(len=*),parameter :: subname='(med_method_FB_init)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lname = 'undefined'
    if (present(name)) then
      lname = trim(name)
    endif

    !--- check argument consistency and verify that geom argument has a field

    if (present(fieldNameList) .and. present(FBflds) .and. present(STflds)) then
      call ESMF_LogWrite(trim(subname)//": ERROR only fieldNameList, FBflds, or STflds can be an argument", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    if (present(FBgeom) .and. present(STgeom)) then
      call ESMF_LogWrite(trim(subname)//": ERROR FBgeom and STgeom cannot both be arguments", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    if (.not.present(FBgeom) .and. .not.present(STgeom)) then
      call ESMF_LogWrite(trim(subname)//": ERROR FBgeom or STgeom must be an argument", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    if (present(FBgeom)) then
      call ESMF_FieldBundleGet(FBgeom, fieldCount=fieldCountGeom, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    elseif (present(STgeom)) then
      call ESMF_StateGet(STgeom, itemCount=fieldCountGeom, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    else
      call ESMF_LogWrite(trim(subname)//": ERROR FBgeom or STgeom must be passed", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    !--- field name list

    if (present(fieldNameList)) then
      fieldcount = size(fieldNameList)
      allocate(lfieldNameList(fieldcount))
      lfieldNameList = fieldNameList
      call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" fieldNameList from argument", ESMF_LOGMSG_INFO, rc=rc)
    elseif (present(FBflds)) then
      call ESMF_FieldBundleGet(FBflds, fieldCount=fieldCount, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(lfieldNameList(fieldCount))
      call ESMF_FieldBundleGet(FBflds, fieldNameList=lfieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" fieldNameList from FBflds", ESMF_LOGMSG_INFO, rc=rc)
    elseif (present(STflds)) then
      call ESMF_StateGet(STflds, itemCount=fieldCount, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(lfieldNameList(fieldCount))
      call ESMF_StateGet(STflds, itemNameList=lfieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" fieldNameList from STflds", ESMF_LOGMSG_INFO, rc=rc)
    elseif (present(FBgeom)) then
      call ESMF_FieldBundleGet(FBgeom, fieldCount=fieldCount, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(lfieldNameList(fieldCount))
      call ESMF_FieldBundleGet(FBgeom, fieldNameList=lfieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" fieldNameList from FBgeom", ESMF_LOGMSG_INFO, rc=rc)
    elseif (present(STgeom)) then
      call ESMF_StateGet(STgeom, itemCount=fieldCount, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(lfieldNameList(fieldCount))
      call ESMF_StateGet(STgeom, itemNameList=lfieldNameList, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" fieldNameList from STflds", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite(trim(subname)//": ERROR fieldNameList, FBflds, STflds, FBgeom, or STgeom must be passed", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    !--- remove scalar field from field bundle

    found = .true.
    do while (found)
      found = .false.
      do n = 1, fieldCount
        if (lfieldnamelist(n) == trim(seq_flds_scalar_name)) then
          found = .true.
          do n1 = n, fieldCount-1
            lfieldnamelist(n1) = lfieldnamelist(n1+1)
          enddo
          fieldCount = fieldCount - 1
        endif
      enddo  ! n
    enddo  ! while

    !--- field grid or mesh

    if (fieldcount > 0 .and. fieldcountgeom > 0) then

      if (present(FBgeom)) then
        call med_method_FB_getFieldN(FBgeom, 1, lfield, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" grid/mesh from FBgeom", ESMF_LOGMSG_INFO, rc=rc)
      elseif (present(STgeom)) then
        call med_method_State_getFieldN(STgeom, 1, lfield, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" grid/mesh from STgeom", ESMF_LOGMSG_INFO, rc=rc)
      else
        call ESMF_LogWrite(trim(subname)//": ERROR FBgeom or STgeom must be passed", ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
        return
      endif

      call ESMF_FieldGet(lfield, status=status, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      if (status == ESMF_FIELDSTATUS_EMPTY) then
        call ESMF_LogWrite(trim(subname)//":"//trim(lname)//": ERROR field does not have a geom yet ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif

      call ESMF_FieldGet(lfield, geomtype=geomtype, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (geomtype == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(lfield, grid=lgrid, staggerloc=staggerloc, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
        call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" use grid", ESMF_LOGMSG_INFO, rc=rc)
      elseif (geomtype == ESMF_GEOMTYPE_MESH) then
        call ESMF_FieldGet(lfield, mesh=lmesh, meshloc=meshloc, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
        call ESMF_LogWrite(trim(subname)//":"//trim(lname)//" use mesh", ESMF_LOGMSG_INFO, rc=rc)
      else  ! geomtype
        call ESMF_LogWrite(trim(subname)//": ERROR geomtype not supported ", ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
        return
      endif ! geomtype

    endif  ! fieldcount > 0

    !--- create FBout

    FBout = ESMF_FieldBundleCreate(name=trim(lname), rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (fieldcountgeom > 0) then

      do n = 1, fieldCount
        if (geomtype == ESMF_GEOMTYPE_GRID) then
          field = ESMF_FieldCreate(lgrid, ESMF_TYPEKIND_R8, staggerloc=staggerloc, name=lfieldNameList(n), rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        elseif (geomtype == ESMF_GEOMTYPE_MESH) then
          field = ESMF_FieldCreate(lmesh, ESMF_TYPEKIND_R8, meshloc=meshloc, name=lfieldNameList(n), rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        else  ! geomtype
          call ESMF_LogWrite(trim(subname)//": ERROR no grid/mesh for field ", ESMF_LOGMSG_INFO, rc=rc)
          rc = ESMF_FAILURE
          return
        endif
        call ESMF_FieldBundleAdd(FBout, (/field/), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(lfieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      enddo  ! fieldCount

    endif  ! fieldcountgeom

    deallocate(lfieldNameList)

    call med_method_FB_reset(FBout, value=spval_init, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
  end subroutine med_method_FB_init

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_getName(FB, fieldnum, fieldname, rc)
    ! ----------------------------------------------
    ! Get name of field number fieldnum in FB
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(in)    :: FB
    integer               , intent(in)    :: fieldnum
    character(len=*)      , intent(out)   :: fieldname
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    character(len=*),parameter :: subname='(med_method_FB_getName)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    fieldname = ' '

    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (fieldnum > fieldCount) then
      call ESMF_LogWrite(trim(subname)//": ERROR fieldnum > fieldCount ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fieldname = lfieldnamelist(fieldnum)

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_getName

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_getFieldN(FB, fieldnum, field, rc)
    ! ----------------------------------------------
    ! Get field number fieldnum out of FB
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(in)    :: FB
    integer               , intent(in)    :: fieldnum
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=ESMF_MAXSTR) :: name
    character(len=*),parameter :: subname='(med_method_FB_getFieldN)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_FB_getName(FB, fieldnum, name, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_FieldBundleGet(FB, fieldName=name, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_getFieldN

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_getFieldName(FB, fieldname, field, rc)
    ! ----------------------------------------------
    ! Get field associated with fieldname out of FB
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(in)    :: FB
    character(len=*)      , intent(in)    :: fieldname
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=*),parameter :: subname='(med_method_FB_getFieldName)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FB, fieldName=fieldname, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_getFieldName

  !-----------------------------------------------------------------------------

  subroutine med_method_State_getName(State, fieldnum, fieldname, rc)
    ! ----------------------------------------------
    ! Get field number fieldnum name out of State
    ! ----------------------------------------------
    type(ESMF_State), intent(in)    :: State
    integer         , intent(in)    :: fieldnum
    character(len=*), intent(out)   :: fieldname
    integer         , intent(out)   :: rc

    ! local variables
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    character(len=*),parameter :: subname='(med_method_State_getName)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    fieldname = ' '

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (fieldnum > fieldCount) then
      call ESMF_LogWrite(trim(subname)//": ERROR fieldnum > fieldCount ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(State, itemNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    fieldname = lfieldnamelist(fieldnum)

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_getName

  !-----------------------------------------------------------------------------

  subroutine med_method_State_getFieldN(State, fieldnum, field, rc)
    ! ----------------------------------------------
    ! Get field number fieldnum in State
    ! ----------------------------------------------
    type(ESMF_State), intent(in)    :: State
    integer         , intent(in)    :: fieldnum
    type(ESMF_Field), intent(inout) :: field
    integer         , intent(out)   :: rc

    ! local variables
    character(len=ESMF_MAXSTR) :: name
    character(len=*),parameter :: subname='(med_method_State_getFieldN)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_State_getName(State, fieldnum, name, rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_StateGet(State, itemName=name, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_getFieldN

  !-----------------------------------------------------------------------------

  subroutine med_method_State_getFieldName(State, fieldname, field, rc)
    ! ----------------------------------------------
    ! Get field associated with fieldname from State
    ! ----------------------------------------------
    type(ESMF_State), intent(in)    :: State
    character(len=*), intent(in)    :: fieldname
    type(ESMF_Field), intent(inout) :: field
    integer         , intent(out)   :: rc

    ! local variables
    character(len=*),parameter :: subname='(med_method_State_getFieldName)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(State, itemName=fieldname, field=field, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_getFieldName

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_clean(FB, rc)
    ! ----------------------------------------------
    ! Destroy fields in FB and FB
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FB
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    type(ESMF_Field)            :: field
    character(len=*),parameter :: subname='(med_method_FB_clean)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    do n = 1, fieldCount
      call ESMF_FieldBundleGet(FB, fieldName=lfieldnamelist(n), field=field, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_FieldDestroy(field, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo
    call ESMF_FieldBundleDestroy(FB, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_clean

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_reset(FB, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in FB
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FB
    real(ESMF_KIND_R8)    , intent(in), optional :: value
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    real(ESMF_KIND_R8)          :: lvalue
    character(len=*),parameter :: subname='(med_method_FB_reset)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lvalue = czero
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    do n = 1, fieldCount
      call med_method_FB_SetFldPtr(FB, lfieldnamelist(n), lvalue, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_reset

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_FieldCopy(FBin,fldin,FBout,fldout,rc)
    ! ----------------------------------------------
    ! Copy a field in a field bundle to another field in a field bundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBin
    character(len=*)      , intent(in)    :: fldin
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fldout
    integer               , intent(out)   :: rc

    ! local
    real(ESMF_KIND_R8), pointer :: dataPtrIn1(:)
    real(ESMF_KIND_R8), pointer :: dataPtrOut1(:)
    real(ESMF_KIND_R8), pointer :: dataPtrIn2(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtrOut2(:,:)
    integer :: lrankIn, lrankOut
    character(len=*),parameter :: subname='(med_method_FB_FieldCopy)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (med_method_FB_FldChk(FBin , trim(fldin) , rc=rc) .and. &
        med_method_FB_FldChk(FBout, trim(fldout), rc=rc)) then

      call med_method_FB_GetFldPtr(FBin, trim(fldin), dataPtrIn1, dataPtrIn2, lrankIn, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_FB_GetFldPtr(FBout, trim(fldout), dataPtrOut1, dataPtrOut2, lrankOut, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      if (lrankIn /= lrankOut) then
        call ESMF_LogWrite(trim(subname)//": ERROR FBin and FBout different rank", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif

      if (lrankIn == 1 .and. lrankOut == 1) then
        if (.not.med_method_FieldPtr_Compare(dataPtrIn1, dataPtrOut1, subname, rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR data1d different sizes", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

        dataPtrOut1 = dataPtrIn1

      elseif (lrankIn == 2 .and. lrankOut == 2) then
        if (.not.med_method_FieldPtr_Compare(dataPtrIn2, dataPtrOut2, subname, rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR data2d different sizes", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

        dataPtrOut2 = dataPtrIn2

      elseif (lrankIn == 2 .and. lrankOut == 2) then
        call ESMF_LogWrite(trim(subname)//": ERROR raskIn and rankOut invalid", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return

      endif

    else

       if (dbug_flag > 1) then
         call ESMF_LogWrite(trim(subname)//" field not found: "//trim(fldin)//","//trim(fldout), ESMF_LOGMSG_INFO, rc=dbrc)
       endif

    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_FieldCopy

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_Regrid(fldlist, FBin, FBout, consfmap, consdmap, bilnrmap, patchmap, &
                                fcopymap, string, rc)
    type(shr_nuopc_fldList_Type) :: fldlist
    type(ESMF_FieldBundle) :: FBin
    type(ESMF_FieldBundle) :: FBout
    type(ESMF_Routehandle),optional :: consfmap
    type(ESMF_Routehandle),optional :: consdmap
    type(ESMF_Routehandle),optional :: bilnrmap
    type(ESMF_Routehandle),optional :: patchmap
    type(ESMF_Routehandle),optional :: fcopymap
    character(len=*)      ,optional :: string
    integer               ,optional :: rc

    ! local variables
    integer :: n
    character(len=64) :: lstring
    logical :: okconsf, okconsd, okbilnr, okpatch, okfcopy
    character(len=*),parameter :: subname='(med_method_FB_Regrid)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (present(string)) then
      lstring = trim(string)
    else
      lstring = " "
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR rc expected", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    endif

    okconsf = .false.
    if (present(consfmap)) then
      if (ESMF_RouteHandleIsCreated(consfmap, rc=rc)) okconsf = .true.
    endif

    okconsd = .false.
    if (present(consdmap)) then
      if (ESMF_RouteHandleIsCreated(consdmap, rc=rc)) okconsd = .true.
    endif

    okbilnr = .false.
    if (present(bilnrmap)) then
      if (ESMF_RouteHandleIsCreated(bilnrmap, rc=rc)) okbilnr = .true.
    endif

    okpatch = .false.
    if (present(patchmap)) then
      if (ESMF_RouteHandleIsCreated(patchmap, rc=rc)) okpatch = .true.
    endif

    okfcopy = .false.
    if (present(fcopymap)) then
      if (ESMF_RouteHandleIsCreated(fcopymap, rc=rc)) okfcopy = .true.
    endif

    do n = 1,fldlist%num
      if (fldlist%shortname(n) == trim(seq_flds_scalar_name)) then
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//trim(lstring)//": skip : fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
   
      elseif (med_method_FB_FldChk(FBin , fldlist%shortname(n), rc=rc) .and. &
              med_method_FB_FldChk(FBout, fldlist%shortname(n), rc=rc)) then

        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//trim(lstring)//": map="//trim(fldlist%mapping(n))// &
            ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif

        if (fldlist%mapping(n) == 'bilinear') then
          if (.not. okbilnr) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call med_method_FB_FieldRegrid(FBin,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       bilnrmap,rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        elseif (fldlist%mapping(n) == "conservefrac") then
          if (.not. okconsf) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call med_method_FB_FieldRegrid(FBin ,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       consfmap, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        elseif (fldlist%mapping(n) == "conservedst") then
          if (.not. okconsd) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call med_method_FB_FieldRegrid(FBin ,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       consdmap, rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        elseif (fldlist%mapping(n) == 'patch') then
          if (.not. okpatch) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call med_method_FB_FieldRegrid(FBin ,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       patchmap,rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        elseif (fldlist%mapping(n) == 'copy') then
          !-------------------------------------------
          ! copy will not exist for some grid combinations
          ! so fall back to conservative frac as a secondary option
          !-------------------------------------------
          if (.not. okfcopy) then
            if (.not. okconsf) then
              call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
                ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
              rc = ESMF_FAILURE
              return
            else
              call ESMF_LogWrite(trim(subname)//trim(lstring)//": NOTE using conservative instead of copy for"// &
                " fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
              call med_method_FB_FieldRegrid(FBin ,fldlist%shortname(n), &
                                           FBout,fldlist%shortname(n), &
                                           consfmap,rc)
              if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
            endif
          else
            call med_method_FB_FieldRegrid(FBin ,fldlist%shortname(n), &
                                         FBout,fldlist%shortname(n), &
                                         fcopymap,rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          endif

        else
          call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR unrecognized mapping "//trim(fldlist%mapping(n))// &
            ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

      else
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//" field not found in FB: "//trim(fldlist%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      endif
    enddo

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_Regrid

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_FieldRegrid(FBin,fldin,FBout,fldout,RH,rc)
    ! ----------------------------------------------
    ! Regrid a field in a field bundle to another field in a field bundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBin
    character(len=*)      , intent(in)    :: fldin
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fldout
    type(ESMF_RouteHandle), intent(inout) :: RH
    integer               , intent(out)   :: rc

    ! local
    type(ESMF_Field) :: field1, field2
    character(len=*),parameter :: subname='(med_method_FB_FieldRegrid)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (med_method_FB_FldChk(FBin , trim(fldin) , rc=rc) .and. &
        med_method_FB_FldChk(FBout, trim(fldout), rc=rc)) then

      call med_method_FB_GetFieldName(FBin, trim(fldin), field1, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_FB_GetFieldName(FBout, trim(fldout), field2, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      call ESMF_FieldRegrid(field1, field2, routehandle=RH, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    else

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//" field not found: "//trim(fldin)//","//trim(fldout), ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_FieldRegrid

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_FieldMerge(FBout, fnameout, &
                                    FBinA, fnameA, wgtA, &
                                    FBinB, fnameB, wgtB, &
                                    FBinC, fnameC, wgtC, &
                                    FBinD, fnameD, wgtD, &
                                    FBinE, fnameE, wgtE, rc)
    ! ----------------------------------------------
    ! Supports up to a five way merge
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fnameout
    type(ESMF_FieldBundle), intent(in), optional :: FBinA
    character(len=*)      , intent(in), optional :: fnameA
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtA(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinB
    character(len=*)      , intent(in), optional :: fnameB
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtB(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinC
    character(len=*)      , intent(in), optional :: fnameC
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtC(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinD
    character(len=*)      , intent(in), optional :: fnameD
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtD(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinE
    character(len=*)      , intent(in), optional :: fnameE
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtE(:,:)
    integer               , intent(out)   :: rc

    ! local variables
    real(ESMF_KIND_R8), pointer :: dataOut(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    real(ESMF_KIND_R8), pointer :: wgt(:,:)
    integer :: lb1,ub1,lb2,ub2,i,j,n
    logical :: wgtfound, FBinfound
    character(len=*),parameter :: subname='(med_method_FB_FieldMerge)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc=ESMF_SUCCESS

    if (.not. med_method_FB_FldChk(FBout, trim(fnameout), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": WARNING field not in FBout, skipping merge "//trim(fnameout), ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif
!tcraig, needs to be extended to 1d/2d
    call med_method_FB_GetFldPtr(FBout, trim(fnameout), fldptr2=dataOut, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    lb1 = lbound(dataOut,1)
    ub1 = ubound(dataOut,1)
    lb2 = lbound(dataOut,2)
    ub2 = ubound(dataOut,2)
    allocate(wgt(lb1:ub1,lb2:ub2))

    dataOut = czero

    ! check each field has a fieldname passed in
    if ((present(FBinA) .and. .not.present(fnameA)) .or. &
        (present(FBinB) .and. .not.present(fnameB)) .or. &
        (present(FBinC) .and. .not.present(fnameC)) .or. &
        (present(FBinD) .and. .not.present(fnameD)) .or. &
        (present(FBinE) .and. .not.present(fnameE))) then
      call ESMF_LogWrite(trim(subname)//": ERROR fname not present with FBin", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! check that each field passed in actually exists, if not DO NOT do any merge
    FBinfound = .true.
    if (present(FBinA)) then
      if (.not. med_method_FB_FldChk(FBinA, trim(fnameA), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinB)) then
      if (.not. med_method_FB_FldChk(FBinB, trim(fnameB), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinC)) then
      if (.not. med_method_FB_FldChk(FBinC, trim(fnameC), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinD)) then
      if (.not. med_method_FB_FldChk(FBinD, trim(fnameD), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinE)) then
      if (.not. med_method_FB_FldChk(FBinE, trim(fnameE), rc=rc)) FBinfound = .false.
    endif
    if (.not. FBinfound) then
      call ESMF_LogWrite(trim(subname)//": WARNING field not found in FBin, skipping merge "//trim(fnameout), ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif

    ! n=1,5 represents adding A to E inputs if they exist
    do n = 1,5
      FBinfound = .false.
      wgtfound = .false.

      if (n == 1 .and. present(FBinA)) then
        FBinfound = .true.
        call med_method_FB_GetFldPtr(FBinA, trim(fnameA), fldptr2=dataPtr, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (present(wgtA)) then
          wgtfound = .true.
          wgt => wgtA
        endif

      elseif (n == 2 .and. present(FBinB)) then
        FBinfound = .true.
        call med_method_FB_GetFldPtr(FBinB, trim(fnameB), fldptr2=dataPtr, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (present(wgtB)) then
          wgtfound = .true.
          wgt => wgtB
        endif

      elseif (n == 3 .and. present(FBinC)) then
        FBinfound = .true.
        call med_method_FB_GetFldPtr(FBinC, trim(fnameC), fldptr2=dataPtr, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (present(wgtC)) then
          wgtfound = .true.
          wgt => wgtC
        endif

      elseif (n == 4 .and. present(FBinD)) then
        FBinfound = .true.
        call med_method_FB_GetFldPtr(FBinD, trim(fnameD), fldptr2=dataPtr, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (present(wgtD)) then
          wgtfound = .true.
          wgt => wgtD
        endif

      elseif (n == 5 .and. present(FBinE)) then
        FBinfound = .true.
        call med_method_FB_GetFldPtr(FBinE, trim(fnameE), fldptr2=dataPtr, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (present(wgtE)) then
          wgtfound = .true.
          wgt => wgtE
        endif

      endif

      if (FBinfound) then
        if (.not.med_method_FieldPtr_Compare(dataPtr, dataOut, subname, rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR FBin wrong size", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

        if (wgtfound) then
          if (.not.med_method_FieldPtr_Compare(dataPtr, wgt, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR wgt wrong size", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          do j = lb2,ub2
          do i = lb1,ub1
            dataOut(i,j) = dataOut(i,j) + dataPtr(i,j) * wgt(i,j)
          enddo
          enddo
        else
          do j = lb2,ub2
          do i = lb1,ub1
            dataOut(i,j) = dataOut(i,j) + dataPtr(i,j)
          enddo
          enddo
        endif  ! wgtfound

      endif  ! FBin found
    enddo  ! n

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_FieldMerge

  !-----------------------------------------------------------------------------

  subroutine med_method_State_reset(State, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_State)  , intent(inout) :: State
    real(ESMF_KIND_R8), intent(in), optional :: value
    integer           , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    real(ESMF_KIND_R8)          :: lvalue
    character(len=*),parameter :: subname='(med_method_State_reset)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lvalue = czero
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(State, itemNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    do n = 1, fieldCount
      call med_method_State_SetFldPtr(State, lfieldnamelist(n), lvalue, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_reset

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_average(FB, count, rc)
    ! ----------------------------------------------
    ! Set all fields to zero in FB
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FB
    integer               , intent(in)    :: count
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount, lrank
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:)
    real(ESMF_KIND_R8), pointer :: dataPtr2(:,:)
    character(len=*),parameter :: subname='(med_method_FB_average)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (count == 0) then

      call ESMF_LogWrite(trim(subname)//": WARNING count is 0", ESMF_LOGMSG_INFO, rc=dbrc)
!      call ESMF_LogWrite(trim(subname)//": WARNING count is 0 set avg to spval", ESMF_LOGMSG_INFO, rc=dbrc)
!      call med_method_FB_reset(FB, value=spval, rc=rc)
!      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    else

      call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      allocate(lfieldnamelist(fieldCount))
      call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      do n = 1, fieldCount
        call med_method_FB_GetFldPtr(FB, lfieldnamelist(n), dataPtr1, dataPtr2, lrank, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (lrank == 1) then
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            dataptr1(i) = dataptr1(i) / real(count, ESMF_KIND_R8)
          enddo
        elseif (lrank == 2) then
          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            dataptr2(i,j) = dataptr2(i,j) / real(count, ESMF_KIND_R8)
          enddo
          enddo
        else
          call ESMF_LogWrite(trim(subname)//": ERROR rank not supported ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif
      enddo
      deallocate(lfieldnamelist)

    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_average

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_diagnose(FB, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of FB
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FB
    character(len=*)      , intent(in), optional :: string
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount, lrank
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr1(:)
    real(ESMF_KIND_R8), pointer :: dataPtr2(:,:)
    character(len=*),parameter :: subname='(med_method_FB_diagnose)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FB, fieldNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    do n = 1, fieldCount
      call med_method_FB_GetFldPtr(FB, lfieldnamelist(n), dataPtr1, dataPtr2, lrank, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (lrank == 1) then
        if (size(dataPtr1) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            minval(dataPtr1),maxval(dataPtr1),sum(dataPtr1),size(dataPtr1)
        else
          write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            " no data"
        endif
      elseif (lrank == 2) then
        if (size(dataPtr2) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            minval(dataPtr2),maxval(dataPtr2),sum(dataPtr2),size(dataPtr2)
        else
          write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            " no data"
        endif
      else
        call ESMF_LogWrite(trim(subname)//": ERROR rank not supported ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_diagnose

  !-----------------------------------------------------------------------------

  subroutine med_method_Array_diagnose(array, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of FB
    ! ----------------------------------------------
    type(ESMF_Array), intent(inout) :: array
    character(len=*), intent(in), optional :: string
    integer         , intent(out) :: rc

    ! local variables
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:,:)
    character(len=*),parameter :: subname='(med_method_Array_diagnose)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! this is not working yet, not sure about dataPtr dim/type
    return

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_ArrayGet(Array, farrayPtr=dataPtr, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Array_diagnose

  !-----------------------------------------------------------------------------

  subroutine med_method_State_diagnose(State, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of FB
    ! ----------------------------------------------
    type(ESMF_State), intent(in) :: State
    character(len=*), intent(in), optional :: string
    integer         , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount, lrank
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr1(:)
    real(ESMF_KIND_R8), pointer :: dataPtr2(:,:)
    character(len=*),parameter :: subname='(med_method_State_diagnose)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(State, itemNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    do n = 1, fieldCount
      call med_method_State_GetFldPtr(State, lfieldnamelist(n), dataPtr1, dataPtr2, lrank, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (lrank == 1) then
        if (size(dataPtr1) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            minval(dataPtr1),maxval(dataPtr1),sum(dataPtr1),size(dataPtr1)
        else
          write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            " no data"
        endif
      elseif (lrank == 2) then
        if (size(dataPtr2) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            minval(dataPtr2),maxval(dataPtr2),sum(dataPtr2),size(dataPtr2)
        else
          write(msgString,'(A,a)') trim(subname)//' '//trim(lstring)//':'//trim(lfieldnamelist(n)), &
            " no data"
        endif
      else
        call ESMF_LogWrite(trim(subname)//": ERROR rank not supported ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_diagnose

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_copyFB2FB(FBout, FBin, rc)
    ! ----------------------------------------------
    ! Copy common field names from FBin to FBout
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(med_method_FB_copyFB2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_FB_accum(FBout, FBin, copy=.true., rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_copyFB2FB

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_copyFB2ST(STout, FBin, rc)
    ! ----------------------------------------------
    ! Copy common field names from FBin to STout
    ! ----------------------------------------------
    type(ESMF_State)      , intent(inout) :: STout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(med_method_FB_copyFB2ST)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_FB_accum(STout, FBin, copy=.true., rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_copyFB2ST

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_copyST2FB(FBout, STin, rc)
    ! ----------------------------------------------
    ! Copy common field names from STin to FBout
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_State)      , intent(in)    :: STin
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(med_method_FB_copyST2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_FB_accum(FBout, STin, copy=.true., rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_copyST2FB

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_accumFB2FB(FBout, FBin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from FBin to FBout
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount, lranki, lranko
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    logical                     :: exists
    logical                     :: lcopy
    real(ESMF_KIND_R8), pointer :: dataPtri1(:)  , dataPtro1(:)
    real(ESMF_KIND_R8), pointer :: dataPtri2(:,:), dataPtro2(:,:)
    character(len=*),parameter :: subname='(med_method_FB_accumFB2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lcopy = .false.  ! accumulate by default
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBout, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FBout, fieldNameList=lfieldnamelist, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    do n = 1, fieldCount
      call ESMF_FieldBundleGet(FBin, fieldName=lfieldnamelist(n), isPresent=exists, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (exists) then
        call med_method_FB_GetFldPtr(FBin,  lfieldnamelist(n), dataPtri1, dataPtri2, lranki, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call med_method_FB_GetFldPtr(FBout, lfieldnamelist(n), dataPtro1, dataPtro2, lranko, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (lranki == 1 .and. lranko == 1) then

          if (.not.med_method_FieldPtr_Compare(dataPtro1, dataPtri1, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr1 size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            rc = ESMF_FAILURE
            return
          endif

          if (lcopy) then
            do i=lbound(dataPtri1,1),ubound(dataPtri1,1)
              dataPtro1(i) = dataPtri1(i)
            enddo
          else
            do i=lbound(dataPtri1,1),ubound(dataPtri1,1)
              dataPtro1(i) = dataPtro1(i) + dataPtri1(i)
            enddo
          endif

        elseif (lranki == 2 .and. lranko == 2) then

          if (.not.med_method_FieldPtr_Compare(dataPtro2, dataPtri2, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr2 size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            rc = ESMF_FAILURE
            return
          endif

          if (lcopy) then
            do j=lbound(dataPtri2,2),ubound(dataPtri2,2)
            do i=lbound(dataPtri2,1),ubound(dataPtri2,1)
              dataPtro2(i,j) = dataPtri2(i,j)
            enddo
            enddo
          else
            do j=lbound(dataPtri2,2),ubound(dataPtri2,2)
            do i=lbound(dataPtri2,1),ubound(dataPtri2,1)
              dataPtro2(i,j) = dataPtro2(i,j) + dataPtri2(i,j)
            enddo
            enddo
          endif

        else

          write(msgString,'(a,2i8)') trim(subname)//": ranki, ranko = ",lranki,lranko
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_LogWrite(trim(subname)//": ERROR ranki ranko not supported "//trim(lfieldnamelist(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return

        endif

      endif
    enddo

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_accumFB2FB
  !-----------------------------------------------------------------------------

  subroutine med_method_FB_accumST2FB(FBout, STin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from State to FB
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_State)      , intent(in)    :: STin
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount, lrankS, lrankB
    logical                     :: lcopy
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS1(:)  , dataPtrB1(:)
    real(ESMF_KIND_R8), pointer :: dataPtrS2(:,:), dataPtrB2(:,:)
    character(len=*),parameter :: subname='(med_method_FB_accumST2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lcopy = .false.
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBout, fieldCount=fieldCount, rc=rc)
    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FBout, fieldNameList=lfieldnamelist, rc=rc)
    do n = 1, fieldCount
      call ESMF_StateGet(STin, itemName=lfieldnamelist(n), itemType=itemType, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then

        call med_method_State_GetFldPtr(STin, lfieldnamelist(n), dataPtrS1, dataPtrS2, lrankS, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call med_method_FB_GetFldPtr(FBout, lfieldnamelist(n), dataPtrB1, dataPtrB2, lrankB, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (lrankS == 1 .and. lrankB == 1) then

          if (.not.med_method_FieldPtr_Compare(dataPtrS1, dataPtrB1, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr1 size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            rc = ESMF_FAILURE
            return
          endif

          if (lcopy) then
            do i=lbound(dataPtrB1,1),ubound(dataPtrB1,1)
              dataPtrB1(i) = dataPtrS1(i)
            enddo
          else
            do i=lbound(dataPtrB1,1),ubound(dataPtrB1,1)
              dataPtrB1(i) = dataPtrB1(i) + dataPtrS1(i)
            enddo
          endif

        elseif (lrankS == 2 .and. lrankB == 2) then

          if (.not.med_method_FieldPtr_Compare(dataPtrS2, dataPtrB2, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr2 size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            rc = ESMF_FAILURE
            return
          endif

          if (lcopy) then
            do j=lbound(dataPtrB2,2),ubound(dataPtrB2,2)
            do i=lbound(dataPtrB2,1),ubound(dataPtrB2,1)
              dataPtrB2(i,j) = dataPtrS2(i,j)
            enddo
            enddo
          else
            do j=lbound(dataPtrB2,2),ubound(dataPtrB2,2)
            do i=lbound(dataPtrB2,1),ubound(dataPtrB2,1)
              dataPtrB2(i,j) = dataPtrB2(i,j) + dataPtrS2(i,j)
            enddo
            enddo
          endif

        else

          write(msgString,'(a,2i8)') trim(subname)//": rankB, ranks = ",lrankB,lrankS
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_LogWrite(trim(subname)//": ERROR rankB rankS not supported "//trim(lfieldnamelist(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return

        endif

      endif  ! statefound
    enddo  ! fieldCount

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_accumST2FB

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_accumFB2ST(STout, FBin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from FB to State
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_State)      , intent(inout) :: STout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount, lrankS, lrankB
    logical                     :: lcopy
    character(ESMF_MAXSTR) ,pointer  :: lfieldnamelist(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS1(:), dataPtrB1(:)
    real(ESMF_KIND_R8), pointer :: dataPtrS2(:,:), dataPtrB2(:,:)
    character(len=*),parameter :: subname='(med_method_FB_accumFB2ST)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lcopy = .false.
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBin, fieldCount=fieldCount, rc=rc)
    allocate(lfieldnamelist(fieldCount))
    call ESMF_FieldBundleGet(FBin, fieldNameList=lfieldnamelist, rc=rc)
    do n = 1, fieldCount
      call ESMF_StateGet(STout, itemName=lfieldnamelist(n), itemType=itemType, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then

        call med_method_FB_GetFldPtr(FBin, lfieldnamelist(n), dataPtrB1, dataPtrB2, lrankB, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call med_method_State_GetFldPtr(STout, lfieldnamelist(n), dataPtrS1, dataPtrS2, lrankS, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        if (lrankB == 1 .and. lrankS == 1) then

          if (.not.med_method_FieldPtr_Compare(dataPtrS1, dataPtrB1, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr1 size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            rc = ESMF_FAILURE
            return
          endif

          if (lcopy) then
            do i=lbound(dataPtrB1,1),ubound(dataPtrB1,1)
              dataPtrS1(i) = dataPtrB1(i)
            enddo
          else
            do i=lbound(dataPtrB1,1),ubound(dataPtrB1,1)
              dataPtrS1(i) = dataPtrS1(i) + dataPtrB1(i)
            enddo
          endif

        elseif (lrankB == 2 .and. lrankS == 2) then

          if (.not.med_method_FieldPtr_Compare(dataPtrS2, dataPtrB2, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr2 size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            rc = ESMF_FAILURE
            return
          endif

          if (lcopy) then
            do j=lbound(dataPtrB2,2),ubound(dataPtrB2,2)
            do i=lbound(dataPtrB2,1),ubound(dataPtrB2,1)
              dataPtrS2(i,j) = dataPtrB2(i,j)
            enddo
            enddo
          else
            do j=lbound(dataPtrB2,2),ubound(dataPtrB2,2)
            do i=lbound(dataPtrB2,1),ubound(dataPtrB2,1)
              dataPtrS2(i,j) = dataPtrS2(i,j) + dataPtrB2(i,j)
            enddo
            enddo
          endif

        else

          write(msgString,'(a,2i8)') trim(subname)//": rankB, ranks = ",lrankB,lrankS
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_LogWrite(trim(subname)//": ERROR rankB rankS not supported "//trim(lfieldnamelist(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return

        endif

      endif  ! statefound
    enddo  ! fieldCount

    deallocate(lfieldnamelist)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_accumFB2ST

  !-----------------------------------------------------------------------------

  logical function med_method_FB_FldChk(FB, fldname, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    integer               , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(med_method_FB_FldChk)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    med_method_FB_FldChk = .false.

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), isPresent=isPresent, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (isPresent) then
       med_method_FB_FldChk = .true.
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function med_method_FB_FldChk

  !-----------------------------------------------------------------------------

  subroutine med_method_Field_GetFldPtr(field, fldptr1, fldptr2, rank, abort, rc)
    ! for a field, determine rank and return fldptr1 or fldptr2
    ! abort is true by default and will abort if fldptr is not yet allocated in field
    ! rank returns 0, 1, or 2.  0 means fldptr not allocated and abort=false
    type(ESMF_Field)           , intent(in)              :: field
    real(ESMF_KIND_R8), pointer, intent(inout), optional :: fldptr1(:)
    real(ESMF_KIND_R8), pointer, intent(inout), optional :: fldptr2(:,:)
    integer                    , intent(out)  , optional :: rank
    logical                    , intent(in)   , optional :: abort
    integer                    , intent(out)  , optional :: rc

    ! local variables
    integer :: lrank
    logical :: labort
    character(len=*),parameter :: subname='(med_method_Field_GetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR rc not present ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    rc = ESMF_SUCCESS

    labort = .true.
    if (present(abort)) then
      labort = abort
    endif
    lrank = -99

    call ESMF_FieldGet(field, status=status, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      lrank = 0
      call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
      if (labort) then
        rc = ESMF_FAILURE
        return
      endif
    else

      call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

      if (geomtype == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field, rank=lrank, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      elseif (geomtype == ESMF_GEOMTYPE_MESH) then
        lrank = 1
      else  ! geomtype
        call ESMF_LogWrite(trim(subname)//": ERROR geomtype not supported ", ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
        return
      endif ! geomtype

      if (lrank == 1) then
        if (.not.present(fldptr1)) then
          call ESMF_LogWrite(trim(subname)//": ERROR missing rank=1 array ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif
        call ESMF_FieldGet(field, farrayPtr=fldptr1, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      elseif (lrank == 2) then
        if (.not.present(fldptr2)) then
          call ESMF_LogWrite(trim(subname)//": ERROR missing rank=2 array ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif
        call ESMF_FieldGet(field, farrayPtr=fldptr2, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      else
        call ESMF_LogWrite(trim(subname)//": ERROR in rank ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif

    endif  ! status

    if (present(rank)) then
      rank = lrank
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Field_GetFldPtr

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_GetFldPtr(FB, fldname, fldptr1, fldptr2, rank, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    real(ESMF_KIND_R8), pointer, intent(inout), optional :: fldptr1(:)
    real(ESMF_KIND_R8), pointer, intent(inout), optional :: fldptr2(:,:)
    integer               , intent(out), optional :: rank
    integer               , intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrank
    character(len=*),parameter :: subname='(med_method_FB_GetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR rc not present "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    rc = ESMF_SUCCESS

    if (.not. med_method_FB_FldChk(FB, trim(fldname), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR field not in FB "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Field_GetFldPtr(lfield, fldptr1, fldptr2, lrank, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (present(rank)) then
      rank = lrank
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_GetFldPtr

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_SetFldPtr(FB, fldname, val, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    real(ESMF_KIND_R8)    , intent(in)  :: val
    integer               , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer          :: lrank
    real(ESMF_KIND_R8), pointer :: fldptr1(:)
    real(ESMF_KIND_R8), pointer :: fldptr2(:,:)
    character(len=*),parameter :: subname='(med_method_FB_SetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_FB_GetFldPtr(FB, fldname, fldptr1, fldptr2, lrank, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (lrank == 1) then
      fldptr1 = val
    elseif (lrank == 2) then
      fldptr2 = val
    else
      call ESMF_LogWrite(trim(subname)//": ERROR in rank "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_SetFldPtr

  !-----------------------------------------------------------------------------

  subroutine med_method_State_GetFldPtr(ST, fldname, fldptr1, fldptr2, rank, rc)
    type(ESMF_State), intent(in)  :: ST
    character(len=*), intent(in)  :: fldname
    real(ESMF_KIND_R8), pointer, intent(inout), optional :: fldptr1(:)
    real(ESMF_KIND_R8), pointer, intent(inout), optional :: fldptr2(:,:)
    integer         , intent(out), optional :: rank
    integer         , intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrank
    character(len=*),parameter :: subname='(med_method_State_GetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR rc not present "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    rc = ESMF_SUCCESS

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=rc)
    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": fldname ="//trim(fldname), ESMF_LOGMSG_INFO,rc=dbrc)
    endif
!    call ESMF_StatePrint(ST,rc=dbrc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call med_method_Field_GetFldPtr(lfield, fldptr1, fldptr2, lrank, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (present(rank)) then
      rank = lrank
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_GetFldPtr

  !-----------------------------------------------------------------------------

  subroutine med_method_State_SetFldPtr(ST, fldname, val, rc)
    type(ESMF_State)  , intent(in)  :: ST
    character(len=*)  , intent(in)  :: fldname
    real(ESMF_KIND_R8), intent(in)  :: val
    integer           , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer          :: lrank
    real(ESMF_KIND_R8), pointer :: fldptr1(:)
    real(ESMF_KIND_R8), pointer :: fldptr2(:,:)
    character(len=*),parameter :: subname='(med_method_State_SetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call med_method_State_GetFldPtr(ST, fldname, fldptr1, fldptr2, lrank, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (lrank == 1) then
      fldptr1 = val
    elseif (lrank == 2) then
      fldptr2 = val
    else
      call ESMF_LogWrite(trim(subname)//": ERROR in rank "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_SetFldPtr

  !-----------------------------------------------------------------------------

  logical function med_method_FieldPtr_Compare1(fldptr1, fldptr2, cstring, rc)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr1(:)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr2(:)
    character(len=*)           , intent(in)  :: cstring
    integer                    , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(med_method_FieldPtr_Compare1)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    med_method_FieldPtr_Compare1 = .false.
    if (lbound(fldptr2,1) /= lbound(fldptr1,1) .or. &
        ubound(fldptr2,1) /= ubound(fldptr1,1)) then
      call ESMF_LogWrite(trim(subname)//": ERROR in data size "//trim(cstring), ESMF_LOGMSG_ERROR, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      write(msgString,*) trim(subname)//': fldptr1 ',lbound(fldptr1),ubound(fldptr1)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
      write(msgString,*) trim(subname)//': fldptr2 ',lbound(fldptr2),ubound(fldptr2)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    else
      med_method_FieldPtr_Compare1 = .true.
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function med_method_FieldPtr_Compare1

  !-----------------------------------------------------------------------------

  logical function med_method_FieldPtr_Compare2(fldptr1, fldptr2, cstring, rc)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr1(:,:)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr2(:,:)
    character(len=*)           , intent(in)  :: cstring
    integer                    , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(med_method_FieldPtr_Compare2)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    med_method_FieldPtr_Compare2 = .false.
    if (lbound(fldptr2,2) /= lbound(fldptr1,2) .or. &
        lbound(fldptr2,1) /= lbound(fldptr1,1) .or. &
        ubound(fldptr2,2) /= ubound(fldptr1,2) .or. &
        ubound(fldptr2,1) /= ubound(fldptr1,1)) then
      call ESMF_LogWrite(trim(subname)//": ERROR in data size "//trim(cstring), ESMF_LOGMSG_ERROR, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      write(msgString,*) trim(subname)//': fldptr2 ',lbound(fldptr2),ubound(fldptr2)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
      write(msgString,*) trim(subname)//': fldptr1 ',lbound(fldptr1),ubound(fldptr1)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    else
      med_method_FieldPtr_Compare2 = .true.
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function med_method_FieldPtr_Compare2

  !-----------------------------------------------------------------------------

  subroutine med_method_State_GeomPrint(state, string, rc)

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Field)  :: lfield
    integer           :: fieldcount
    character(len=*),parameter  :: subname='(med_method_State_GeomPrint)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(state, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (fieldCount > 0) then
      call med_method_State_GetFieldN(state, 1, lfield, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Field_GeomPrint(lfield, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    else
      call ESMF_LogWrite(trim(subname)//":"//trim(string)//": no fields", ESMF_LOGMSG_INFO, rc=dbrc)
    endif  ! fieldCount > 0

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_GeomPrint

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_GeomPrint(FB, string, rc)

    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Field)  :: lfield
    integer           :: fieldcount
    character(len=*),parameter  :: subname='(med_method_FB_GeomPrint)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (fieldCount > 0) then

      call med_method_Field_GeomPrint(lfield, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    else
      call ESMF_LogWrite(trim(subname)//":"//trim(string)//": no fields", ESMF_LOGMSG_INFO, rc=dbrc)
    endif  ! fieldCount > 0

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_GeomPrint

  !-----------------------------------------------------------------------------

  subroutine med_method_Field_GeomPrint(field, string, rc)

    type(ESMF_Field), intent(in)  :: field
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Grid)     :: lgrid
    type(ESMF_Mesh)     :: lmesh
    integer             :: lrank
    real(ESMF_KIND_R8), pointer :: dataPtr1(:)
    real(ESMF_KIND_R8), pointer :: dataPtr2(:,:)
    character(len=*),parameter  :: subname='(med_method_Field_GeomPrint)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, status=status, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    if (status == ESMF_FIELDSTATUS_EMPTY) then
      call ESMF_LogWrite(trim(subname)//":"//trim(string)//": ERROR field does not have a geom yet ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (geomtype == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=lgrid, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Grid_Print(lgrid, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    elseif (geomtype == ESMF_GEOMTYPE_MESH) then
      call ESMF_FieldGet(field, mesh=lmesh, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Mesh_Print(lmesh, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    call med_method_Field_GetFldPtr(field, dataPtr1, dataPtr2, lrank, abort=.false., rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (lrank == 1) then
      write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=1 ",lbound(dataptr1,1),ubound(dataptr1,1)
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    elseif (lrank == 2) then
      write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=1 ",lbound(dataptr2,1),ubound(dataptr2,1)
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=2 ",lbound(dataptr2,2),ubound(dataptr2,2)
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    elseif (lrank == 0) then
      ! means data allocation does not exist yet
      continue
    else
      call ESMF_LogWrite(trim(subname)//": ERROR rank not supported ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Field_GeomPrint

  !-----------------------------------------------------------------------------

  subroutine med_method_Mesh_Print(mesh, string, rc)

    type(ESMF_Mesh) , intent(in)  :: mesh
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Distgrid)         :: distgrid  
    integer                     :: pdim, sdim, nnodes, nelements
    integer                     :: localDeCount
    integer                     :: DeCount
    integer                     :: dimCount, tileCount
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)
    character(len=*),parameter  :: subname='(med_method_Mesh_Print)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! access localDeCount to show this is a real Grid
    call ESMF_MeshGet(mesh, parametricDim=pdim, spatialDim=sdim, elementDistgrid=distgrid, &
      numOwnedNodes=nnodes, numOwnedElements=nelements, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
   
    write (msgString,*) trim(subname)//":"//trim(string)//": parametricDim=", pdim
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    write (msgString,*) trim(subname)//":"//trim(string)//": spatialDim=", sdim
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    write (msgString,*) trim(subname)//":"//trim(string)//": numOwnedNodes=", nnodes
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    write (msgString,*) trim(subname)//":"//trim(string)//": numOwnedElements=", nelements
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, deCount=deCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    write (msgString,*) trim(subname)//":"//trim(string)//": dimCount=", dimCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write (msgString,*) trim(subname)//":"//trim(string)//": tileCount=", tileCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write (msgString,*) trim(subname)//":"//trim(string)//": deCount=", deCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
             maxIndexPTile(dimCount, tileCount))
    
    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    write (msgString,*) trim(subname)//":"//trim(string)//": minIndexPTile=", minIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write (msgString,*) trim(subname)//":"//trim(string)//": maxIndexPTile=", maxIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    deallocate(minIndexPTile, maxIndexPTile)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Mesh_Print

  !-----------------------------------------------------------------------------

  subroutine med_method_Grid_Print(grid, string, rc)

    type(ESMF_Grid) , intent(in)  :: grid
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Distgrid) :: distgrid  
    integer                     :: localDeCount
    integer                     :: DeCount
    integer                     :: dimCount, tileCount
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)
    character(len=*),parameter  :: subname='(med_method_Grid_Print)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
   
    write (msgString,*) trim(subname)//":"//trim(string)//": localDeCount=", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, deCount=deCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    
    write (msgString,*) trim(subname)//":"//trim(string)//": dimCount=", dimCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write (msgString,*) trim(subname)//":"//trim(string)//": tileCount=", tileCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write (msgString,*) trim(subname)//":"//trim(string)//": deCount=", deCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
             maxIndexPTile(dimCount, tileCount))
    
    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      
    write (msgString,*) trim(subname)//":"//trim(string)//": minIndexPTile=", minIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    write (msgString,*) trim(subname)//":"//trim(string)//": maxIndexPTile=", maxIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    deallocate(minIndexPTile, maxIndexPTile)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Grid_Print

!-----------------------------------------------------------------------------
  subroutine med_method_Clock_TimePrint(clock,string,rc)

    type(ESMF_Clock),intent(in) :: clock
    character(len=*),intent(in),optional :: string
    integer, intent(out) :: rc

    type(ESMF_Time)      :: time
    type(ESMF_TimeInterval) :: timeStep
    character(len=64)    :: timestr
    character(len=512)   :: lstring
    character(len=*),parameter :: subname='(med_method_Clock_TimePrint)'

    rc = ESMF_SUCCESS

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (present(string)) then
      lstring = trim(subname)//":"//trim(string)
    else
      lstring = trim(subname)
    endif

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_TimeGet(time,timestring=timestr,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_LogWrite(trim(lstring)//": currtime = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_ClockGet(clock,starttime=time,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_TimeGet(time,timestring=timestr,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_LogWrite(trim(lstring)//": startime = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_ClockGet(clock,stoptime=time,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_TimeGet(time,timestring=timestr,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_LogWrite(trim(lstring)//": stoptime = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_ClockGet(clock,timestep=timestep,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_TimeIntervalGet(timestep,timestring=timestr,rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_LogWrite(trim(lstring)//": timestep = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Clock_TimePrint

  !-----------------------------------------------------------------------------

  subroutine med_method_Mesh_Write(mesh, string, rc)
    type(ESMF_Mesh) ,intent(in)  :: mesh
    character(len=*),intent(in)  :: string
    integer         ,intent(out) :: rc
  
    ! local 
    integer  :: n,l,i,lsize,ndims
    character(len=64) :: name
    type(ESMF_DISTGRID)         :: distgrid
    type(ESMF_Array)            :: array
    real(ESMF_KIND_R8), pointer :: rawdata(:)
    real(ESMF_KIND_R8), pointer :: coord(:)
    character(len=*),parameter  :: subname='(med_method_Mesh_Write)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

#if (1 == 0)
    !--- elements ---

    call ESMF_MeshGet(mesh, spatialDim=ndims, numownedElements=lsize, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    allocate(rawdata(ndims*lsize))
    allocate(coord(lsize))

    call ESMF_MeshGet(mesh, elementDistgrid=distgrid, ownedElemCoords=rawdata, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    do n = 1,ndims
      name = "unknown"
      if (n == 1) name = "lon_element"
      if (n == 2) name = "lat_element"
    do l = 1,lsize
      i = 2*(l-1) + n
      coord(l) = rawdata(i)
      array = ESMF_ArrayCreate(distgrid, farrayPtr=coord, name=name, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo
    enddo

    deallocate(rawdata,coord)

    !--- nodes ---

    call ESMF_MeshGet(mesh, spatialDim=ndims, numownedNodes=lsize, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    allocate(rawdata(ndims*lsize))
    allocate(coord(lsize))

    call ESMF_MeshGet(mesh, nodalDistgrid=distgrid, ownedNodeCoords=rawdata, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    do n = 1,ndims
      name = "unknown"
      if (n == 1) name = "lon_nodes"
      if (n == 2) name = "lat_nodes"
    do l = 1,lsize
      i = 2*(l-1) + n
      coord(l) = rawdata(i)
      array = ESMF_ArrayCreate(distgrid, farrayPtr=coord, name=name, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
      call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo
    enddo

    deallocate(rawdata,coord)
#else
      call ESMF_LogWrite(trim(subname)//": turned off right now", ESMF_LOGMSG_INFO, rc=dbrc)
#endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Mesh_Write

  !-----------------------------------------------------------------------------

  subroutine med_method_State_GeomWrite(state, string, rc)

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Field)  :: lfield
    integer           :: fieldcount
    character(len=*),parameter  :: subname='(med_method_State_GeomWrite)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(state, itemCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (fieldCount > 0) then
      call med_method_State_getFieldN(state, 1, lfield, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Field_GeomWrite(lfield, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    else
      call ESMF_LogWrite(trim(subname)//":"//trim(string)//": no fields", ESMF_LOGMSG_INFO, rc=dbrc)
    endif  ! fieldCount > 0

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_State_GeomWrite

  !-----------------------------------------------------------------------------

  subroutine med_method_FB_GeomWrite(FB, string, rc)

    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Field)  :: lfield
    integer           :: fieldcount
    character(len=*),parameter  :: subname='(med_method_FB_GeomWrite)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return

    if (fieldCount > 0) then
      call med_method_FB_getFieldN(FB, 1, lfield, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Field_GeomWrite(lfield, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    else
      call ESMF_LogWrite(trim(subname)//":"//trim(string)//": no fields", ESMF_LOGMSG_INFO, rc=dbrc)
    endif  ! fieldCount > 0

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_FB_GeomWrite

  !-----------------------------------------------------------------------------

  subroutine med_method_Field_GeomWrite(field, string, rc)

    type(ESMF_Field), intent(in)  :: field
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Grid)     :: lgrid
    type(ESMF_Mesh)     :: lmesh
    character(len=*),parameter  :: subname='(med_method_Field_GeomWrite)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, status=status, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return
    if (status == ESMF_FIELDSTATUS_EMPTY) then
      call ESMF_LogWrite(trim(subname)//":"//trim(string)//": ERROR field does not have a geom yet ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (geomtype == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=lgrid, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Grid_Write(lgrid, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    elseif (geomtype == ESMF_GEOMTYPE_MESH) then
      call ESMF_FieldGet(field, mesh=lmesh, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Mesh_Write(lmesh, string, rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Field_GeomWrite

  !-----------------------------------------------------------------------------

  subroutine med_method_Grid_Write(grid, string, rc)
    type(ESMF_Grid) ,intent(in)  :: grid
    character(len=*),intent(in)  :: string
    integer         ,intent(out) :: rc
  
    ! local 
    type(ESMF_Array)            :: array
    character(len=64)           :: name
    character(len=*),parameter  :: subname='(med_method_Grid_Write)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! -- centers --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (isPresent) then
      name = "lon_center"
      call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArraySet(array, name=name, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

      name = "lat_center"
      call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArraySet(array, name=name, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    ! -- corners --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, isPresent=isPresent, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (isPresent) then
      name = "lon_corner"
      call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) then
        call ESMF_ArraySet(array, name=name, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      endif

      name = "lat_corner"
      call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) then
        call ESMF_ArraySet(array, name=name, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      endif
    endif

    ! -- mask --

    name = "mask"
    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArraySet(array, name=name, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Array_diagnose(array,trim(string)//"_"//trim(name), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    ! -- area --

    name = "area"
    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArraySet(array, name=name, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call med_method_Array_diagnose(array,trim(string)//trim(name), rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      call ESMF_ArrayWrite(array, trim(string)//"_"//trim(name)//".nc", overwrite=.true., rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Grid_Write

  !-----------------------------------------------------------------------------

  logical function med_method_Distgrid_Match(distGrid1, distGrid2, rc)

    ! Arguments
    type(ESMF_DistGrid), intent(in)     :: distGrid1
    type(ESMF_DistGrid), intent(in)     :: distGrid2
    integer, intent(out), optional  :: rc

    ! Local Variables
    integer                         :: dimCount1, dimCount2
    integer                         :: tileCount1, tileCount2
    integer, allocatable            :: minIndexPTile1(:,:), minIndexPTile2(:,:)
    integer, allocatable            :: maxIndexPTile1(:,:), maxIndexPTile2(:,:)
    integer, allocatable            :: elementCountPTile1(:), elementCountPTile2(:)
    character(len=*), parameter :: subname='(med_method_Distgrid_Match)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if(present(rc)) rc = ESMF_SUCCESS
    med_method_Distgrid_Match = .true.

    call ESMF_DistGridGet(distGrid1, &
      dimCount=dimCount1, tileCount=tileCount1, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_DistGridGet(distGrid2, &
      dimCount=dimCount2, tileCount=tileCount2, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if ( dimCount1 /= dimCount2) then
      med_method_Distgrid_Match = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid dimCount MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if ( tileCount1 /= tileCount2) then
      med_method_Distgrid_Match = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid tileCount MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    allocate(elementCountPTile1(tileCount1))
    allocate(elementCountPTile2(tileCount2))
    allocate(minIndexPTile1(dimCount1,tileCount1))
    allocate(minIndexPTile2(dimCount2,tileCount2))
    allocate(maxIndexPTile1(dimCount1,tileCount1))
    allocate(maxIndexPTile2(dimCount2,tileCount2))

    call ESMF_DistGridGet(distGrid1, &
      elementCountPTile=elementCountPTile1, &
      minIndexPTile=minIndexPTile1, &
      maxIndexPTile=maxIndexPTile1, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_DistGridGet(distGrid2, &
      elementCountPTile=elementCountPTile2, &
      minIndexPTile=minIndexPTile2, &
      maxIndexPTile=maxIndexPTile2, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if ( ANY((elementCountPTile1 - elementCountPTile2) .NE. 0) ) then
      med_method_Distgrid_Match = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid elementCountPTile MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if ( ANY((minIndexPTile1 - minIndexPTile2) .NE. 0) ) then
      med_method_Distgrid_Match = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid minIndexPTile MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if ( ANY((maxIndexPTile1 - maxIndexPTile2) .NE. 0) ) then
      med_method_Distgrid_Match = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid maxIndexPTile MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    deallocate(elementCountPTile1)
    deallocate(elementCountPTile2)
    deallocate(minIndexPTile1)
    deallocate(minIndexPTile2)
    deallocate(maxIndexPTile1)
    deallocate(maxIndexPTile2)

    ! TODO: Optionally Check Coordinates


    if (dbug_flag > 10) then
      call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function med_method_Distgrid_Match

  !-----------------------------------------------------------------------------

  subroutine med_method_Grid_CopyCoord(gridcomp, gridSrc, gridDst, staggerloc, tolerance, compare, invert, rc)

    ! Arguments
    type(ESMF_GridComp),intent(in)      :: gridcomp
    type(ESMF_Grid), intent(in)         :: gridSrc
    type(ESMF_Grid), intent(in)         :: gridDst
    type(ESMF_StaggerLoc),intent(in)    :: staggerloc(:)
    real, intent(in), optional          :: tolerance
    logical, intent(in), optional       :: compare
    integer, intent(in), optional       :: invert(:)
    integer, intent(out),optional       :: rc

    ! Local Variables
    real                                :: l_tolerance
    logical                             :: l_compare
    integer, allocatable                :: l_invert(:)
    integer                             :: i
    type(ESMF_VM)                       :: vm
    type(ESMF_DistGrid)                 :: distGridSrc, distGridDst
    type(ESMF_Array)                    :: coordArraySrc, coordArrayDst
    integer(ESMF_KIND_I4),allocatable   :: factorList(:)
    integer, allocatable                :: factorIndexList(:,:)
    type(ESMF_RouteHandle)          :: routehandle
    integer                         :: dimCountSrc, dimCountDst
    integer                         :: deCountDst
    integer, allocatable            :: elementCountPDeDst(:)
    integer(ESMF_KIND_I8)           :: sumElementCountPDeDst
    type(ESMF_TypeKind_Flag)        :: coordTypeKindSrc, coordTypeKindDst
    type(ESMF_CoordSys_Flag)        :: coordSysSrc, coordSysDst
    logical                         :: isPresentSrc, isPresentDst
    integer                         :: dimIndex, staggerlocIndex
    integer                         :: localPet
    character(len=10)               :: numString
    character(len=*), parameter :: subname='(med_method_Grid_CopyCoord)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    l_tolerance = 0.0
    if (present(tolerance)) l_tolerance = tolerance
    l_compare = .FALSE.
    if (present(compare)) l_compare = compare
    if (present(invert)) then
      allocate(l_invert(size(invert)))
      l_invert = invert
    else
      allocate(l_invert(1))
      l_invert = -1
    endif

    call ESMF_GridGet(gridSrc, distGrid=distGridSrc, &
      dimCount=dimCountSrc, coordTypeKind=coordTypeKindSrc, &
      coordSys=coordSysSrc, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_GridGet(gridDst, distGrid=distGridDst, &
      dimCount=dimCountDst, coordTypeKind=coordTypeKindDst, &
      coordSys=coordSysDst, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (.NOT. med_method_Distgrid_Match(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": Unable to redistribute coordinates. DistGrids do not match.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( dimCountSrc /= dimCountDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": DIMCOUNT MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordTypeKindSrc /= coordTypeKindDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": COORDTYPEKIND MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordSysSrc /= coordSysDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": COORDSYS MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    do dimIndex=1, dimCountDst
    do staggerlocIndex=1, size(staggerloc)
      call ESMF_GridGetCoord(gridSrc, staggerloc=staggerloc(staggerlocIndex), &
        isPresent=isPresentSrc, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if(isPresentSrc) then
        call ESMF_GridGetCoord(gridSrc, coordDim=dimIndex, &
          staggerloc=staggerloc(staggerlocIndex), &
          array=coordArraySrc, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_GridGetCoord(gridDst, &
          staggerloc=staggerloc(staggerlocIndex), &
          isPresent=isPresentDst, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if(.NOT.isPresentDst) then
          call ESMF_GridAddCoord(gridDst, &
            staggerLoc=staggerloc(staggerlocIndex), rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        else
          if(l_compare .EQV. .TRUE.) then
            ! TODO: Compare existing coordinates
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=subname//": Cannot compare existing coordinates.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
          end if
        endif
        call ESMF_GridGetCoord(gridDst, coordDim=dimIndex, &
          staggerloc=staggerloc(staggerlocIndex), &
          array=coordArrayDst, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_ArrayGet(coordArraySrc, distGrid=distGridSrc, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_ArrayGet(coordArrayDst, distGrid=distGridDst, &
          dimCount=dimCountDst, deCount=deCountDst, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (.NOT. med_method_Distgrid_Match(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=subname//": Unable to redistribute coordinates. DistGrids do not match.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
        endif

        if ( ANY( l_invert == dimIndex )) then
          call ESMF_GridCompGet(gridcomp, vm=vm, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          call ESMF_VMGet(vm, localPet=localPet, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

          if (localPet == 0) then
            call ESMF_DistGridGet(distGridDst, deCount=deCountDst, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

            allocate(elementCountPDeDst(deCountDst))
            call ESMF_DistGridGet(distGridDst, &
              elementCountPDe=elementCountPDeDst, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

            sumElementCountPDeDst = SUM(elementCountPDeDst)
            if (dbug_flag >= 2) then
              write (numString, "(I10)") sumElementCountPDeDst
              call ESMF_LogWrite(subname//": sumElementCountPDeDst: "//trim(adjustl(numString)), ESMF_LOGMSG_INFO, rc=dbrc)
            endif

            allocate(factorList(sumElementCountPDeDst))
            allocate(factorIndexList(2,sumElementCountPDeDst))

            factorList(:) = 1
            factorIndexList(1,:) = (/(i, i=1, sumElementCountPDeDst, 1)/)
            factorIndexList(2,:) = (/(i, i=sumElementCountPDeDst, 1, -1)/)

            if (dbug_flag >= 2) then
              write (numString, "(I10)") factorIndexList(1,1)
              write (msgString, "(A)") "Src=>Dst: "//trim(adjustl(numString))//"=>"
              write (numString, "(I10)") factorIndexList(2,1)
              write (msgString, "(A)") trim(msgString)//trim(adjustl(numString))
              write (numString, "(I10)") factorIndexList(1,sumElementCountPDeDst) 
              write (msgString, "(A)") trim(msgString)//" "//trim(adjustl(numString))//"=>"
              write (numString, "(I10)") factorIndexList(2,sumElementCountPDEDst)
     	      write (msgString, "(A)") trim(msgString)//trim(adjustl(numString))
              call ESMF_LogWrite(subname//": Invert Mapping: "//msgString, ESMF_LOGMSG_INFO, rc=dbrc)
            endif

            call ESMF_ArraySMMStore(srcArray=coordArraySrc, dstArray=coordArrayDst, &
              routehandle=routehandle, factorList=factorList, &
              factorIndexList=factorIndexList, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
            deallocate(elementCountPDeDst)
            deallocate(factorList)
            deallocate(factorIndexList)
          else
            call ESMF_ArraySMMStore(srcArray=coordArraySrc, dstArray=coordArrayDst, &
              routehandle=routehandle, rc=rc)
            if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          endif

          call ESMF_ArraySMM(srcArray=coordArraySrc, dstArray=coordArrayDst, &
            routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArraySMMRelease(routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

        else
          call ESMF_ArrayRedistStore(coordArraySrc, coordArrayDst, routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArrayRedist(coordArraySrc, coordArrayDst, routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=subname//": SOURCE GRID MISSING STAGGER LOCATION", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
    enddo
    enddo

    deallocate(l_invert)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Grid_CopyCoord

  !-----------------------------------------------------------------------------

  subroutine med_method_Grid_CopyItem(gridcomp, gridSrc, gridDst, item, tolerance, compare, invert, rc)

    ! Arguments
    type(ESMF_GridComp),intent(in)      :: gridcomp
    type(ESMF_Grid), intent(in)         :: gridSrc
    type(ESMF_Grid), intent(in)         :: gridDst
    type(ESMF_GridItem_Flag),intent(in) :: item(:)
    real, intent(in), optional          :: tolerance
    logical, intent(in), optional       :: compare
    integer, intent(in), optional       :: invert(:)
    integer, intent(out),optional       :: rc

    ! Local Variables
    real                                :: l_tolerance
    logical                             :: l_compare
    integer, allocatable                :: l_invert(:)
    type(ESMF_StaggerLoc)               :: l_staggerloc
    type(ESMF_DistGrid)                 :: distGridSrc, distGridDst
    type(ESMF_Array)                    :: itemArraySrc, itemArrayDst
    type(ESMF_RouteHandle)          :: routehandle
    integer                         :: dimCountSrc, dimCountDst
    type(ESMF_TypeKind_Flag)        :: coordTypeKindSrc, coordTypeKindDst
    type(ESMF_CoordSys_Flag)        :: coordSysSrc, coordSysDst
    logical                         :: isPresentSrc, isPresentDst
    integer                         :: itemIndex
    integer                         :: localPet
    character(len=10)               :: numString
    character(len=*), parameter :: subname='(med_method_Grid_CopyItem)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(subname//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    l_tolerance = 0.0
    if (present(tolerance)) l_tolerance = tolerance
    l_compare = .FALSE.
    if (present(compare)) l_compare = compare
    if (present(invert)) then
      allocate(l_invert(size(invert)))
      l_invert = invert
    else
      allocate(l_invert(1))
      l_invert = -1
    endif
    l_staggerloc = ESMF_STAGGERLOC_CENTER

    call ESMF_GridGet(gridSrc, distGrid=distGridSrc, &
      dimCount=dimCountSrc, coordTypeKind=coordTypeKindSrc, &
      coordSys=coordSysSrc, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    call ESMF_GridGet(gridDst, distGrid=distGridDst, &
      dimCount=dimCountDst, coordTypeKind=coordTypeKindDst, &
      coordSys=coordSysDst, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    if (.NOT. med_method_Distgrid_Match(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": Unable to redistribute coordinates. DistGrids do not match.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( dimCountSrc /= dimCountDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": DIMCOUNT MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordTypeKindSrc /= coordTypeKindDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": COORDTYPEKIND MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordSysSrc /= coordSysDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=subname//": COORDSYS MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    do itemIndex=1, size(item)
      call ESMF_GridGetItem(gridSrc, itemflag=item(itemIndex), &
        staggerloc=l_staggerloc, isPresent=isPresentSrc, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
      if(isPresentSrc) then
        call ESMF_GridGetItem(gridSrc, itemflag=item(itemIndex), &
          staggerloc=l_staggerloc, array=itemArraySrc, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_GridGetItem(gridDst, itemflag=item(itemIndex), &
          staggerloc=l_staggerloc, isPresent=isPresentDst, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if(.NOT.isPresentDst) then
          call ESMF_GridAddItem(gridDst, itemflag=item(itemIndex), &
            staggerLoc=l_staggerloc, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        else
          if(l_compare .EQV. .TRUE.) then
            ! TODO: Compare existing coordinates
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=subname//": Cannot compare existing coordinates.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
          end if
        endif
        call ESMF_GridGetItem(gridDst, itemflag=item(itemIndex), &
          staggerloc=l_staggerloc, array=itemArrayDst, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_ArrayGet(itemArraySrc, distGrid=distGridSrc, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        call ESMF_ArrayGet(itemArrayDst, distGrid=distGridDst, rc=rc)
        if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        if (.NOT. med_method_Distgrid_Match(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg=subname//": Unable to redistribute coordinates. DistGrids do not match.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
            return  ! bail out
        endif

        if ( ANY( l_invert > 0 )) then
          ! TODO: Invert Item
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=subname//": Cannot invert item.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
        else
          call ESMF_ArrayRedistStore(itemArraySrc, itemArrayDst, routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArrayRedist(itemArraySrc, itemArrayDst, routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
          call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
          if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=subname//": SOURCE GRID MISSING ITEM", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
    enddo

    deallocate(l_invert)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(subname//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_method_Grid_CopyItem

!================================================================================

  subroutine med_method_CopyStateToScalar(State, data, mpicom, rc)
    ! ----------------------------------------------
    ! Copy scalar data from State to local data on root then broadcast data
    ! to all PETs in component.
    ! ----------------------------------------------
    type(ESMF_State),  intent(in)     :: State
    real(ESMF_KIND_R8),intent(inout)  :: data(:)
    integer,           intent(in)     :: mpicom
    integer,           intent(inout)  :: rc

    ! local variables
    integer :: mytask, ierr, len
    character(MPI_MAX_ERROR_STRING) :: lstring
    type(ESMF_Field) :: field
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: farrayptr(:,:)
    character(len=*), parameter :: subname='(med_method_CopyStateToScalar)'

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, mytask, rc)
    call ESMF_StateGet(State, itemName=trim(seq_flds_scalar_name), itemType=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (itemType == ESMF_STATEITEM_NOTFOUND) then
      call ESMF_LogWrite(trim(subname)//": "//trim(seq_flds_scalar_name)//" not found", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
    else
      call ESMF_StateGet(State, itemName=trim(seq_flds_scalar_name), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      if (mytask == 0) then
        call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (size(data) < seq_flds_scalar_num .or. size(farrayptr) < seq_flds_scalar_num) then
          call ESMF_LogWrite(trim(subname)//": ERROR on data size", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        endif
        data(1:seq_flds_scalar_num) = farrayptr(1,1:seq_flds_scalar_num)
      endif

      call MPI_BCAST(data, seq_flds_scalar_num, MPI_REAL8, 0, mpicom, rc)
      if (rc /= MPI_SUCCESS) then
        call MPI_ERROR_STRING(rc,lstring,len,ierr)
        call ESMF_LogWrite(trim(subname)//": ERROR "//trim(lstring), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
    endif

  end subroutine med_method_CopyStateToScalar

!================================================================================

  subroutine med_method_CopyScalarToState(data, State, mpicom, rc)
    ! ----------------------------------------------
    ! Copy local scalar data into State, root only, 
    ! but called on all PETs in component
    ! ----------------------------------------------
    real(ESMF_KIND_R8),intent(in)     :: data(:)
    type(ESMF_State),  intent(inout)  :: State
    integer,           intent(in)     :: mpicom
    integer,           intent(inout)  :: rc

    ! local variables
    integer :: mytask
    type(ESMF_Field) :: field
    type(ESMF_StateItem_Flag) :: ItemType
    real(ESMF_KIND_R8), pointer :: farrayptr(:,:)
    character(len=*), parameter :: subname='(med_method_CopyScalarToState)'

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, mytask, rc)

    call ESMF_StateGet(State, itemName=trim(seq_flds_scalar_name), itemType=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (itemType == ESMF_STATEITEM_NOTFOUND) then
      call ESMF_LogWrite(trim(subname)//": "//trim(seq_flds_scalar_name)//" not found", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
    else
      call ESMF_StateGet(State, itemName=trim(seq_flds_scalar_name), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (mytask == 0) then
        call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (size(data) < seq_flds_scalar_num .or. size(farrayptr) < seq_flds_scalar_num) then
          call ESMF_LogWrite(trim(subname)//": ERROR on data size", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        endif
        farrayptr(1,1:seq_flds_scalar_num) = data(1:seq_flds_scalar_num)
      endif
    endif

  end subroutine med_method_CopyScalarToState

!================================================================================

  subroutine med_method_State_GetScalar(State, scalar_id, value, mpicom, rc)
    ! ----------------------------------------------
    ! Get scalar data from State for a particular name
    ! ----------------------------------------------
    type(ESMF_State),  intent(in)     :: State
    integer,           intent(in)     :: scalar_id
    real(ESMF_KIND_R8),intent(out)    :: value
    integer,           intent(in)     :: mpicom
    integer,           intent(inout)  :: rc

    ! local variables
    integer :: mytask, ierr, len
    character(MPI_MAX_ERROR_STRING) :: lstring
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8), pointer :: farrayptr(:,:)
    character(len=*), parameter :: subname='(med_method_State_GetScalar)'

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, mytask, rc)
    call ESMF_StateGet(State, itemName=trim(seq_flds_scalar_name), field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (mytask == 0) then
      call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (scalar_id < 0 .or. scalar_id > seq_flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR in scalar_id", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
      value = farrayptr(1,scalar_id)  
    endif
 
    call MPI_BCAST(value, 1, MPI_REAL8, 0, mpicom, rc)
    if (rc /= MPI_SUCCESS) then
      call MPI_ERROR_STRING(rc,lstring,len,ierr)
      call ESMF_LogWrite(trim(subname)//": ERROR "//trim(lstring), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    endif

  end subroutine med_method_State_GetScalar

!================================================================================

  subroutine med_method_State_SetScalar(value, scalar_id, State, mpicom, rc)
    ! ----------------------------------------------
    ! Set scalar data from State for a particular name
    ! ----------------------------------------------
    real(ESMF_KIND_R8),intent(in)     :: value
    integer,           intent(in)     :: scalar_id
    type(ESMF_State),  intent(inout)  :: State
    integer,           intent(in)     :: mpicom
    integer,           intent(inout)  :: rc

    ! local variables
    integer :: mytask, ierr, len
    character(MPI_MAX_ERROR_STRING) :: lstring
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8), pointer :: farrayptr(:,:)
    character(len=*), parameter :: subname='(med_method_State_SetScalar)'

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, mytask, rc)
    call ESMF_StateGet(State, itemName=trim(seq_flds_scalar_name), field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (mytask == 0) then
      call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (scalar_id < 0 .or. scalar_id > seq_flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR in scalar_id", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
      farrayptr(1,scalar_id) = value
    endif
 
  end subroutine med_method_State_SetScalar

  !-----------------------------------------------------------------------------

  subroutine med_method_State_UpdateTimestamp(state, time, rc)
    type(ESMF_State), intent(inout) :: state
    type(ESMF_Time), intent(in)     :: time
    integer, intent(out)  :: rc

    ! local variables
    integer :: i
    type(ESMF_Field),pointer    :: fieldList(:)
    character(len=*), parameter :: subname='(med_method_State_UpdateTimestamp)'

    rc = ESMF_SUCCESS

    call NUOPC_GetStateMemberLists(state, fieldList=fieldList, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

    do i=1, size(fieldList)
      call med_method_Field_UpdateTimestamp(fieldList(i), time, rc=rc)
      if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    enddo
    
  end subroutine med_method_State_UpdateTimestamp

  !-----------------------------------------------------------------------------

  subroutine med_method_Field_UpdateTimestamp(field, time, rc)
    type(ESMF_Field), intent(inout) :: field
    type(ESMF_Time) , intent(in)    :: time
    integer, intent(out)  :: rc

    ! local variables
    integer               :: yy, mm, dd, h, m, s, ms, us, ns
    character(len=*), parameter :: subname='(med_method_Field_UpdateTimestamp)'

    rc = ESMF_SUCCESS
    
    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, us=us, &
      ns=ns, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 
    call ESMF_AttributeSet(field, &
      name="TimeStamp", valueList=(/yy,mm,dd,h,m,s,ms,us,ns/), &
      convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (med_method_ChkErr(rc,__LINE__,__FILE__)) return 

  end subroutine med_method_Field_UpdateTimestamp

  !-----------------------------------------------------------------------------

  logical function med_method_ChkErr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    med_method_ChkErr = .false.

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
      med_method_ChkErr = .true.
    endif

  end function med_method_ChkErr

  !-----------------------------------------------------------------------------

end module med_method_mod

