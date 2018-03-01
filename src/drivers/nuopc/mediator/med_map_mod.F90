module med_map_mod

  use mpi
  use ESMF
  use NUOPC
  use shr_kind_mod                , only: SHR_KIND_CX, SHR_KIND_CL, SHR_KIND_CS
  use shr_sys_mod                 , only: shr_sys_flush
  use shr_nuopc_fldList_types_mod , only: shr_nuopc_fldList_src_entry_type
  use shr_nuopc_fldList_types_mod , only: flds_scalar_name
  use shr_nuopc_fldList_types_mod , only: fldListFr, fldListTo
  use shr_nuopc_fldList_types_mod , only: ncomps, compmed, compatm, compocn
  use shr_nuopc_fldList_types_mod , only: compice, complnd, comprof, compwav, compglc, compname
  use shr_nuopc_fldList_types_mod , only: mapbilnr, mapconsf, mapconsd, mappatch, mapfcopy, mapunset 
  use shr_nuopc_fldList_types_mod , only: mapnames, nmappers
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_Init
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_Reset
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_Clean
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_Copy
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_GetFldPtr
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_FieldRegrid
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_FB_FldChk
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_RH_Init
  use shr_nuopc_methods_mod       , only: shr_nuopc_methods_ChkErr
  use med_internalstate_mod       , only: InternalState
  use med_constants_mod           

  implicit none

  private

  ! public routines
  public :: med_map_RouteHandles_init
  public :: med_map_MapNorm_init
  public :: med_map_FB_Regrid_Norm

  ! private module variables
  integer                       :: dbrc
  logical                       :: mastertask
  integer                       :: dbug_flag   = med_constants_dbug_flag
  real(ESMF_KIND_R8), parameter :: spval_init  = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval       = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero       = med_constants_czero
  integer           , parameter :: ispval_mask = med_constants_ispval_mask
  character(*)      , parameter :: u_FILE_u    = __FILE__

!================================================================================
contains
!================================================================================

  subroutine med_map_RouteHandles_init(gcomp, llogunit, rc)

    !---------------------------------------------
    ! Initialize route handles in the mediator
    !---------------------------------------------

    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: llogunit
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)              :: is_local
    type(ESMF_VM)                    :: vm
    type(ESMF_FieldBundle)           :: FBSrc
    type(ESMF_FieldBundle)           :: FBDst
    type(ESMF_Field)                 :: fldsrc
    type(ESMF_Field)                 :: flddst
    integer                          :: localPet
    integer                          :: n,n1,n2,m,nf,nflds,ncomp
    integer                          :: SrcMaskValue
    integer                          :: DstMaskValue
    character(len=128)               :: value
    character(len=128)               :: rhname
    character(len=128)               :: rhname_file
    character(len=SHR_KIND_CS)       :: string
    character(len=SHR_KIND_CS)       :: mapname
    character(len=SHR_KIND_CX)       :: mapfile
    character(MPI_MAX_ERROR_STRING)  :: lstring
    integer                          :: mapindex
    logical                          :: rhprint_flag = .false.
    integer                          :: srcTermProcessing_Value = 0
    real(ESMF_KIND_R8)     , pointer :: factorList(:)
    character(SHR_KIND_CL) , pointer :: fldnames(:)
    type(ESMF_PoleMethod_Flag), parameter :: polemethod=ESMF_POLEMETHOD_ALLAVG
    character(len=*), parameter :: subname='(module_MED_Map:RouteHandles_init)'
    !-----------------------------------------------------------
    
    if (dbug_flag > 1) then
       call ESMF_LogWrite("Starting to initialize RHs", ESMF_LOGMSG_INFO)
       call ESMF_LogFlush()
    endif

    rc = ESMF_SUCCESS

    ! Determine mastertask
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    mastertask = .false.
    if (localPet == 0) mastertask=.true.

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (mastertask) write(llogunit,*) ' '
    do n1 = 1, ncomps
       do n2 = 1, ncomps

          dstMaskValue = ispval_mask
          srcMaskValue = ispval_mask
          if (n1 == compocn .or. n1 == compice) srcMaskValue = 0
          if (n2 == compocn .or. n2 == compice) dstMaskValue = 0

          !--- get single fields from bundles
          !--- 1) ASSUMES all fields in the bundle are on identical grids
          !--- 2) MULTIPLE route handles are going to be generated for
          !---    given field bundle source and destination grids

          call shr_nuopc_methods_FB_getFieldN(FBsrc, 1, fldsrc, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_getFieldN(FBdst, 1, flddst, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (n1 /= n2) then
             if (is_local%wrap%med_coupling_active(n1,n2)) then ! If coupling is active between n1 and n2

                ! Determine route handle names
                rhname = trim(compname(n1))//"2"//trim(compname(n2))
                if (mastertask) write(llogunit,*) subname,' initialize RH for '//trim(rhname)

                ! Loop over fields 
                do nf = 1,size(fldListFr(n1)%flds)

                   mapindex = fldListFr(n1)%flds(nf)%mapindex(n2)

                   ! Create route handle for target mapindex if route handle is required
                   ! (i.e. mapindex /= mapunset) and route handle has not already been created
                   if (mapindex /= mapunset .and. &
                        .not. ESMF_RouteHandleIsCreated(is_local%wrap%RH(n1,n2,mapindex), rc=rc)) then

                      mapname  = trim(mapnames(mapindex))
                      mapfile  = trim(fldListFr(n1)%flds(nf)%mapfile(n2))
                      string   = trim(rhname)//'_weights'

                      if (mapindex == mapfcopy) then
                         if (mastertask) write(llogunit,'(3A)') subname,trim(string),' RH redist '
                         call ESMF_LogWrite(trim(subname) // trim(string) // ' RH redist ', ESMF_LOGMSG_INFO, rc=dbrc)
                         call ESMF_FieldRedistStore(fldsrc, flddst, &
                              routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                              ignoreUnmatchedIndices = .true., rc=rc)
                         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                      else if (mapfile /= 'unset') then
                         if (mastertask) then
                            write(llogunit,'(4A)') subname,trim(string),' RH '//trim(mapname)//' via input file ',&
                                 trim(mapfile)
                         end if
                         call ESMF_LogWrite(subname // trim(string) //&
                              ' RH '//trim(mapname)//' via input file '//trim(mapfile), ESMF_LOGMSG_INFO, rc=dbrc)
                         call ESMF_FieldSMMStore(fldsrc, flddst, &
                              mapfile, &
                              routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                              ignoreUnmatchedIndices=.true., &
                              srcTermProcessing=srcTermProcessing_Value, rc=rc)
                         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                      else
                         if (mastertask) write(llogunit,'(3A)') subname,trim(string),&
                              ' RH regrid for '//trim(mapname)//' computed on the fly'
                         call ESMF_LogWrite(subname // trim(string) //&
                              ' RH regrid for '//trim(mapname)//' computed on the fly', ESMF_LOGMSG_INFO, rc=dbrc)
                         if (mapindex == mapbilnr) then
                            srcTermProcessing_Value = 0
                            call ESMF_FieldRegridStore(fldsrc, flddst, &
                                 routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                 srcMaskValues=(/srcMaskValue/), &
                                 dstMaskValues=(/dstMaskValue/), &
                                 regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                                 polemethod=polemethod, &
                                 srcTermProcessing=srcTermProcessing_Value, &
                                 factorList=factorList, &
                                 ignoreDegenerate=.true., &
                                 unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                         else if (mapindex == mapconsf) then
                            call ESMF_FieldRegridStore(fldsrc, flddst, &
                                 routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                 srcMaskValues=(/srcMaskValue/), &
                                 dstMaskValues=(/dstMaskValue/), &
                                 regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                                 normType=ESMF_NORMTYPE_FRACAREA, &
                                 srcTermProcessing=srcTermProcessing_Value, &
                                 factorList=factorList, &
                                 ignoreDegenerate=.true., &
                                 unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                         else if (mapindex == mapconsd) then
                            call ESMF_FieldRegridStore(fldsrc, flddst, &
                                 routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                 srcMaskValues=(/srcMaskValue/), &
                                 dstMaskValues=(/dstMaskValue/), &
                                 regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                                 normType=ESMF_NORMTYPE_DSTAREA, &
                                 srcTermProcessing=srcTermProcessing_Value, &
                                 factorList=factorList, &
                                 ignoreDegenerate=.true., &
                                 unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                         else if (mapindex == mappatch) then
                            call ESMF_FieldRegridStore(fldsrc, flddst, &
                                 routehandle=is_local%wrap%RH(n1,n2,mapindex), &
                                 srcMaskValues=(/srcMaskValue/), &
                                 dstMaskValues=(/dstMaskValue/), &
                                 regridmethod=ESMF_REGRIDMETHOD_PATCH, &
                                 polemethod=polemethod, &
                                 srcTermProcessing=srcTermProcessing_Value, &
                                 factorList=factorList, &
                                 ignoreDegenerate=.true., &
                                 unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
                         end if
                         if (rhprint_flag) then
                            call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_consf.nc", rc)
                            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                            call ESMF_RouteHandlePrint(is_local%wrap%RH(n1,n2,mapindex), rc=rc)
                            if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                         endif
                         if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                      end if
                      ! Check that a valid route handle has been created
                      if (ESMF_RouteHandleIsCreated(is_local%wrap%RH(n1,n2,mapindex), rc=rc)) then
                         call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH "//trim(mapname), &
                              ESMF_LOGMSG_INFO, rc=dbrc)
                      else
                         call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH "//trim(mapname), &
                              ESMF_LOGMSG_INFO, rc=dbrc)
                      endif
                   end if
                end do ! loop over fields

             elseif (is_local%wrap%comp_present(n1) .and. is_local%wrap%comp_present(n2)) then
                
                ! If coupling is not active between n1 and n2 - but the two components are present
                
                call NUOPC_CompAttributeGet(gcomp, name=trim(rhname)//"_fmapname", value=mapfile, rc=rc)
                if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
                   call ESMF_LogWrite(trim(rhname)//"_fmapname = "//trim(mapfile), ESMF_LOGMSG_INFO)
                   
                   call shr_nuopc_methods_RH_init(&
                        FBsrc=is_local%wrap%FBfrac(n1), &
                        FBdst=is_local%wrap%FBfrac(n2), &
                        consfmap=is_local%wrap%RH(n1,n2,mapconsf), &
                        srcMaskValue=srcMaskValue, &
                        dstMaskValue=dstMaskValue, &
                        string=trim(rhname)//'_weights_for_fraction', &
                        consffn=trim(mapfile), &
                        spvalfn=med_constants_spval_rhfile, &
                        mastertask=mastertask, &
                        rc=rc)
                   if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                endif
             end if ! if coupling is active between n1 and n2
          end if ! if n1 not equal to n2

       end do ! loop over n2
    end do ! loop over n1


    if (mastertask) call shr_sys_flush(llogunit)

  end subroutine med_map_RouteHandles_init

  !================================================================================

  subroutine med_map_MapNorm_init(gcomp, llogunit, rc)

    !---------------------------------------
    ! Initialize unity normalization field bundle
    ! and do the mapping for unity normalization up front
    !---------------------------------------

    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: llogunit
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)         :: is_local
    type(ESMF_FieldBundle)      :: FBTmp
    integer                     :: n1, n2, m
    character(len=SHR_KIND_CS)  :: normname
    real(ESMF_KIND_R8), pointer :: dataptr(:)
    character(len=*),parameter :: subname='(module_MED_MAP:MapNorm_init)'
    !-----------------------------------------------------------

    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
       call ESMF_LogFlush()
    endif

    rc = ESMF_SUCCESS

    normname = 'one'
    do n1 = 1,ncomps
       do n2 = 1,ncomps
          if (n1 /= n2 .and. is_local%wrap%med_coupling_active(n1,n2)) then
             do m = 1,nmappers
                if (ESMF_RouteHandleIsCreated(is_local%wrap%RH(n1,n2,m), rc=rc)) then
                   call shr_nuopc_methods_FB_init(FBout=is_local%wrap%FBNormOne(n1,n2,m), &
                        FBgeom=is_local%wrap%FBImp(n1,n2), fieldNameList=(/trim(normname)/), rc=rc)
                   if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

                   call shr_nuopc_methods_FB_reset(is_local%wrap%FBNormOne(n1,n2,m), value=czero, rc=rc)
                   if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

                   call shr_nuopc_methods_FB_init( FBout=FBTmp, &
                        STgeom=is_local%wrap%NStateImp(n1), fieldNameList=(/trim(normname)/), rc=rc)
                   if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

                   call shr_nuopc_methods_FB_GetFldPtr(FBTmp, trim(normname), fldptr1=dataPtr, rc=rc)
                   if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                   dataptr(:) = 1.0_ESMF_KIND_R8

                   call shr_nuopc_methods_FB_FieldRegrid(&
                        FBTmp                           , normname, &
                        is_local%wrap%FBNormOne(n1,n2,m), normname, &
                        is_local%wrap%RH(n1,n2,m), rc)
                   if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

                   call shr_nuopc_methods_FB_clean(FBTmp, rc=rc)
                   if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
                end if
             end do
          end if
       end do
    end do

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_map_MapNorm_init

  !================================================================================

  subroutine med_map_FB_Regrid_Norm(fldsSrc, destcomp, &
       FBSrc, FBDst, FBFrac, FBNormOne, RouteHandles, string, rc)

    ! ----------------------------------------------
    ! Map field bundles with appropriate fraction weighting
    ! ----------------------------------------------

    type(shr_nuopc_fldList_src_entry_type) , pointer       :: fldsSrc(:)
    integer                                , intent(in)    :: destcomp
    type(ESMF_FieldBundle)                 , intent(inout) :: FBSrc
    type(ESMF_FieldBundle)                 , intent(inout) :: FBDst
    type(ESMF_FieldBundle)                 , intent(in)    :: FBFrac
    type(ESMF_FieldBundle)                 , intent(in)    :: FBNormOne(:)
    type(ESMF_RouteHandle)                 , intent(inout) :: RouteHandles(:)
    character(len=*), optional             , intent(in)    :: string
    integer                                , intent(out)   :: rc

    ! local variables
    integer                     :: i, n
    type(ESMF_FieldBundle)      :: FBSrcTmp        ! temporary
    type(ESMF_FieldBundle)      :: FBNormSrc       ! temporary
    type(ESMF_FieldBundle)      :: FBNormDst       ! temporary
    integer                     :: mapindex
    character(len=SHR_KIND_CS)  :: lstring
    character(len=SHR_KIND_CS)  :: mapnorm
    character(len=SHR_KIND_CS)  :: fldname
    real(ESMF_KIND_R8), pointer :: data_srctmp(:)  ! temporary
    real(ESMF_KIND_R8), pointer :: data_src(:)     ! temporary
    real(ESMF_KIND_R8), pointer :: data_dst(:)     ! temporary
    real(ESMF_KIND_R8), pointer :: data_srcnorm(:) ! temporary
    real(ESMF_KIND_R8), pointer :: data_dstnorm(:) ! temporary
    real(ESMF_KIND_R8), pointer :: data_frac(:)    ! temporary
    real(ESMF_KIND_R8), pointer :: data_norm(:)    ! temporary
    character(len=*), parameter :: subname='(module_MED_Map:med_map_Regrid_Norm)'
    !---------------------------------------

    if (present(string)) then
      lstring = trim(string)
    else
      lstring = " "
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! Loop over all fields in the field bundle
    !---------------------------------------
    do n = 1,size(fldsSrc)

       fldname  = fldsSrc(n)%shortname
       mapindex = fldsSrc(n)%mapindex(destcomp)
       mapnorm  = fldsSrc(n)%mapnorm(destcomp)

       ! Error checks
       if (.not. shr_nuopc_methods_FB_FldChk(FBSrc, fldname, rc=rc)) then
          if (dbug_flag > 1) then
             call ESMF_LogWrite(trim(subname)//" field not found in FB: "//trim(fldname), ESMF_LOGMSG_INFO, rc=dbrc)
          endif
       else if (.not. ESMF_RouteHandleIsCreated(RouteHandles(mapindex), rc=rc)) then
          call ESMF_LogWrite(trim(subname)//trim(lstring)//&
               ": ERROR RH not available for "//mapnames(mapindex)//": fld="//trim(fldname), &
               ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
          rc = ESMF_FAILURE
       end if

       ! Do the mapping
       if (mapindex == mapfcopy) then

          call shr_nuopc_methods_FB_FieldRegrid(FBSrc, fldname, FBDst, fldname, RouteHandles(mapindex), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       else

          ! Create a new temporary field bundle if needed
          if (.not. ESMF_FieldBundleIsCreated(FBSrcTmp)) then
             call shr_nuopc_methods_FB_init(FBSrcTmp, FBgeom=FBSrc, fieldNameList=(/trim(fldname)/), rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          end if

          ! Get pointer to source field data in both FBSrc and FBSRcTmp
          call shr_nuopc_methods_FB_GetFldPtr(FBSrc, fldname, data_src, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call shr_nuopc_methods_FB_GetFldPtr(FBSrcTmp, fldname, data_srctmp, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if ( trim(mapnorm) /= 'unset' .and. trim(mapnorm) /= 'one') then

             !-------------------------------------------------
             ! fractional normalization
             !-------------------------------------------------
             ! create a temporary field bundle that will contain the mapped normalization factor

             call shr_nuopc_methods_FB_init(FBout=FBNormSrc, FBgeom=FBSrc, fieldNameList=(/trim(mapnorm)/), rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
             call shr_nuopc_methods_FB_init(FBout=FBNormDst, FBgeom=FBDst, fieldNameList=(/trim(mapnorm)/), rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

             call shr_nuopc_methods_FB_reset(FBNormSrc, value=czero, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
             call shr_nuopc_methods_FB_reset(FBNormDst, value=czero, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

             call shr_nuopc_methods_FB_GetFldPtr(FBNormSrc, trim(mapnorm), data_srcnorm, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             ! error checks
             if (size(data_srcnorm) /= size(data_frac)) then
                call ESMF_LogWrite(trim(subname)//": ERROR data_normsrc size and data_frac size are inconsistent", &
                     ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
                rc = ESMF_FAILURE
                return
             else if (size(data_srcnorm) /= size(data_srctmp)) then
                call ESMF_LogWrite(trim(subname)//": ERROR data_normsrc size and data_srctmp size are inconsistent", &
                     ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
                rc = ESMF_FAILURE
                return
             end if

             ! get a pointer to the FBFrac array based on the mapnorm name
             call shr_nuopc_methods_FB_GetFldPtr(FBFrac, trim(mapnorm), data_frac, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             ! now fill in the values for data_srcnorm and data_srctmp - these are the two arrays needed for normalization
             ! Note that FBsrcTmp will now have the data_srctmp value
             do i = 1,size(data_frac)
                data_srcnorm(i) = data_frac(i)
                data_srctmp(i)  = data_src(i) * data_frac(i)  ! Multiply initial field by data_frac
             end do

             ! regrid FBSrcTmp to FBDst

             if (trim(fldname) == trim(flds_scalar_name)) then
                if (dbug_flag > 1) then
                   call ESMF_LogWrite(trim(subname)//trim(lstring)//": skip : fld="//trim(fldname), &
                        ESMF_LOGMSG_INFO, rc=dbrc)
                endif
             else
                call shr_nuopc_methods_FB_FieldRegrid(FBSrcTmp, fldname, FBDst, fldname, RouteHandles(mapindex), rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
             end if

             ! regrid FBNormSrc to from the source to the desination grid (FBNormDst)

             call shr_nuopc_methods_FB_reset(FBNormSrc, value=czero, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
             call shr_nuopc_methods_FB_FieldRegrid(FBNormSrc, mapnorm, FBNormDst, mapnorm, RouteHandles(mapindex), rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             ! multiply interpolated field (FBDst) by reciprocal of fraction on destination grid (FBNormDst)

             call shr_nuopc_methods_FB_GetFldPtr(FBNormDst, trim(mapnorm), data_dstnorm, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
             call shr_nuopc_methods_FB_GetFldPtr(FBDst, trim(fldname), data_dst, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             do i= 1,size(data_dst)
                if (data_dstnorm(i) == 0.0_ESMF_KIND_R8) then
                   data_dst(i) = 0.0_ESMF_KIND_R8
                else
                   data_dst(i) = data_dst(i)/data_dstnorm(i)
                endif
             end do

          else if (trim(mapnorm) == 'one' .or. trim(mapnorm) == 'none') then

             !-------------------------------------------------
             ! unity or no normalization
             !-------------------------------------------------

             ! map source field to destination grid
             mapindex = fldsSrc(n)%mapindex(destcomp)
             call shr_nuopc_methods_FB_FieldRegrid(FBSrc, fldname, FBDst, fldname, RouteHandles(mapindex), rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             ! obtain unity normalization factor and multiply interpolated field by reciprocal of normalization factor
             if (trim(mapnorm) == 'one') then
                call shr_nuopc_methods_FB_GetFldPtr(FBNormOne(mapindex), 'one', data_norm, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

                call shr_nuopc_methods_FB_GetFldPtr(FBDst, trim(fldname), data_dst, rc=rc)
                if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
                do i= 1,size(data_dst)
                   if (data_norm(i) == 0.0_ESMF_KIND_R8) then
                      data_dst(i) = 0.0_ESMF_KIND_R8
                   else
                      data_dst(i) = data_dst(i)/data_norm(i)
                   endif
                enddo
             end if ! mapnorm is 'one'

          end if ! mapnorm is 'one' or 'nne'
       end if ! mapindex is not mapfcopy and field exists
    end do  ! loop over fields

    ! Clean up temporary field bundle
    if (ESMF_FieldBundleIsCreated(FBSrcTmp)) then
       call shr_nuopc_methods_FB_clean(FBSrcTmp, rc=rc)
       if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
    end if

  end subroutine med_map_FB_Regrid_Norm

end module med_map_mod
