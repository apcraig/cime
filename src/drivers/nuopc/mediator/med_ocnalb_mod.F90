module med_ocnalb_mod

  use shr_kind_mod          , only: r8=>shr_kind_r8, in=>shr_kind_in
  use shr_kind_mod          , only: cs=>shr_kind_cs, cl=>shr_kind_cl
  use shr_sys_mod           , only: shr_sys_abort
  use shr_orb_mod           , only: shr_orb_cosz, shr_orb_decl
  use shr_const_mod         , only: SHR_CONST_PI, shr_const_spval
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_FB_getFieldN
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_FB_GetFldPtr
  use shr_nuopc_methods_mod , only: shr_nuopc_methods_ChkErr
  use seq_timemgr_mod       , only: seq_timemgr_EclockGetData
  use med_constants_mod     , only: med_constants_dbug_flag
  use ESMF
  use NUOPC

  implicit none
  private

  !--------------------------------------------------------------------------
  ! Public interfaces
  !--------------------------------------------------------------------------

  public med_ocnalb_init
  public med_ocnalb

  !--------------------------------------------------------------------------
  ! Private data
  !--------------------------------------------------------------------------

  real(r8) , pointer :: lats  (:) ! latitudes  (degrees)
  real(r8) , pointer :: lons  (:) ! longitudes (degrees)
  integer  , pointer :: mask  (:) ! ocn domain mask: 0 <=> inactive cell
  real(r8) , pointer :: anidr (:) ! albedo: near infrared, direct
  real(r8) , pointer :: avsdr (:) ! albedo: visible      , direct
  real(r8) , pointer :: anidf (:) ! albedo: near infrared, diffuse
  real(r8) , pointer :: avsdf (:) ! albedo: visible      , diffuse
  real(r8) , pointer :: swndr (:) ! direct near-infrared  incident solar radiation
  real(r8) , pointer :: swndf (:) ! diffuse near-infrared incident solar radiation
  real(r8) , pointer :: swvdr (:) ! direct visible  incident solar radiation
  real(r8) , pointer :: swvdf (:) ! diffuse visible incident solar radiation

  ! Conversion from degrees to radians
  integer                :: dbug_flag = med_constants_dbug_flag
  integer                :: dbrc
  character(len=1024)    :: tmpstr
  real(r8)    ,parameter :: const_pi      = SHR_CONST_PI       ! pi
  real(r8)    ,parameter :: const_deg2rad = const_pi/180.0_r8  ! deg to rads
  character(*),parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine med_ocnalb_init(gcomp, FBAtm_o, FBXao_ocnalb_o, rc)
    !-----------------------------------------------------------------------
    ! Initialize pointers to the module variables and then use the module
    ! variables in the med_ocnalb_run phase
    !-----------------------------------------------------------------------

    ! Arguments
    type(ESMF_GridComp)    :: gcomp
    type(ESMF_FieldBundle) :: FBAtm_o        ! Atm Import fields on atmocn flux grid
    type(ESMF_FieldBundle) :: FBXao_ocnalb_o ! Ocean albedos computed in mediator on ocean grid
    integer, intent(out)   :: rc
    !
    ! Local variables
    type(ESMF_VM)               :: vm
    integer(in)                 :: iam
    type(ESMF_Field)            :: lfield
    type(ESMF_Grid)             :: lgrid
    type(ESMF_Mesh)             :: lmesh
    type(ESMF_GeomType_Flag)    :: geomtype
    integer                     :: n
    integer                     :: lsize
    real(r8), pointer           :: rmask(:)  ! ocn domain mask
    real(r8), pointer           :: ofrac(:)
    real(r8), pointer           :: ifrac(:)
    integer                     :: dimCount
    integer                     :: spatialDim
    integer                     :: numOwnedElements
    real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)
    character(*),parameter   :: subName =   '(med_ocnalb_init) '
    !-----------------------------------------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! The following is for debugging
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=iam, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !----------------------------------
    ! fields needed for albedo calculations - must be on the ocean grid
    !----------------------------------

    call shr_nuopc_methods_FB_GetFldPtr(FBAtm_o, fldname='Faxa_swndr', fldptr1=swndr, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(FBAtm_o, fldname='Faxa_swndf', fldptr1=swndf, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(FBAtm_o, fldname='Faxa_swvdr', fldptr1=swvdr, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(FBAtm_o, fldname='Faxa_swvdf', fldptr1=swvdf, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_GetFldPtr(FBXao_ocnalb_o, fldname='So_avsdr', fldptr1=avsdr, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(FBXao_ocnalb_o, fldname='So_avsdf', fldptr1=avsdf, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(FBXao_ocnalb_o, fldname='So_anidr', fldptr1=anidr, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(FBAXao_ocnalb_o, fldname='So_anidf', fldptr1=anidf, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !----------------------------------
    ! Get lat, lon, which are time-invariant
    !----------------------------------

    ! The following assumes that all fields in FBXao_ocnalb_o have the same grid - so
    ! only need to query field 1
    call shr_nuopc_methods_FB_getFieldN(FBXao_ocnalb_o, fieldnum=1, field=lfield, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine if first field is on a grid or a mesh - default will be mesh
    call ESMF_FieldGet(lfield, geomtype=geomtype, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (geomtype == ESMF_GEOMTYPE_MESH) then
       call ESMF_LogWrite(trim(subname)//" : FBATM_o is on a mesh ", ESMF_LOGMSG_INFO, rc=rc)
       call ESMF_FieldGet(lfield, mesh=lmesh, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_MeshGet(lmesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       if (numOwnedElements /= lsize) then
          call ESMF_LogWrite(trim(subname)//": ERROR numOwnedElements not equal to local size", &
               ESMF_LOGMSG_INFO, rc=rc)
          rc = ESMF_FAILURE
          return
       end if
       allocate(ownedElemCoords(spatialDim*numOwnedElements))
       allocate(lons(numOwnedElements))
       allocate(lats(numOwnedElements))
       call ESMF_MeshGet(lmesh, ownedElemCoords=ownedElemCoords)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       do n = 1,lsize
          lons(n) = ownedElemCoords(2*n-1)
          lats(n) = ownedElemCoords(2*n)
       end do
    else if (geomtype == ESMF_GEOMTYPE_GRID) then
       call ESMF_LogWrite(trim(subname)//" : FBATM_o is on a grid ", ESMF_LOGMSG_INFO, rc=rc)
       call ESMF_FieldGet(lfield, grid=lgrid, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_GridGet(lgrid, dimCount=dimCount, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_GridGetCoord(lgrid, coordDim=1, farrayPtr=lons, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_GridGetCoord(lgrid, coordDim=2, farrayPtr=lats, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call ESMF_LogWrite(trim(subname)//": ERROR FBATM must be either on a grid or a mesh", &
            ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    end if

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_ocnalb_init

  !===============================================================================

  subroutine med_ocnalb_run(gcomp, FBFrac_o, nextsw_cday, rc)

    !-----------------------------------------------------------------------
    ! Update albedoes on ocean grid
    !-----------------------------------------------------------------------
    !
    ! Arguments
    type(ESMF_GridComp)    :: gcomp
    type(ESMF_FieldBundle) :: FBFrac_o     ! Fraction data on ocean grid
    real(r8), intent(in)   :: nextsw_cday  ! calendar day of next atm shortwave
    integer , intent(out)  :: rc
    !
    ! Local variables
    type(ESMF_VM)     :: vm
    integer(in)       :: iam
    character(CL)     :: cvalue
    real(r8), pointer :: ofrac_o(:)
    real(r8), pointer :: ofrad_o(:)
    real(r8), pointer :: ifrac_o(:)
    real(r8), pointer :: ifrad_o(:)
    integer(in)       :: lsize      ! local size
    logical	      :: flux_albav ! flux avg option
    logical	      :: update_alb ! was albedo updated
    integer(in)	      :: n,i	    ! indices
    real(r8)	      :: rlat	    ! gridcell latitude in radians
    real(r8)	      :: rlon	    ! gridcell longitude in radians
    real(r8)	      :: cosz	    ! Cosine of solar zenith angle
    real(r8)	      :: eccen	    ! Earth orbit eccentricity
    real(r8)	      :: mvelpp	    ! Earth orbit
    real(r8)	      :: lambm0	    ! Earth orbit
    real(r8)	      :: obliqr	    ! Earth orbit
    real(r8)	      :: delta	    ! Solar declination angle  in radians
    real(r8)	      :: eccf	    ! Earth orbit eccentricity factor
    real(r8)    , parameter :: albdif = 0.06_r8 ! 60 deg reference albedo, diffuse
    real(r8)    , parameter :: albdir = 0.07_r8 ! 60 deg reference albedo, direct
    character(*), parameter :: subName =   '(med_ocnalb) '
    !-----------------------------------------------------------------------

    call NUOPC_CompAttributeGet(gcomp, name='flux_albav', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) flux_albav

    call NUOPC_CompAttributeGet(gcomp, name='orb_eccen', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) eccen

    call NUOPC_CompAttributeGet(gcomp, name='orb_obliqr', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) obliqr

    call NUOPC_CompAttributeGet(gcomp, name='orb_lambm0', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) lambm0

    call NUOPC_CompAttributeGet(gcomp, name='orb_mvelpp', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) mvelpp

    ! The following is for debugging
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=iam, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine indices

    update_alb = .false.
    lsize = size(anidr)

    if (flux_albav) then

       do n = 1,lsize
          anidr(n) = albdir
          avsdr(n) = albdir
          anidf(n) = albdif
          avsdf(n) = albdif

          ! Albedo is now function of latitude (will be new implementation)
          !rlat = const_deg2rad * lats(n)
          !anidr = 0.069_r8 - 0.011_r8 * cos(2._r8 * rlat)
          !avsdr = anidr
          !anidf = anidr
          !avsdf = anidr
       end do
       update_alb = .true.

    else

       ! need swdn & swup = swdn*(-albedo)
       ! swdn & albedos are time-aligned  BEFORE albedos get updated below ---

       do n=1,lsize
          if ( anidr(n) == 1.0_r8 ) then ! dark side of earth
             swup(n) = 0.0_r8
             swdn(n) = 0.0_r8
          else
             swup(n) = swndr(n) * (-anidr(n)) + swndf(n) * (-anidf(n)) + &
                       swvdr(n) * (-avsdr(n)) + swvdf(n) * (-avsdf(n))
             swdn(n) = swndr(n) + swndf(n) + swvdr(n) + swvdf(n)
          end if
       end do

       ! Solar declination
       ! Will only do albedo calculation if nextsw_cday is not -1.
       if (nextsw_cday >= -0.5_r8) then

          call shr_orb_decl(nextsw_cday, eccen, mvelpp,lambm0, obliqr, delta, eccf)

          write(6,*)'DEBUG: ocnalb nextsw_cday = ',nextsw_cday
          ! Compute albedos
          do n = 1,lsize
             rlat = const_deg2rad * lats(n)
             rlon = const_deg2rad * lons(n)
             cosz = shr_orb_cosz( nextsw_cday, rlat, rlon, delta )
             if (cosz  >  0.0_r8) then !--- sun hit --
                anidr(n) = (.026_r8/(cosz**1.7_r8 + 0.065_r8)) +   &
                           (.150_r8*(cosz         - 0.100_r8 ) *   &
                           (cosz - 0.500_r8 ) * (cosz - 1.000_r8 )  )
                avsdr(n) = anidr(n)
                anidf(n) = albdif
                avsdf(n) = albdif
             else !--- dark side of earth ---
                anidr(n) = 1.0_r8
                avsdr(n) = 1.0_r8
                anidf(n) = 1.0_r8
                avsdf(n) = 1.0_r8
             end if
          end do
          update_alb = .true.
       endif    ! nextsw_cday

    end if   ! flux_albav

    !--- update current ifrad/ofrad values if albedo was updated
    if (update_alb) then
       call shr_nuopc_methods_FB_getFldPtr(FBFrac_o, 'ifrac', fldptr1=ifrac_o, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_getFldPtr(FBFrac_o, 'ifrad', fldptr1=ifrad_o, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       ifrad_o(:) = ifrac_o(:)

       call shr_nuopc_methods_FB_getFldPtr(FBFrac_o, fldname='ofrac', fldptr1=ofrac_o, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_getFldPtr(FBFrac_o, fldname='ofrad', fldptr1=ofrad_o, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       ofrad_o(:) = ofrac_o(:)
    endif

  end subroutine med_ocnalb_run

end module med_ocnalb_mod
