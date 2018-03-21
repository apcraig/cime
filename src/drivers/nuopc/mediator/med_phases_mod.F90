module med_phases_mod

  !-----------------------------------------------------------------------------
  ! Mediator Phases
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use shr_kind_mod            , only : CL=>SHR_KIND_CL
  use esmFlds                 , only : compmed, compatm, complnd, compocn
  use esmFlds                 , only : compice, comprof, compwav, compglc
  use esmFlds                 , only : ncomps, compname 
  use esmFlds                 , only : flds_scalar_name
  use esmFlds                 , only : fldListFr, fldListTo
  use esmFlds                 , only : fldListMed_aoflux_a
  use esmFlds                 , only : fldListMed_aoflux_o
  use esmFlds                 , only : fldListMed_ocnalb_o
  use shr_nuopc_fldList_mod   , only : shr_nuopc_fldList_GetFldNames
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_init
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_reset
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_clean
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_diagnose
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_GetFldPtr
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_accum
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_FldChk
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_average
  use shr_nuopc_methods_mod   , only : shr_nuopc_methods_FB_copy
  use med_fraction_mod        , only : med_fraction_init
  use med_constants_mod       , only : med_constants_dbug_flag
  use med_constants_mod       , only : med_constants_czero
  use med_merge_mod           , only : med_merge_auto
  use med_phases_ocnalb_mod   , only : med_phases_ocnalb_init
  use med_phases_ocnalb_mod   , only : med_phases_ocnalb_mapo2a
  use med_phases_aofluxes_mod , only : med_phases_aofluxes_init
  use med_map_mod             , only : med_map_FB_Regrid_Norm 
  use med_internalstate_mod   , only : InternalState

  implicit none
  private

  integer           , parameter :: dbug_flag = med_constants_dbug_flag
  real(ESMF_KIND_R8), parameter :: czero     = med_constants_czero
  character(*)      , parameter :: u_FILE_u  = __FILE__
  integer                       :: dbrc
  logical                       :: mastertask

  public  :: med_phases_init
  public  :: med_phases_prep_atm
  public  :: med_phases_prep_ocn
  public  :: med_phases_prep_ice
  public  :: med_phases_prep_lnd
  public  :: med_phases_prep_rof
  public  :: med_phases_prep_wav
  public  :: med_phases_prep_glc
  public  :: med_phases_accum_fast

!-----------------------------------------------------------------------------
  contains
!-----------------------------------------------------------------------------

  subroutine med_phases_init(gcomp, llogunit, rc)

    !----------------------------------------------------------
    ! Initialize field bundles, etc. that are needed as part of
    ! the med_phases routines
    !----------------------------------------------------------

    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: llogunit
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)    :: is_local
    type(ESMF_VM)          :: vm
    integer                :: localPet
    integer                :: n, n1, n2, ncomp, nflds
    character(CL), pointer :: fldnames(:)
    !-----------------------------------------------------------

    if (dbug_flag > 1) then
       call ESMF_LogWrite("Starting to initialize mediator phases", ESMF_LOGMSG_INFO)
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

    !----------------------------------------------------------
    ! Create FBfrac field bundles and initialize fractions
    !----------------------------------------------------------
    
    call med_fraction_init(gcomp,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    ! Initialize ocn albedo and ocn/atm flux calculation modules if appropriate
    !---------------------------------------

    if (is_local%wrap%med_coupling_active(compocn,compatm) .and. &
        is_local%wrap%med_coupling_active(compatm,compocn)) then

       ! NOTE: the NStateImp(compocn) or NStateImp(compatm) used below
       ! rather than NStateExp(n2), since the export state might only
       ! contain control data and no grid information if if the target
       ! component (n2) is not prognostic only receives control data back

       ! Create field bundles for ocean albedo computation

       nflds = size(fldListMed_ocnalb_o%flds)
       allocate(fldnames(nflds))
       call shr_nuopc_fldList_getfldnames(fldListMed_ocnalb_o%flds, fldnames)

       call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_ocnalb_a, flds_scalar_name, &
            STgeom=is_local%wrap%NStateImp(compatm), fieldnamelist=fldnames, name='FBMed_ocnalb_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_ocnalb_o, flds_scalar_name, &
            STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=fldnames, name='FBMed_ocnalb_o', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       deallocate(fldnames)

       ! Create field bundles for ocean/atmosphere flux computation

       nflds = size(fldListMed_aoflux_o%flds)
       allocate(fldnames(nflds))
       call shr_nuopc_fldList_getfldnames(fldListMed_aoflux_a%flds, fldnames)

       call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_aoflux_a, flds_scalar_name, &
            STgeom=is_local%wrap%NStateImp(compatm), fieldnamelist=fldnames, name='FBMed_aoflux_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_aoflux_o, flds_scalar_name, &
            STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=fldnames, name='FBMed_aoflux_o', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       deallocate(fldnames)

       ! Initialize the atm/ocean fluxes and compute the ocean albedos

       call ESMF_LogWrite("MED - initialize atm/ocn fluxes and compute ocean albedo", ESMF_LOGMSG_INFO, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize ocean albedo module and compute ocean albedos

       call med_phases_ocnalb_init(gcomp, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize atm/ocn fluxes module

       call med_phases_aofluxes_init(gcomp, rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    !----------------------------------------------------------
    ! Create mediator specific field bundles needed in phases routines
    !----------------------------------------------------------

    ! FBs for lnd <-> glc accumulation and elevation class downscaling
    if (is_local%wrap%comp_present(complnd) .and. is_local%wrap%comp_present(compglc)) then
       ! call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_l2x_to_glc_accum, &
       !      STgeom=is_local%wrap%NStateImp(complnd), fieldnamelist=flds_l2x_to_glc, name='FBMed_l2g_l_accum', rc=rc)
       ! if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_g2x_to_lnd, &
       !      STgeom=is_local%wrap%NStateImp(complnd), fieldnamelist=flds_g2x_to_lnd, name='FBMed_g2x_to_lnd', rc=rc)
       ! if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine med_phases_init

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_atm(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the ATM import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    real(ESMF_KIND_R8), pointer :: ifrac_i(:)   ! ice fraction on ice grid
    real(ESMF_KIND_R8), pointer :: ifrac_a(:)   ! ice fraction on atm grid
    real(ESMF_KIND_R8), pointer :: ifrac_ar(:)  ! 1./ifrac_a
    real(ESMF_KIND_R8), pointer :: ocnwgt(:),icewgt(:),customwgt(:)
    integer                     :: mapindex
    integer                     :: i, j, n, n1, ncnt, lsize, is, ie
    type(ESMF_FieldBundle)      :: FBtmp1
    logical                     :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_atm)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(compatm), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(compatm), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Map to create import field bundle on atm grid - FBimp(:,compatm)
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,compatm)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,compatm), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, compatm, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,compatm), &
               is_local%wrap%FBFrac(n1), &
               is_local%wrap%FBNormOne(n1,compatm,:), &
               is_local%wrap%RH(n1,compatm,:), &
               string=trim(compname(n1))//'2'//trim(compname(compatm)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,compatm), &
                  string=trim(subname)//' FBImp('//trim(compname(n1))//','//trim(compname(compatm))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- map FBMed_ocnalb_o to atm grid
    !---------------------------------------

    if (is_local%wrap%med_coupling_active(compocn,compatm)) then
       call med_phases_ocnalb_mapo2a(gcomp, rc)
    end if

    !---------------------------------------
    !--- map FBMed_aoflux_o to atm grid
    !---------------------------------------

    if (is_local%wrap%med_coupling_active(compocn,compatm)) then
       call shr_nuopc_methods_FB_reset(is_local%wrap%FBMed_aoflux_a, value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call med_map_FB_Regrid_Norm(&
            fldListMed_aoflux_o%flds, compatm, &
            is_local%wrap%FBMed_aoflux_o, &
            is_local%wrap%FBMed_aoflux_a, &
            is_local%wrap%FBFrac(compatm), &
            is_local%wrap%FBNormOne(compocn,compatm,:), &
            is_local%wrap%RH(compocn,compatm,:), &
            string='FBMed_aoflux_o_To_FBMEd_aoflux_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBMed_aoflux_a, string=trim(subname)//' FBMed_aoflux_a ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       endif
    endif

    !---------------------------------------
    !--- auto merges
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(compatm), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call med_merge_auto(is_local%wrap%FBExp(compatm), &
         FB1=is_local%wrap%FBImp(compocn,compatm), FB1w=is_local%wrap%FBfrac(compatm), fldw1='ofrac', &
         FB2=is_local%wrap%FBMed_ocnalb_a        , FB2w=is_local%wrap%FBfrac(compatm), fldw2='ofrac', &
         FB3=is_local%wrap%FBMed_aoflux_a        , FB3w=is_local%wrap%FBfrac(compatm), fldw3='ofrac', &
         FB4=is_local%wrap%FBImp(compice,compatm), FB4w=is_local%wrap%FBfrac(compatm), fldw4='ifrac', &
         FB5=is_local%wrap%FBImp(complnd,compatm), FB5w=is_local%wrap%FBfrac(compatm), fldw5='lfrac', &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBExp(compatm), string=trim(subname)//' FBexp(compatm) ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    !---------------------------------------
    !--- set fractions to send back to atm
    !---------------------------------------

    if (shr_nuopc_methods_FB_FldChk(is_local%wrap%FBExp(compatm), 'So_ofrac', rc=rc)) then
       call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBExp(compatm), 'So_ofrac', dataptr1, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBFrac(compatm), 'ofrac', dataptr2, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       lsize = size(dataptr1)
       do n = 1,lsize
          dataptr1(n) = dataptr2(n)
       end do
    end if
    if (shr_nuopc_methods_FB_FldChk(is_local%wrap%FBExp(compatm), 'Si_ifrac', rc=rc)) then
       call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBExp(compatm), 'Si_ifrac', dataptr1, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBFrac(compatm), 'ifrac', dataptr2, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       lsize = size(dataptr1)
       do n = 1,lsize
          dataptr1(n) = dataptr2(n)
       end do
    end if
    if (shr_nuopc_methods_FB_FldChk(is_local%wrap%FBExp(compatm), 'Sl_lfrac', rc=rc)) then
       call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBExp(compatm), 'Sl_lfrac', dataptr1, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBFrac(compatm), 'lfrac', dataptr2, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       lsize = size(dataptr1)
       do n = 1,lsize
          dataptr1(n) = dataptr2(n)
       end do
    end if

    !---------------------------------------
    !--- custom calculations
    !---------------------------------------
#if (1 == 0)
    !---  ocn and ice fraction for merges
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compice,compatm), 'Si_ifrac', icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(ocnwgt(lbound(icewgt,1):ubound(icewgt,1), lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
       do i=lbound(icewgt,1),ubound(icewgt,1)
          !TODO: the sizes here are inconsistent with the declarations
          ocnwgt(i,j) = 1.0_ESMF_KIND_R8 - icewgt(i,j)
       enddo
    enddo

    !--- merges
    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBExp(compatm)  ,'surface_temperature' ,         &
         FBinA=is_local%wrap%FBImp(compocn,compatm) ,fnameA='sea_surface_temperature', wgtA=ocnwgt,     &
         FBinB=is_local%wrap%FBImp(compice,compatm) ,fnameB='sea_ice_temperature'    , wgtB=icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    deallocate(ocnwgt)
#endif

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_atm

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_ice(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the ICE import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:), dataPtr2(:), dataPtr3(:), dataPtr4(:)
    integer                     :: i,n,n1,ncnt
    logical,save                :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_ice)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(compice), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(compice), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- map to create FBimp(:,compice)
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,compice)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,compice), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, compice, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,compice), &
               is_local%wrap%FBFrac(compice), &
               is_local%wrap%FBNormOne(n1,compice,:), &
               is_local%wrap%RH(n1,compice,:), &
               string=trim(compname(n1))//'2'//trim(compname(compice)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,compice), &
                  string=trim(subname)//' FBImp('//trim(compname(n1))//','//trim(compname(compice))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- auto merges
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(compice), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call med_merge_auto(is_local%wrap%FBExp(compice), &
         FB1=is_local%wrap%FBImp(compocn,compice), FB1w=is_local%wrap%FBfrac(compice), fldw1='ofrac', &
         FB2=is_local%wrap%FBImp(compatm,compice), FB2w=is_local%wrap%FBfrac(compice), fldw2='afrac', &
         FB3=is_local%wrap%FBImp(compglc,compice), &
         FB4=is_local%wrap%FBImp(comprof,compice), &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- custom calculations
    !---------------------------------------

    ! TODO: document custom merges below
    ! TODO: need to obtain flux_epbalfact

    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compatm,compice), 'Faxa_rainc', dataPtr1, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compatm,compice), 'Faxa_rainl', dataPtr2, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBExp(compice), 'Faxa_rain' , dataPtr3, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    dataPtr3(:) = dataPtr1(:) + dataPtr2(:)
#if (1 == 0)
    dataPtr3(:) = dataPtr3(:) * flux_epbalfact
#endif

    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compatm,compice), 'Faxa_snowc', dataPtr1, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compatm,compice), 'Faxa_snowl', dataPtr2, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBExp(compice), 'Faxa_snow' , dataPtr3, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    dataPtr3(:) = dataPtr1(:) + dataPtr2(:)
#if (1 == 0)
    dataPtr3(:) = dataPtr3(:) * flux_epbalfact
#endif

#if (1 == 0)
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compglc,compice), 'Figg_rofi', dataPtr1, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(comprof,compice), 'Firr_rofi', dataPtr2, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBExp(compice), 'Fixx_rofi', dataPtr3, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    dataPtr3(:) = dataPtr1(:) + dataPtr2(:)
    dataPtr3(:) = dataPtr3(:) * flux_epbalfact
#endif

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_ice

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_lnd(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the LND import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    integer                     :: i,j,n,n1,ncnt
    logical,save                :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_lnd)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(complnd), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(complnd), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- map to create FBimp(:,complnd)
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,complnd)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,complnd), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, complnd, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,complnd), &
               is_local%wrap%FBFrac(complnd), &
               is_local%wrap%FBNormOne(n1,complnd,:), &
               is_local%wrap%RH(n1,complnd,:), &
               string=trim(compname(n1))//'2'//trim(compname(complnd)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,complnd), &
                  string=trim(subname)//' FBImp('//trim(compname(n1))//','//trim(compname(complnd))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- auto merges
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(complnd), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call med_merge_auto(is_local%wrap%FBExp(complnd), &
         FB1=is_local%wrap%FBImp(compatm,complnd), FB1w=is_local%wrap%FBfrac(complnd), fldw1='afrac', &
         FB2=is_local%wrap%FBImp(compglc,complnd), &
         FB3=is_local%wrap%FBImp(comprof,complnd), &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- custom calculations
    !---------------------------------------

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_lnd

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_rof(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the ROF import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    integer                     :: i,j,n,n1,ncnt
    logical,save                :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_rof)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(comprof), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(comprof), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- map to create FBimp(:,comprof)
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,comprof)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,comprof), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, comprof, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,comprof), &
               is_local%wrap%FBFrac(comprof), &
               is_local%wrap%FBNormOne(n1,comprof,:), &
               is_local%wrap%RH(n1,comprof,:), &
               string=trim(compname(n1))//'2'//trim(compname(comprof)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,comprof), &
                  string=trim(subname)//' FBImp('//trim(compname(n1))//','//trim(compname(comprof))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- auto merges
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(comprof), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call med_merge_auto(is_local%wrap%FBExp(comprof), &
         FB1=is_local%wrap%FBImp(complnd,comprof), FB1w=is_local%wrap%FBfrac(comprof), fldw1='lfrac', &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- custom calculations
    !---------------------------------------

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_rof

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_wav(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the WAV import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    integer                     :: i,j,n,n1,ncnt
    logical,save                :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_wav)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(compwav), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(compwav), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- map to create FBimp(:,compwav)
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,compwav)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,compwav), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, compwav, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,compwav), &
               is_local%wrap%FBFrac(compwav), &
               is_local%wrap%FBNormOne(n1,compwav,:), &
               is_local%wrap%RH(n1,compwav,:), &
               string=trim(compname(n1))//'2'//trim(compname(compwav)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,compwav), &
                  string=trim(subname)//' FBImp('//trim(compname(n1))//','//trim(compname(compwav))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- auto merges
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(compwav), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_merge_auto(is_local%wrap%FBExp(compwav), &
         FB1=is_local%wrap%FBImp(compocn,compwav), &
         FB2=is_local%wrap%FBImp(compice,compwav), &
         FB3=is_local%wrap%FBImp(compatm,compwav), &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- custom calculations
    !---------------------------------------

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_wav

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_glc(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the GLC import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    integer                     :: i,j,n,n1,ncnt
    logical,save                :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_glc)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(compglc), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(compglc), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- mapping
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,compglc)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,compglc), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, compglc, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,compglc), &
               is_local%wrap%FBFrac(compglc), &
               is_local%wrap%FBNormOne(n1,compglc,:), &
               is_local%wrap%RH(n1,compglc,:), &
               string=trim(compname(n1))//'2'//trim(compname(compglc)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,compglc), &
                  string=trim(subname)//' FBImp('//trim(compname(n1))//','//trim(compname(compglc))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- auto merges
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(compglc), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call med_merge_auto(is_local%wrap%FBExp(compglc), &
         FB1=is_local%wrap%FBImp(complnd,compglc), FB1w=is_local%wrap%FBfrac(compglc), fldw1='lfrac', &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- custom calculations
    !---------------------------------------

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_glc

  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_ocn(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Prepares the OCN import Fields.

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_StateItem_Flag)   :: itemType
    type(InternalState)         :: is_local
    integer                     :: i,j,n,n1,ncnt
    character(ESMF_MAXSTR)      :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:)
    real(ESMF_KIND_R8), pointer :: atmwgt(:),icewgt(:),customwgt(:)
    logical                     :: checkOK, checkOK1, checkOK2
    logical,save                :: first_call = .true.
    character(len=*),parameter  :: subname='(med_phases_prep_ocn)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(compocn), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(compocn), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- average ocn accumulator
    !---------------------------------------

    if (dbug_flag > 7) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBExpAccum(compocn), &
            string=trim(subname)//' FBaccO_B4avg ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call shr_nuopc_methods_FB_average(is_local%wrap%FBExpAccum(compocn), &
         is_local%wrap%FBExpAccumCnt(compocn), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBExpAccum(compocn), &
            string=trim(subname)//' FBaccO_avg ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    !---------------------------------------
    !--- copy to FBExp(compocn)
    !---------------------------------------

    call shr_nuopc_methods_FB_copy(is_local%wrap%FBExp(compocn), &
         is_local%wrap%FBExpAccum(compocn), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- zero accumulator
    !---------------------------------------

    is_local%wrap%FBExpAccumCnt(compocn) = 0
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExpAccum(compocn), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBExpAccum(compocn), &
            string=trim(subname)//' FBaccO_AFzero ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

    !is_local%wrap%scalar_data(1) =

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_ocn

  !-----------------------------------------------------------------------------

  subroutine med_phases_accum_fast(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Carry out fast accumulation for the ocean

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(InternalState)         :: is_local
    integer                     :: i,j,n,n1,ncnt
    logical,save                :: first_call = .true.
    character(len=*), parameter :: ice_fraction_name = 'Si_ifrac'
    character(len=*), parameter :: subname='(med_phases_accum_fast)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    ! --- Get the internal state
    !---------------------------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Count the number of fields outside of scalar data, if zero, then return
    !---------------------------------------

    ! Note - the scalar field has been removed from all mediator field bundles - so this is why we check if the
    ! fieldCount is 0 and not 1 here

    call ESMF_FieldBundleGet(is_local%wrap%FBExp(compocn), fieldCount=ncnt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (ncnt == 0) then
       if (dbug_flag > 5) then
          call ESMF_LogWrite(trim(subname)//": only scalar data is present in FBexp(compocn), returning", &
               ESMF_LOGMSG_INFO, rc=dbrc)
       endif
       RETURN
    end if

    !---------------------------------------
    !--- Get the current time from the clock
    !---------------------------------------

    call ESMF_GridCompGet(gcomp, clock=clock)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(time,timestring=timestr)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 1) then
       call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- map all fields in FBImp that have active
    !--- coupling to the ocean to the ocean grid
    !---------------------------------------

    do n1 = 1,ncomps
       if (is_local%wrap%med_coupling_active(n1,compocn)) then
          call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(n1,compocn), value=czero, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call med_map_FB_Regrid_Norm( &
               fldListFr(n1)%flds, compocn, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,compocn), &
               is_local%wrap%FBFrac(compocn), &
               is_local%wrap%FBNormOne(n1,compocn,:), &
               is_local%wrap%RH(n1,compocn,:), &
               string=trim(compname(n1))//'2'//trim(compname(compocn)), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          if (dbug_flag > 1) then
             call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(n1,compocn), &
                  string=trim(subname) //' FBImp('//trim(compname(n1))//','//trim(compname(compocn))//') ', rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
       endif
    enddo

    !---------------------------------------
    !--- auto merges to ocn
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBExp(compocn), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call med_merge_auto(is_local%wrap%FBExp(compocn), &
         FB1=is_local%wrap%FBImp(compatm,compocn)   , FB1w=is_local%wrap%FBfrac(compocn), fldw1='afrac', &
         FB2=is_local%wrap%FBMed_aoflux_o           , FB2w=is_local%wrap%FBfrac(compocn), fldw2='afrac', &
         FB3=is_local%wrap%FBImp(compice,compocn)   , FB3w=is_local%wrap%FBfrac(compocn), fldw3='ifrac', &
         FB4=is_local%wrap%FBImp(comprof,compocn)   , &
         FB5=is_local%wrap%FBImp(compglc,compocn)   , &
         document=first_call, string=subname, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- custom calculations to ocn
    !---------------------------------------

#if (1 == 0)
    ! atm and ice fraction
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBImp(compice,compocn), trim(ice_fraction_name), icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    allocate(atmwgt   (lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(customwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
       do i=lbound(icewgt,1),ubound(icewgt,1)
          atmwgt = 1.0_ESMF_KIND_R8 - icewgt
       enddo
    enddo

    !-------------
    ! mean_evap_rate = mean_laten_heat_flux * (1-ice_fraction)/const_lhvap
    !-------------

    !    customwgt = atmwgt / const_lhvap
    !    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBExp(compocn),'mean_evap_rate' , &
    !         FBinA=is_local%wrap%FBImp(compatm,compocn), fnameA='mean_laten_heat_flux', wgtA=customwgt, rc=rc)
    !    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction)
    !-------------

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBExp(compocn),'mean_fprec_rate' , &
         FBinA=is_local%wrap%FBImp(compatm,compocn), fnameA='mean_fprec_rate', wgtA=atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! End merges
    !-------------

    deallocate(atmwgt,customwgt)

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBExp(compocn), &
            string=trim(subname)//' FB4ocn_AFmrg ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
#endif

    !---------------------------------------
    !--- ocean accumulator
    !---------------------------------------

    call shr_nuopc_methods_FB_accum(is_local%wrap%FBExpAccum(compocn), is_local%wrap%FBExp(compocn), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    is_local%wrap%FBExpAccumCnt(compocn) = is_local%wrap%FBExpAccumCnt(compocn) + 1

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBExpAccum(compocn), &
            string=trim(subname)//' FBaccOcn_AFaccum ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    first_call = .false.

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_accum_fast

  !-----------------------------------------------------------------------------

end module med_phases_mod
