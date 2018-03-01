module med_phases_mod

  !-----------------------------------------------------------------------------
  ! Mediator Phases
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use shr_kind_mod                , only : CL=>SHR_KIND_CL, CS=>SHR_KIND_CS
  use shr_sys_mod                 , only : shr_sys_abort
  use shr_nuopc_fldList_types_mod , only : flds_scalar_name
  use shr_nuopc_fldList_types_mod , only : flds_scalar_index_nextsw_cday
  use shr_nuopc_fldList_types_mod , only : flds_scalar_index_dead_comps
  use shr_nuopc_fldList_types_mod , only : compmed, compatm, complnd, compocn
  use shr_nuopc_fldList_types_mod , only : compice, comprof, compwav, compglc
  use shr_nuopc_fldList_types_mod , only : ncomps, compname, mapnames, mapconsf
  use shr_nuopc_fldList_types_mod , only : mapconsf, mapfcopy
  use shr_nuopc_fldList_types_mod , only : fldListFr, fldListTo
  use shr_nuopc_fldList_types_mod , only : fldListMed_aoflux_a, fldListMed_aoflux_o
  use shr_nuopc_fldList_types_mod , only : fldListMed_ocnalb_a, fldListMed_ocnalb_o
  use shr_nuopc_fldList_types_mod , only : shr_nuopc_fldList_src_entry_type
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_init
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_reset
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_clean
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_diagnose
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_FieldRegrid
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_GetFldPtr
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_accum
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_FldChk
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_average
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_FB_copy
  use shr_nuopc_methods_mod       , only : shr_nuopc_methods_State_GetScalar
  use med_internalstate_mod       , only : InternalState
  use med_fraction_mod            , only : fraclist
  use med_fraction_mod            , only : med_fraction_setupflds
  use med_fraction_mod            , only : med_fraction_init
  use med_fraction_mod            , only : med_fraction_set
  use med_constants_mod           , only : med_constants_dbug_flag
  use med_constants_mod           , only : med_constants_statewrite_flag
  use med_constants_mod           , only : med_constants_spval_init
  use med_constants_mod           , only : med_constants_spval
  use med_constants_mod           , only : med_constants_czero
  use med_constants_mod           , only : med_constants_ispval_mask
  use med_merge_mod               , only : med_merge_auto
  use med_ocnalb_mod              , only : med_ocnalb_init, med_ocnalb
  use med_aofluxes_mod            , only : med_aofluxes_init, med_aofluxes
  use med_map_mod                 , only : med_map_FB_Regrid_Norm 

  implicit none

  private

  integer           , parameter :: dbug_flag       = med_constants_dbug_flag
  logical           , parameter :: statewrite_flag = med_constants_statewrite_flag
  real(ESMF_KIND_R8), parameter :: spval_init      = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval           = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero           = med_constants_czero
  integer           , parameter :: ispval_mask     = med_constants_ispval_mask
  character(len=*)  , parameter :: ice_fraction_name = 'Si_ifrac'
  character(*)      , parameter :: u_FILE_u        = __FILE__
  integer                       :: dbrc
  logical                       :: mastertask

  public med_phases_prep_atm
  public med_phases_prep_ocn
  public med_phases_prep_ice
  public med_phases_prep_lnd
  public med_phases_prep_rof
  public med_phases_prep_wav
  public med_phases_prep_glc
  public med_phases_accum_fast
  public med_phases_ocnalb_init
  public med_phases_ocnalb
  public med_phases_aofluxes_init
  public med_phases_aofluxes

  private med_phases_map_frac

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
    type(InternalState) :: is_local
    type(ESMF_VM)       :: vm
    integer             :: localPet
    integer             :: n, n1, n2, ncomp
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
    
    ! Initialize fraclist module array in med_fraction_mod 
    call med_fraction_setupflds(rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize FBfrac for component n1 - the field names will be fraclist(:,n1)
    ! Note - must use import state here - since export state might not contain anything other
    ! than scalar data if the component is not prognostic

    do n1 = 1,ncomps
       if (is_local%wrap%comp_present(n1) .and. &
            ESMF_StateIsCreated(is_local%wrap%NStateImp(n1),rc=rc) .and. &
            ESMF_StateIsCreated(is_local%wrap%NStateExp(n1),rc=rc)) then

          call shr_nuopc_methods_FB_init(is_local%wrap%FBfrac(n1), &
               STgeom=is_local%wrap%NStateImp(n1), &
               fieldNameList=fraclist(:,n1), &
               name='FBfrac'//trim(compname(n1)), rc=rc)
       end if
    end do

    ! Initialize fractions
    call med_fraction_init(gcomp,rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    ! Initialize ocn albedo and ocn/atm flux calculation modules if appropriate
    !---------------------------------------

    if (is_local%wrap%med_coupling_active(compocn,compatm) .and. &
        is_local%wrap%med_coupling_active(compatm,compocn)) then

       ! Initialize the atm/ocean fluxes and compute the ocean albedos
       call ESMF_LogWrite("MED - initialize atm/ocn fluxes and compute ocean albedo", ESMF_LOGMSG_INFO, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Initialize ocean albedo module
       call med_phases_ocnalb_init(gcomp, rc)

       ! Compute ocean albedoes
       ! This will update the relevant module arrays in med_ocnalb_mod.F90
       ! since they are simply pointers into field bundle arrays in the internal state is_local%wrap
       call med_phases_ocnalb(gcomp, rc)
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
    !--- map FBMed_ocnalb_o and FBMed_aoflux_o to atm grid
    !---------------------------------------

    if (is_local%wrap%med_coupling_active(compocn,compatm)) then
       call shr_nuopc_methods_FB_reset(is_local%wrap%FBMed_ocnalb_a, value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call med_map_FB_Regrid_Norm( &
            fldListMed_ocnalb_o%flds, compocn, &
            is_local%wrap%FBMed_ocnalb_o, &
            is_local%wrap%FBMed_ocnalb_a, &
            is_local%wrap%FBFrac(compocn), &
            is_local%wrap%FBNormOne(compocn,compatm,:), &
            is_local%wrap%RH(compocn,compatm,:), &
            string='FBMed_ocnalb_o_To_FBMed_ocnalb_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBMed_ocnalb_a, string=trim(subname)//&
               ' FBMed_ocnalb_a ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       endif

       call shr_nuopc_methods_FB_reset(is_local%wrap%FBMed_aoflux_a, value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call med_map_FB_Regrid_Norm(&
            fldListMed_aoflux_o%flds, compatm, &
            is_local%wrap%FBMed_aoflux_o, &
            is_local%wrap%FBMed_aoflux_a, &
            is_local%wrap%FBFrac(compatm), &
            is_local%wrap%FBNormOne(compocn,compatm,:), &
            is_local%wrap%RH(compocn,compatm,:), &
            string='FBMed_aoflux_o_To_FBMed_aoflux_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBMed_aoflux_a, string=trim(subname)//&
               ' FBMed_aoflux_a ', rc=rc)
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
    character(len=*),parameter :: subname='(med_phases_accum_fast)'
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

  subroutine med_phases_map_frac(FBin, fldin, FBout, fldout, RH, iarr, iarr_r, rc)

    ! Map field bundles with appropriate fraction weighting

    type(ESMF_FieldBundle)      :: FBin      ! input FB
    character(len=*)            :: fldin     ! input fieldname
    type(ESMF_FieldBundle)      :: FBout     ! output FB
    character(len=*)            :: fldout    ! output fieldname
    type(ESMF_RouteHandle)      :: RH        ! RH
    real(ESMF_KIND_R8), pointer :: iarr(:)   ! mapped field
    real(ESMF_KIND_R8), pointer :: iarr_r(:) ! reciprical of mapped field
    integer                     :: rc        ! return code

    ! local
    integer :: i
    real(ESMF_KIND_R8), pointer :: dataPtr2(:)
    character(len=*),parameter :: subname='(med_phases_map_frac)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (ESMF_RouteHandleIsCreated(RH, rc=rc)) then
       call shr_nuopc_methods_FB_FieldRegrid(FBin, fldin, FBout, fldout, RH, rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       !--- copy out the ifrac on ice grid and ifrac on atm grid
       call shr_nuopc_methods_FB_GetFldPtr(FBout, fldout, fldptr1=dataPtr2, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       allocate(iarr  (lbound(dataptr2,1):ubound(dataptr2,1)))
       allocate(iarr_r(lbound(dataptr2,1):ubound(dataptr2,1)))

       do i=lbound(dataptr2,1),ubound(dataptr2,1)
          !--- compute ice fraction on atm grid and reciprocal
          if (dataPtr2(i) == 0.0_ESMF_KIND_R8) then
             iarr(i)   = 0.0_ESMF_KIND_R8
             iarr_r(i) = 0.0_ESMF_KIND_R8
          else
             iarr(i)   = dataPtr2(i)
             iarr_r(i) = 1.0_ESMF_KIND_R8/dataPtr2(i)
          endif
       enddo
    endif

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_map_frac

  !-----------------------------------------------------------------------------

  subroutine med_phases_ocnalb_init(gcomp, rc)

    !----------------------------------------------------------
    ! Initialize the ocean albedo calculation and the field bundles 
    ! FBMed_ocnalb_a and FBMed_ocnalb_o
    !----------------------------------------------------------

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)    :: is_local
    integer                :: nflds
    character(CL), pointer :: fldnames(:)
    character(len=*), parameter :: subname='(module_MED_PHASES:med_phases_ocnalb_init)'
    !-----------------------------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    rc = ESMF_SUCCESS

    ! Get the internal state from gcomp
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Create field bundles for ocean albedo computation
    ! NOTE: the NStateImp(compocn) or NStateImp(compatm) used here
    ! rather than NStateExp(n2), since the export state might only
    ! contain control data and no grid information if if the target
    ! component (n2) is not prognostic only receives control data back

    nflds = size(fldListMed_aoflux_o%flds)
    allocate(fldnames(nflds))
    call shr_nuopc_fldList_getfldnames(fldnames)

    call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_ocnalb_a, &
         STgeom=is_local%wrap%NStateImp(compatm), fieldnamelist=fldnames, name='FBMed_ocnalb_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_ocnalb_o, &
         STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=fldnames, name='FBMed_ocnalb_o', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    deallocate(fldnames)

    ! Initialize module array pointers in med_ocnalb_mod
    call med_ocnalb_init(gcomp, is_local%wrap%FBImp(compocn,compocn), is_local%wrap%FBMed_ocnalb_o, rc=rc)

  end subroutine med_phases_ocnalb_init

  !-----------------------------------------------------------------------------

  subroutine med_phases_ocnalb(gcomp, rc)

    ! Compute ocean albedos (on the ocean grid)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)         :: is_local
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: currTime
    character(CL)               :: cvalue
    character(CS)               :: starttype     ! config start type
    character(CL)               :: runtype       ! initial, continue, hybrid, branch
    character(CL)               :: aoflux_grid
    real(ESMF_KIND_R8)          :: nextsw_cday  ! calendar day of next atm shortwave
    real(ESMF_KIND_R8), pointer :: ofrac(:)
    real(ESMF_KIND_R8), pointer :: ofrad(:)
    real(ESMF_KIND_R8), pointer :: ifrac(:)
    real(ESMF_KIND_R8), pointer :: ifrad(:)
    logical                     :: update_alb
    logical                     :: first_time = .true.
    character(len=*),parameter :: subname='(med_phases_ocnalb)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Note that in the mct version the atm was initialized first so
    ! that nextsw_cday could be passed to the other components - this
    ! assumed that atmosphere component was ALWAYS initialized first.
    ! In the nuopc version it will be easier to assume that on startup
    ! - nextsw_cday is just what cam was setting it as the current
    ! calendar day

    ! TODO: need to determine how to handle restart and branch runs -
    ! for now will just assume that nextsw_cday is not computed on
    ! initialization and is read from the restart file.

    if (first_time) then

       call NUOPC_CompAttributeGet(gcomp, name='start_type', value=cvalue, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       read(cvalue,*) starttype

       if (trim(starttype) == trim('startup')) then
          runtype = "initial"
       else if (trim(starttype) == trim('continue') ) then
          runtype = "continue"
       else if (trim(starttype) == trim('branch')) then
          runtype = "continue"
       else
          call shr_sys_abort( subname//' ERROR: unknown starttype' )
       end if

       if (trim(runtype) /= 'initial') then
          nextsw_cday = -1.0_ESMF_KIND_R8
       else
          call ESMF_GridCompGet(gcomp, clock=clock)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          call ESMF_ClockGet( clock, currTime=currTime, rc=rc )
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          call ESMF_TimeGet( currTime, dayOfYear_r8=nextsw_cday, rc=rc )
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       end if
       first_time = .false.

    else

       ! Note that shr_nuopc_methods_State_GetScalar includes a broadcast to all other pets im mpicom
       call shr_nuopc_methods_State_GetScalar(is_local%wrap%NstateImp(compatm), &
            scalar_id=flds_scalar_index_nextsw_cday, value=nextsw_cday, mpicom=is_local%wrap%mpicom, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    end if

    ! Map relevant atm fluxes to the ocean grid
    ! NOTE: there are already pointers in place as module variables in med_ocnalb_mod.F90
    ! to the arrays in FBMed_aoflux_o below - so do not need to pass them as arguments to med_ocnalb_run

    if (ESMF_RouteHandleIsCreated(is_local%wrap%RH(compatm,compocn,mapconsf), rc=rc)) then
       call shr_nuopc_methods_FB_FieldRegrid(&
            is_local%wrap%FBMed_aoflux_a, 'Faxa_swvdf', &
            is_local%wrap%FBMed_aoflux_o, 'Faxa_swvdf', &
            is_local%wrap%RH(compatm,compocn,mapconsf), rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_FieldRegrid(&
            is_local%wrap%FBMed_aoflux_a, 'Faxa_swndf', &
            is_local%wrap%FBMed_aoflux_o, 'Faxa_swndf', &
            is_local%wrap%RH(compatm,compocn,mapconsf), rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_FieldRegrid(&
            is_local%wrap%FBMed_aoflux_a, 'Faxa_swvdr', &
            is_local%wrap%FBMed_aoflux_o, 'Faxa_swvdr', &
            is_local%wrap%RH(compatm,compocn,mapconsf), rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_FieldRegrid(&
            is_local%wrap%FBMed_aoflux_a, 'Faxa_swndr', &
            is_local%wrap%FBMed_aoflux_o, 'Faxa_swndr', &
            is_local%wrap%RH(compatm,compocn,mapconsf), rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call ESMF_LogWrite(trim(subname)//": ERROR RH not available for "//trim(mapnames(mapconsf)), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
       rc = ESMF_FAILURE
    end if

    ! Calculate ocean albedos on the ocean grid
    call med_ocnalb(gcomp, nextsw_cday=nextsw_cday, update_alb=update_alb, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! If the aoflux grid is the atm - then need to map swdn and swup from the ocean grid (where
    ! they were calculated to the atmosphere grid)
    call NUOPC_CompAttributeGet(gcomp, name='aoflux_grid', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) aoflux_grid

    if (trim(aoflux_grid) == 'atm') then
       if (ESMF_RouteHandleIsCreated(is_local%wrap%RH(compocn,compatm,mapconsf), rc=rc)) then
          call shr_nuopc_methods_FB_FieldRegrid(&
               is_local%wrap%FBMed_ocnalb_o, 'Faox_swdn', &
               is_local%wrap%FBMed_ocnalb_a, 'Faox_swdn', &
               is_local%wrap%RH(compocn,compatm,mapconsf), rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          call shr_nuopc_methods_FB_FieldRegrid(&
               is_local%wrap%FBMed_ocnalb_o, 'Faox_swsup', &
               is_local%wrap%FBMed_ocnalb_a, 'Faox_swsup', &
               is_local%wrap%RH(compocn,compatm,mapconsf), rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    end if

    ! Update current ifrad/ofrad values if albedo was updated in field bundle
    if (update_alb) then
       call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac(compocn), fldname='ifrac', fldptr1=ifrac, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac(compocn), fldname='ifrad', fldptr1=ifrad, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac(compocn), fldname='ofrac', fldptr1=ofrac, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac(compocn), fldname='ofrad', fldptr1=ofrad, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       ifrad(:) = ifrac(:)
       ofrad(:) = ofrac(:)
    endif

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBMed_aoflux_o, string=trim(subname)//' FBMed_aoflux_o', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine med_phases_ocnalb

  !-----------------------------------------------------------------------------

  subroutine med_phases_aofluxes_init(gcomp, rc)

    ! Initialize ocn/atm flux calculations

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(CL)          :: cvalue
    character(CL)          :: aoflux_grid
    type(InternalState)    :: is_local
    integer                :: nflds
    character(CL), pointer :: fldnames(:)
    character(len=*),parameter :: subname='(med_phases_aofluxes_init)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Create field bundles for ocean/atmosphere flux computation
    ! NOTE: the NStateImp(compocn) or NStateImp(compatm) used here
    ! rather than NStateExp(n2), since the export state might only
    ! contain control data and no grid information if if the target
    ! component (n2) is not prognostic only receives control data back

    nflds = size(fldListMed_ocnalb_o%flds)
    allocate(fldnames(nflds))
    call shr_nuopc_fldList_getfldnames(fldnames)

    call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_aoflux_a, &
         STgeom=is_local%wrap%NStateImp(compatm), fieldnamelist=fldnames, name='FBMed_aoflux_a', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_aoflux_o, &
         STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=fldnames, name='FBMed_aoflux_o', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_init(is_local%wrap%FBMed_aoflux_accum_o, &
         STgeom=is_local%wrap%NStateImp(compocn), fieldnamelist=fldnames, name='FBMed_aoflux_o_accum', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    deallocate(fldnames)

    ! Determine src and dst comps depending on the aoflux_grid setting
    call NUOPC_CompAttributeGet(gcomp, name='aoflux_grid', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) aoflux_grid

    if (trim(aoflux_grid) == 'ocn') then

       call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(compatm,compocn), value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Regrid atm import field bundle from atm to ocn grid as input for ocn/atm flux calculation
       call med_map_FB_Regrid_Norm( &
            fldListFr(compatm)%flds, compocn,  &
            is_local%wrap%FBImp(compatm,compatm), &
            is_local%wrap%FBImp(compatm,compocn), &
            is_local%wrap%FBFrac(compatm), &
            is_local%wrap%FBNormOne(compatm,compocn,:), &
            is_local%wrap%RH(compatm,compocn,:), &
            string=trim(compname(compatm))//'2'//trim(compname(compocn)), rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compatm,compocn), &
               string=trim(subname) //' FBImp('//trim(compname(compatm))//','//trim(compname(compocn))//') ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Create FBMed_aoflux_o (field bundle on the ocean grid)
       call med_aofluxes_init(gcomp, &
            is_local%wrap%FBImp(compatm,compocn), &
            is_local%wrap%FBImp(compocn,compocn), &
            is_local%wrap%FBfrac(compocn), &
            is_local%wrap%FBMed_ocnalb_o, &
            is_local%wrap%FBMed_aoflux_o, &
            is_local%wrap%FBMed_aoflux_diurnl_o, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    else if (trim(aoflux_grid) == 'atm') then

       call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(compocn,compatm), value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call med_map_FB_Regrid_Norm( &
            fldListFr(compocn)%flds, compatm, &
            is_local%wrap%FBImp(compocn,compocn), &
            is_local%wrap%FBImp(compocn,compatm), &
            is_local%wrap%FBFrac(compocn), &
            is_local%wrap%FBNormOne(compocn,compatm,:), &
            is_local%wrap%RH(compocn,compatm,:), &
            string=trim(compname(compocn))//'2'//trim(compname(compatm)), rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compocn,compatm), &
               string=trim(subname) //' FBImp('//trim(compname(compocn))//','//trim(compname(compatm))//') ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Create FBMed_aoflux_a (field bundle on the atmosphere grid)
       call med_aofluxes_init(gcomp, &
            is_local%wrap%FBImp(compatm,compatm), &
            is_local%wrap%FBImp(compocn,compatm), &
            is_local%wrap%FBfrac(compatm), &
            is_local%wrap%FBMed_ocnalb_a, &
            is_local%wrap%FBMed_aoflux_a, &
            is_local%wrap%FBMed_aoflux_diurnl_a, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    else

       call ESMF_LogWrite(trim(subname)//' aoflux_grid = '//trim(aoflux_grid)//' not available', &
            ESMF_LOGMSG_INFO, rc=dbrc)
       return

    end if

  end subroutine med_phases_aofluxes_init

  !-----------------------------------------------------------------------------

  subroutine med_phases_aofluxes(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    type(ESMF_Clock)    :: clock
    character(CL)       :: cvalue
    real(ESMF_KIND_R8)  :: rdead_comps
    logical             :: dead_comps
    logical             :: ocn_prognostic
    character(CL)       :: aoflux_grid
    character(len=*),parameter :: subname='(med_phases_aofluxes)'
    !---------------------------------------

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Get the clock from the mediator Component
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get the internal state from the mediator Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine dead_comps
    call shr_nuopc_methods_State_GetScalar(is_local%wrap%NStateImp(compatm), &
         scalar_id=flds_scalar_index_dead_comps, value=rdead_comps, mpicom=is_local%wrap%mpicom, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    dead_comps = (rdead_comps /= 0._ESMF_KIND_r8)

    call shr_nuopc_methods_State_GetScalar(is_local%wrap%NStateImp(compocn), &
         scalar_id=flds_scalar_index_dead_comps, value=rdead_comps, mpicom=is_local%wrap%mpicom, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    dead_comps = (rdead_comps /= 0._ESMF_KIND_r8)

    ! Determine if ocean is prognostic
    ocn_prognostic = NUOPC_IsConnected(is_local%wrap%NStateImp(compocn))

    ! Determine source and destination comps depending on the aoflux_grid setting
    call NUOPC_CompAttributeGet(gcomp, name='aoflux_grid', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) aoflux_grid

    if (trim(aoflux_grid) == 'ocn') then

       ! Reset the field bundle on the destination grid to zero
       call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(compatm,compocn), value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Regrid atm import field bundle from atm to ocn grid as input for ocn/atm flux calculation
       call med_map_FB_Regrid_Norm( &
            fldListFr(compatm)%flds, compocn, &
            is_local%wrap%FBImp(compatm,compatm), &
            is_local%wrap%FBImp(compatm,compocn), &
            is_local%wrap%FBFrac(compatm), &
            is_local%wrap%FBNormOne(compatm,compocn,:), &
            is_local%wrap%RH(compatm,compocn,:), &
            string=trim(compname(compatm))//'2'//trim(compname(compocn)), rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compatm,compocn), &
               string=trim(subname) //' FBImp('//trim(compname(compatm))//','//trim(compname(compocn))//') ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Calculate atm/ocn fluxes on the destination grid
       call med_aofluxes(gcomp, clock, ocn_prognostic, dead_comps, rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBMed_aoflux_o, string=trim(subname) //' FBAMed_aoflux_o' , rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

    else if (trim(aoflux_grid) == 'atm') then

       call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(compocn,compatm), value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call med_map_FB_Regrid_Norm( &
            fldListFr(compocn)%flds, compatm, &
            is_local%wrap%FBImp(compocn,compocn), &
            is_local%wrap%FBImp(compocn,compatm), &
            is_local%wrap%FBFrac(compocn), &
            is_local%wrap%FBNormOne(compocn,compatm,:), &
            is_local%wrap%RH(compocn,compatm,:), &
            string=trim(compname(compocn))//'2'//trim(compname(compatm)), rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compocn,compatm), &
               string=trim(subname) //' FBImp('//trim(compname(compocn))//','//trim(compname(compatm))//') ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

       ! Calculate atm/ocn fluxes on the destination grid
       call med_aofluxes(gcomp, clock, ocn_prognostic, dead_comps, rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compocn,compatm), &
               string=trim(subname) //' FBImp('//trim(compname(compocn))//','//trim(compname(compatm))//') ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       end if

    else

       call ESMF_LogWrite(trim(subname)//' aoflux_grid = '//trim(aoflux_grid)//' not available', &
            ESMF_LOGMSG_INFO, rc=dbrc)
       return

    end if

  end subroutine med_phases_aofluxes

end module med_phases_mod
