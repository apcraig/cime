module med_phases_mod

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to
  ! do so.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use shr_kind_mod          , only : CL=>SHR_KIND_CL, CS=>SHR_KIND_CS
  use shr_sys_mod           , only : shr_sys_abort
  use shr_nuopc_fldList_mod , only : compmed, compatm, complnd, compocn
  use shr_nuopc_fldList_mod , only : compice, comprof, compwav, compglc
  use shr_nuopc_fldList_mod , only : ncomps, compname
  use shr_nuopc_fldList_mod , only : fldListFr, fldListTo
  use shr_nuopc_fldList_mod , only : fldListXao_fluxes_a, fldListXao_fluxes_o
  use shr_nuopc_fldList_mod , only : fldListXao_ocnalb_a, fldListXao_ocnalb_o
  use shr_nuopc_fldList_mod , only : mapbilnr, mapconsf, mapconsd, mappatch, mapfcopy
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_ChkErr
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_init
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_reset
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_clean
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_diagnose
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_FieldRegrid
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_GetFldPtr
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_accum
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_FldChk
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_average
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_FB_copy
  use shr_nuopc_methods_mod , only : shr_nuopc_methods_State_GetScalar
  use med_internalstate_mod , only : InternalState
  use med_constants_mod     , only : med_constants_dbug_flag
  use med_constants_mod     , only : med_constants_statewrite_flag
  use med_constants_mod     , only : med_constants_spval_init
  use med_constants_mod     , only : med_constants_spval
  use med_constants_mod     , only : med_constants_czero
  use med_constants_mod     , only : med_constants_ispval_mask
  use med_merge_mod         , only : med_merge_auto
  use med_ocnatm_flux_mod   , only : med_ocnatm_flux_init, med_ocnatm_flux_run
  use med_ocnalb_mod        , only : med_ocnalb_init, med_ocnalb_run

  implicit none

  private

  integer           , parameter :: dbug_flag       = med_constants_dbug_flag
  logical           , parameter :: statewrite_flag = med_constants_statewrite_flag
  real(ESMF_KIND_R8), parameter :: spval_init      = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval           = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero           = med_constants_czero
  integer           , parameter :: ispval_mask     = med_constants_ispval_mask
  character(len=*)  , parameter :: ice_fraction_name = 'Si_ifrac'
  character(*)      , parameter :: u_FILE_u = __FILE__
  integer                       :: dbrc

  public med_phases_prep_atm
  public med_phases_prep_ocn
  public med_phases_prep_ice
  public med_phases_prep_lnd
  public med_phases_prep_rof
  public med_phases_prep_wav
  public med_phases_prep_glc
  public med_phases_accum_fast
  public med_phases_ocnalb_init
  public med_phases_ocnatm_flux_init
  public med_phases_ocnalb
  public med_phases_ocnatm_flux

  private med_phases_map_frac

  !-----------------------------------------------------------------------------
  contains
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

          call shr_nuopc_methods_FB_Regrid_Norm( &
               fldListFr(n1)%flds, &
               is_local%wrap%FBImp(n1,n1), &
               is_local%wrap%FBImp(n1,compatm), &
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
    !--- map FBXao_ocnalb_o and FBXao_fluxes_o to atm grid
    !---------------------------------------

    if (is_local%wrap%med_coupling_active(compocn,compatm)) then
       call shr_nuopc_methods_FB_reset(is_local%wrap%Xao_ocnalb_a, value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_Regrid_Norm(&
            fldListXao_ocnalb%flds, &
            is_local%wrap%FBXao_ocnalb_o, &
            is_local%wrap%FBXao_ocnalb_a, &
            is_local%wrap%FBNormOne(compatm,compocn,:), &
            is_local%wrap%RH(compatm,compocn,:), &
            string='FBAXao_ocnalb_o_To_FBXao_ocnalb_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBXao_ocnalb_a, string=trim(subname)//&
               ' FBXao_ocnalb_a ', rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
       endif

       call shr_nuopc_methods_FB_reset(is_local%wrap%Xao_fluxes_a, value=czero, rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       call shr_nuopc_methods_FB_Regrid_Norm(&
            fldListXao_ocnalb%flds, &
            is_local%wrap%FBXao_fluxes_o, &
            is_local%wrap%FBXao_fluxes_a, &
            is_local%wrap%FBNormOne(compocn,compatm,:), &
            is_local%wrap%RH(compocn,compatm,:), &
            string='FBAXao_fluxes_o_To_FBXao_fluxes_a', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       if (dbug_flag > 1) then
          call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBXao_fluxes_a, string=trim(subname)//&
               ' FBXao_fluxes_a ', rc=rc)
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
         FB2=is_local%wrap%FBXao_ocnalb_a        , FB2w=is_local%wrap%FBfrac(compatm), fldw2='ofrac', &
         FB3=is_local%wrap%FBXao_fluxes_a        , FB3w=is_local%wrap%FBfrac(compatm), fldw2='ofrac', &
         FB4=is_local%wrap%FBImp(compice,compatm), FB4w=is_local%wrap%FBfrac(compatm), fldw3='ifrac', &
         FB5=is_local%wrap%FBImp(complnd,compatm), FB5w=is_local%wrap%FBfrac(compatm), fldw4='lfrac', &
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

        call shr_nuopc_methods_FB_Regrid_Norm( &
             fldListFr(n1)%flds, &
             is_local%wrap%FBImp(n1,n1), &
             is_local%wrap%FBImp(n1,compice), &
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

        call shr_nuopc_methods_FB_Regrid_Norm( &
             fldListFr(n1)%flds, &
             is_local%wrap%FBImp(n1,n1), &
             is_local%wrap%FBImp(n1,complnd), &
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

        call shr_nuopc_methods_FB_Regrid_Norm( &
             fldListFr(n1)%flds, &
             is_local%wrap%FBImp(n1,n1), &
             is_local%wrap%FBImp(n1,comprof), &
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

        call shr_nuopc_methods_FB_Regrid_Norm( &
             fldListFr(n1)%flds, &
             is_local%wrap%FBImp(n1,n1), &
             is_local%wrap%FBImp(n1,compwav), &
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

        call shr_nuopc_methods_FB_Regrid_Norm( &
             fldListFr(n1)%flds, &
             is_local%wrap%FBImp(n1,n1), &
             is_local%wrap%FBImp(n1,compglc), &
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

        call shr_nuopc_methods_FB_Regrid_Norm( &
             fldListFr(n1)%flds, &
             is_local%wrap%FBImp(n1,n1), &
             is_local%wrap%FBImp(n1,compocn), &
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
         FB2=is_local%wrap%FBAtmOcn_o               , FB2w=is_local%wrap%FBfrac(compocn), fldw2='afrac', &
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
    ! Initialize atm/ocn module variables

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(CL)       :: cvalue
    character(CL)       :: aoflux_grid
    type(InternalState) :: is_local
    character(len=*),parameter :: subname='(med_phases_atmocn_init)'
    !---------------------------------------

    call med_ocnalb_init(gcomp,          &
         FBAtmOcn=is_local%wrap%FBAtmOcn(compdst)    &
         FBAtm=is_local%wrap%FBImp(compsrc,compdst), &
         FBOcn=is_local%wrap%FBImp(compdst,compdst), &
         FBfrac=is_local%wrap%FBfrac(compdst), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine med_phases_ocnalb_init

  !-----------------------------------------------------------------------------

  subroutine med_phases_ocnatm_flux_init(gcomp, rc)
    ! Initialize atm/ocn module variables

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(CL)       :: cvalue
    character(CL)       :: aoflux_grid
    type(InternalState) :: is_local
    character(len=*),parameter :: subname='(med_phases_atmocn_init)'
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine src and dst comps depending on the aoflux_grid setting
    call NUOPC_CompAttributeGet(gcomp, name='aoflux_grid', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) aoflux_grid

    if (trim(aoflux_grid) == 'ocn') then
       compsrc = compatm
       compdst = compocn
    else if (trim(aoflux_grid) == 'atm')
       compsrc = compocn
       compdst = compatm
    else
       call ESMF_LogWrite(trim(subname)//' aoflux_grid = '//trim(aoflux_grid)//' not available', &
            ESMF_LOGMSG_INFO, rc=dbrc)
       return
    end if

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(compsrc,compdst), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_Regrid_Norm( &
         fldListFr(compsrc)%flds, &
         is_local%wrap%FBImp(compsrc,compsrc), &
         is_local%wrap%FBImp(compsrc,compdst), &
         is_local%wrap%FBNormOne(compsrc,compdst,:), &
         is_local%wrap%RH(compsrc,compdst,:), &
         string=trim(compname(compsrc))//'2'//trim(compname(compdst)), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compsrc,compdst), &
            string=trim(subname) //' FBImp('//trim(compname(compsrc))//','//trim(compname(compdst))//') ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    call med_atmocn_init(gcomp, aoflux_grid,         &
         FBAtmOcn=is_local%wrap%FBAtmOcn(compdst)    &
         FBAtm=is_local%wrap%FBImp(compsrc,compdst), &
         FBOcn=is_local%wrap%FBImp(compdst,compdst), &
         FBfrac=is_local%wrap%FBfrac(compdst), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine med_phases_atmocn_init

  !-----------------------------------------------------------------------------

  subroutine med_phases_ocnalb(gcomp, rc)

    use shr_nuopc_flds_mod  , only : flds_scalar_index_nextsw_cday

    ! Compute ocean albedos (on the ocean grid)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    type(ESMF_Clock)    :: clock
    type(ESMF_Time)     :: currTime
    character(CL)       :: cvalue
    character(CS)       :: starttype     ! config start type
    character(CL)       :: runtype       ! initial, continue, hybrid, branch
    real(ESMF_KIND_R8)  :: nextsw_cday  ! calendar day of next atm shortwave
    logical             :: first_time = .true.
    character(len=*),parameter :: subname='(med_phases_atmocn_ocnalb)'
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Note that in the mct version the atm was initialized first so that nextsw_cday could be passed to the other
    ! components - this assumed that atmosphere component was ALWAYS initialized first.
    ! In the nuopc version it will be easier to assume that on startup - nextsw_cday is just what cam was setting
    ! it as the current calendar day
    ! TODO: need to determine how to handle restart and branch runs - for now will just assume that nextsw_cday
    ! is not computed on initialization and is read from the restart file.

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
    ! TODO: must add a normalization of one here
    call shr_nuopc_methods_FB_FieldRegrid(FBAtmOcn_a, 'Faxa_swvdf', FBAtmOcn_o, 'Faxa_swvdf', consfmap, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_FieldRegrid(FBAtmOcn_a, 'Faxa_swndf', FBAtmOcn_o, 'Faxa_swndf', consfmap, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_FieldRegrid(FBAtmOcn_a, 'Faxa_swvdr', FBAtmOcn_o, 'Faxa_swvdr', consfmap, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_FieldRegrid(FBAtmOcn_a, 'Faxa_swndr', FBAtmOcn_o, 'Faxa_swndr', consfmap, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Calculate ocean albedoes on the ocean grid
    call med_atmocn_ocnalb(gcomp, FBfrac_o=is_local%wrap%FBfrac(compocn), nextsw_cday=nextsw_cday, rc=rc)

    ! ***** TODO: map the ocean albedos to the atm grid *****

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBAtmOcn_o, &
            string=trim(subname) //' FBAtmOcn_o', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine med_phases_atmocn_ocnalb

  !-----------------------------------------------------------------------------

  subroutine med_phases_atmocn_flux(gcomp, rc)
    use shr_nuopc_flds_mod  , only : flds_scalar_index_dead_comps

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    type(ESMF_Clock)    :: clock
    real(ESMF_KIND_R8)  :: rdead_comps
    logical             :: dead_comps
    logical             :: ocn_prognostic
    character(len=*),parameter :: subname='(med_phases_atmocn_flux)'
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

    ! Determine ocn_prognostic
    ocn_prognostic = NUOPC_IsConnected(is_local%wrap%NStateImp(compocn))

    ! Determine src and dst comps depending on the aoflux_grid setting
    call NUOPC_CompAttributeGet(gcomp, name='aoflux_grid', value=cvalue, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) aoflux_grid

    if (trim(aoflux_grid) == 'ocn') then
       compsrc = compatm
       compdst = compocn
    else if (trim(aoflux_grid) == 'atm')
       compsrc = compocn
       compdst = compatm
    else
       call ESMF_LogWrite(trim(subname)//' aoflux_grid = '//trim(aoflux_grid)//' not available', &
            ESMF_LOGMSG_INFO, rc=dbrc)
       return
    end if

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBImp(compsrc,compdst), value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_Regrid_Norm( &
         fldListFr(compsrc)%flds, &
         is_local%wrap%FBImp(compsrc,compsrc), &
         is_local%wrap%FBImp(compsrc,compdst), &
         is_local%wrap%FBNormOne(compsrc,compdst,:), &
         is_local%wrap%RH(compsrc,compdst,:), &
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBImp(compsrc,compdst), &
            string=trim(subname) //' FBImp('//trim(compname(compsrc))//','//trim(compname(compdst))//') ', rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! Calculate atm/ocn fluxes on the ocean grid - for now)
    call med_atmocn_flux(gcomp, clock, ocn_prognostic=ocn_prognostic, dead_comps=dead_comps, rc=rc)

    ! TODO: map the atm/ocn fluxes to the atmosphere grid

    if (dbug_flag > 1) then
       call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBAtmOcn_o, &
            string=trim(subname) //' FBAtmOcn_o' , rc=rc)
       if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine med_phases_atmocn_flux

  !-----------------------------------------------------------------------------

  subroutine shr_nuopc_methods_FB_Regrid_Norm(fldsSrc, FBSrc, FBDst, FBNormOne, RouteHandles, string, rc)

    ! ----------------------------------------------
    ! Map field bundles with appropriate fraction weighting
    ! ----------------------------------------------

    type (shr_nuopc_src_entry_type)   , intent(in)    :: fldsSrc(:)
    type(ESMF_FieldBundle)            , intent(inout) :: FBSrc
    type(ESMF_FieldBundle)            , intent(inout) :: FBDst
    type(ESMF_FieldBundle)            , intent(in)    :: FBFrac
    type(ESMF_FieldBundle)            , intent(in)    :: FBNormOne(:)
    type(ESMF_RouteHandle)            , intent(inout) :: RouteHandles(:)
    character(len=*), optional        , intent(in)    :: string
    integer                           , intent(out)   :: rc

    ! local variables
    type(ESMF_FieldBundle)      :: FBSrcTmp        ! temporary
    type(ESMF_FieldBundle)      :: FBNormSrc       ! temporary
    type(ESMF_FieldBundle)      :: FBNormDst       ! temporary
    real(ESMF_KIND_R8), pointer :: data_srctmp(:)  ! temporary
    real(ESMF_KIND_R8), pointer :: data_src(:)     ! temporary
    real(ESMF_KIND_R8), pointer :: data_dst(:)     ! temporary
    real(ESMF_KIND_R8), pointer :: data_srcnorm(:) ! temporary
    real(ESMF_KIND_R8), pointer :: data_dstnormdst ! temporary
    real(ESMF_KIND_R8), pointer :: data_frac(:)    ! temporary
    integer                     :: mapindex
    character(len=64)           :: lstring
    character(len=CS)           :: mapnorm
    character(len=CS)           :: fldname
    integer                     :: i, n
    character(len=*),parameter  :: subname='(med_phases_FB_Regrid_Norm)'
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
       mapindex = fldsSrc(n)%mapindex(dstcomp)
       mapnorm  = fldsSrc(n)%mapnorm(dstcomp)

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

          call shr_nuopc_methods_FB_FieldRegrid(FBSrc, fldname), FBDst, fldname, RouteHandles(mapindex), rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

       else

          ! Create a new temporary field bundle if needed
          if (.not. ESMF_FieldBundleIsCreated(FBSrcTmp)) then
             call shr_nuopc_methods_FB_init(FBSrcTmp, FBgeom=FBSrc, fieldNameList=/trim(fldname)/, rc=rc)
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

             call shr_nuopc_methods_FB_init(FBout=FBNormSrc, FBgeom=FBSrc, fieldNameList=/trim(mapnorm)/, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
             call shr_nuopc_methods_FB_init(FBout=FBNormDst, FBgeom=FBDst, fieldNameList=/trim(mapnorm)/, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

             call shr_nuopc_methods_FB_reset(FBNormSrc, value=czero, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return
             call shr_nuopc_methods_FB_reset(FBNormDst, value=czero, rc=rc)
             if (shr_nuopc_methods_chkerr(rc,__line__,u_file_u)) return

             call shr_nuopc_methods_FB_GetFldPtr(FBNormSrc, trim(mapnorm), data_srcnorm, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             ! error checks
             if (size(data_normsrc) /= size(data_frac)) then
                call ESMF_LogWrite(trim(subname)//": ERROR data_normsrc size and data_frac size are inconsistent" //, &
                     ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
                rc = ESMF_FAILURE
                return
             else if (size(data_norm) /= size(data_srctmp)) then
                call ESMF_LogWrite(trim(subname)//": ERROR data_normsrc size and data_srctmp size are inconsistent" //, &
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
             end if

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

             call shr_nuopc_methods_FB_GetFldPtr(FBNormDst, trim(mapnorm), data_dstnnorm, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
             call shr_nuopc_methods_FB_GetFldPtr(FBDst, trim(fldname), data_dst, rc=rc)
             if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

             do i= 1,size(data_dst)
                if (data_normdst(i) == 0.0_ESMF_KIND_R8) then
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
             mapindex = fldListSrc%flds(n)%mapindex(dstcomp)
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

  end subroutine shr_nuopc_methods_FB_Regrid_Norm

end module med_phases_mod
