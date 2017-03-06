module med_phases_mod

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to 
  ! do so.
  !-----------------------------------------------------------------------------

  use ESMF
  use shr_nuopc_methods_mod
  use med_internalstate_mod
  use med_constants_mod

  implicit none
  
  private
  
  integer            :: dbug_flag   = med_constants_dbug_flag
  logical            :: statewrite_flag = .true.
  real(ESMF_KIND_R8), parameter :: spval_init  = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval       = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero       = med_constants_czero
  integer           , parameter :: ispval_mask = med_constants_ispval_mask
  integer            :: dbrc
  character(*),parameter :: u_FILE_u = &
    __FILE__

  public med_phases_prep_atm
  public med_phases_prep_ocn
  public med_phases_accum_fast
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_atm(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase runs before ATM and ICE are being called and
    ! prepares the ATM and ICE import Fields.
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    real(ESMF_KIND_R8), pointer :: ifrac_i(:)                   ! ice fraction on ice grid
    real(ESMF_KIND_R8), pointer :: ifrac_af(:), ifrac_afr(:)  ! ice fraction on atm grid consf map
    real(ESMF_KIND_R8), pointer :: ifrac_ad(:), ifrac_adr(:)  ! ice fraction on atm grid consd map
    real(ESMF_KIND_R8), pointer :: ifrac_ab(:), ifrac_abr(:)  ! ice fraction on atm grid bilnr map
    real(ESMF_KIND_R8), pointer :: ifrac_ap(:), ifrac_apr(:)  ! ice fraction on atm grid patch map
    real(ESMF_KIND_R8), pointer :: ocnwgt(:),icewgt(:),customwgt(:)
    integer                     :: i,j,n
    character(len=*),parameter  :: subname='(module_MEDIATOR:med_phases_prep_atm)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------

    is_local%wrap%atmcntr = is_local%wrap%atmcntr + 1

    !---------------------------------------

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    call ESMF_TimeGet(time,timestring=timestr)
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->"//trim(subname)//" mediating for: ", rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- this is fast, no accumulator needed
    !---------------------------------------

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_reset(is_local%wrap%FBAtm_a, value=czero, rc=rc)
      call shr_nuopc_methods_FB_reset(is_local%wrap%FBOcn_o, value=czero, rc=rc)
      call shr_nuopc_methods_FB_reset(is_local%wrap%FBIce_i, value=czero, rc=rc)
      call shr_nuopc_methods_FB_reset(is_local%wrap%FBLnd_l, value=czero, rc=rc)
      call shr_nuopc_methods_FB_reset(is_local%wrap%FBRof_h, value=czero, rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a zero', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o zero', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i zero', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l zero', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBRof_h, trim(subname)//' FBrof_h zero', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_OcnImp, trim(subname)//' OcnImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_RofImp, trim(subname)//' RofImp ', rc=rc)
    endif

    call shr_nuopc_methods_FB_copy(is_local%wrap%FBAtm_a, is_local%wrap%NState_AtmImp, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBOcn_o, is_local%wrap%NState_OcnImp, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBIce_i, is_local%wrap%NState_IceImp, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBLnd_l, is_local%wrap%NState_LndImp, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBRof_h, is_local%wrap%NState_RofImp, rc=rc)

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBRof_h, trim(subname)//' FBRof_h ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBOcn_a, value=czero, rc=rc)
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBIce_a, value=czero, rc=rc)
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBIce_if, value=czero, rc=rc)
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBLnd_a, value=czero, rc=rc)
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBRof_a, value=czero, rc=rc)
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBAtmOcn_a, value=czero, rc=rc)

    if (is_local%wrap%o2a_active) then
      call shr_nuopc_methods_FB_Regrid(fldsFrOcn, is_local%wrap%FBOcn_o, is_local%wrap%FBOcn_a, &
         consfmap=is_local%wrap%RH_o2a_consf, &
         consdmap=is_local%wrap%RH_o2a_consd, &
         bilnrmap=is_local%wrap%RH_o2a_bilnr, &
         patchmap=is_local%wrap%RH_o2a_patch, &
         string='o2a', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      call shr_nuopc_methods_FB_Regrid(fldsAtmOcn, is_local%wrap%FBAtmOcn_o, is_local%wrap%FBAtmOcn_a, &
         consfmap=is_local%wrap%RH_o2a_consf, &
         consdmap=is_local%wrap%RH_o2a_consd, &
         bilnrmap=is_local%wrap%RH_o2a_bilnr, &
         patchmap=is_local%wrap%RH_o2a_patch, &
         string='o2aatmocn', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    if (is_local%wrap%i2a_active) then
      if (shr_nuopc_methods_FB_FldChk(is_local%wrap%FBIce_i, 'ice_fraction', rc=rc) .and. &
          shr_nuopc_methods_FB_FldChk(is_local%wrap%FBIce_a, 'ice_fraction', rc=rc)) then
        !--- tcraig, need to weight the ice2atm regrid by the ice fraction
        !--- need to compute weight by the frac mapped with the correct mapping
        !--- first compute the ice fraction on the atm grid for all active mappings

        call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_i, 'ice_fraction', fldptr1=dataPtr1, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        allocate(ifrac_i (lbound(dataPtr1,1):ubound(dataPtr1,1)))

        !--- conservative frac
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consf, rc=rc)) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_consf, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', fldptr1=dataPtr2, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          allocate(ifrac_afr(lbound(dataptr2,1):ubound(dataptr2,1)))
          allocate(ifrac_af (lbound(dataptr2,1):ubound(dataptr2,1)))

          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i) = dataPtr1(i)
          enddo

          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_af(i) = dataPtr2(i)
            if (dataPtr2(i) == 0._ESMF_KIND_R8) then
              ifrac_afr(i) = 1.0_ESMF_KIND_R8
            else
              ifrac_afr(i) = 1.0_ESMF_KIND_R8/dataPtr2(i)
            endif
          enddo
        endif

        !--- conservative dst
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consd, rc=rc)) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_consd, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          allocate(ifrac_adr(lbound(dataptr2,1):ubound(dataptr2,1)))
          allocate(ifrac_ad (lbound(dataptr2,1):ubound(dataptr2,1)))

          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i) = dataPtr1(i)
          enddo

          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ad(i) = dataPtr2(i)
            if (dataPtr2(i) == 0._ESMF_KIND_R8) then
              ifrac_adr(i) = 1.0_ESMF_KIND_R8
            else
              ifrac_adr(i) = 1.0_ESMF_KIND_R8/dataPtr2(i)
            endif
          enddo
        endif

        !--- bilinear
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_bilnr, rc=rc)) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_bilnr, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          allocate(ifrac_abr(lbound(dataptr2,1):ubound(dataptr2,1)))
          allocate(ifrac_ab (lbound(dataptr2,1):ubound(dataptr2,1)))

          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i) = dataPtr1(i)
          enddo

          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ab(i) = dataPtr2(i)
            if (dataPtr2(i) == 0._ESMF_KIND_R8) then
              ifrac_abr(i) = 1.0_ESMF_KIND_R8
            else
              ifrac_abr(i) = 1.0_ESMF_KIND_R8/dataPtr2(i)
            endif
          enddo
        endif

        !--- patch
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_patch, rc=rc)) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_patch, rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

          allocate(ifrac_apr(lbound(dataptr2,1):ubound(dataptr2,1)))
          allocate(ifrac_ap (lbound(dataptr2,1):ubound(dataptr2,1)))

          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i) = dataPtr1(i)
          enddo

          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ap(i) = dataPtr2(i)
            if (dataPtr2(i) == 0._ESMF_KIND_R8) then
              ifrac_apr(i) = 1.0_ESMF_KIND_R8
            else
              ifrac_apr(i) = 1.0_ESMF_KIND_R8/dataPtr2(i)
            endif
          enddo
        endif

        !--- multiply FBIce_i by ifrac_i

        do n = 1,fldsFrIce%num
          if (shr_nuopc_methods_FB_FldChk(is_local%wrap%FBIce_i, fldsFrIce%shortname(n), rc=rc) .and. &
              shr_nuopc_methods_FB_FldChk(is_local%wrap%FBIce_if,fldsFrIce%shortname(n), rc=rc)) then
            call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_i , fldsFrIce%shortname(n), dataPtr3, rc=rc)
            call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_if, fldsFrIce%shortname(n), dataPtr4, rc=rc)
            do i=lbound(dataptr3,1),ubound(dataptr3,1)
              dataPtr4(i) = dataPtr3(i) * ifrac_i(i)
            enddo
          endif
        enddo

        !--- regrid FBIce_if, fields with fraction multiplied

        call shr_nuopc_methods_FB_Regrid(fldsFrIce, is_local%wrap%FBIce_if, is_local%wrap%FBIce_a, &
           consfmap=is_local%wrap%RH_i2a_consf, &
           consdmap=is_local%wrap%RH_i2a_consd, &
           bilnrmap=is_local%wrap%RH_i2a_bilnr, &
           patchmap=is_local%wrap%RH_i2a_patch, &
           string='i2a', rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

        !--- divide FBIce_a by ifrac_a, interpolated ice fraction
        !--- actually multiply by reciprocal of ifrac_a, ifrac_ar

        do n = 1,fldsFrIce%num
          if (shr_nuopc_methods_FB_FldChk(is_local%wrap%FBIce_a, fldsFrIce%shortname(n), rc=rc)) then
            call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, fldsFrIce%shortname(n), dataPtr3, rc=rc)
            if (fldsFrIce%mapping(n) == "conservefrac") then
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i) = dataPtr3(i) * ifrac_afr(i)
              enddo
            elseif (fldsFrIce%mapping(n) == "conservedst") then
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i) = dataPtr3(i) * ifrac_adr(i)
              enddo
            elseif (fldsFrIce%mapping(n) == 'bilinear') then
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i) = dataPtr3(i) * ifrac_abr(i)
              enddo
            elseif (fldsFrIce%mapping(n) == 'patch') then
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i) = dataPtr3(i) * ifrac_apr(i)
              enddo
            else
              call ESMF_LogWrite(trim(subname)//": mapping name error "//trim(fldsFrIce%mapping(n)), ESMF_LOGMSG_INFO, rc=rc)
              rc=ESMF_FAILURE
              return
            endif
          endif
        enddo
        !--- make sure ifrac_a in the mapped bundle is correct
        call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr3, rc=rc)
        do i=lbound(dataptr3,1),ubound(dataptr3,1)
          dataPtr3(i) = ifrac_af(i)
        enddo

        deallocate(ifrac_i)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consf, rc=rc)) &
          deallocate(ifrac_af, ifrac_afr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consd, rc=rc)) &
          deallocate(ifrac_ad, ifrac_adr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_bilnr, rc=rc)) &
          deallocate(ifrac_ab, ifrac_abr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_patch, rc=rc)) &
          deallocate(ifrac_ap, ifrac_apr)

      else
        call shr_nuopc_methods_FB_Regrid(fldsFrIce, is_local%wrap%FBIce_i, is_local%wrap%FBIce_a, &
           consfmap=is_local%wrap%RH_i2a_consf, &
           consdmap=is_local%wrap%RH_i2a_consd, &
           bilnrmap=is_local%wrap%RH_i2a_bilnr, &
           patchmap=is_local%wrap%RH_i2a_patch, &
           string='i2a', rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    endif

    if (is_local%wrap%l2a_active) then
      call shr_nuopc_methods_FB_Regrid(fldsFrLnd, is_local%wrap%FBLnd_l, is_local%wrap%FBLnd_a, &
         consfmap=is_local%wrap%RH_l2a_consf, &
         consdmap=is_local%wrap%RH_l2a_consd, &
         bilnrmap=is_local%wrap%RH_l2a_bilnr, &
         patchmap=is_local%wrap%RH_l2a_patch, &
         string='l2a', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    if (is_local%wrap%h2a_active) then
      call shr_nuopc_methods_FB_Regrid(fldsFrRof, is_local%wrap%FBRof_h, is_local%wrap%FBRof_a, &
         consfmap=is_local%wrap%RH_h2a_consf, &
         consdmap=is_local%wrap%RH_h2a_consd, &
         bilnrmap=is_local%wrap%RH_h2a_bilnr, &
         patchmap=is_local%wrap%RH_h2a_patch, &
         string='h2a', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBOcn_a, trim(subname)//' FBOcn_a ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBIce_a, trim(subname)//' FBIce_a ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBLnd_a, trim(subname)//' FBLnd_a ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBRof_a, trim(subname)//' FBRof_a ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBAtmOcn_a, trim(subname)//' FBAtmOcn_a ', rc=rc)
    endif

    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBOcn_a, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBIce_a, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBLnd_a, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBRof_a, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforAtm, is_local%wrap%FBAtmOcn_a, rc=rc)

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBforAtm, trim(subname)//' FBforAtm ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      if (is_local%wrap%o2a_active) then
        call ESMF_FieldBundleWrite(is_local%wrap%FBOcn_a, 'fields_med_ocn_a.nc', &
          singleFile=.true., overwrite=.true., timeslice=is_local%wrap%atmcntr, &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif

      if (is_local%wrap%i2a_active) then
        call ESMF_FieldBundleWrite(is_local%wrap%FBIce_a, 'fields_med_ice_a.nc', &
          singleFile=.true., overwrite=.true., timeslice=is_local%wrap%atmcntr, &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    endif

    !---------------------------------------
    !--- custom calculations to atm
    !---------------------------------------

#if (1 == 0)
    !---  ocn and ice fraction for merges

    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(ocnwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
    do i=lbound(icewgt,1),ubound(icewgt,1)
      ocnwgt = 1.0_ESMF_KIND_R8 - icewgt
    enddo
    enddo

    !--- fill land mask every coupling from initial computation

    if (generate_landmask) then
      call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBforAtm, 'land_mask', dataPtr3, rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      do j=lbound(dataPtr3,2),ubound(dataPtr3,2)
      do i=lbound(dataPtr3,1),ubound(dataPtr3,1)
        dataPtr3(i,j) = land_mask(i,j)
      enddo
      enddo
    else
      call ESMF_LogWrite(trim(subname)//": ERROR generate_landmask must be true ", ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    !--- merges

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'surface_temperature' , & 
                                  is_local%wrap%FBOcn_a   ,'sea_surface_temperature',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'sea_ice_temperature',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_sensi_heat_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_sensi_heat_flx_atm_into_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_sensi_heat_flx_atm_into_ice',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_laten_heat_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_laten_heat_flx_atm_into_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_laten_heat_flx_atm_into_ice',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_up_lw_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_up_lw_flx_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_up_lw_flx_ice',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_evap_rate' , & 
                                  is_local%wrap%FBAtmOcn_a,'mean_evap_rate_atm_into_ocn',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'mean_evap_rate_atm_into_ice',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_zonal_moment_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'stress_on_air_ocn_zonal',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'stress_on_air_ice_zonal',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforAtm  ,'mean_merid_moment_flx' , & 
                                  is_local%wrap%FBAtmOcn_a,'stress_on_air_ocn_merid',ocnwgt, &
                                  is_local%wrap%FBIce_a   ,'stress_on_air_ice_merid',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    deallocate(ocnwgt)
#endif

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

#if (1 == 0)
! tcraig test data
    is_local%wrap%scalar_data(1) = 100._r8 + is_local%wrap%atmcntr
    is_local%wrap%scalar_data(2) = 110._r8 + is_local%wrap%atmcntr
#endif

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call shr_nuopc_methods_State_reset(is_local%wrap%NState_AtmExp, value=spval, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_AtmExp, trim(subname)//' AtmExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call shr_nuopc_methods_FB_copy(is_local%wrap%NState_AtmExp, is_local%wrap%FBforAtm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

#if (1 == 0)
    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_AtmExp, trim(subname)//' AtmExp_final ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to atm to file
      call NUOPC_Write(is_local%wrap%NState_AtmExp, &
        fldsToAtm%shortname(1:fldsToAtm%num), &
        "field_med_to_atm_", timeslice=is_local%wrap%atmcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
#endif
    
    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_atm

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine med_phases_prep_ocn(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_StateItem_Flag)   :: itemType
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(ESMF_MAXSTR)      :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:)
    real(ESMF_KIND_R8), pointer :: atmwgt(:),icewgt(:),customwgt(:)
    logical                     :: checkOK, checkOK1, checkOK2
    character(len=*),parameter  :: subname='(module_MEDIATOR:med_phases_prep_ocn)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------

    is_local%wrap%ocncntr = is_local%wrap%ocncntr + 1

    !---------------------------------------

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

#if (1 == 0)
    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      call NUOPC_Write(is_local%wrap%NState_OcnImp, &
        fldsFrOcn%shortname(1:fldsFrOcn%num), &
        "field_med_from_ocn_", timeslice=is_local%wrap%ocncntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
#endif

    !---------------------------------------
    !--- average atm, ice, lnd accumulators
    !---------------------------------------

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccA_B4avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccI_B4avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccL_B4avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccH_B4avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAO_B4avg ', rc=rc)
    endif

    call shr_nuopc_methods_FB_average(is_local%wrap%FBaccumAtm, is_local%wrap%accumcntAtm, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_average(is_local%wrap%FBaccumIce, is_local%wrap%accumcntIce, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_average(is_local%wrap%FBaccumLnd, is_local%wrap%accumcntLnd, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_average(is_local%wrap%FBaccumRof, is_local%wrap%accumcntRof, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    
    call shr_nuopc_methods_FB_average(is_local%wrap%FBaccumAtmOcn, is_local%wrap%accumcntAtmOcn, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccA_avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccI_avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccL_avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccH_avg ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAO_avg ', rc=rc)
    endif

    !---------------------------------------
    !--- regrid average atm+ice+lnd+rof fields to ocean grid
    !---------------------------------------

    if (is_local%wrap%a2o_active) then
      call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumAtm to FBAtm_o', ESMF_LOGMSG_INFO, rc=rc)
      call shr_nuopc_methods_FB_Regrid(fldsFrAtm, is_local%wrap%FBaccumAtm, is_local%wrap%FBAtm_o, &
        consfmap=is_local%wrap%RH_a2o_consf, &
        consdmap=is_local%wrap%RH_a2o_consd, &
        bilnrmap=is_local%wrap%RH_a2o_bilnr, &
        patchmap=is_local%wrap%RH_a2o_patch, &
        string='a2o', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    if (is_local%wrap%i2o_active) then
      call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumIce to FBIce_o', ESMF_LOGMSG_INFO, rc=rc)
      call shr_nuopc_methods_FB_Regrid(fldsFrIce, is_local%wrap%FBaccumIce, is_local%wrap%FBIce_o, &
        consfmap=is_local%wrap%RH_i2o_consf, &
        consdmap=is_local%wrap%RH_i2o_consd, &
        bilnrmap=is_local%wrap%RH_i2o_bilnr, &
        patchmap=is_local%wrap%RH_i2o_patch, &
        fcopymap=is_local%wrap%RH_i2o_fcopy, &
        string='i2o', rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBAtm_o, trim(subname)//' FBAtm_o ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBIce_o, trim(subname)//' FBIce_o ', rc=rc)
    endif

! tcx Xgrid
    ! XGrid intermediary required? instantiate FBXgrid FieldBundle?
    ! call ESMF_FieldBundleRegrid(is_local%wrap%FBaccumAtm, FBXgrid, is_local%wrap%RHa2x, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! call ESMF_FieldBundleRegrid(FBXgrid, is_local%wrap%FBforOcn  , is_local%wrap%RHx2o, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! tcraig temporarily copy
    
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforOcn, is_local%wrap%FBAtm_o, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforOcn, is_local%wrap%FBIce_o, rc=rc)
    call shr_nuopc_methods_FB_copy(is_local%wrap%FBforOcn, is_local%wrap%FBAccumAtmOcn, rc=rc)

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFregrid ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to ocn
    !---------------------------------------

!    if (dbug_flag > 1) then
!      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFcc ', rc=rc)
!    endif

    !---------------------------------------
    !--- merges to ocn
    !---------------------------------------

#if (1 == 0)

    ! atm and ice fraction
    call shr_nuopc_methods_FB_GetFldPtr(is_local%wrap%FBIce_o, 'ice_fraction', icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(atmwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
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
!    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_evap_rate' , & 
!                                  is_local%wrap%FBAtm_o, 'mean_laten_heat_flux' ,customwgt, rc=rc)
!    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction)
    !-------------

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_fprec_rate' , & 
                                  is_local%wrap%FBAtm_o, 'mean_fprec_rate' ,atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_down_lw_flx' , & 
                                  is_local%wrap%FBAtm_o, 'mean_down_lw_flx' ,atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_evap_rate' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_evap_rate_atm_into_ocn' ,atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_laten_heat_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_laten_heat_flx_atm_into_ocn' ,atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_net_lw_flx' , & 
                                  is_local%wrap%FBAtm_o      ,'mean_down_lw_flx  ' ,atmwgt, &
                                  is_local%wrap%FBAccumAtmOcn,'mean_up_lw_flx_ocn' ,atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_up_lw_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_up_lw_flx_ocn' ,atmwgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction) + field_from_ice * (ice_fraction)
    !-------------

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_prec_rate' , & 
                                  is_local%wrap%FBAtm_o, 'mean_prec_rate' ,atmwgt, &
                                  is_local%wrap%FBIce_o, 'mean_fresh_water_to_ocean_rate', icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn     ,'mean_sensi_heat_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'mean_sensi_heat_flx_atm_into_ocn' ,atmwgt, &
                                  is_local%wrap%FBIce_o      ,'net_heat_flx_to_ocn', icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn  ,'mean_zonal_moment_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'stress_on_air_ocn_zonal',atmwgt, &
                                  is_local%wrap%FBIce_o   ,'stress_on_ocn_ice_zonal',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn  ,'mean_merid_moment_flx' , & 
                                  is_local%wrap%FBAccumAtmOcn,'stress_on_air_ocn_merid',atmwgt, &
                                  is_local%wrap%FBIce_o   ,'stress_on_ocn_ice_merid',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! netsw_for_ocn = downsw_from_atm * (1-ocn_albedo) * (1-ice_fraction) + pensw_from_ice * (ice_fraction)
    !-------------

    customwgt = atmwgt * (1.0 - 0.06)
!    customwgt = (1.0 - 0.06)
    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_vis_dir_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_vis_dir_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_vis_dir_flx' ,icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_vis_dif_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_vis_dif_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_vis_dif_flx',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_ir_dir_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_ir_dir_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_ir_dir_flx',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_nuopc_methods_FB_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_ir_dif_flx' , & 
                                  is_local%wrap%FBAtm_o ,'mean_down_sw_ir_dif_flx',customwgt, &
                                  is_local%wrap%FBIce_o ,'mean_net_sw_ir_dif_flx',icewgt, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! End merges
    !-------------

    deallocate(atmwgt,customwgt)

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFmrg ', rc=rc)
    endif
    
#endif

    !---------------------------------------
    !--- zero accumulator
    !---------------------------------------

    is_local%wrap%accumcntAtm = 0
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumAtm, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    is_local%wrap%accumcntIce = 0
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumIce, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    is_local%wrap%accumcntLnd = 0
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumLnd, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    is_local%wrap%accumcntRof = 0
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumRof, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    is_local%wrap%accumcntAtmOcn = 0
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBaccumAtmOcn, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
!tcx      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBacc_AFzero ', rc=rc)
!dcr      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBacc_AFzero ', rc=rc)
!dcr      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBacc_AFzero ', rc=rc)
    endif

    !---------------------------------------
    !--- update local scalar data
    !---------------------------------------

#if (1 == 0)
! tcraig test data
    is_local%wrap%scalar_data(1) = 200._r8 + is_local%wrap%ocncntr
    is_local%wrap%scalar_data(2) = 210._r8 + is_local%wrap%ocncntr
#endif

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call shr_nuopc_methods_State_reset(is_local%wrap%NState_OcnExp, value=spval, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_OcnExp, trim(subname)//' es_AF99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call shr_nuopc_methods_FB_copy(is_local%wrap%NState_OcnExp, is_local%wrap%FBforOcn, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_OcnExp, trim(subname)//' es_AFcp ', rc=rc)
    endif

#if (1 == 0)
    if (statewrite_flag) then
      ! write the fields exported to ocn to file
      call NUOPC_Write(is_local%wrap%NState_OcnExp, &
        fldsToOcn%shortname(1:fldsToOcn%num), &
        "field_med_to_ocn_", timeslice=is_local%wrap%ocncntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
#endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_prep_ocn

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine med_phases_accum_fast(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:med_phases_accum_fast)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
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

#if (1 == 0)
    if (statewrite_flag) then
      ! write the fields imported from atm to file
      call NUOPC_Write(is_local%wrap%NState_AtmImp, &
        fldsFrAtm%shortname(1:fldsFrAtm%num), &
        "field_med_from_atm_", timeslice=is_local%wrap%atmcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      ! write the fields imported from ice to file
      call NUOPC_Write(is_local%wrap%NState_IceImp, &
        fldsFrIce%shortname(1:fldsFrIce%num), &
        "field_med_from_ice_", timeslice=is_local%wrap%atmcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      ! write the fields imported from lnd to file
      call NUOPC_Write(is_local%wrap%NState_LndImp, &
        fieldNameList=fldsFrLnd%shortname(1:fldsFrLnd%num), &
        fileNamePrefix="field_med_from_lnd_", timeslice=is_local%wrap%atmcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

      ! write the fields imported from rof to file
      call NUOPC_Write(is_local%wrap%NState_RofImp, &
        fieldNameList=fldsFrRof%shortname(1:fldsFrRof%num), &
        fileNamePrefix="field_med_from_rof_", timeslice=is_local%wrap%atmcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
#endif

    !---------------------------------------
    !--- atm, ice, lnd, rof accumulator for ocean
    !---------------------------------------

    if (dbug_flag > 1) then
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call shr_nuopc_methods_State_diagnose(is_local%wrap%NState_RofImp, trim(subname)//' RofImp ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_B4accum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccIce_B4accum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccLnd_B4accum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccRof_B4accum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAtmOcn_B4accum ', rc=rc)
    endif

    call shr_nuopc_methods_FB_accum(is_local%wrap%FBaccumAtm, is_local%wrap%NState_AtmImp, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    is_local%wrap%accumcntAtm = is_local%wrap%accumcntAtm + 1

    call shr_nuopc_methods_FB_accum(is_local%wrap%FBaccumIce, is_local%wrap%NState_IceImp, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    is_local%wrap%accumcntIce = is_local%wrap%accumcntIce + 1

    call shr_nuopc_methods_FB_accum(is_local%wrap%FBaccumLnd, is_local%wrap%NState_LndImp, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    is_local%wrap%accumcntLnd = is_local%wrap%accumcntLnd + 1

    call shr_nuopc_methods_FB_accum(is_local%wrap%FBaccumRof, is_local%wrap%NState_RofImp, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    is_local%wrap%accumcntRof = is_local%wrap%accumcntRof + 1

    call shr_nuopc_methods_FB_accum(is_local%wrap%FBaccumAtmOcn, is_local%wrap%FBAtmOcn_o, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    is_local%wrap%accumcntAtmOcn = is_local%wrap%accumcntAtmOcn + 1

    if (dbug_flag > 1) then
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_AFaccum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccIce_AFaccum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccLnd_AFaccum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumRof, trim(subname)//' FBaccRof_AFaccum ', rc=rc)
      call shr_nuopc_methods_FB_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAtmOcn_AFaccum ', rc=rc)
    endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_phases_accum_fast

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

end module med_phases_mod

