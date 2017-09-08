module med_fraction_mod

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
  
  integer            :: dbrc
  integer           , parameter :: dbug_flag   = med_constants_dbug_flag
  logical           , parameter :: statewrite_flag = med_constants_statewrite_flag
  real(ESMF_KIND_R8), parameter :: spval_init  = med_constants_spval_init
  real(ESMF_KIND_R8), parameter :: spval       = med_constants_spval
  real(ESMF_KIND_R8), parameter :: czero       = med_constants_czero
  character(*),parameter :: u_FILE_u = &
    __FILE__

  public med_fraction_init
  public med_fraction_set

  character(len=5),parameter,public,dimension(5) :: fraclist_a = (/'afrac','ifrac','ofrac','lfrac','lfrin'/)
  character(len=5),parameter,public,dimension(5) :: fraclist_o = (/'afrac','ifrac','ofrac','ifrad','ofrad'/)
  character(len=5),parameter,public,dimension(3) :: fraclist_i = (/'afrac','ifrac','ofrac'/)
  character(len=5),parameter,public,dimension(3) :: fraclist_l = (/'afrac','lfrac','lfrin'/)
  character(len=5),parameter,public,dimension(2) :: fraclist_g = (/'gfrac','lfrac'/)
  character(len=5),parameter,public,dimension(2) :: fraclist_r = (/'lfrac','rfrac'/)
  character(len=5),parameter,public,dimension(1) :: fraclist_w = (/'wfrac'/)
  
  !--- standard ---                                                                             
  real(ESMF_KIND_R8),parameter :: eps_fracsum = 1.0e-02   ! allowed error in sum of fracs                 
  real(ESMF_KIND_R8),parameter :: eps_fracval = 1.0e-02   ! allowed error in any frac +- 0,1              
  real(ESMF_KIND_R8),parameter :: eps_fraclim = 1.0e-03   ! truncation limit in fractions_a(lfrac)        
  logical ,parameter :: atm_frac_correct = .false. ! turn on frac correction on atm grid        
  !--- standard plus atm fraction consistency ---                                               
  !  real(ESMF_KIND_R8),parameter :: eps_fracsum = 1.0e-12   ! allowed error in sum of fracs              
  !  real(ESMF_KIND_R8),parameter :: eps_fracval = 1.0e-02   ! allowed error in any frac +- 0,1           
  !  real(ESMF_KIND_R8),parameter :: eps_fraclim = 1.0e-03   ! truncation limit in fractions_a(lfrac)     
  !  logical ,parameter :: atm_frac_correct = .true. ! turn on frac correction on atm grid      
  !--- unconstrained and area conserving? ---                                                   
  !  real(ESMF_KIND_R8),parameter :: eps_fracsum = 1.0e-12   ! allowed error in sum of fracs              
  !  real(ESMF_KIND_R8),parameter :: eps_fracval = 1.0e-02   ! allowed error in any frac +- 0,1           
  !  real(ESMF_KIND_R8),parameter :: eps_fraclim = 1.0e-20   ! truncation limit in fractions_a(lfrac)     
  !  logical ,parameter :: atm_frac_correct = .true. ! turn on frac correction on atm grid      

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine med_fraction_init(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This subroutine initializes the fractions
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr(:)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    integer                     :: i,j,n
    character(len=*),parameter  :: subname='(med_fraction_init)'
    
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
    !--- zero out FBfracs
    !---------------------------------------

    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_a, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_o, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_i, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_l, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_r, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_g, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_nuopc_methods_FB_reset(is_local%wrap%FBfrac_w, value=czero, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- Initialize fractions on atm grid/decomp
    !---------------------------------------

    if (is_local%wrap%atm_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_a, 'afrac', dataPtr, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        dataPtr = 1.0_ESMF_KIND_R8
      endif
      if (is_local%wrap%ocn_present) then
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_a, 'afrac', &
             is_local%wrap%FBfrac_o, 'afrac', is_local%wrap%RH_a2o_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
      if (is_local%wrap%ice_present) then
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_a, 'afrac', &
             is_local%wrap%FBfrac_i, 'afrac', is_local%wrap%RH_a2i_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    endif

    !---------------------------------------
    !--- Initialize fractions on glc grid decomp
    !---------------------------------------

    if (is_local%wrap%glc_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_g, 'gfrac', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBglc_g , 'frac' , dataPtr2, rc=rc)
        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
          dataPtr1 = dataPtr2
        endif
      endif
    endif

    !---------------------------------------
    !--- Initialize fractions on land grid decomp, just an initial "guess", updated later
    !---------------------------------------

    if (is_local%wrap%lnd_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_l, 'lfrin', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBlnd_l , 'Sl_lfrin' , dataPtr2, rc=rc)
        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
          dataPtr1 = dataPtr2
        endif
        if (is_local%wrap%atm_present) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_l, 'lfrin', &
               is_local%wrap%FBfrac_a, 'lfrin', is_local%wrap%RH_l2a_consf, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_a, 'afrac', &
               is_local%wrap%FBfrac_l, 'afrac', is_local%wrap%RH_a2l_consf, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        endif
      endif
    endif

    !---------------------------------------
    !--- Initialize fractions on rof grid/decomp
    !---------------------------------------

    if (is_local%wrap%rof_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_r, 'rfrac', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
         dataPtr1 = 1.0_ESMF_KIND_R8
!        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBrof_r , 'frac' , dataPtr2, rc=rc)
!        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
!          dataPtr1 = dataPtr2
!        endif
      endif
    endif

    !---------------------------------------
    !--- Initialize fractions on wav grid decomp
    !---------------------------------------

    if (is_local%wrap%wav_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_w, 'wfrac', dataPtr, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        dataPtr = 1.0_ESMF_KIND_R8
      endif
    endif

    !---------------------------------------
    !--- Initialize fractions on ice grid/decomp (initialize ice fraction to zero)
    !---------------------------------------

    if (is_local%wrap%ice_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_i, 'ofrac', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBice_i , 'Si_imask' , dataPtr2, rc=rc)
        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
          dataPtr1 = dataPtr2
        endif
        if (is_local%wrap%atm_present) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_i, 'ofrac', &
               is_local%wrap%FBfrac_a, 'ofrac', is_local%wrap%RH_i2a_consf, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_a, 'afrac', &
               is_local%wrap%FBfrac_i, 'afrac', is_local%wrap%RH_a2i_consf, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        endif
      endif
    endif

    !---------------------------------------
    !--- Initialize fractions on ocean grid/decomp
    !--- These are initialized the same as for ice
    !---------------------------------------

    if (is_local%wrap%ocn_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_o, 'ofrac', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBocn_o , 'So_omask' , dataPtr2, rc=rc)
        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
          dataPtr1 = dataPtr2
        endif
        if (is_local%wrap%atm_present) then
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_o, 'ofrac', &
               is_local%wrap%FBfrac_a, 'ofrac', is_local%wrap%RH_o2a_consf, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_a, 'afrac', &
               is_local%wrap%FBfrac_o, 'afrac', is_local%wrap%RH_a2o_consf, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        endif
      endif
    endif

    !---------------------------------------
    !--- Set ofrac and lfrac on atm grid.  These should actually be mapo2a of
    !--- ofrac and lfrac but we can't map lfrac from o2a due to masked mapping
    !---  weights.  So we have to settle for a residual calculation that is
    !---  truncated to zero to try to preserve "all ocean" cells.
    !---------------------------------------

    if (is_local%wrap%atm_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_a, 'ofrac', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_a, 'lfrac', dataPtr2, rc=rc)
        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
          if (is_local%wrap%ocn_present .or. is_local%wrap%ice_present) then
            dataPtr2 = 1.0_ESMF_KIND_R8 - dataPtr1
            if (atm_frac_correct) then
              where (dataPtr2 < eps_fraclim) dataPtr1 = 1.0_ESMF_KIND_R8
            endif
            where (dataPtr2 < eps_fraclim) dataPtr2 = 0.0_ESMF_KIND_R8
          elseif (is_local%wrap%lnd_present) then
            dataPtr1 = 1.0_ESMF_KIND_R8 - dataPtr2
            if (atm_frac_correct) then
              where (dataPtr1 < eps_fraclim) dataPtr2 = 1.0_ESMF_KIND_R8
            endif
            where (dataPtr1 < eps_fraclim) dataPtr1 = 0.0_ESMF_KIND_R8
          endif
        endif
      endif
    endif

    !---------------------------------------
    !--- finally, set fractions_l(lfrac) from fractions_a(lfrac)
    !--- and fractions_r(lfrac) from fractions_l(lfrac)
    !--- and fractions_g(lfrac) from fractions_l(lfrac)
    !---------------------------------------

    if (is_local%wrap%lnd_present) then
      if (is_local%wrap%atm_present) then
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_a, 'lfrac', &
             is_local%wrap%FBfrac_l, 'lfrac', is_local%wrap%RH_a2l_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_l, 'lfrin', dataPtr1, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_l, 'lfrac', dataPtr2, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        dataPtr2 = dataPtr1
      endif

      if (is_local%wrap%rof_present) then
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_l, 'lfrac', &
             is_local%wrap%FBfrac_r, 'lfrac', is_local%wrap%RH_l2r_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
      if (is_local%wrap%glc_present) then
! tcraig l2g_consf does not exist yet
!        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_l, 'lfrac', &
!             is_local%wrap%FBfrac_g, 'lfrac', is_local%wrap%RH_l2g_consf, rc=rc)
!        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    endif

    !---------------------------------------
    !--- update time varying fractions
    !---------------------------------------

    call med_fraction_set(gcomp, rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- check fractions
    !---------------------------------------

    call med_fraction_check(is_local%wrap%FBfrac_a, 'atmfrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_fraction_check(is_local%wrap%FBfrac_l, 'lndfrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_fraction_check(is_local%wrap%FBfrac_o, 'ocnfrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_fraction_check(is_local%wrap%FBfrac_i, 'icefrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_fraction_check(is_local%wrap%FBfrac_r, 'roffrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_fraction_check(is_local%wrap%FBfrac_g, 'glcfrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
    call med_fraction_check(is_local%wrap%FBfrac_w, 'wavfrac init', rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_fraction_init

  !-----------------------------------------------------------------------------

  subroutine med_fraction_set(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This subroutine initializes the fractions
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr(:)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:),dataPtr2(:),dataPtr3(:),dataPtr4(:)
    integer                     :: i,j,n
    character(len=*),parameter  :: subname='(med_fraction_set)'
    
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
    !--- update ice fraction
    !---------------------------------------

    if (is_local%wrap%ice_present) then
      call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_i, 'ifrac', dataPtr1, rc=rc)
      if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
        call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_i, 'ofrac', dataPtr3, rc=rc)
        if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
          call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBice_i , 'Si_ifrac' , dataPtr2, rc=rc)
          if (.not. shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) then
            dataPtr1 = dataPtr2
            dataPtr3 = 1.0_ESMF_KIND_R8 - dataPtr1
          endif
        endif
      endif

      if (is_local%wrap%ocn_present) then
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_i, 'ifrac', is_local%wrap%FBfrac_o, 'ifrac', is_local%wrap%RH_i2o_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_i, 'ofrac', is_local%wrap%FBfrac_o, 'ofrac', is_local%wrap%RH_i2o_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
      endif

      if (is_local%wrap%atm_present) then
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_i, 'ifrac', is_local%wrap%FBfrac_a, 'ifrac', is_local%wrap%RH_i2a_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        call shr_nuopc_methods_FB_FieldRegrid(is_local%wrap%FBfrac_i, 'ofrac', is_local%wrap%FBfrac_a, 'ofrac', is_local%wrap%RH_i2a_consf, rc=rc)
        if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
        if (atm_frac_correct) then
          call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_a, 'ifrac', dataPtr1, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_a, 'ofrac', dataPtr2, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          call shr_nuopc_methods_FB_getFldPtr(is_local%wrap%FBfrac_a, 'lfrac', dataPtr3, rc=rc)
          if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return
          where (dataPtr1 + dataPtr2 > 0.0_ESMF_KIND_R8)
            dataPtr1 = dataPtr1 * ((1.0_ESMF_KIND_R8 - dataPtr3)/(dataPtr2+dataPtr1))
            dataPtr2 = dataPtr2 * ((1.0_ESMF_KIND_R8 - dataPtr3)/(dataPtr2+dataPtr1))
          elsewhere
            dataPtr1 = 0.0_ESMF_KIND_R8
            dataPtr2 = 0.0_ESMF_KIND_R8
          end where
        endif
      endif
    endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_fraction_set

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine med_fraction_check(FB, string, rc)
    type(ESMF_FieldBundle)  :: FB
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc
    
    ! This subroutine initializes the fractions
    
    character(len=*),parameter  :: subname='(med_fraction_check)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    !---------------------------------------
    !--- write fraction info
    !---------------------------------------

    call shr_nuopc_methods_FB_diagnose(FB, subname//trim(string), rc=rc)
    if (shr_nuopc_methods_ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine med_fraction_check

  !-----------------------------------------------------------------------------

end module med_fraction_mod

