module shr_nuopc_flds_mod

  !====================================================================
  !   New standardized naming convention
  !====================================================================
  !
  !  ---------
  !  definitions:
  !  ---------
  !  state-prefix
  !    first 3 characters: Sx_, Sa_, Si_, Sl_, So_
  !    one letter indices: x,a,l,i,o,g,r
  !    x => mediator (mapping, merging, atm/ocn flux calc done on mediator procs)
  !    a => atm
  !    l => lnd
  !    i => ice
  !    o => ocn
  !    g => glc
  !    r => rof
  !    w => wav
  !
  !  state-name
  !    what follows state prefix
  !
  !  flux-prefix
  !    first 5 characters: Flmn__
  !    lm => between components l and m
  !    n  => computed by component n
  !    example: Fioi => ice/ocn flux computed by ice
  !    example: Fall => atm/lnd flux computed by lnd
  !    If flux prefix has first letter of P (so first five characters are PFlmn_)
  !    then flux is passed straight through without scaling by the corresponding fraction)
  !
  !  flux-name
  !    what follows flux-prefix
  !
  !  ---------
  !  rules:
  !  ---------
  !  1) states:
  !     a) atm fields that HAVE a state-prefix of Sx_ in flds_x2a
  !        rule: will merge all identical values of the state-names from
  !           flds_i2x
  !           flds_l2x
  !           flds_o2x
  !           flds_xao
  !         to obtain output state-name in flds_x2a
  !
  !        rule: to merge input states that originate in the
  !           lnd (l2x_a) will be scaled by the lndfrac
  !           ice (i2x_a) will be scaled by the icefrac
  !           med (xao_a) will be scaled by the ocnfrac
  !           ocn (o2x_a) will be scaled by the ocnfrac
  !
  !        example:
  !           flds_l2x = "Sl_t"
  !           flds_i2x = "Si_t"
  !           flds_o2x = "So_t"
  !           flds_x2a = "Sx_t"
  !           fields Sl_t, Si_t, So_t, in
  !           fields l2x_a, i2x_a, o2x_a will be
  !           merged to obtain field Sx_t in field x2a_a
  !
  !     b) atm fields that DO NOT HAVE a state-prefix of Sx_ in flds_x2a
  !        rule: copy directly all variables that identical state-prefix
  !               AND state-name in
  !           flds_i2x and flds_x2a
  !           flds_l2x and flds_x2a
  !           flds_o2x and flds_x2a
  !           flds_xao and flds_x2a
  !
  !        example
  !           flds_i2x = ":Si_snowh"
  !           flds_x2a = ":Si_snowh"
  !           field of Si_snowh in i2x_a will be copied to
  !           field Si_snowh in x2a_a
  !
  !  2) fluxes:
  !     rule: will merge all identical values of the flux-names from
  !         flds_i2x
  !         flds_l2x
  !         flds_o2x
  !         flds_xao
  !       to obtain output state-name in flds_x2a
  !
  !     rule: input flux fields that originate in the
  !         lnd (l2x_a) will be scaled by the lndfrac
  !         ice (i2x_a) will be scaled by the icefrac
  !            - ignore all fluxes that are ice/ocn fluxes (e.g. Fioi_)
  !         med (xao_a) will be scaled by the ocnfrac
  !         ocn (o2x_a) will be scaled by the ocnfrac+icefrac
  !
  !====================================================================
  !
  !   New user specified fields
  !
  !====================================================================
  ! New fields that are user specidied can be added as namelist variables
  ! by the user in the namelist flds_user using the namelist variable
  ! array cplflds_customs. The user specified new fields must follow the
  ! above naming convention.
  ! As an example, say you want to add a new state 'foo' that is passed
  ! from the land to the atm - you would do this as follows
  !    &flds_user
  !       cplflds_custom = 'Sa_foo->a2x', 'Sa_foo->x2a'
  !    /
  ! This would add the field 'Sa_foo' to the character strings defining the
  ! fields a2x and x2a. It is assumed that code would need to be
  ! introduced in the atm and land components to deal with this new field.
  ! Currently, the only way to add this is to edit $CASEROOT/user_nl_cpl
  !====================================================================
  !
  !  fields use cases
  !
  !====================================================================
  ! Previously, new fields that were needed to be passed between components
  ! for certain compsets were specified by cpp-variables. This has been
  ! modified to now be use cases. The use cases are specified in the
  ! namelist cpl_flds_inparm and are currently triggered by the xml
  ! variables CCSM_VOC, CCSM_BGC and GLC_NEC.
  !====================================================================

  use shr_kind_mod      , only : CX => shr_kind_CX, CXX => shr_kind_CXX, CS=>shr_kind_CS
  use shr_sys_mod       , only : shr_sys_abort
  use seq_comm_mct      , only : seq_comm_iamroot, seq_comm_setptrs, llogunit => logunit
  use seq_drydep_mod    , only : seq_drydep_init, seq_drydep_readnl, lnd_drydep
  use shr_megan_mod     , only : shr_megan_readnl, shr_megan_mechcomps_n
  use shr_fire_emis_mod , only : shr_fire_emis_readnl, shr_fire_emis_mechcomps_n, shr_fire_emis_ztop_token
  use shr_carma_mod     , only : shr_carma_readnl
  use shr_ndep_mod      , only : shr_ndep_readnl
  use shr_flds_mod      , only : shr_flds_dom_coord, shr_flds_dom_other

  implicit none
  public

  interface flds_lookup; module procedure &
       fld_metadata_get
  end interface flds_lookup

  integer, parameter, private :: CSS = 256  ! use longer short character
  integer, parameter, private :: CLL = 1024

  !----------------------------------------------------------------------------
  ! public routines
  !----------------------------------------------------------------------------

  public :: shr_nuopc_flds_set
  public :: shr_nuopc_flds_get_num_entries
  public :: shr_nuopc_flds_get_entry

  !----------------------------------------------------------------------------
  ! private routines
  !----------------------------------------------------------------------------

  private :: fld_dom_add
  private :: fld_add
  private :: fld_set_glc_elevclass
  private :: fld_metadata_set
  private :: fld_metadata_get
  private :: fld_metadata_entry

  !----------------------------------------------------------------------------
  ! other field lists
  !----------------------------------------------------------------------------

  character(len=CXX) :: drydep_fields       ! List of dry-deposition fields
  character(len=CXX) :: megan_voc_fields    ! List of MEGAN VOC emission fields
  character(len=CXX) :: fire_emis_fields    ! List of fire emission fields
  character(len=CX)  :: carma_fields        ! List of CARMA fields from lnd->atm
  character(len=CX)  :: ndep_fields         ! List of nitrogen deposition fields from atm->lnd/ocn
  integer            :: ice_ncat            ! number of sea ice thickness categories
  logical            :: flds_i2o_per_cat    ! .true. if select per ice thickness category fields are passed from ice to ocean
  logical            :: add_ndep_fields     ! .true. => add ndep fields

  !----------------------------------------------------------------------------
  ! metadata
  !----------------------------------------------------------------------------

  character(len=*),parameter :: undef     = 'undefined'
  integer         ,parameter :: nmax      = 1000        ! maximum number of entries in lookup_entry
  integer                    :: n_entries = 0           ! actual number of entries in lookup_entry
  character(len=CSS), dimension(nmax, 4) :: lookup_entry = undef

  !----------------------------------------------------------------------------
  ! for the scalars
  !----------------------------------------------------------------------------

  character(len=*) ,parameter :: flds_scalar_name = "cpl_scalars"
  integer , parameter :: flds_scalar_index_nx                 = 1
  integer , parameter :: flds_scalar_index_ny                 = 2
  integer , parameter :: flds_scalar_index_precip_fact        = 3
  integer , parameter :: flds_scalar_index_nextsw_cday        = 4
  integer , parameter :: flds_scalar_index_dead_comps         = 5
  integer , parameter :: flds_scalar_index_rofice_present     = 6  ! does rof have iceberg coupling on
  integer , parameter :: flds_scalar_index_flood_present      = 7  ! does rof have flooding on
  integer , parameter :: flds_scalar_index_ocnrof_prognostic  = 8  ! does ocn need rof data
  integer , parameter :: flds_scalar_index_iceberg_prognostic = 9  ! does ice model support icebergs
  integer , parameter :: flds_scalar_index_glclnd_present     = 10 ! does glc have land coupling fields on
  integer , parameter :: flds_scalar_index_glcocn_present     = 11 ! does glc have ocean runoff on
  integer , parameter :: flds_scalar_index_glcice_present     = 12 ! does glc have iceberg coupling on
  integer , parameter :: flds_scalar_index_glc_valid_input    = 13 ! does glc have is valid accumulated data being sent to it?
                                                                   ! (only valid of glc_prognostic is .true.)
  integer , parameter :: flds_scalar_index_glc_coupled        = 14 ! does glc send fluxes to other components
                                                                   ! (only relevant if glc_present is .true.)
  integer , parameter :: flds_scalar_num                      = 14

  !----------------------------------------------------------------------------
  ! for the domain
  !----------------------------------------------------------------------------

  !character(CXX) :: flds_dom_coord
  !character(CXX) :: flds_dom_other

  !----------------------------------------------------------------------------
  ! for the combined state and flux fields
  !----------------------------------------------------------------------------

  character(CXX) :: flds_a2x = ''
  character(CXX) :: flds_a2x_map = ''
  character(CXX) :: flds_x2a = ''
  character(CXX) :: flds_x2a_map = ''

  character(CXX) :: flds_i2x = ''
  character(CXX) :: flds_i2x_map = ''
  character(CXX) :: flds_x2i = ''
  character(CXX) :: flds_x2i_map = ''

  character(CXX) :: flds_l2x = ''
  character(CXX) :: flds_l2x_map = ''
  character(CXX) :: flds_l2x_to_glc = ''
  character(CXX) :: flds_l2x_to_glc_map = ''
  character(CXX) :: flds_x2l = ''
  character(CXX) :: flds_x2l_map = ''
  character(CXX) :: flds_x2l_from_glc = ''
  character(CXX) :: flds_x2l_from_glc_map = ''

  character(CXX) :: flds_o2x = ''
  character(CXX) :: flds_o2x_map = ''
  character(CXX) :: flds_x2o = ''
  character(CXX) :: flds_x2o_map = ''

  character(CXX) :: flds_g2x = ''
  character(CXX) :: flds_g2x_map = ''
  character(CXX) :: flds_g2x_to_lnd = ''
  character(CXX) :: flds_g2x_to_lnd_map = ''
  character(CXX) :: flds_x2g = ''
  character(CXX) :: flds_x2g_map = ''

  character(CXX) :: flds_w2x = ''
  character(CXX) :: flds_w2x_map = ''
  character(CXX) :: flds_x2w = ''
  character(CXX) :: flds_x2w_map = ''

  character(CXX) :: flds_xao = ''
  character(CXX) :: flds_xao_map = ''
  character(CXX) :: flds_xao_albedo = ''
  character(CXX) :: flds_xao_albedo_map = ''
  character(CXX) :: flds_xao_diurnl = ''      ! for diurnal cycle
  character(CXX) :: flds_xao_diurnl_map = ''  ! for diurnal cycle

  character(CXX) :: flds_r2x = ''
  character(CXX) :: flds_r2x_map = ''
  character(CXX) :: flds_x2r = ''
  character(CXX) :: flds_x2r_map = ''
  character(CXX) :: flds_r2o_liq = ''
  character(CXX) :: flds_r2o_liq_map = ''
  character(CXX) :: flds_r2o_ice = ''
  character(CXX) :: flds_r2o_ice_map = ''

!----------------------------------------------------------------------------
contains
!----------------------------------------------------------------------------

  subroutine shr_nuopc_flds_set(nmlfile, ID)

    ! uses:
    use shr_file_mod,      only : shr_file_getUnit, shr_file_freeUnit
    use shr_mpi_mod,       only : shr_mpi_bcast
    use glc_elevclass_mod, only : glc_elevclass_init

    ! input/output parameters:
    character(len=*) , intent(in) :: nmlfile    ! Name-list filename
    integer          , intent(in) :: ID         ! seq_comm ID

    ! local variables:
    integer            :: mpicom ! MPI communicator
    integer            :: ierr   ! I/O error code
    integer            :: unitn  ! Namelist unit number to read
    character(len=CSS) :: attname
    character(len=CSS) :: units
    character(len=CSS) :: longname
    character(len=CSS) :: stdname
    integer            :: num
    character(len=  2) :: cnum
    character(len=CSS) :: name

    !------ namelist -----
    character(len=CSS)  :: fldname, fldflow
    integer :: i,n

    ! use cases namelists
    logical :: flds_co2a
    logical :: flds_co2b
    logical :: flds_co2c
    logical :: flds_wiso
    integer :: glc_nec

    namelist /seq_cplflds_inparm/  &
         flds_co2a, flds_co2b, flds_co2c, flds_wiso, glc_nec, ice_ncat, flds_i2o_per_cat

    integer,  parameter :: nfldmax = 200
    character(len=CLL)  :: cplflds_custom(nfldmax) = '' ! user specified new fields

    namelist /seq_cplflds_userspec/ cplflds_custom

    character(len=*),parameter :: subname = '(shr_nuopc_flds_set) '
    !-------------------------------------------------------------------------------

    call seq_comm_setptrs(ID,mpicom=mpicom)

    !---------------------------------------------------------------------------
    ! Read in namelist for use cases
    !---------------------------------------------------------------------------
    ! TODO: permit duplicates to occur - then check for this in flds_add
    ! TODO: add entries for lookup entry table for custom fields
    !---------------------------------------------------------------------------

    if (seq_comm_iamroot(ID)) then
       flds_co2a        = .false.
       flds_co2b        = .false.
       flds_co2c        = .false.
       flds_wiso        = .false.
       glc_nec          = 0
       ice_ncat         = 1
       flds_i2o_per_cat = .false.

       unitn = shr_file_getUnit()
       write(llogunit, "(A)") subname//': read seq_cplflds_inparm namelist from: '//trim(nmlfile)
       open( unitn, file=trim(nmlfile), status='old' )
       ierr = 1
       do while( ierr /= 0 )
          read(unitn,nml=seq_cplflds_inparm,iostat=ierr)
          if (ierr < 0) then
             call shr_sys_abort(subname//"ERROR: namelist read returns an EOF or EOR condition" )
          end if
       end do
       close(unitn)
       call shr_file_freeUnit( unitn )
    end if
    call shr_mpi_bcast(flds_co2a    , mpicom)
    call shr_mpi_bcast(flds_co2b    , mpicom)
    call shr_mpi_bcast(flds_co2c    , mpicom)
    call shr_mpi_bcast(flds_wiso    , mpicom)
    call shr_mpi_bcast(glc_nec      , mpicom)
    call shr_mpi_bcast(ice_ncat     , mpicom)
    call shr_mpi_bcast(flds_i2o_per_cat, mpicom)

    call glc_elevclass_init(glc_nec)

    !---------------------------------------------------------------------------
    ! Read in namelists for user specified new fields
    !---------------------------------------------------------------------------
    ! TODO: fix this for
    ! TODO: permit duplicates to occur - then check for this in flds_add
    ! TODO: add entries for lookup entry table for custom fields
    !---------------------------------------------------------------------------

    ! if (seq_comm_iamroot(ID)) then
    !    cplflds_custom(:) = ' '

    !    unitn = shr_file_getUnit()
    !    write(llogunit, "(A)") subname//': read seq_cplflds_userspec namelist from: '&
    !         //trim(nmlfile)
    !    open( unitn, file=trim(nmlfile), status='old' )
    !    ierr = 1
    !    do while( ierr /= 0 )
    !       read(unitn,nml=seq_cplflds_userspec,iostat=ierr)
    !       if (ierr < 0) then
    !          call shr_sys_abort( &
    !               subname//"ERROR: namelist read returns an EOF or EOR condition" )
    !       end if
    !    end do
    !    close(unitn)
    !    call shr_file_freeUnit( unitn )
    ! end if
    ! do n = 1, nfldmax
    !    call shr_mpi_bcast(cplflds_custom(n), mpicom)
    ! end do

    ! add customized fields through mediator

    ! do n = 1,nfldmax
    !    if (cplflds_custom(n) /= ' ') then
    !       i = scan(cplflds_custom(n),'->')
    !       fldname = trim(adjustl(cplflds_custom(n)(:i-1)))
    !       fldflow = trim(adjustl(cplflds_custom(n)(i+2:)))

    !       if (fldname(1:1) == 'S') then
    !          is_state = .true.
    !          is_flux  = .false.
    !       else if (fldname (1:1) == 'F')  then
    !          is_state = .false.
    !          is_flux  = .true.
    !       else if (fldname (1:2) == 'PF') then
    !          is_state = .false.
    !          is_flux  = .true.
    !       else
    !          write(llogunit,*) subname//'ERROR: fldname must start with S,F,P, not ',trim(fldname)
    !          call shr_sys_abort(subname//"ERROR: fldname must start with S, F, or P")
    !       end if

    !       select case (trim(fldflow))
    !       case('a2x')
    !          if (is_state) call fld_add(a2x,trim(fldname))
    !       case('x2a')
    !          if (is_state) call fld_add(x2a,trim(fldname))
    !       case('l2x')
    !          if (is_state) call fld_add(l2x,trim(fldname))
    !       case('x2l')
    !          if (is_state) call fld_add(x2l,trim(fldname))
    !       case('r2x')
    !          if (is_state) call fld_add(r2x,trim(fldname))
    !       case('x2r')
    !          if (is_state) call fld_add(x2r,trim(fldname))
    !       case('i2x')
    !          if (is_state) call fld_add(i2x,trim(fldname))
    !       case('x2i')
    !          if (is_state) call fld_add(x2i,trim(fldname))
    !       case('o2x')
    !          if (is_state) call fld_add(o2x,trim(fldname))
    !       case('x2o')
    !          if (is_state) call fld_add(x2o,trim(fldname))
    !       case('g2x')
    !          if (is_state) call fld_add(g2x,trim(fldname))
    !       case('x2g')
    !          if (is_state) call fld_add(x2g,trim(fldname))
    !       case default
    !          write(llogunit,*) subname//'ERROR: ',trim(cplflds_custom(n)),' not a recognized value'
    !          call shr_sys_abort()
    !       end select
    !    else
    !       exit
    !    end if
    ! end do

    !----------------------------------------------------------
    ! scalar information
    !----------------------------------------------------------

    longname = trim(flds_scalar_name)
    stdname  = trim(flds_scalar_name)
    units    = 'unitless'
    attname  = trim(flds_scalar_name)
    call fld_metadata_set(attname, longname, stdname, units=units)

    !----------------------------------------------------------
    ! domain coordinates (appear in the share module shr_flds_mod)
    !----------------------------------------------------------

    shr_flds_dom_coord  = ''
    shr_flds_dom_other  = ''

    longname = 'latitude'
    stdname  = 'latitude'
    units    = 'degrees north'
    call fld_dom_add(shr_flds_dom_coord,'lat', longname, stdname, units=units)

    longname = 'longitude'
    stdname  = 'longitude'
    units    = 'degrees east'
    call fld_dom_add(shr_flds_dom_coord,'lon', longname, stdname, units=units)

    longname = 'height'
    stdname  = 'height, depth, or levels'
    units    = 'unitless'
    call fld_dom_add(shr_flds_dom_coord,'hgt', longname, stdname, units=units)

    longname = 'cell_area_model'
    stdname  = 'cell area from model'
    units    = 'radian^2'
    call fld_dom_add(shr_flds_dom_other,'area', longname, stdname, units=units)

    longname = 'cell_area_mapping'
    stdname  = 'cell area from mapping file'
    units    = 'radian^2'
    call fld_dom_add(shr_flds_dom_other,'aream', longname, stdname, units=units)

    longname = 'mask'
    stdname  = 'mask'
    units    = '1'
    call fld_dom_add(shr_flds_dom_other,'mask', longname, stdname, units=units)

    longname = 'area_fraction'
    stdname  = 'area fraction'
    units    = '1'
    call fld_dom_add(shr_flds_dom_other,'frac', longname, stdname, units=units)

    !----------------------------------------------------------
    ! states/fluxes from atm
    !----------------------------------------------------------

    ! height at the lowest model level (m)
    longname = 'Height at the lowest model level'
    stdname  = 'height'
    units    = 'm'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_z", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_z")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_z")

    ! topographic height (m)
    longname = 'Surface height'
    stdname  = 'height'
    units    = 'm'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_topo", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_topo")

    ! zonal wind at the lowest model level (m/s)
    longname = 'Zonal wind at the lowest model level'
    stdname  = 'eastward_wind'
    units    = 'm s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_u", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_u")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_u")
    call fld_add(flds_x2w, flds_x2w_map, "Sa_u")

    ! meridional wind at the lowest model level (m/s)
    longname = 'Meridional wind at the lowest model level'
    stdname  = 'northward_wind'
    units    = 'm s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_v", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_v")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_v")
    call fld_add(flds_x2w, flds_x2w_map, "Sa_v")

    ! temperature at the lowest model level (K)
    longname = 'Temperature at the lowest model level'
    stdname  = 'air_temperature'
    units    = 'K'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_tbot", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_tbot")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_tbot")
    call fld_add(flds_x2w, flds_x2w_map, "Sa_tbot")

    ! potential temperature at the lowest model level (K)
    longname = 'Potential temperature at the lowest model level'
    stdname  = 'air_potential_temperature'
    units    = 'K'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_ptem", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_ptem")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_ptem")

    ! specific humidity at the lowest model level (kg/kg)
    longname = 'Specific humidity at the lowest model level'
    stdname  = 'specific_humidity'
    units    = 'kg kg-1'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_shum", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_shum")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_shum")

    ! pressure at the lowest model level (Pa)
    longname = 'Pressure at the lowest model level'
    stdname  = 'air_pressure'
    units    = 'Pa'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_pbot", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Sa_pbot")
    call fld_add(flds_x2i, flds_x2i_map, "Sa_pbot")

    ! air density at the lowest model level (kg/m**3)
    longname = 'Density at the lowest model level'
    stdname  = 'air_density'
    units    = 'kg m-3'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_dens", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Sa_dens")

    ! convective precipitation rate
    ! large-scale (stable) snow rate (water equivalent)
    units    = 'kg m-2 s-1'
    longname = 'Convective precipitation rate'
    stdname  = 'convective_precipitation_flux'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainc", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc")
    longname = 'Large-scale (stable) precipitation rate'
    stdname  = 'large_scale_precipitation_flux'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainl", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl")
    longname = 'Water flux due to rain'
    stdname  = 'rainfall_flux'
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain" , longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain" , longname=longname, stdname=stdname, units=units)

    ! convective snow rate (water equivalent)
    ! large-scale (stable) snow rate (water equivalent)
    units    = 'kg m-2 s-1'
    longname = 'Convective snow rate (water equivalent)'
    stdname  = 'convective_snowfall_flux'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowc", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc")
    longname = 'Large-scale (stable) snow rate (water equivalent)'
    stdname  = 'large_scale_snowfall_flux'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowl", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl")
    longname = 'Water flux due to snow'
    stdname  = 'surface_snow_melt_flux'
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow" , longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow" , longname=longname, stdname=stdname, units=units)

    ! total precipitation to ocean
    longname = 'Water flux (rain+snow)'
    stdname  = 'precipitation_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

    ! downward longwave heat flux (W/m**2)
    longname = 'Downward longwave heat flux'
    stdname  = 'downwelling_longwave_flux'
    units    = 'W m-2'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_lwdn", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_lwdn")
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_lwdn")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_lwdn", longname=longname, stdname=stdname, units=units)

    ! direct near-infrared incident solar radiation
    longname = 'Direct near-infrared incident solar radiation'
    stdname  = 'surface_downward_direct_shortwave_flux_due_to_near_infrared_radiation'
    units    = 'W m-2'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_swndr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_swndr")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_swndr")

    ! direct visible incident solar radiation
    longname = 'Direct visible incident solar radiation'
    stdname  = 'surface_downward_direct_shortwave_flux_due_to_visible_radiation'
    units    = 'W m-2'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_swvdr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_swvdr")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_swvdr")

    ! diffuse near-infrared incident solar radiation
    longname = 'Diffuse near-infrared incident solar radiation'
    stdname  = 'surface_downward_diffuse_shortwave_flux_due_to_near_infrared_radiation'
    units    = 'W m-2'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_swndf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_swndf")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_swndf")

    ! diffuse visible incident solar radiation
    longname = 'Diffuse visible incident solar radiation'
    stdname  = 'surface_downward_diffuse_shortwave_flux_due_to_visible_radiation'
    units    = 'W m-2'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_swvdf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_swvdf")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_swvdf")

    ! Net shortwave radiation
    longname = 'Net shortwave radiation'
    stdname  = 'surface_net_shortwave_flux'
    units    = 'W m-2'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_swnet", longname=longname, stdname=stdname, units=units) ! diagnostic
    call fld_add(flds_l2x, flds_l2x_map, "Fall_swnet", longname=longname, stdname=stdname, units=units) ! diagnostic
    call fld_add(flds_i2x, flds_i2x_map, "Faii_swnet", longname=longname, stdname=stdname, units=units) ! diagnostic
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_swnet", longname=longname, stdname=stdname, units=units) ! derived using albedos, Faxa_swxxx and swpen

    longname = 'Net shortwave radiation penetrating into ice and ocean'
    stdname  = 'net_downward_shortwave_flux_in_sea_ice_due_to_penetration'
    units    = 'W m-2'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_swpen", longname=longname, stdname=stdname, units=units) ! used for Foxx_swnet below

    ! Black Carbon hydrophilic dry deposition
    longname = 'Hydrophylic black carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_bcphidry", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_bcphidry")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_bcphidry")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_bcphidry", longname=longname, stdname=stdname, units=units)

    ! Black Carbon hydrophobic dry deposition
    longname = 'Hydrophobic black carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophobic_black_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_bcphodry", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_bcphodry")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_bcphodry")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_bcphodry", longname=longname, stdname=stdname, units=units)

    ! Black Carbon hydrophilic wet deposition
    longname = 'Hydrophylic black carbon wet deposition flux'
    stdname  = 'wet_deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_bcphiwet", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_bcphiwet")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_bcphiwet")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_bcphiwet", longname=longname, stdname=stdname, units=units)

    ! Organic Carbon hydrophilic dry deposition
    longname = 'Hydrophylic organic carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophylic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_ocphidry", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_ocphidry")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_ocphidry")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_ocphidry", longname=longname, stdname=stdname, units=units)

    ! Organic Carbon hydrophobic dry deposition
    longname = 'Hydrophobic organic carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophobic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_ocphodry", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_ocphodry")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_ocphodry")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_ocphodry", longname=longname, stdname=stdname, units=units)

    ! Organic Carbon hydrophilic wet deposition
    longname = 'Hydrophylic organic carbon wet deposition flux'
    stdname  = 'wet_deposition_flux_of_hydrophylic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_ocphiwet", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_ocphiwet")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_ocphiwet")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_ocphiwet", longname=longname, stdname=stdname, units=units)

    ! Size 1 dust -- wet deposition
    longname = 'Dust wet deposition flux (size 1)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstwet1", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstwet1")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstwet1")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstwet1", longname=longname, stdname=stdname, units=units)

    ! Size 2 dust -- wet deposition
    longname = 'Dust wet deposition flux (size 2)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstwet2", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstwet2")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstwet2")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstwet2", longname=longname, stdname=stdname, units=units)

    ! Size 3 dust -- wet deposition
    longname = 'Dust wet deposition flux (size 3)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstwet3", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstwet3")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstwet3")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstwet3", longname=longname, stdname=stdname, units=units)

    ! Size 4 dust -- wet deposition
    longname = 'Dust wet deposition flux (size 4)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstwet4", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstwet4")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstwet4")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstwet4", longname=longname, stdname=stdname, units=units)

    ! Size 1 dust -- dry deposition
    longname = 'Dust dry deposition flux (size 1)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstdry1", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstdry1")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstdry1")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstdry1", longname=longname, stdname=stdname, units=units)

    ! Size 2 dust -- dry deposition
    longname = 'Dust dry deposition flux (size 2)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstdry2", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstdry2")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstdry2")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstdry2", longname=longname, stdname=stdname, units=units)

    ! Size 3 dust -- dry deposition
    longname = 'Dust dry deposition flux (size 3)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstdry3", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstdry3")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstdry3")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstdry3", longname=longname, stdname=stdname, units=units)

    ! Size 4 dust -- dry deposition
    longname = 'Dust dry deposition flux (size 4)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fld_add(flds_a2x, flds_a2x_map, "Faxa_dstdry4", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Faxa_dstdry4")
    call fld_add(flds_x2l, flds_x2l_map, "Faxa_dstdry4")
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_dstdry4", longname=longname, stdname=stdname, units=units)

    !----------------------------------------------------------
    ! states/fluxes to atm (and ocean)
    !----------------------------------------------------------

    ! land/sea-ice/ocean fractions
    longname = 'Surface land fraction'
    stdname  = 'land_area_fraction'
    units    = '1'
    call fld_add(flds_x2a, flds_x2a_map,'Sl_lfrac', longname=longname, stdname=stdname, units=units)

    longname = 'Surface ice fraction'
    stdname  = 'sea_ice_area_fraction'
    call fld_add(flds_x2a, flds_x2a_map,'Si_ifrac', longname=longname, stdname=stdname, units=units)

    longname = 'Surface ocean fraction'
    stdname  = 'sea_area_fraction'
    call fld_add(flds_x2a, flds_x2a_map,'So_ofrac', longname=longname, stdname=stdname, units=units)

    ! Direct albedo (visible radiation)
    longname = 'Direct albedo (visible radiation)'
    stdname  = 'surface_direct_albedo_due_to_visible_radiation'
    units    = '1'
    call fld_add(flds_i2x, flds_i2x_map, "Si_avsdr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_l2x, flds_l2x_map, "Sl_avsdr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao_albedo, flds_xao_albedo_map, "So_avsdr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_avsdr", longname=longname, stdname=stdname, units=units)

    ! Direct albedo (near-infrared radiation)
    longname = 'Direct albedo (near-infrared radiation)'
    stdname  = 'surface_direct_albedo_due_to_near_infrared_radiation'
    units    = '1'
    call fld_add(flds_i2x, flds_i2x_map, "Si_anidr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_l2x, flds_l2x_map, "Sl_anidr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao_albedo, flds_xao_albedo_map, "So_anidr", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_anidr", longname=longname, stdname=stdname, units=units)

    ! Diffuse albedo (visible radiation)
    longname = 'Diffuse albedo (visible radiation)'
    stdname  = 'surface_diffuse_albedo_due_to_visible_radiation'
    units    = '1'
    call fld_add(flds_i2x, flds_i2x_map, "Si_avsdf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_l2x, flds_l2x_map, "Sl_avsdf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao_albedo, flds_xao_albedo_map, "So_avsdf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_avsdf", longname=longname, stdname=stdname, units=units)

    ! Diffuse albedo (near-infrared radiation)
    longname = 'Diffuse albedo (near-infrared radiation)'
    stdname  = 'surface_diffuse_albedo_due_to_near_infrared_radiation'
    units    = '1'
    call fld_add(flds_i2x, flds_i2x_map, "Si_anidf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_l2x, flds_l2x_map, "Sl_anidf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao_albedo, flds_xao_albedo_map, "So_anidf", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_anidf", longname=longname, stdname=stdname, units=units)

    ! Reference temperature at 2 meters
    longname = 'Reference temperature at 2 meters'
    stdname  = 'air_temperature'
    units    = 'K'
    call fld_add(flds_l2x, flds_l2x_map, "Sl_tref", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Si_tref", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "So_tref", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_tref", longname=longname, stdname=stdname, units=units)

    ! Reference specific humidity at 2 meters
    longname = 'Reference specific humidity at 2 meters'
    stdname  = 'specific_humidity'
    units    = 'kg kg-1'
    call fld_add(flds_l2x, flds_l2x_map, "Sl_qref", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Si_qref", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "So_qref", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_qref", longname=longname, stdname=stdname, units=units)

    ! Surface temperature
    longname = 'Surface temperature'
    stdname  = 'surface_temperature'
    units    = 'K'
    call fld_add(flds_o2x, flds_o2x_map, "So_t", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_l2x, flds_l2x_map, "Sl_t", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Si_t", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_t", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "So_t")
    call fld_add(flds_x2i, flds_x2i_map, "So_t")
    call fld_add(flds_x2w, flds_x2w_map, "So_t")

    ! Surface fraction in land determined by land (land/atm only)
    longname = 'Surface fraction in land'
    stdname  = 'land_fraction_from_land'
    units    = '1'
    call fld_add(flds_l2x, flds_l2x_map, "Sl_lfrin", longname=longname, stdname=stdname, units=units)

    ! Surface friction velocity in land (land/atm only)
    longname = 'Surface fraction velocity in land'
    stdname  = 'fraction_velocity'
    units    = 'm s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Sl_fv", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sl_fv")

    ! Aerodynamical resistance (land/atm only)
    longname = 'aerodynamic resistance'
    stdname  = 'aerodynamic_resistance'
    units    = 's/m'
    call fld_add(flds_l2x, flds_l2x_map, "Sl_ram1", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sl_ram1")

    ! Surface snow water equivalent (land/atm only)
    longname = 'Surface snow water equivalent'
    stdname  = 'surface_snow_water_equivalent'
    units    = 'm'
    call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh")

    ! Surface snow depth (ice/atm only)
    longname = 'Surface snow depth'
    stdname  = 'surface_snow_thickness'
    units    = 'm'
    call fld_add(flds_i2x, flds_i2x_map, "Si_snowh", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Si_snowh")

    ! Surface saturation specific humidity in ocean (ocn/atm only)
    longname = 'Surface saturation specific humidity in ocean'
    stdname  = 'specific_humidity_at_saturation'
    units    = 'kg kg-1'
    call fld_add(flds_xao, flds_xao_map, "So_ssq", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "So_ssq")

    ! Square of exch. coeff (tracers) (ocn/atm only)
    longname = 'Square of exch. coeff (tracers)'
    stdname  = 'square_of_exch_coeff'
    units    = '1'
    call fld_add(flds_xao, flds_xao_map, "So_re", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "So_re")

    ! 10 meter wind
    longname = '10m wind'
    stdname  = '10m_wind'
    units    = 'm'
    call fld_add(flds_i2x, flds_i2x_map, "Si_u10", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "So_u10", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_l2x, flds_l2x_map, "Sl_u10", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Sx_u10", longname=longname, stdname=stdname, units=units)

    ! Zonal surface stress"
    longname = 'Zonal surface stress'
    stdname  = 'surface_downward_eastward_stress'
    units    = 'N m-2'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_taux", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "Faox_taux", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Faii_taux", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_taux", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_taux", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_taux", longname=longname, stdname=stdname, units=units)

    ! Meridional surface stress
    longname = 'Meridional surface stress'
    stdname  = 'surface_downward_northward_stress'
    units    = 'N m-2'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_tauy", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "Faox_tauy", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Faii_tauy", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_tauy", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_tauy", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_tauy", longname=longname, stdname=stdname, units=units)

    ! Surface latent heat flux
    longname = 'Surface latent heat flux'
    stdname  = 'surface_upward_latent_heat_flux'
    units    = 'W m-2'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_lat", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "Faox_lat", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Faii_lat", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_lat", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_lat", longname=longname, stdname=stdname, units=units)

    ! Surface sensible heat flux
    longname = 'Sensible heat flux'
    stdname  = 'surface_upward_sensible_heat_flux'
    units    = 'W m-2'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_sen", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "Faox_sen", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Faii_sen", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_sen", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_sen", longname=longname, stdname=stdname, units=units)

    ! Surface upward longwave heat flux
    longname = 'Surface upward longwave heat flux'
    stdname  = 'surface_net_upward_longwave_flux'
    units    = 'W m-2'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_lwup", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "Faox_lwup", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Faii_lwup", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_lwup", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_lwup", longname=longname, stdname=stdname, units=units)

    ! Evaporation water flux
    longname = 'Evaporation water flux'
    stdname  = 'water_evaporation_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_evap", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_xao, flds_xao_map, "Faox_evap", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_i2x, flds_i2x_map, "Faii_evap", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_evap", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_evap", longname=longname, stdname=stdname, units=units)

    ! Dust flux (particle bin number 1)
    longname = 'Dust flux (particle bin number 1)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_flxdst1", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_flxdst1", longname=longname, stdname=stdname, units=units)

    ! Dust flux (particle bin number 2)
    longname = 'Dust flux (particle bin number 2)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_flxdst2", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_flxdst2", longname=longname, stdname=stdname, units=units)

    ! Dust flux (particle bin number 3)
    longname = 'Dust flux (particle bin number 3)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_flxdst3", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_flxdst3", longname=longname, stdname=stdname, units=units)

    ! Dust flux (particle bin number 4)
    longname = 'Dust flux (particle bin number 4)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Fall_flxdst4", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "Faxx_flxdst4", longname=longname, stdname=stdname, units=units)

    !-----------------------------
    ! atm<->ocn only exchange
    !-----------------------------

    ! Sea level pressure (Pa)
    longname = 'Sea level pressure'
    stdname  = 'air_pressure_at_sea_level'
    units    = 'Pa'
    call fld_add(flds_a2x, flds_a2x_map, "Sa_pslv", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Sa_pslv")

    ! Wind speed squared at 10 meters
    longname = 'Wind speed squared at 10 meters'
    stdname  = 'square_of_wind_speed'
    units    = 'm2 s-2'
    call fld_add(flds_xao, flds_xao_map, "So_duu10n", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "So_duu10n")

    ! Surface friction velocity in ocean
    longname = 'Surface fraction velocity in ocean'
    stdname  = 'fraction_velocity'
    units    = 'm s-1'
    call fld_add(flds_xao, flds_xao_map, "So_ustar", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2a, flds_x2a_map, "So_ustar")

    !-----------------------------
    ! ice<->ocn only exchange
    !-----------------------------

    ! Fractional ice coverage wrt ocean
    longname = 'Sea Ice mask'
    stdname  = 'sea_ice_mask'
    units    = '1'
    call fld_add(flds_i2x, flds_i2x_map, "Si_imask", longname=longname, stdname=stdname, units=units)

    ! Fractional ice coverage wrt ocean
    longname = 'Fractional ice coverage wrt ocean'
    stdname  = 'sea_ice_area_fraction'
    units    = '1'
    call fld_add(flds_i2x, flds_i2x_map, "Si_ifrac", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Si_ifrac")
    call fld_add(flds_x2w, flds_x2w_map, "Si_ifrac")

    ! Ocean melt and freeze potential
    longname = 'Ocean melt and freeze potential'
    stdname  = 'surface_snow_and_ice_melt_heat_flux'
    units    = 'W m-2'
    call fld_add(flds_o2x, flds_o2x_map, "Fioo_q", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "Fioo_q")

    ! Heat flux from melting
    longname = 'Heat flux from melting'
    stdname  = 'surface_snow_melt_heat_flux'
    units    = 'W m-2'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_melth", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_melth", longname=longname, stdname=stdname, units=units)

    ! Water flux from melting
    longname = 'Water flux due to melting'
    stdname  = 'surface_snow_melt_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw", longname=longname, stdname=stdname, units=units)

    ! Salt flux
    longname = 'Salt flux'
    stdname  = 'virtual_salt_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_salt", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_salt", longname=longname, stdname=stdname, units=units)

    ! Black Carbon hydrophilic deposition
    longname = 'Hydrophylic black carbon deposition flux'
    stdname  = 'deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_bcphi" , longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_bcphi"   , longname=longname, stdname=stdname, units=units)

    ! Black Carbon hydrophobic deposition
    longname = 'Hydrophobic black carbon deposition flux'
    stdname  = 'deposition_flux_of_hydrophobic_black_carbon'
    units    = 'kg m-2 s-1'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_bcpho" , longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_bcpho"   , longname=longname, stdname=stdname, units=units)

    ! Dust flux
    longname = 'Dust flux'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_i2x, flds_i2x_map, "Fioi_flxdst", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map, "Foxx_flxdst", longname=longname, stdname=stdname, units=units)

    ! Sea surface mask
    longname = 'Sea surface mask'
    stdname  = 'sea_surface_mask'
    units    = '1'
    call fld_add(flds_o2x, flds_o2x_map, "So_omask", longname=longname, stdname=stdname, units=units)

    ! Sea surface salinity
    longname = 'Sea surface salinity'
    stdname  = 'sea_surface_salinity'
    units    = 'g kg-1'
    call fld_add(flds_o2x, flds_o2x_map, "So_s", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "So_s")

    ! Zonal sea water velocity
    longname = 'Zonal sea water velocity'
    stdname  = 'eastward_sea_water_velocity'
    units    = 'm s-1'
    call fld_add(flds_o2x, flds_o2x_map, "So_u", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "So_u")
    call fld_add(flds_x2w, flds_x2w_map, "So_u")

    ! Meridional sea water velocity
    longname = 'Meridional sea water velocity'
    stdname  = 'northward_sea_water_velocity'
    units    = 'm s-1'
    call fld_add(flds_o2x, flds_o2x_map, "So_v", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "So_v")
    call fld_add(flds_x2w, flds_x2w_map, "So_v")

    ! Zonal sea surface slope
    longname = 'Zonal sea surface slope'
    stdname  = 'sea_surface_eastward_slope'
    units    = 'm m-1'
    call fld_add(flds_o2x, flds_o2x_map, "So_dhdx", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "So_dhdx")

    ! Meridional sea surface slope
    longname = 'Meridional sea surface slope'
    stdname  = 'sea_surface_northward_slope'
    units    = 'm m-1'
    call fld_add(flds_o2x, flds_o2x_map, "So_dhdy", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map, "So_dhdy")

    ! Boundary Layer Depth
    longname = 'Ocean Boundary Layer Depth'
    stdname  = 'ocean_boundary_layer_depth'
    units    = 'm'
    call fld_add(flds_o2x, flds_o2x_map, "So_bldepth", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2w, flds_x2w_map, "So_bldepth")

    longname = 'Fraction of sw penetrating surface layer for diurnal cycle'
    stdname  = 'Fraction of sw penetrating surface layer for diurnal cycle'
    units    = '1'
    call fld_add(flds_xao, flds_xao_map, "So_fswpen", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_o2x, flds_o2x_map, "So_fswpen")

    !-----------------------------
    ! lnd->rof exchange
    !-----------------------------

    ! TODO: put in attributes below

    longname = 'Water flux from land (liquid surface)'
    stdname  = 'water_flux_into_runoff_surface'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofsur', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofsur', longname=longname, stdname=stdname, units=units)

    longname = 'Water flux from land (liquid glacier, wetland, and lake)'
    stdname  = 'water_flux_into_runoff_from_gwl'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofgwl', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofgwl', longname=longname, stdname=stdname, units=units)

    longname = 'Water flux from land (liquid subsurface)'
    stdname  = 'water_flux_into_runoff_subsurface'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofsub', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofsub', longname=longname, stdname=stdname, units=units)

    longname = 'Water flux from land direct to ocean'
    stdname  = 'water_flux_direct_to_ocean'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofdto', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofdto', longname=longname, stdname=stdname, units=units)

    longname = 'Water flux from land (frozen)'
    stdname  = 'frozen_water_flux_into_runoff'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi', longname=longname, stdname=stdname, units=units)

    ! Currently only the CESM land and runoff models treat irrigation as a separate
    ! field: in ACME, this field is folded in to the other runoff fields. Eventually,
    ! ACME may want to update its land and runoff models to map irrigation specially, as
    ! CESM does.
    ! (Once ACME is using this irrigation field, all that needs to be done is to remove
    ! this conditional: Code in other places in the mediator is written to trigger off of
    ! whether Flrl_irrig has been added to the field list, so it should Just Work if this
    ! conditional is removed.)

    ! Irrigation flux (land/rof only)
    longname = 'Irrigation flux (withdrawal from rivers)'
    stdname  = 'irrigation'
    units    = 'kg m-2 s-1'
    call fld_add(flds_l2x, flds_l2x_map, "Flrl_irrig", longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2r, flds_x2r_map, "Frxx_irrig", longname=longname, stdname=stdname, units=units)

    !-----------------------------
    ! rof->ocn (liquid and frozen), rof->ice (frozen) and  rof->lnd (flooding)
    !-----------------------------

    longname = 'Water flux into sea water due to runoff (liquid)'
    stdname  = 'water_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl', longname=longname, stdname=stdname, units=units)

    longname = 'Water flux into sea water due to runoff (frozen)'
    stdname  = 'frozen_water_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi', longname=longname, stdname=stdname, units=units)

    longname = 'Water flux into sea ice due to runoff (frozen)'
    stdname  = 'frozen_water_flux_into_sea_ice'
    units    = 'kg m-2 s-1'
    call fld_add(flds_r2x, flds_r2x_map,'Firr_rofi', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2i, flds_x2i_map,'Fixx_rofi', longname=longname, stdname=stdname, units=units)

    longname = 'Waterrflux back to land due to flooding'
    stdname  = 'flooding_water_flux'
    units    = 'kg m-2 s-1'
    call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood')

    longname = 'River channel total water volume'
    stdname  = 'rtm_volr'
    units    = 'm'
    call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr')

    longname = 'River channel main channel water volume'
    stdname  = 'rtm_volrmch'
    units    = 'm'
    call fld_add(flds_r2x, flds_r2x_map,'Flrr_volrmch', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map,'Flrr_volrmch')

    !-----------------------------
    ! wav->ocn and ocn->wav
    !-----------------------------

    longname = 'Langmuir multiplier'
    stdname  = 'wave_model_langmuir_multiplier'
    units    = '1'
    call fld_add(flds_w2x, flds_w2x_map,'Sw_lamult', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map,'Sw_lamult')

    longname = 'Stokes drift u component'
    stdname  = 'wave_model_stokes_drift_eastward_velocity'
    units    = 'm/s'
    call fld_add(flds_w2x, flds_w2x_map,'Sw_ustokes', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map,'Sw_ustokes')

    longname = 'Stokes drift v component'
    stdname  = 'wave_model_stokes_drift_northward_velocity'
    units    = 'm/s'
    call fld_add(flds_w2x, flds_w2x_map,'Sw_vstokes', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map,'Sw_vstokes')

    longname = 'Stokes drift depth'
    stdname  = 'wave_model_stokes_drift_depth'
    units    = 'm'
    call fld_add(flds_w2x, flds_w2x_map,'Sw_hstokes', longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2o, flds_x2o_map,'Sw_hstokes')

    !-----------------------------
    ! New xao diagnostic
    ! fields for history output only
    !-----------------------------

    longname = 'Downward solar radiation'
    stdname  = 'surface_downward_shortwave_flux'
    units    = 'W m-2'
    call fld_add(flds_xao, flds_xao_map, "Faox_swdn", longname=longname, stdname=stdname, units=units)

    longname = 'Upward solar radiation'
    stdname  = 'surface_upward_shortwave_flux'
    units    = 'W m-2'
    call fld_add(flds_xao, flds_xao_map, "Faox_swup", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux temperature bulk'
    stdname  = 'aoflux_tbulk'
    units    = 'K'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_tbulk_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux temperature skin'
    stdname  = 'aoflux_tskin'
    units    = 'K'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_tskin_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux temperature skin at night'
    stdname  = 'aoflux_tskin_night'
    units    = 'K'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_tskin_night_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux temperature skin at day'
    stdname  = 'aoflux_tskin_day'
    units    = 'K'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_tskin_day_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux cool skin'
    stdname  = 'aoflux_cskin'
    units    = 'K'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_cskin_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux cool skin at night'
    stdname  = 'aoflux_cskin_night'
    units    = 'K'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_cskin_night_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux warming'
    stdname  = 'aoflux_warm'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_warm_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux salting'
    stdname  = 'aoflux_salt'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_salt_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux speed'
    stdname  = 'aoflux_speed'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_speed_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux regime'
    stdname  = 'aoflux_regime'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_regime_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux warming dialy max'
    stdname  = 'aoflux_warmmax'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_warmmax_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux wind daily max'
    stdname  = 'aoflux_windmax'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_windmax_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux q-solar daily avg'
    stdname  = 'aoflux_qsolavg'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_qsolavg_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux wind daily avg'
    stdname  = 'aoflux_windavg'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_windavg_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux daily max increment'
    stdname  = 'aoflux_warmmaxinc'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_warmmaxinc_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux wind daily max increment'
    stdname  = 'aoflux_windmaxinc'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_windmaxinc_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux q-solar increment'
    stdname  = 'aoflux_qsolinc'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_qsolinc_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux wind increment'
    stdname  = 'aoflux_windinc'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_windinc_diurn", longname=longname, stdname=stdname, units=units)

    longname = 'atm/ocn flux increment counter'
    stdname  = 'aoflux_ninc'
    units    = 'unitless'
    call fld_add(flds_xao_diurnl, flds_xao_diurnl_map, "So_ninc_diurn", longname=longname, stdname=stdname, units=units)

    !-----------------------------
    ! glc fields
    !-----------------------------

    name = 'Fogg_rofl'
    longname = 'glc liquid runoff flux to ocean'
    stdname  = 'glacier_liquid_runoff_flux_to_ocean'
    units    = 'kg m-2 s-1'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)

    name = 'Fogg_rofi'
    longname = 'glc frozen runoff flux to ocean'
    stdname  = 'glacier_frozen_runoff_flux_to_ocean'
    units    = 'kg m-2 s-1'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)

    name = 'Figg_rofi'
    longname = 'glc frozen runoff_iceberg flux to ice'
    stdname  = 'glacier_frozen_runoff_flux_to_seaice'
    units    = 'kg m-2 s-1'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)

    name = 'Sg_icemask'
    longname = 'Ice sheet grid coverage on global grid'
    stdname  = 'ice_sheet_grid_mask'
    units    = '1'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_g2x_to_lnd, flds_g2x_to_lnd_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l_from_glc, flds_x2l_from_glc_map,trim(name), longname=longname, stdname=stdname, units=units)

    name = 'Sg_icemask_coupled_fluxes'
    longname = 'Ice sheet mask where we are potentially sending non-zero fluxes'
    stdname  = 'icemask_coupled'
    units    = '1'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_g2x_to_lnd, flds_g2x_to_lnd_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l, flds_x2l_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_x2l_from_glc, flds_x2l_from_glc_map,trim(name), longname=longname, stdname=stdname, units=units)

    ! glc fields with multiple elevation classes: lnd->glc
    !
    ! Note that these fields are sent in multiple elevation classes from lnd->med, but
    ! the fields sent from med->glc do NOT have elevation classes
    !
    ! Also note that we need to keep track of the l2x fields destined for glc in the
    ! additional variables, l2x_to_glc. This is needed so that
    ! we can set up an additional fields holding accumulated quantities of just these fields.

    name = 'Flgl_qice'
    attname = name
    longname = 'New glacier ice flux'
    stdname  = 'ice_flux_out_of_glacier'
    units    = 'kg m-2 s-1'
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_l2x, flds_l2x_map)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_l2x_to_glc, flds_l2x_to_glc_map, &
         additional_list=.true.)
    call fld_add(flds_x2g, flds_x2g_map, trim(name), longname=longname, stdname=stdname, units=units)

    name = 'Sl_tsrf'
    attname = name
    longname = 'Surface temperature of glacier'
    stdname  = 'surface_temperature'
    units    = 'deg C'
    call fld_add(flds_x2g, flds_x2g_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_l2x, flds_l2x_map)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_l2x_to_glc, flds_l2x_to_glc_map, &
         additional_list=.true.)

    ! Sl_topo is sent from lnd -> med, but is NOT sent to glc (it is only used for the
    ! remapping in the mediator)
    name = 'Sl_topo'
    attname = name
    longname = 'Surface height'
    stdname  = 'height'
    units    = 'm'
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_l2x, flds_l2x_map)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_l2x_to_glc, flds_l2x_to_glc_map, &
         additional_list=.true.)

    ! glc fields with multiple elevation classes: glc->lnd
    !
    ! Note that the fields sent from glc->med do NOT have elevation classes, but the
    ! fields from med->lnd are broken into multiple elevation classes

    name = 'Sg_ice_covered'
    attname = name
    longname = 'Fraction of glacier area'
    stdname  = 'glacier_area_fraction'
    units    = '1'
    call fld_add(flds_g2x, flds_g2x_map, trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_g2x_to_lnd, flds_g2x_to_lnd_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_x2l, flds_x2l_map)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_x2l_from_glc, flds_x2l_from_glc_map, &
         additional_list=.true.)

    name = 'Sg_topo'
    attname = name
    longname = 'Surface height of glacier'
    stdname  = 'height'
    units    = 'm'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_g2x_to_lnd, flds_g2x_to_lnd_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_x2l, flds_x2l_map)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_x2l_from_glc, flds_x2l_from_glc_map, &
         additional_list=.true.)

    name = 'Flgg_hflx'
    attname = name
    longname = 'Downward heat flux from glacier interior'
    stdname  = 'downward_heat_flux_in_glacier'
    units    = 'W m-2'
    call fld_add(flds_g2x, flds_g2x_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_add(flds_g2x_to_lnd, flds_g2x_to_lnd_map,trim(name), longname=longname, stdname=stdname, units=units)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_x2l, flds_x2l_map)
    call fld_set_glc_elevclass(name, attname, longname, stdname, units, flds_x2l_from_glc, flds_x2l_from_glc_map, &
         additional_list=.true.)

    !-----------------------------
    ! co2 fields
    !-----------------------------

    if (flds_co2a) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_co2prog", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_co2prog")
       call fld_add(flds_x2o, flds_x2o_map, "Sa_co2prog")

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_co2diag", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_co2diag")
       call fld_add(flds_x2o, flds_x2o_map, "Sa_co2diag")

    else if (flds_co2b) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fld_add(flds_a2x, flds_a2x_map,  "Sa_co2prog", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,  "Sa_co2prog")

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fld_add(flds_a2x, flds_a2x_map,  "Sa_co2diag", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,  "Sa_co2diag")

       longname = 'Surface flux of CO2 from land'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_land'
       units    = 'moles m-2 s-1'
       call fld_add(flds_l2x, flds_l2x_map,  "Fall_fco2_lnd", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map,  "Faxx_fco2_lnd", longname=longname, stdname=stdname, units=units)

    else if (flds_co2c) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_co2prog", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_co2prog")
       call fld_add(flds_x2o, flds_x2o_map, "Sa_co2prog")

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_co2diag", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_co2diag")
       call fld_add(flds_x2o, flds_x2o_map, "Sa_co2diag")

       longname = 'Surface flux of CO2 from land'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_land'
       units    = 'moles m-2 s-1'
       call fld_add(flds_l2x, flds_l2x_map, "Fall_fco2_lnd", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Faxx_fco2_lnd", longname=longname, stdname=stdname, units=units)

       longname = 'Surface flux of CO2 from ocean'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_open_sea'
       units    = 'moles m-2 s-1'
       call fld_add(flds_o2x, flds_o2x_map, "Faoo_fco2_ocn", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Faxx_fco2_ocn", longname=longname, stdname=stdname, units=units)

    endif

    !-----------------------------
    ! water isotope fields
    !-----------------------------

    if (flds_wiso) then
       longname = 'Ratio of ocean surface level abund. H2_16O/H2O/Rstd'
       stdname  = 'ratio_ocean_surface_16O_abund'
       units    = '1'
       call fld_add(flds_o2x, flds_o2x_map, "So_roce_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2i, flds_x2i_map, "So_roce_16O")

       longname = 'Ratio of ocean surface level abund. HDO/H2O/Rstd'
       stdname  = 'ratio_ocean_surface_HDO_abund'
       call fld_add(flds_o2x, flds_o2x_map, "So_roce_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2i, flds_x2i_map, "So_roce_HDO")

       !--------------------------------------------
       ! Atmospheric specific humidty at lowest level:
       !--------------------------------------------

       ! specific humidity of H216O at the lowest model level (kg/kg)
       longname = 'Specific humidty of H216O at the lowest model level'
       stdname  = 'H216OV'
       units    = 'kg kg-1'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_shum_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_shum_16O")
       call fld_add(flds_x2i, flds_x2i_map, "Sa_shum_16O")

       ! specific humidity of HD16O at the lowest model level (kg/kg)
       longname = 'Specific humidty of HD16O at the lowest model level'
       stdname  = 'HD16OV'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_shum_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_shum_HDO")
       call fld_add(flds_x2i, flds_x2i_map, "Sa_shum_HDO")

       ! specific humidity of H218O at the lowest model level (kg/kg)
       longname = 'Specific humidty of H218O at the lowest model level'
       stdname  = 'H218OV'
       call fld_add(flds_a2x, flds_a2x_map, "Sa_shum_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Sa_shum_18O")
       call fld_add(flds_x2i, flds_x2i_map, "Sa_shum_18O")

       ! Surface snow water equivalent (land/atm only)
       longname = 'Isotopic surface snow water equivalent'
       stdname  = 'surface_snow_water_equivalent'
       units    = 'm'
       call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh_16O")
       call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh_18O")
       call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh_HDO")

       !--------------
       ! Isotopic Rain:
       !--------------

       !Isotopic Precipitation Fluxes:
       units    = 'kg m-2 s-1'
       longname = 'H216O Convective precipitation rate'
       stdname  = 'H2_16O_convective_precipitation_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainc_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc_16O")
       longname = 'H216O Large-scale (stable) precipitation rate'
       stdname  = 'H2_16O_large_scale_precipitation_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainl_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl_16O")
       longname = 'Water flux due to H216O rain' !equiv. to bulk
       stdname  = 'H2_16O_rainfall_flux'
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain_16O")

       longname = 'H218O Convective precipitation rate'
       stdname  = 'H2_18O_convective_precipitation_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainc_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc_18O")
       longname = 'H218O Large-scale (stable) precipitation rate'
       stdname  = 'H2_18O_large_scale_precipitation_flux'
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainl_18O")
       longname = 'Water flux due to H218O rain'
       stdname  = 'h2_18o_rainfall_flux'
       call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain_18O", longname=longname, stdname=stdname, units=units)

       longname = 'HDO Convective precipitation rate'
       stdname  = 'HDO_convective_precipitation_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainc_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc_HDO")
       longname = 'HDO Large-scale (stable) precipitation rate'
       stdname  = 'HDO_large_scale_precipitation_flux'
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_rainl_HDO")
       longname = 'Water flux due to HDO rain'
       stdname  = 'hdo_rainfall_flux'
       call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain_HDO", longname=longname, stdname=stdname, units=units)

       !-------------
       ! Isotopic snow:
       !-------------

       longname = 'H2_16O Convective snow rate (water equivalent)'
       stdname  = 'H2_16O_convective_snowfall_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowc_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc_16O")

       longname = 'H2_16O Large-scale (stable) snow rate (water equivalent)'
       stdname  = 'H2_16O_large_scale_snowfall_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowl_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl_16O")

       longname = 'Water equiv. H216O snow flux'
       stdname  = 'h2_16o_snowfall_flux'
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow_16O", longname=longname, stdname=stdname, units=units)

       longname = 'H2_18O Convective snow rate (water equivalent)'
       stdname  = 'H2_18O_convective_snowfall_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowc_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc_18O")

       longname = 'H2_18O Large-scale (stable) snow rate (water equivalent)'
       stdname  = 'H2_18O_large_scale_snowfall_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowl_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl_18O")

       longname = 'Isotopic water equiv. snow flux of H218O'
       stdname  = 'h2_18o_snowfall_flux'
       call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow_18O", longname=longname, stdname=stdname, units=units)

       longname = 'HDO Convective snow rate (water equivalent)'
       stdname  = 'HDO_convective_snowfall_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowc_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc_HDO")

       longname = 'HDO Large-scale (stable) snow rate (water equivalent)'
       stdname  = 'HDO_large_scale_snowfall_flux'
       call fld_add(flds_a2x, flds_a2x_map, "Faxa_snowl_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl_HDO")

       longname = 'Isotopic water equiv. snow flux of HDO'
       stdname  = 'hdo_snowfall_flux'
       call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow_HDO", longname=longname, stdname=stdname, units=units)

       !----------------------------------
       ! Isotopic precipitation (rain+snow):
       !----------------------------------

       longname = 'Isotopic Water flux (rain+snow) for H2_16O'
       stdname  = 'h2_18o_precipitation_flux'
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec_16O", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

       longname = 'Isotopic Water flux (rain+snow) for H2_18O'
       stdname  = 'h2_18o_precipitation_flux'
       units    = 'kg m-2 s-1'
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec_18O", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

       longname = 'Isotopic Water flux (rain+snow) for HD_O'
       stdname  = 'hdo_precipitation_flux'
       units    = 'kg m-2 s-1'
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec_HDO", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

       !-------------------------------------
       ! Isotopic two meter reference humidity:
       !-------------------------------------

       ! H216O Reference specific humidity at 2 meters
       longname = 'Reference H216O specific humidity at 2 meters'
       stdname  = 'H216O_specific_humidity'
       units    = 'kg kg-1'
       call fld_add(flds_l2x, flds_l2x_map, "Sl_qref_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_i2x, flds_i2x_map, "Si_qref_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_xao, flds_xao_map, "So_qref_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Sx_qref_16O", longname=longname, stdname=stdname, units=units)

       ! HD16O Reference specific humidity at 2 meters
       longname = 'Reference HD16O specific humidity at 2 meters'
       stdname  = 'HD16O_specific_humidity'
       units    = 'kg kg-1'
       call fld_add(flds_l2x, flds_l2x_map, "Sl_qref_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_i2x, flds_i2x_map, "Si_qref_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_xao, flds_xao_map, "So_qref_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Sx_qref_HDO", longname=longname, stdname=stdname, units=units)

       ! H218O Reference specific humidity at 2 meters
       longname = 'Reference H218O specific humidity at 2 meters'
       stdname  = 'H218O_specific_humidity'
       units    = 'kg kg-1'
       call fld_add(flds_l2x, flds_l2x_map, "Sl_qref_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_i2x, flds_i2x_map, "Si_qref_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_xao, flds_xao_map, "So_qref_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Sx_qref_18O", longname=longname, stdname=stdname, units=units)

       !-------------------------
       ! Isotopic Evaporation flux:
       !-------------------------

       ! H216O Evaporation water flux
       longname = 'Evaporation H216O flux'
       stdname  = 'H216O_evaporation_flux'
       units    = 'kg m-2 s-1'
       call fld_add(flds_l2x, flds_l2x_map, "Fall_evap_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_i2x, flds_i2x_map, "Faii_evap_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_xao, flds_xao_map, "Faox_evap_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Faxx_evap_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_evap_16O", longname=longname, stdname=stdname, units=units)

       ! HD16O Evaporation water flux
       longname = 'Evaporation HD16O flux'
       stdname  = 'HD16O_evaporation_flux'
       units    = 'kg m-2 s-1'
       call fld_add(flds_l2x, flds_l2x_map, "Fall_evap_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_i2x, flds_i2x_map, "Faii_evap_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_xao, flds_xao_map, "Faox_evap_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Faxx_evap_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_evap_HDO", longname=longname, stdname=stdname, units=units)

       ! H218O Evaporation water flux
       longname = 'Evaporation H218O flux'
       stdname  = 'H218O_evaporation_flux'
       units    = 'kg m-2 s-1'
       call fld_add(flds_l2x, flds_l2x_map, "Fall_evap_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_i2x, flds_i2x_map, "Faii_evap_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_xao, flds_xao_map, "Faox_evap_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, "Faxx_evap_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_evap_18O", longname=longname, stdname=stdname, units=units)

       !-----------------------------
       ! Isotopic sea ice melting flux:
       !-----------------------------

       ! H216O Water flux from melting
       units    = 'kg m-2 s-1'
       longname = 'H2_16O flux due to melting'
       stdname  = 'h2_16o_surface_snow_melt_flux'
       call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw_16O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw_16O", longname=longname, stdname=stdname, units=units)

       ! H218O Water flux from melting
       longname = 'H2_18O flux due to melting'
       stdname  = 'h2_18o_surface_snow_melt_flux'
       call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw_18O", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw_18O", longname=longname, stdname=stdname, units=units)

       ! HDO Water flux from melting
       units    = 'kg m-2 s-1'
       longname = 'HDO flux due to melting'
       stdname  = 'hdo_surface_snow_melt_flux'
       call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw_HDO", longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw_HDO", longname=longname, stdname=stdname, units=units)

       !Iso-Runoff
       ! r2o, l2x, x2r

       units    = 'kg m-2 s-1'
       longname = 'H2_16O Water flux from land (frozen)'
       stdname  = 'H2_16O_frozen_water_flux_into_runoff'
       call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi_16O', longname=longname, stdname=stdname, units=units)

       longname = 'H2_18O Water flux from land (frozen)'
       stdname  = 'H2_18O_frozen_water_flux_into_runoff'
       call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi_18O', longname=longname, stdname=stdname, units=units)

       longname = 'HDO Water flux from land (frozen)'
       stdname  = 'HDO_frozen_water_flux_into_runoff'
       call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi_HDO', longname=longname, stdname=stdname, units=units)

       longname = 'H2_16O Water flux from land (liquid)'
       stdname  = 'H2_16O_liquid_water_flux_into_runoff'
       call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofl_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofl_16O', longname=longname, stdname=stdname, units=units)

       longname = 'H2_18O Water flux from land (liquid)'
       stdname  = 'H2_18O_liquid_water_flux_into_runoff'
       call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofl_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofl_18O', longname=longname, stdname=stdname, units=units)

       longname = 'HDO Water flux from land (liquid)'
       stdname  = 'HDO_liquid_water_flux_into_runoff'
       call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofl_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofl_HDO', longname=longname, stdname=stdname, units=units)

       !-----------------------------
       ! Isotopic r2x, x2o
       !-----------------------------

       units    = 'kg m-2 s-1'
       longname = 'H2_16O Water flux due to liq runoff '
       stdname  = 'H2_16O_water_flux_into_sea_water'
       call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl_16O', longname=longname, stdname=stdname, units=units)

       longname = 'H2_18O Water flux due to liq runoff '
       stdname  = 'H2_18O_water_flux_into_sea_water'
       call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl_18O', longname=longname, stdname=stdname, units=units)

       longname = 'HDO Water flux due to liq runoff '
       stdname  = 'HDO_water_flux_into_sea_water'
       call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl_HDO', longname=longname, stdname=stdname, units=units)

       longname = 'H2_16O Water flux due to ice runoff '
       stdname  = 'H2_16O_water_flux_into_sea_water'
       call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi_16O', longname=longname, stdname=stdname, units=units)

       longname = 'H2_18O Water flux due to ice runoff '
       stdname  = 'H2_18O_water_flux_into_sea_water'
       call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi_18O', longname=longname, stdname=stdname, units=units)

       longname = 'HDO Water flux due to ice runoff '
       stdname  = 'HDO_water_flux_into_sea_water'
       call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi_HDO', longname=longname, stdname=stdname, units=units)

       ! r2x, x2l

       units    = 'kg m-2 s-1'
       longname = 'H2_16O waterrflux due to flooding'
       stdname  = 'H2_16O_flodding_water_flux_back_to_land'
       call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_16O')

       longname = 'H2_18O waterrflux due to flooding'
       stdname  = 'H2_18O_flodding_water_flux_back_to_land'
       call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_18O')

       longname = 'HDO Waterrflux due to flooding'
       stdname  = 'HDO_flodding_water_flux_back_to_land'
       call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_HDO')

       longname = 'H2_16O river channel water volume '
       stdname  = 'H2_16O_rtm_volr'
       call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr_16O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr_16O')

       longname = 'H2_18O river channel water volume '
       stdname  = 'H2_18O_rtm_volr'
       call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr_18O', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr_18O')

       longname = 'HDO river channel water volume '
       stdname  = 'HDO_rtm_volr'
       call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr_HDO', longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr_HDO')

       ! longname = 'H2_18O Waterrflux due to flooding'
       ! stdname  = 'H2_18O_flodding_water_flux_back_to_land'
       ! call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_HDO', longname=longname, stdname=stdname, units=units)
       ! call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_HDO')

       !-----------------------------

    endif !Water isotopes

    !-----------------------------------------------------------------------------
    ! optional per thickness category fields
    !-----------------------------------------------------------------------------

    if (flds_i2o_per_cat) then
       do num = 1, ice_ncat
          write(cnum,'(i2.2)') num

          ! Fractional ice coverage wrt ocean

          name = 'Si_ifrac_' // cnum
          longname = 'fractional ice coverage wrt ocean for thickness category ' // cnum
          stdname  = 'sea_ice_area_fraction'
          units    = '1'
          call fld_add(flds_i2x, flds_i2x_map, name, longname=longname, stdname=stdname, units=units)
          call fld_add(flds_x2o, flds_x2o_map, name)

          ! Net shortwave radiation

          name = 'PFioi_swpen_ifrac_' // cnum
          longname = 'net shortwave radiation penetrating into ice and ocean times ice fraction for thickness category ' // cnum
          stdname  = 'product_of_net_downward_shortwave_flux_at_sea_water_surface_and_sea_ice_area_fraction'
          units    = 'W m-2'
          call fld_add(flds_i2x, flds_i2x_map, name, longname=longname, stdname=stdname, units=units)
          call fld_add(flds_x2o, flds_x2o_map, name)

       end do

       ! Fractional atmosphere coverage wrt ocean

       name = 'Sf_afrac'
       longname = 'fractional atmosphere coverage wrt ocean'
       stdname  = 'atmosphere_area_fraction'
       units    = '1'
       call fld_add(flds_x2o, flds_x2o_map, name, longname=longname, stdname=stdname, units=units)

       name = 'Sf_afracr'
       longname = 'fractional atmosphere coverage used in radiation computations wrt ocean'
       stdname  = 'atmosphere_area_fraction'
       units    = '1'
       call fld_add(flds_x2o, flds_x2o_map, name, longname=longname, stdname=stdname, units=units)

       ! Net shortwave radiation

       name = 'Foxx_swnet_afracr'
       longname = 'net shortwave radiation times atmosphere fraction'
       stdname = 'product_of_net_downward_shortwave_flux_at_sea_water_surface_and_atmosphere_area_fraction'
       units = 'W m-2'
       call fld_add(flds_x2o, flds_x2o_map, name, longname=longname, stdname=stdname, units=units)
    endif

    !-----------------------------------------------------------------------------
    ! Read namelist for CARMA
    ! if carma_flds are specified then setup fields for CLM to CAM communication
    !-----------------------------------------------------------------------------

    call shr_carma_readnl(nlfilename='drv_flds_in', carma_fields=carma_fields)
    if (carma_fields /= ' ') then
       longname = 'Volumetric soil water'
       stdname  = 'soil_water'
       units    = 'm3/m3'
       call fld_add(flds_l2x, flds_l2x_map, trim(carma_fields), longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, trim(carma_fields))
    endif

    !-----------------------------------------------------------------------------
    ! Read namelist for MEGAN
    ! if MEGAN emission are specified then setup fields for CLM to CAM communication
    ! (emissions fluxes)
    !-----------------------------------------------------------------------------

    call shr_megan_readnl(nlfilename='drv_flds_in', ID=ID, megan_fields=megan_voc_fields)
    if (shr_megan_mechcomps_n>0) then
       longname = 'MEGAN emission fluxes'
       stdname  = 'megan'
       units    = 'molecules/m2/sec'
       call fld_add(flds_l2x, flds_l2x_map, trim(megan_voc_fields), longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, trim(megan_voc_fields))
    endif

    !-----------------------------------------------------------------------------
    ! Read namelist for Fire Emissions
    ! if fire emission are specified then setup fields for CLM to CAM communication
    ! (emissions fluxes)
    !-----------------------------------------------------------------------------

    call shr_fire_emis_readnl(nlfilename='drv_flds_in', ID=ID, emis_fields=fire_emis_fields)
    if (shr_fire_emis_mechcomps_n>0) then
       longname = 'wild fire emission fluxes'
       stdname  = 'fire_emis'
       units    = 'kg/m2/sec'
       call fld_add(flds_l2x, flds_l2x_map, trim(fire_emis_fields), longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, trim(fire_emis_fields))

       longname = 'wild fire plume height'
       stdname  = 'fire_plume_top'
       units    = 'm'
       call fld_add(flds_l2x, flds_l2x_map, trim(shr_fire_emis_ztop_token), longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, trim(shr_fire_emis_ztop_token))
    endif

    !-----------------------------------------------------------------------------
    ! Dry Deposition fields
    ! First read namelist and figure out the drydep field list to pass
    ! Then check if file exists and if not, n_drydep will be zero
    ! Then add dry deposition fields to land export and atmosphere import states
    ! Then initialize dry deposition fields
    ! Note: CAM and CLM will then call seq_drydep_setHCoeff
    !-----------------------------------------------------------------------------

    call seq_drydep_readnl(nlfilename="drv_flds_in", ID=ID, seq_drydep_fields=drydep_fields)
    if ( lnd_drydep ) then
       longname = 'dry deposition velocity'
       stdname  = 'drydep_vel'
       units    = 'cm/sec'
       call fld_add(flds_l2x, flds_l2x_map, drydep_fields, longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2a, flds_x2a_map, drydep_fields, longname=longname, stdname=stdname, units=units)
    endif
    call seq_drydep_init( )

    !-----------------------------------------------------------------------------
    ! Nitrogen Deposition fields
    ! First read namelist and figure out the ndepdep field list to pass
    ! Then check if file exists and if not, n_drydep will be zero
    ! Then add nitrogen deposition fields to atm export, lnd import and ocn import
    !-----------------------------------------------------------------------------

    call shr_ndep_readnl(nlfilename="drv_flds_in", ID=ID, ndep_fields=ndep_fields, add_ndep_fields=add_ndep_fields)
    if (add_ndep_fields) then
       longname = 'nitrogen deposition flux'
       stdname  = 'nitrogen_deposition'
       units    = 'kg(N)/m2/sec'
       call fld_add(flds_a2x, flds_a2x_map, ndep_fields, longname=longname, stdname=stdname, units=units)
       call fld_add(flds_x2l, flds_x2l_map, ndep_fields)
       call fld_add(flds_x2o, flds_x2o_map, ndep_fields)
    end if

    !----------------------------------------------------------------------------
    ! state + flux fields
    !----------------------------------------------------------------------------

    if (seq_comm_iamroot(ID)) then
       write(llogunit, "(A)") subname//': flds_a2x        = ',trim(flds_a2x)
       write(llogunit, "(A)") subname//': flds_a2x_map    = ',trim(flds_a2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2a        = ',trim(flds_x2a)
       write(llogunit, "(A)") subname//': flds_x2a_map    = ',trim(flds_x2a_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_l2x        = ',trim(flds_l2x)
       write(llogunit, "(A)") subname//': flds_l2x_map    = ',trim(flds_l2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2l        = ',trim(flds_x2l)
       write(llogunit, "(A)") subname//': flds_x2l_map    = ',trim(flds_x2l_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_i2x        = ',trim(flds_i2x)
       write(llogunit, "(A)") subname//': flds_i2x_map    = ',trim(flds_i2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2i        = ',trim(flds_x2i)
       write(llogunit, "(A)") subname//': flds_x2i_map    = ',trim(flds_x2i_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_o2x        = ',trim(flds_o2x)
       write(llogunit, "(A)") subname//': flds_o2x_map    = ',trim(flds_o2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2o        = ',trim(flds_x2o)
       write(llogunit, "(A)") subname//': flds_x2o_map    = ',trim(flds_x2o_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_g2x        = ',trim(flds_g2x)
       write(llogunit, "(A)") subname//': flds_g2x_map    = ',trim(flds_g2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2g        = ',trim(flds_x2g)
       write(llogunit, "(A)") subname//': flds_x2g_map    = ',trim(flds_x2g_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_xao        = ',trim(flds_xao)
       write(llogunit, "(A)") subname//': flds_xao_map    = ',trim(flds_xao_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_xao_albedo     = ',trim(flds_xao_albedo)
       write(llogunit, "(A)") subname//': flds_xao_albedo_map = ',trim(flds_xao_albedo_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_xao_diurnl     = ',trim(flds_xao_diurnl)
       write(llogunit, "(A)") subname//': flds_xao_diurnl_map = ',trim(flds_xao_diurnl_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_r2x        = ',trim(flds_r2x)
       write(llogunit, "(A)") subname//': flds_r2x_map    = ',trim(flds_r2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2r        = ',trim(flds_x2r)
       write(llogunit, "(A)") subname//': flds_x2r_map    = ',trim(flds_x2r_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_w2x        = ',trim(flds_w2x)
       write(llogunit, "(A)") subname//': flds_w2x_map    = ',trim(flds_w2x_map)
       write(llogunit, "(A)") '-------------------------------------------------'

       write(llogunit, "(A)") subname//': flds_x2w        = ',trim(flds_x2w)
       write(llogunit, "(A)") subname//': flds_x2w_map    = ',trim(flds_x2w_map)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if

  end subroutine shr_nuopc_flds_set

  !===============================================================================

  subroutine shr_nuopc_flds_get_num_entries(num_entries)

    ! input/output parameters:
    integer, intent(out)  :: num_entries

    ! local variables
    character(len=*),parameter :: subname = '(shr_nuopc_flds_get_num_entries) '
    !-------------------------------------------------------------------------------

    num_entries = n_entries

  end subroutine shr_nuopc_flds_get_num_entries

  !===============================================================================

  subroutine shr_nuopc_flds_get_entry(nentry, shortname, longname, stdname, units)

    ! input/output parameters:
    integer, intent(in)  :: nentry
    character(len=*),optional, intent(out) :: shortname
    character(len=*),optional, intent(out) :: longname
    character(len=*),optional, intent(out) :: stdname
    character(len=*),optional, intent(out) :: units

    ! local variables
    character(len=*),parameter :: subname = '(shr_nuopc_flds_get_entry) '
    !-------------------------------------------------------------------------------

    if (present(shortname)) then
       shortname = trim(lookup_entry(nentry,1))
    endif
    if (present(longname)) then
       longname = trim(lookup_entry(nentry,2))
    endif
    if (present(stdname)) then
       stdname = trim(lookup_entry(nentry,3))
    endif
    if (present(units)) then
       units = trim(lookup_entry(nentry,4))
    endif

  end subroutine shr_nuopc_flds_get_entry

  !===============================================================================

  subroutine fld_dom_add(fldlist, fldname, longname, stdname, units)

    ! Returns new concatentated field and map lists

    ! input/output parameters:
    character(len=*),intent(inout)       :: fldlist   ! output field name
    character(len=*),intent(in)          :: fldname   ! fldname to add to fldlist
    character(len=*),intent(in),optional :: longname
    character(len=*),intent(in),optional :: stdname
    character(len=*),intent(in),optional :: units

    ! local variables
    character(len=*),parameter :: subname = '(fld_dom_add) '
    !-------------------------------------------------------------------------------

    if (trim(fldlist) == '') then
       fldlist = trim(fldname)
    else
       fldlist = trim(fldlist)//':'//trim(fldname)
    end if

    if (len_trim(fldlist) >= CXX) then
       write(llogunit,*)'fields are = ',trim(fldlist)
       write(llogunit,*)'fields length = ',len_trim(fldlist)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

    if (present(longname) .and. present(stdname) .and. present(units)) then
       call fld_metadata_set(trim(fldname), longname, stdname, units)
    endif

  end subroutine fld_dom_add

  !===============================================================================

  subroutine fld_add(fldlist, maplist, fldname, longname, stdname , units)

    ! Returns new concatentated field and map lists

    ! uses:
    use shr_string_mod, only : shr_string_listGetNum, shr_string_listGetName

    ! input/output parameters:
    character(len=*),intent(inout)       :: fldlist   ! output field name
    character(len=*),intent(inout)       :: maplist   ! output map list to match fldlist
    character(len=*),intent(in)          :: fldname   ! fldname to add to fldlist
    character(len=*),intent(in),optional :: longname
    character(len=*),intent(in),optional :: stdname
    character(len=*),intent(in),optional :: units

    ! local variables
    integer           :: n,num
    character(len=CS) :: mapname
    character(len=CS) :: tempname
    character(len=*),parameter :: subname = '(fld_add) '
    !-------------------------------------------------------------------------------

    if (trim(fldlist) == '') then
       fldlist = trim(fldname)
    else
       fldlist = trim(fldlist)//':'//trim(fldname)
    end if

    if (len_trim(fldlist) >= CXX) then
       write(llogunit,*)'fields are = ',trim(fldlist)
       write(llogunit,*)'fields length = ',len_trim(fldlist)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

    ! for maps this includes the possibility of more than one field (e.g. megan)
    num = shr_string_listGetNum(fldname)
    do n = 1,num
       call shr_string_listGetName(fldname, n, tempname)
       if (tempname(1:1) == 'S') then
          if (tempname(1:4) == 'Sa_u' .or. tempname(1:4) == 'Sa_v') then
             mapname = 'patch'
          else
             mapname = 'bilinear'
          endif
       else if (tempname(1:1) == 'F')  then
          ! TODO: the following is a hack to work with the way that mapping files are
          ! defined in med.F90 - this needs to be reworked!!!
          if (trim(tempname) == 'Forr_rofi') then
             mapname = 'bilinear'
          else
             mapname = 'conservefrac'
          end if
       else if (tempname(1:2) == 'PF') then
          mapname = 'conservedst'
       else
          write(llogunit,*) subname//'ERROR: fldname must start with S,F,P, not ',trim(tempname)
          call shr_sys_abort(subname//"ERROR: fldname must start with S, F, or P")
       end if
       if (trim(maplist) == '') then
          maplist = trim(mapname)
       else
          maplist = trim(maplist)//':'//trim(mapname)
       end if
    end do

    if (len_trim(maplist) >= CXX) then
       write(llogunit,*)'fields are = ',trim(maplist)
       write(llogunit,*)'fields length = ',len_trim(maplist)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx  has been exceeded')
    end if

    if (present(longname) .and. present(stdname) .and. present(units)) then
       call fld_metadata_set(trim(fldname), longname=longname, stdname=stdname, units=units)
    endif

  end subroutine fld_add

  !===============================================================================

  subroutine fld_metadata_set(attname , longname, stdname, units)

    ! input/output parameters:
    character(len=*), intent(in) :: attname
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: stdname
    character(len=*), intent(in) :: units

    ! local variables
    integer :: i, j
    character(len=*),parameter :: subname = '(fld_metadata_set) '
    !-------------------------------------------------------------------------------

    ! If the attname passed in contains colons it is assumed to be a list of fields
    ! all of which have the same names and units

    i = index(attname,':')
    j=1

    do while(i>j .and. i<=len_trim(attname))
       if (fld_metadata_entry(attname(j:i-1)) <= 0) then
          n_entries = n_entries + 1
          lookup_entry(n_entries,1) = attname(j:i-1)
          lookup_entry(n_entries,2) = trim(longname)
          lookup_entry(n_entries,3) = trim(stdname )
          lookup_entry(n_entries,4) = trim(units   )
          j=i+1
          i =  index(attname(j:),':') + j - 1
       endif
    enddo
    if (fld_metadata_entry(attname(j:i)) <= 0) then
       n_entries = n_entries + 1
       i = len_trim(attname)
       lookup_entry(n_entries,1) = attname(j:i)
       lookup_entry(n_entries,2) = trim(longname)
       lookup_entry(n_entries,3) = trim(stdname )
       lookup_entry(n_entries,4) = trim(units   )
    endif

    if (n_entries .ge. nmax) then
       write(llogunit,*)'n_entries= ',n_entries,' nmax = ',nmax,' attname= ',trim(attname)
       call shr_sys_abort(subname//'ERROR: nmax fields in lookup_entry table exceeded')
    end if

  end subroutine fld_metadata_set

  !===============================================================================

  subroutine fld_set_glc_elevclass(name, attname, longname, stdname, units, fieldlist, maplist, additional_list)

    ! Sets a coupling field for all glc elevation classes (1:glc_nec) plus bare land
    ! (index 0).
    !
    ! Note that, if glc_nec = 0, then we don't create any coupling fields (not even the
    ! bare land (0) index)
    !
    ! Puts the coupling fields in the given fieldlist, and also does the appropriate
    ! metadata_set calls.
    !
    ! additional_list should be .false. (or absent) the first time this is called for a
    ! given set of coupling fields. However, if this same set of coupling fields is being
    ! added to multiple field lists, then additional_list should be set to true for the
    ! second and subsequent calls; in this case, the metadata_set calls are not done
    ! (because they have already been done).
    !
    ! name, attname and longname give the base name of the field; the elevation class
    ! index will be appended as a suffix

    ! uses:
    use glc_elevclass_mod, only : glc_get_num_elevation_classes, glc_elevclass_as_string

    ! input/output parameters:
    character(len=*) , intent(in)    :: name                   ! base field name to add to fieldlist
    character(len=*) , intent(in)    :: attname                ! base field name for metadata
    character(len=*) , intent(in)    :: longname               ! base long name for metadata
    character(len=*) , intent(in)    :: stdname                ! standard name for metadata
    character(len=*) , intent(in)    :: units                  ! units for metadata
    character(len=*) , intent(inout) :: fieldlist              ! field list into which the fields should be added
    character(len=*) , intent(inout) :: maplist                ! mapping list into which the field mapping types should be added
    logical          , intent(in), optional :: additional_list ! whether this is an additional list for
                                                               ! the same set of coupling fields
                                                               ! (see above for details; defaults to false)
    ! local variables
    integer            :: num
    character(len= 16) :: cnum
    logical            :: l_additional_list  ! local version of the optional additional_list argument
    !-------------------------------------------------------------------------------

    l_additional_list = .false.
    if (present(additional_list)) then
       l_additional_list = additional_list
    end if

    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)

          call fld_add(fieldlist, maplist, trim(name) // trim(cnum))

          if (.not. l_additional_list) then
             call fld_metadata_set( &
                  attname  = trim(attname) // trim(cnum), &
                  longname = trim(longname) // ' of elevation class ' // trim(cnum), &
                  stdname  = stdname, &
                  units    = units)
          end if
       end do
    end if

  end subroutine fld_set_glc_elevclass

  !===============================================================================

  integer function fld_metadata_entry(shortname)

    ! !INPUT/OUTPUT PARAMETERS:
    character(len=*), intent(in)  :: shortname

    !--- local ---
    integer :: nentry
    character(len=*),parameter :: subname = '(fld_metadata_entry) '
    !-------------------------------------------------------------------------------

    call fld_metadata_get(shortname, nentry=nentry)
    fld_metadata_entry = nentry

  end function fld_metadata_entry

  !===============================================================================

  subroutine fld_metadata_get(shortname, longname, stdname, units, nentry)

    ! uses:
    use shr_string_mod, only : shr_string_lastindex

    ! input/output parameters:
    character(len=*), intent(in)  :: shortname
    character(len=*),optional, intent(out) :: longname
    character(len=*),optional, intent(out) :: stdname
    character(len=*),optional, intent(out) :: units
    integer         ,optional, intent(out) :: nentry

    ! local variables
    integer            :: i,n,lnentry
    logical            :: found
    character(len=CSS) :: llongname, lstdname, lunits, lshortname  ! local copies
    character(len=*),parameter :: unknown = 'unknown'
    character(len=*),parameter :: subname = '(fld_metadata_get) '
    !-------------------------------------------------------------------------------

    !--- define field metadata (name, long_name, standard_name, units=units) ---

    llongname = trim(unknown)
    lstdname  = trim(unknown)
    lunits    = trim(unknown)

    found = .false.
    lnentry = 0

    if (.not.found) then
       i = 1
       do while (i <= n_entries .and. .not.found)
          lshortname = trim(shortname)
          if (trim(lshortname) == trim(lookup_entry(i,1))) then
             llongname = trim(lookup_entry(i,2))
             lstdname  = trim(lookup_entry(i,3))
             lunits    = trim(lookup_entry(i,4))
             found     =.true.
             lnentry   = i
          end if
          i = i + 1
       end do
    endif

    if (.not.found) then
       i = 1
       do while (i <= n_entries .and. .not.found)
          n = shr_string_lastIndex(shortname, "_")
          lshortname = ""
          if (n < len_trim(shortname)) lshortname = shortname(n+1:len_trim(shortname))
          if (trim(lshortname) == trim(lookup_entry(i,1))) then
             llongname = trim(lookup_entry(i,2))
             lstdname  = trim(lookup_entry(i,3))
             lunits    = trim(lookup_entry(i,4))
             found     = .true.
             lnentry   = i
          end if
          i = i + 1
       end do
    endif

    if (present(longname)) then
       longname = trim(llongname)
    endif
    if (present(stdname))  then
       stdname = trim(lstdname)
    endif
    if (present(units)) then
       units = trim(lunits)
    endif
    if (present(nentry)) then
       nentry = lnentry
    endif

  end subroutine fld_metadata_get

end module shr_nuopc_flds_mod
