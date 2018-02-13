module shr_nuopc_fldList_mod

  use shr_kind_mod      , only : CX => shr_kind_CX, CXX => shr_kind_CXX, CS=>shr_kind_CS
  use shr_sys_mod       , only : shr_sys_abort
  use seq_comm_mct      , only : llogunit => logunit
  use seq_drydep_mod    , only : seq_drydep_init, seq_drydep_readnl, lnd_drydep
  use shr_megan_mod     , only : shr_megan_readnl, shr_megan_mechcomps_n
  use shr_fire_emis_mod , only : shr_fire_emis_readnl, shr_fire_emis_mechcomps_n, shr_fire_emis_ztop_token
  use shr_carma_mod     , only : shr_carma_readnl
  use shr_ndep_mod      , only : shr_ndep_readnl
  use shr_flds_mod      , only : shr_flds_dom_coord, shr_flds_dom_other
  use shr_string_mod    , only : shr_string_listGetNum, shr_string_listGetName

  implicit none
  public

  integer, parameter, private :: CSS = 256  ! use longer short character
  integer, parameter, private :: CLL = 1024

  !----------------------------------------------------------------------------
  ! routines
  !----------------------------------------------------------------------------

  public :: shr_nuopc_fldList_Init
  public :: shr_nuopc_fldList_SetDict
  public :: shr_nuopc_fldList_Advertise
  public :: shr_nuopc_fldList_Realize
  public :: shr_nuopc_fldList_SetScalarField

  interface fldList_AddFld ; module procedure &
    fldList_AddFld_Src, &
    fldList_AddFld_Dst, &
  end interface

  private :: fldList_AddDomain
  private :: fldList_AddMetadata
  private :: fldList_AddMap
  private :: fldList_AddFld

  !----------------------------------------------------------------------------
  ! other field lists - column deliminated string
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
  ! scalar data
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
  ! private data
  !----------------------------------------------------------------------------

  !character(CXX) :: flds_dom_coord
  !character(CXX) :: flds_dom_other

  ! This defines the components.

  integer, parameter :: ncomps =8
  integer, parameter :: compmed=1
  integer, parameter :: compatm=2
  integer, parameter :: complnd=3
  integer, parameter :: compocn=4
  integer, parameter :: compice=5
  integer, parameter :: comprof=6
  integer, parameter :: compwav=7
  integer, parameter :: compglc=8

  character(len=*),parameter :: compname(ncomps) = (/'med','atm','lnd','ocn','ice','rof','wav','glc'/)

  ! This defines the med_mapping_allowed is a starting point for what is
  ! allowed in this coupled system.  It will be revised further after the system
  ! starts, but any coupling set to false will never be allowed.  As new connections
  ! are allowed, just update the table below.
  ! - the rows are the destination of coupling
  ! - the columns are the source of coupling
  ! - So, the second column indicates which models the atm is coupled to.
  ! - And the second row indicates which models are coupled to the atm.
  ! The mediator is not connected to any components because the mediator
  ! doesn't have it's own grid and only acts as a hub.

  ! tcraig, turned off glc2ocn and glc2ice for time being
  logical, parameter :: med_coupling_allowed(ncomps,ncomps) = &
   (/ .false., .false., .false., .false., .false., .false., .false., .false., &  ! med
      .false., .false., .true. , .true. , .true. , .false., .false., .false., &  ! atm
      .false., .true. , .false., .false., .false., .true. , .false., .true. , &  ! lnd
      .false., .true. , .false., .false., .true. , .true. , .true. , .false., &  ! ocn
      .false., .true. , .false., .true. , .false., .true. , .false., .false., &  ! ice
      .false., .false., .true. , .false., .false., .false., .false., .false., &  ! rof
      .false., .true. , .false., .true. , .true. , .false., .false., .false., &  ! wav
      .false., .false., .true. , .false., .false., .false., .false., .false.  /) ! glc
   !   med      atm      lnd      ocn      ice      rof      wav      glc

  ! mappings used for generating route handle (RH) arrays

  integer, parameter :: nmappers=5
  integer, parameter :: mapbilnr=1
  integer, parameter :: mapconsf=2
  integer, parameter :: mapconsd=3
  integer, parameter :: mappatch=4
  integer, parameter :: mapfcopy=5
  integer, parameter :: mapunset=0

  character(len=*), parameter :: mapnames(nmappers) = (/'bilnr','consf','consd','patch','fcopy'/)

  character(len=*), parameter :: undef     = 'undefined'
  integer         , parameter :: nmax      = 1000        ! maximum number of entries in metadta_entry
  integer                     :: n_entries = 0           ! actual number of entries in metadta_entry
  character(len=CSS)          :: metadta_entry(nmax,4) = undef

  type shr_nuopc_src_entry_type
     character(CS) :: stdname
     character(CS) :: shortname
     character(CS) :: mapindex(ncomps) = mapunset
     character(CS) :: mapnorm(ncomps) = 'unset'
     character(CX) :: mapfile(ncomps) = 'unset'
  end type shr_nuopc_fldList_src_entry_type

  type shr_nuopc_dst_entry_type
     character(CS) :: stdname
     character(CS) :: shortname
     logical       :: merge_with_weight = .false.
  end type shr_nuopc_fldList_dst_entry_type

  type shr_nuopc_src_fldlist_type
     type (shr_nuopc_src_entry_type), pointer :: flds(:)
  end type shr_nuopc_src_fldlist_type

  type shr_nuopc_dst_fldlist_type
     type (shr_nuopc_dst_entry_type), pointer :: flds(:)
  end type shr_nuopc_dst_fldlist_type

  ! Instantiations needed to create field bundles

  ! The following is used in the advertise phases of the components and mediator
  type (shr_nuopc_dst_fldlist_type), pointer :: fldListFr(ncomps)
  type (shr_nuopc_src_fldlist_type), pointer :: fldListTo(ncomps)
  type (shr_nuopc_src_fldlist_type)          :: fldListXao_fluxes_a
  type (shr_nuopc_src_fldlist_type)          :: fldListXao_fluxes_o
  type (shr_nuopc_src_fldlist_type)          :: fldListXao_ocnalb_a
  type (shr_nuopc_src_fldlist_type)          :: fldListXao_ocnalb_o

  character(CXX) :: flds_a2x = ''
  character(CXX) :: flds_x2a = ''
  character(CXX) :: flds_i2x = ''
  character(CXX) :: flds_x2i = ''
  character(CXX) :: flds_l2x = ''
  character(CXX) :: flds_x2l = ''
  character(CXX) :: flds_o2x = ''
  character(CXX) :: flds_x2o = ''
  character(CXX) :: flds_g2x = ''
  character(CXX) :: flds_x2g = ''
  character(CXX) :: flds_w2x = ''
  character(CXX) :: flds_x2w = ''
  character(CXX) :: flds_r2x = ''
  character(CXX) :: flds_x2r = ''

  ! Mediator field bundles are initialized from these colon deliminated strings
  character(CXX) :: flds_xao_fluxes = ''
  character(CXX) :: flds_xao_ocnalb = ''
  character(CXX) :: flds_xao_diurnl = ''
  character(CXX) :: flds_l2x_to_glc = ''
  character(CXX) :: flds_x2l_fr_glc = ''
  character(CXX) :: flds_g2x_to_lnd = ''

!================================================================================
contains
!================================================================================

  subroutine shr_nuopc_fldList_SetDict(rc)
    ! ----------------------------------------------
    ! Build NUOPC dictionary from flds data
    ! ----------------------------------------------
    integer,          intent(inout)  :: rc

    ! local variables
    integer       :: n,
    character(CS) :: stdname
    character(CS) :: units
    character(len=*), parameter :: subname='(shr_nuopc_fldList_SetDict)'
    !------------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1,n_entries
       stdname = trim(metadta_entry(n,3))
       units   = trim(metadta_entry(n,4))
       if (.not.NUOPC_FieldDictionaryHasEntry(stdname)) then
          call ESMF_LogWrite(subname//': Add:'//trim(stdname), ESMF_LOGMSG_INFO, rc=rc)
          call NUOPC_FieldDictionaryAddEntry(standardName=stdname, canonicalUnits=units, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
    enddo

  end subroutine shr_nuopc_fldList_SetDict

  !================================================================================

  subroutine shr_nuopc_fldList_Init(gcomp, ID)

    ! uses:
    use shr_file_mod,      only : shr_file_getUnit, shr_file_freeUnit
    use shr_mpi_mod,       only : shr_mpi_bcast
    use glc_elevclass_mod, only : glc_elevclass_init

    ! input/output parameters:
    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: ID        ! seq_comm ID

    ! local variables:
    type(ESMF_VM)      :: vm
    integer            :: localPet
    logical            :: mastertask
    character(len=CSS) :: attname
    character(len=CSS) :: units
    character(len=CSS) :: longname
    character(len=CSS) :: stdname
    integer            :: num, i, n
    integer            :: n1, n2, n3
    character(len=  2) :: cnum
    character(len=CSS) :: name
    character(len=CX)  :: atm2ice_fmapname
    character(len=CX)  :: atm2ice_smapname
    character(len=CX)  :: atm2ice_vmapname
    character(len=CX)  :: atm2lnd_fmapname
    character(len=CX)  :: atm2lnd_smapname
    character(len=CX)  :: atm2ocn_fmapname
    character(len=CX)  :: atm2ocn_smapname
    character(len=CX)  :: atm2ocn_vmapname
    character(len=CX)  :: atm2wav_smapname
    character(len=CX)  :: glc2lnd_fmapname
    character(len=CX)  :: glc2lnd_smapname
    character(len=CX)  :: glc2ice_rmapname
    character(len=CX)  :: glc2ocn_rmapname
    character(len=CX)  :: ice2atm_fmapname
    character(len=CX)  :: ice2atm_smapname
    character(len=CX)  :: ice2wav_smapname
    character(len=CX)  :: lnd2atm_fmapname
    character(len=CX)  :: lnd2atm_smapname
    character(len=CX)  :: lnd2glc_fmapname
    character(len=CX)  :: lnd2glc_smapname
    character(len=CX)  :: lnd2rof_fmapname
    character(len=CX)  :: ocn2atm_fmapname
    character(len=CX)  :: ocn2atm_smapname
    character(len=CX)  :: ocn2wav_smapname
    character(len=CX)  :: rof2lnd_fmapname
    character(len=CX)  :: rof2ocn_fmapname
    character(len=CX)  :: rof2ocn_ice_rmapname
    character(len=CX)  :: rof2ocn_liq_rmapname
    character(len=CX)  :: wav2ocn_smapname
    logical            :: flds_co2a  ! use case
    logical            :: flds_co2b  ! use case
    logical            :: flds_co2c  ! use case
    logical            :: flds_wiso
    logical            :: do_flux_diurnal
    integer            :: glc_nec
    !---------------------------------------------------------------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    mastertask = .false.
    if (localPet == 0) mastertask=.true.

    call NUOPC_CompAttributeGet(gcomp, name='flds_co2a', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) flds_co2a

    call NUOPC_CompAttributeGet(gcomp, name='flds_co2b', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) flds_co2b

    call NUOPC_CompAttributeGet(gcomp, name='flds_co2c', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) flds_co2c

    call NUOPC_CompAttributeGet(gcomp, name='flds_wiso', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) flds_wiso

    call NUOPC_CompAttributeGet(gcomp, name='ice_ncat', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) ice_ncat

    call NUOPC_CompAttributeGet(gcomp, name='flds_i2o_per_cat', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) flds_i2o_per_cat

    call NUOPC_CompAttributeGet(gcomp, name='glc_nec', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) glc_nec
    call glc_elevclass_init(glc_nec)

    call NUOPC_CompAttributeGet(gcomp, name='flux_diurnal', value=do_flux_diurnal, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) do_flux_diurnal

    !----------------------------------------------------------
    ! Initialize mapping file names
    !----------------------------------------------------------

    ! To atm

    call NUOPC_CompAttributeGet(gcomp, name='ice2atm_fmapname', value=ice2atm_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ice2atm_fmapname = ',trim(ice2atm_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='ice2atm_smapname', value=ice2atm_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ice2atm_smapname = ',trim(ice2atm_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='lnd2atm_fmapname', value=lnd2atm_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2atm_fmapname = ',trim(lnd2atm_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_smapname', value=ocn2atm_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ocn2atm_smapname = ',trim(ocn2atm_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_fmapname', value=ocn2atm_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ocn2atm_fmapname = ',trim(ocn2atm_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='lnd2atm_smapname', value=lnd2atm_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2atm_smapname = ',trim(lnd2atm_smapname), ESMF_LOGMSG_INFO)

    ! To lnd

    call NUOPC_CompAttributeGet(gcomp, name='atm2lnd_fmapname', value=atm2lnd_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2lnd_fmapname = ',trim(atm2lnd_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='atm2lnd_smapname', value=atm2lnd_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2lnd_smapname = ',trim(atm2lnd_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='rof2lnd_fmapname', value=rof2lnd_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2lnd_fmapname = ',trim(rof2lnd_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='glc2lnd_fmapname', value=glc2lnd_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2lnd_smapname = ',trim(glc2lnd_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='glc2lnd_smapname', value=glc2lnd_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2lnd_smapname = ',trim(glc2lnd_smapname), ESMF_LOGMSG_INFO)

    ! To ice

    call NUOPC_CompAttributeGet(gcomp, name='atm2ice_fmapname', value=atm2ice_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ice_fmapname = ',trim(atm2ice_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ice_smapname', value=atm2ice_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ice_smapname = ',trim(atm2ice_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ice_vmapname', value=atm2ice_vmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ice_vmapname = ',trim(atm2ice_vmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='glc2ice_rmapname', value=glc2ice_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2ice_rmapname = ',trim(glc2ice_rmapname), ESMF_LOGMSG_INFO)

    ! To ocn

    call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_fmapname', value=atm2ocn_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ocn_fmapname = ',trim(atm2ocn_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_smapname', value=atm2ocn_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ocn_smapname = ',trim(atm2ocn_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_vmapname', value=atm2ocn_vmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ocn_vmapname = ',trim(atm2ocn_vmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='glc2ocn_rmapname', value=glc2ocn_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2ocn_rmapname = ',trim(glc2ocn_rmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='wav2ocn_fmapname', value=wav2ocn_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('wav2ocn_fmapname = ',trim(wav2ocn_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='rof2ocn_fmapname', value=rof2ocn_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2ocn_fmapname = ',trim(rof2ocn_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='rof2ocn_liq_rmapname', value=rof2ocn_liq_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2ocn_liq_rmapname = ',trim(rof2ocn_liq_rmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='rof2ocn_ice_rmapname', value=rof2ocn_ice_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2ocn_ice_rmapname = ',trim(rof2ocn_ice_rmapname), ESMF_LOGMSG_INFO)

    ! To rof

    call NUOPC_CompAttributeGet(gcomp, name='lnd2rof_fmapname', value=lnd2rof_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2rof_fmapname = ',trim(lnd2rof_fmapname), ESMF_LOGMSG_INFO)

    ! To glc

    call NUOPC_CompAttributeGet(gcomp, name='lnd2glc_fmapname', value=lnd2glc_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2glc_fmapname = ',trim(lnd2glc_fmapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='lnd2glc_smapname', value=lnd2glc_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2glc_smapname = ',trim(lnd2glc_smapname), ESMF_LOGMSG_INFO)

    ! To wav

    call NUOPC_CompAttributeGet(gcomp, name='atm2wav_smapname', value=atm2wav_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2wav_smapname = ',trim(atm2wav_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='ice2wav_smapname', value=ice2wav_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ice2wav_smapname = ',trim(ice2wav_smapname), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='ocn2wav_smapname', value=ocn2wav_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ocn2wav_smapname = ',trim(ocn2wav_smapname), ESMF_LOGMSG_INFO)

    !----------------------------------------------------------
    ! scalar information
    !----------------------------------------------------------

    longname = trim(flds_scalar_name)
    stdname  = trim(flds_scalar_name)
    units    = 'unitless'
    attname  = trim(flds_scalar_name)
    call fld_metadata_set(attname, longname, stdname, units=units)
    do n = 1,ncomps
       call fldList_AddFld(fldListFr(ncomp), stdname=trim(flds_scalar_name))
       call fldList_AddFld(fldListTo(ncomp), stdname=trim(flds_scalar_name))
    end do

    !----------------------------------------------------------
    ! domain coordinates (appear in the share module shr_flds_mod)
    !----------------------------------------------------------

    shr_flds_dom_coord  = ''
    shr_flds_dom_other  = ''

    longname = 'latitude'
    stdname  = 'latitude'
    units    = 'degrees north'
    call fldList_AddDomain(shr_flds_dom_coord,'lat', longname, stdname, units=units)

    longname = 'longitude'
    stdname  = 'longitude'
    units    = 'degrees east'
    call fldList_AddDomain(shr_flds_dom_coord,'lon', longname, stdname, units=units)

    longname = 'height'
    stdname  = 'height, depth, or levels'
    units    = 'unitless'
    call fldList_AddDomain(shr_flds_dom_coord,'hgt', longname, stdname, units=units)

    longname = 'cell_area_model'
    stdname  = 'cell area from model'
    units    = 'radian^2'
    call fldList_AddDomain(shr_flds_dom_other,'area', longname, stdname, units=units)

    longname = 'cell_area_mapping'
    stdname  = 'cell area from mapping file'
    units    = 'radian^2'
    call fldList_AddDomain(shr_flds_dom_other,'aream', longname, stdname, units=units)

    longname = 'mask'
    stdname  = 'mask'
    units    = '1'
    call fldList_AddDomain(shr_flds_dom_other,'mask', longname, stdname, units=units)

    longname = 'area_fraction'
    stdname  = 'area fraction'
    units    = '1'
    call fldList_AddDomain(shr_flds_dom_other,'frac', longname, stdname, units=units)

    !----------------------------------------------------------
    ! Masks and Fractions
    !----------------------------------------------------------

    longname = 'Surface fraction in land'
    stdname  = 'land_fraction_from_land'
    units    = '1'
    call fldList_AddMetadata(fldname="Sl_lfrin", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_lfrin')

    longname = 'Surface land fraction'
    stdname  = 'land_area_fraction'
    units    = '1'
    call fldList_AddMetadata(fldname="Sl_lfrac", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_lfrac')

    longname = 'Surface ocean fraction'
    stdname  = 'sea_area_fraction'
    units    = '1'
    call fldList_AddMetadata(fldname="So_ifrac", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compatm)%flds, 'So_ifrac')

    longname = 'Sea surface mask'
    stdname  = 'sea_surface_mask'
    units    = '1'
    call fldList_AddMetadata(fldname="So_omask", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_imask')

    longname = 'Sea Ice mask'
    stdname  = 'sea_ice_mask'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_imask", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_imask')

    longname = 'Fractional ice coverage wrt ocean'
    stdname  = 'sea_ice_area_fraction'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_ifrac", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_ifrac', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Si_ifrac')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Si_ifrac')
    call fldList_AddFld(fldListTo(compwav)%flds, 'Si_ifrac')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compatm,  mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    !----------------------------------------------------------
    ! Fields from atm
    !----------------------------------------------------------

    longname = 'Height at the lowest model level'
    stdname  = 'height'
    units    = 'm'
    call fldList_AddMetadata(fldname='Sa_z', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_z', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd), 'Sa_z')
    call fldList_AddFld(fldListTo(compice), 'Sa_z')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapbilnr, 'one', atm2ice_smapfile)

    longname = 'Surface height'
    stdname  = 'height'
    units    = 'm'
    call fldList_AddMetadata(fldname='Sa_topo', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_topo', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd), 'Sa_topo')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)

    ! TODO: Sa_u and Sa_v are mapped to the ocean grid in the mediator - BUT are not sent to the ocean -
    ! They are only used in the atm/ocn flux calculation - so a special ampping will be done in the mediator
    ! for these fields

    longname = 'Zonal wind at the lowest model level'
    stdname  = 'eastward_wind'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname='Sa_u', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_u', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd), 'Sa_u')
    call fldList_AddFld(fldListTo(compice), 'Sa_u')
    call fldList_AddFld(fldListTo(compwav), 'Sa_u')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mappatch, 'one', atm2ice_pmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compwav, mapbilnr, 'one', atm2wav_smapfile)

    longname = 'Meridional wind at the lowest model level'
    stdname  = 'northward_wind'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname='Sa_v', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_v', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd), 'Sa_v')
    call fldList_AddFld(fldListTo(compice), 'Sa_v')
    call fldList_AddFld(fldListTo(compwav), 'Sa_v')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mappatch, 'one', atm2ice_pmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compwav, mapbilnr, 'one', atm2wav_smapfile)

    longname = 'Temperature at the lowest model level'
    stdname  = 'air_temperature'
    units    = 'K'
    fldname  = 'Sa_tbot'
    call fldList_AddMetadata(fldname='Sa_tbot', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_tbot', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd), 'Sa_tbot')
    call fldList_AddFld(fldListTo(compice), 'Sa_tbot')
    call fldList_AddFld(fldListTo(compwav), 'Sa_tbot')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapbilnr, 'one', atm2ice_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compwav, mapbilnr, 'one', atm2wav_smapfile)

    longname = 'Potential temperature at the lowest model level'
    stdname  = 'air_potential_temperature'
    units    = 'K'
    fldname  = 'Sa_ptem'
    call fldList_AddMetadata(fldname='Sa_ptem', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_ptem', comp=compatm, fldindex=n)
    call fldList_AddFld(fldListTo(complnd), 'Sa_ptem', comp=complnd)
    call fldList_AddFld(fldListTo(compice), 'Sa_ptem', comp=compice)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapbilnr, 'one', atm2ice_smapfile)

    longname = 'Specific humidity at the lowest model level'
    stdname  = 'specific_humidity'
    units    = 'kg kg-1'
    fldname  = 'Sa_shum'
    call fldList_AddMetadata(fldname='Sa_shum', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_shum', comp=compatm, fldindex=n)
    call fldList_AddFld(fldListTo(complnd), 'Sa_shum', comp=complnd)
    call fldList_AddFld(fldListTo(compice), 'Sa_shum', comp=compice)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapbilnr, 'one', atm2ice_smapfile)

    longname = 'Pressure at the lowest model level'
    stdname  = 'air_pressure'
    units    = 'Pa'
    fldname  =
    call fldList_AddMetadata(fldname='Sa_pbot', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm), 'Sa_pbot', comp=compatm, fldindex=n1)
    call fldList_AddFld(fldListTo(complnd), 'Sa_pbot', comp=complnd)
    call fldList_AddFld(fldListTo(compice), 'Sa_pbot', comp=compice)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapbilnr, 'one', atm2ice_smapfile)

    longname = 'Density at the lowest model level'
    stdname  = 'air_density'
    units    = 'kg m-3'
    fldname  = 'Sa_dens'
    call fldList_AddFld(fldListFr(compatm), 'Sa_dens', comp=compatm, fldindex=n1)
    call fldList_AddFld(fldListTo(compice), 'Sa_dens', comp=compice)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice,  mapbilnr, 'one', atm2ice_smapfile)

    units    = 'kg m-2 s-1'
    longname = 'Convective precipitation rate'
    stdname  = 'convective_precipitation_flux'
    fldname  =
    call fldList_AddMetadata(fldname='Faxa_rainc', longname=longname, stdname=stdname, units=units)
    longname = 'Large-scale (stable) precipitation rate' ! water equivalent
    stdname  = 'large_scale_precipitation_flux'
    fldname  = 'Faxa_rainl'
    call fldList_AddMetadata(fldname='Faxa_rainc', longname=longname, stdname=stdname, units=units)
    longname = 'Water flux due to rain'
    stdname  = 'rainfall_flux'
    fldname  = 'Faxa_rain'
    call fldList_AddMetadata(fldname='Faxa_rain', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_rainc', index=n)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_rainc')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_rain' )
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rain' )
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Convective snow rate'
    stdname  = 'convective_snowfall_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_snowc' longname=longname, stdname=stdname, units=units)
    longname = 'Large-scale (stable) snow rate'
    stdname  = 'large_scale_snowfall_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_snowl', longname=longname, stdname=stdname, units=units)
    longname = 'Water flux due to snow'
    stdname  = 'surface_snow_melt_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_snowc', index=n)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_snowc')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_snow' )
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_snow' )
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    ! total precipitation to ocean (derived rain + snow, done AFTER mappings)
    longname = 'Water flux (rain+snow)'
    stdname  = 'precipitation_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Foxx_prec', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_prec')

    longname = 'Downward longwave heat flux'
    stdname  = 'downwelling_longwave_flux'
    units    = 'W m-2'
    fldname  = 'Faxa_lwdn'
    call fldList_AddMetadata(fldname='Faxa_lwdn', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Foxx_lwdn', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_lwdn', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_lwdn')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_lwdn')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_lwdn')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Direct near-infrared incident solar radiation'
    stdname  = 'surface_downward_direct_shortwave_flux_due_to_near_infrared_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swndr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swndr', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swndr')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swndr')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)

    longname = 'Direct visible incident solar radiation'
    stdname  = 'surface_downward_direct_shortwave_flux_due_to_visible_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swvdr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swvdr', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swvdr')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swvdr')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)

    longname = 'Diffuse near-infrared incident solar radiation'
    stdname  = 'surface_downward_diffuse_shortwave_flux_due_to_near_infrared_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swndf", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swndf', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swndf')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swndf')
    call fldlist_AddMap(fldListFr(compatm), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm), compice, mapconsf, 'one', atm2ice_fmapfile)

    longname = 'Diffuse visible incident solar radiation'
    stdname  = 'surface_downward_diffuse_shortwave_flux_due_to_visible_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname='Faxa_swvdf', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swvdf', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swvdf')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swvdf')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)

    longname = 'Net shortwave radiation'
    stdname  = 'surface_net_shortwave_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fall_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swnet', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Fall_swnet')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faii_swnet')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_swnet') ! derived using albedos, Faxa_swxxx and swpen
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Net shortwave radiation penetrating into ice and ocean'
    stdname  = 'net_downward_shortwave_flux_in_sea_ice_due_to_penetration'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname='Fioi_swpen', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_swpen', index=n1)
    call fldlist_AddMap(fldListFr(compatm), complnd, mapfcopy)

    longname = 'Hydrophylic black carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_bcphidry', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Foxx_bcphidry', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphidry', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphidry')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Hydrophobic black carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophobic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_bcphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphodry', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphodry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphodry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphodry')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Hydrophylic black carbon wet deposition flux'
    stdname  = 'wet_deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_bcphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphiwet', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphiwet')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphiwet')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphiwet')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Hydrophylic organic carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophylic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_bcphidry", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphidry", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphidry', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphidry')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Hydrophobic organic carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophobic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_ocphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_ocphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_ocphodry', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_ocphodry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_ocphodry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_ocphodry')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Hydrophylic organic carbon wet deposition flux'
    stdname  = 'wet_deposition_flux_of_hydrophylic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_ocphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_ocphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_ocphiwet', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_ocphiwet')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_ocphiwet')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_ocphiwet')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust wet deposition flux (size 1)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet1", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet1', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet1')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet1')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet1')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust wet deposition flux (size 2)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet2", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet2", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet2', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet2')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet2')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet2')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust wet deposition flux (size 3)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet3", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet3", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet3', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet3')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet3')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet3')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust wet deposition flux (size 4)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet4", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet4", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet4', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet4')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet4')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet4')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust dry deposition flux (size 1)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry1", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry1', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry1')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry1')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry1')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust dry deposition flux (size 2)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry2", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry2", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry2', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry2')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry2')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry2')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust dry deposition flux (size 3)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry3", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry3", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry3', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry3')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry3')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry3')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    longname = 'Dust dry deposition flux (size 4)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry4", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry4", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry4', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry4')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry4')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry4')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapconsf, 'one', atm2lnd_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compice, mapconsf, 'one', atm2ice_fmapfile)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapconsf, 'one', atm2ocn_fmapfile)

    !----------------------------------------------------------
    ! states/fluxes to atm (and ocean)
    !----------------------------------------------------------

    longname = 'Direct albedo (visible radiation)'
    stdname  = 'surface_direct_albedo_due_to_visible_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_avsdr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_avsdr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_avsdr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_avsdr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_avsdr', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_avsdr', index=n2)
    call fldList_AddFld(fldListXao_ocnalb_o%flds, 'So_avsdr', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_avsdr', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_smapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_smapfile)
    call fldlist_AddMap(fldListXao_ocnalb_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_smapfile)

    longname = 'Direct albedo (near-infrared radiation)'
    stdname  = 'surface_direct_albedo_due_to_near_infrared_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_anidr', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_anidr', index=n2)
    call fldList_AddFld(fldListXao_ocnalb_o%flds, 'So_anidr', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_anidr', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_ocnalb_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_smapfile)

    longname = 'Diffuse albedo (visible radiation)'
    stdname  = 'surface_diffuse_albedo_due_to_visible_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_avsdf', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_avsdf', index=n2)
    call fldList_AddFld(fldListXao_ocnalb_o%flds, 'So_avsdf', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_avsdf', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_ocnalb_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_smapfile)

    longname = 'Diffuse albedo (near-infrared radiation)'
    stdname  = 'surface_diffuse_albedo_due_to_near_infrared_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_anidf', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_anidf', index=n2)
    call fldList_AddFld(fldListXao_ocnalb_o%flds, 'So_anidf', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_anidf', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_ocnalb_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_smapfile)

    longname = 'Reference temperature at 2 meters'
    stdname  = 'air_temperature'
    units    = 'K'
    call fldList_AddMetadata(fldname="Si_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_tref', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_tref', index=n2)
    call fldList_AddFld(fldListXao_fluxes_a    , 'So_tref', index=n3)
    call fldList_AddFld(fldListXao_fluxes_o    , 'So_tref', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sx_tref', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n3), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn

    longname = 'Reference specific humidity at 2 meters'
    stdname  = 'specific_humidity'
    units    = 'kg kg-1'
    call fldList_AddMetadata(fldname="Si_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_qref', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_qref', index=n2)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'So_qref', index=n3)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'So_qref', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_qref', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n3), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn

    longname = 'Surface temperature'
    stdname  = 'surface_temperature'
    units    = 'K'
    call fldList_AddMetadata(fldname="Si_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_t', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_t', index=n2)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_t', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sx_t', merge_with_weights=.true.)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_t')
    call fldList_AddFld(fldListTo(compwav)%flds, 'So_t')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2), compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compocn)%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compocn)%flds(n3), compice, mapfcopy)
    call fldlist_AddMap(fldListFr(compocn)%flds(n3), compwav, map_filei, 'one'  , ocn2atm_fmapfile)

    longname = 'Surface fraction velocity in land'
    stdname  = 'fraction_velocity'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname="Sl_fv", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_fv', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_fv')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)

    longname = 'Aerodynamic resistance'
    stdname  = 'aerodynamic_resistance'
    units    = 's/m'
    call fldList_AddMetadata(fldname="Sl_ram1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_ram1', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_ram1')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)

    longname = 'Surface snow water equivalent'
    stdname  = 'surface_snow_water_equivalent'
    units    = 'm'
    call fldList_AddMetadata(fldname="Sl_snowh", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_snowh', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_snowh')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)

    longname = 'Surface snow depth'
    stdname  = 'surface_snow_thickness'
    units    = 'm'
    call fldList_AddMetadata(fldname="Si_snowh", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_snowh', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Si_snowh')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compatm, mapconsf, 'ifrac', ice2atm_fmapfile)

    longname = 'Surface saturation specific humidity in ocean'
    stdname  = 'specific_humidity_at_saturation'
    units    = 'kg kg-1'
    call fldList_AddMetadata(fldname="So_ssq", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'So_ssq', index=n1)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'So_ssq')
    call fldList_AddFld(fldListTo(atmcomp)%flds , 'So_ssq')
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n1), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n1), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn

    longname = 'Square of exch. coeff (tracers)'
    stdname  = 'square_of_exch_coeff'
    units    = '1'
    call fldList_AddMetadata(fldname="So_re", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'So_re', index=n1)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'So_re')
    call fldList_AddFld(fldListTo(atmcomp)%flds , 'So_re')
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n1), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n1), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn

    longname = '10m wind'
    stdname  = '10m_wind'
    units    = 'm'
    call fldList_AddMetadata(fldname="Sl_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Si_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_u10', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_u10', index=n2)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'So_u10', index=n3)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'So_u10', index=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_u10', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n3), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n3), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn

    longname = 'Zonal surface stress'
    stdname  = 'surface_downward_eastward_stress'
    units    = 'N m-2'
    call fldList_AddMetadata(fldname="Fall_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_taux', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_taux', index=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_taux', index=n3)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'Faox_taux', index=n4)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'Faox_taux')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_taux', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n4), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n4), compocn, mapconsf, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compocn, mapfcopy)

    longname = 'Meridional surface stress'
    stdname  = 'surface_downward_northward_stress'
    units    = 'N m-2'
    call fldList_AddMetadata(fldname="Fall_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_tauy', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'Faii_tauy', index=n2)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_tauy', index=n3)
    call fldList_AddFld(fldListXao_fluxes_a    , 'Faox_tauy', index=n4)
    call fldList_AddFld(fldListXao_fluxes_o    , 'Faox_tauy')
    call fldList_AddFld(fldListTo(compatm)%flds, 'Foxx_tauy', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n4), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n4), compocn, mapconsf, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compocn, mapfcopy)

    longname = 'Surface latent heat flux'
    stdname  = 'surface_upward_latent_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fall_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_lat', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_lat', index=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_lat', index=n3)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'Faox_lat', index=n4)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'Faox_lat')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_lat', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n4), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n4), compocn, mapconsf, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compocn, mapfcopy)

    longname = 'Sensible heat flux'
    stdname  = 'surface_upward_sensible_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fall_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_sen', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_sen', index=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_sen', index=n3)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'Faox_sen', index=n4)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'Faox_sen')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_sen', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n4), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n4), compocn, mapconsf, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compocn, mapfcopy)

    longname = 'Surface upward longwave heat flux'
    stdname  = 'surface_net_upward_longwave_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fall_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_lwup', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'Faii_lwup', index=n2)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_lwup', index=n3)
    call fldList_AddFld(fldListXao_fluxes_a    , 'Faox_lwup', index=n4)
    call fldList_AddFld(fldListXao_fluxes_o    , 'Faox_lwup')
    call fldList_AddFld(fldListTo(compatm)%flds, 'Foxx_lwup', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n4), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n4), compocn, mapconsf, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compocn, mapfcopy)

    longname = 'Evaporation water flux'
    stdname  = 'water_evaporation_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_evap', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_evap', index=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_evap', index=n3)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'Faox_evap', index=n4)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'Faox_evap')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_evap', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , compatm, mapconsf, 'lfrin', lnd2atm_fmapfile)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compatm, mapconsf, 'ifrac', ice2atm_fmapfile)
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n4), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n4), compocn, mapconsf, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compocn, mapfcopy)

    longname = 'Dust flux (particle bin number 1)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst1", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst1', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst1', merge_with_weights=.true., rc=rec)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), mapconsf, 'lfrin', lnd2atm_fmapfile)

    longname = 'Dust flux (particle bin number 2)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst2", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst2", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst2', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst2', merge_with_weights=.true., rc=rec)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), mapconsf, 'lfrin', lnd2atm_fmapfile)

    longname = 'Dust flux (particle bin number 3)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst3", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst3", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst3', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst3', merge_with_weights=.true., rc=rec)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), mapconsf, 'lfrin', lnd2atm_fmapfile)

    longname = 'Dust flux (particle bin number 4)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst4", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst4", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst4', index=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst4', merge_with_weights=.true., rc=rec)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), mapconsf, 'lfrin', lnd2atm_fmapfile)

    !-----------------------------
    ! atm<->ocn only exchange
    !-----------------------------

    longname = 'Sea level pressure'
    stdname  = 'air_pressure_at_sea_level'
    units    = 'Pa'
    call fldList_AddMetadata(fldname="Sa_pslv", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_pslv', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_pslv')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapbilnr, 'one', atm2ocn_smapfile)

    longname = 'Wind speed squared at 10 meters'
    stdname  = 'square_of_wind_speed'
    units    = 'm2 s-2'
    call fldList_AddMetadata(fldname="So_duu10n", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'So_duu10n', index=n1)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'So_duu10n')
    call fldList_AddFld(fldListTo(compocn)%flds , 'So_duu10n')
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n1), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n1), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm

    longname = 'Surface fraction velocity in ocean'
    stdname  = 'fraction_velocity'
    units    = 'm s-1'
    call fldList_AddFld(fldListXao_fluxes_a%flds, 'So_ustar', index=n1)
    call fldList_AddFld(fldListXao_fluxes_o%flds, 'So_ustar')
    call fldList_AddFld(fldListTo(compocn)%flds , 'So_ustar')
    call fldlist_AddMap(fldListXao_fluxes_a%flds(n1), compocn, mapbilnr, 'one'  , atm2ocn_fmapfile) ! map atm->ocn
    call fldlist_AddMap(fldListXao_fluxes_o%flds(n1), compatm, mapconsf, 'ofrac', ocn2atm_fmapfile) ! map ocn->atm

    !-----------------------------
    ! ice->ocn exchange
    !-----------------------------

    longname = 'Heat flux from melting'
    stdname  = 'surface_snow_melt_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fioi_melth", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_melth", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_melth', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_melth')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    longname = 'Water flux due to melting'
    stdname  = 'surface_snow_melt_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_meltw", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_meltw", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_meltw', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_meltw')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    longname = 'Salt flux'
    stdname  = 'virtual_salt_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_salt", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_salt", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_salt', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_salt')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    longname = 'Hydrophylic black carbon deposition flux'
    stdname  = 'deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_bcphi", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphi", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_bcphi', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphi')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    longname = 'Hydrophobic black carbon deposition flux'
    stdname  = 'deposition_flux_of_hydrophobic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_bcpho", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcpho", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_bcpho', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcpho')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    longname = 'Dust flux'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_flxdst", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_flxdst", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_flxdst', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_flxdst')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    !-----------------------------
    ! ocn -> ice exchange
    !-----------------------------

    longname = 'Sea surface salinity'
    stdname  = 'sea_surface_salinity'
    units    = 'g kg-1'
    call fldList_AddMetadata(fldname="So_s", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_s', index=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'So_s', index=n1)
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

    longname = 'Ocean melt and freeze potential'
    stdname  = 'surface_snow_and_ice_melt_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fioo_q", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'Fioo_q', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'Fioo_q')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    longname = 'Zonal sea water velocity'
    stdname  = 'eastward_sea_water_velocity'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname="So_u", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_u', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_u')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    longname = 'Meridional sea water velocity'
    stdname  = 'northward_sea_water_velocity'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname="So_v", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_v', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_v')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    longname = 'Zonal sea surface slope'
    stdname  = 'sea_surface_eastward_slope'
    units    = 'm m-1'
    call fldList_AddMetadata(fldname="So_dhdx", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_dhdx', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_dhdx')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    longname = 'Meridional sea surface slope'
    stdname  = 'sea_surface_northward_slope'
    units    = 'm m-1'
    call fldList_AddMetadata(fldname="So_dhdy", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_dhdy', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_dhdy')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    longname = 'Ocean Boundary Layer Depth'
    stdname  = 'ocean_boundary_layer_depth'
    units    = 'm'
    call fldList_AddMetadata(fldname="So_bldepth", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_bldepth', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_bldepth')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    longname = 'Fraction of sw penetrating surface layer for diurnal cycle'
    stdname  = 'Fraction_of_sw_penetrating_surface_layer'
    units    = '1'
    call fldList_AddMetadata(fldname="So_fswpen", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_fswpen', index=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_fswpen')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compice,  mapfcopy)

    !-----------------------------
    ! lnd->rof exchange
    !-----------------------------

    longname = 'Water flux from land (liquid surface)'
    stdname  = 'water_flux_into_runoff_surface'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofsur", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofsur", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofsur', index=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofsur', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land (liquid glacier, wetland, and lake)'
    stdname  = 'water_flux_into_runoff_from_gwl'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofgwl", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofgwl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofgwl', index=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofgwl', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land (liquid subsurface)'
    stdname  = 'water_flux_into_runoff_subsurface'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofsub", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofsub", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofsub', index=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofsub', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land direct to ocean'
    stdname  = 'water_flux_direct_to_ocean'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofdto", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofdto", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofdto', index=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofdto', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land (frozen)'
    stdname  = 'frozen_water_flux_into_runoff'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofi", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofi", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofi', index=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofi', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    ! Irrigation flux (land/rof only)
    longname = 'Irrigation flux (withdrawal from rivers)'
    stdname  = 'irrigation'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_irrig", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_irrig", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_irrig', index=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_irrig', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    !-----------------------------
    ! rof->lnd
    !-----------------------------

    longname = 'Waterflux back to land due to flooding'
    stdname  = 'flooding_water_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrr_flood", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Flrr_flood', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Flrr_flood')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Flrr_flood')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), complnd, mapconsf, 'one', rof2lnd_fmapname)
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), complnd, mapconsf, 'one', rof2ocn_fmapname)

    longname = 'River channel total water volume'
    stdname  = 'rtm_volr'
    units    = 'm'
    call fldList_AddMetadata(fldname="Flrr_volr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Flrr_volr', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Flrr_volr')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), complnd, mapconsf, 'one', rof2lnd_fmapname)

    longname = 'River channel main channel water volume'
    stdname  = 'rtm_volrmch'
    units    = 'm'
    call fldList_AddMetadata(fldname="Flrr_volrmch", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Flrr_volrmch', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Flrr_volrmch')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), complnd, mapconsf, 'one', rof2lnd_fmapname)

    !-----------------------------
    ! rof->ocn (liquid and frozen)
    !-----------------------------

    longname = 'Water flux into sea water due to runoff (liquid)'
    stdname  = 'water_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Foxx_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Forr_rofl', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rofl')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), compocn, mapunset, 'none', rof2ocn_liq_rmapname)

    longname = 'Water flux into sea water due to runoff (frozen)'
    stdname  = 'frozen_water_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Forr_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Forr_rofi', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rofi')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), compocn, mapunset, 'none', rof2ocn_ice_rmapname)

    !-----------------------------
    ! rof->ice (frozen)
    !-----------------------------

    longname = 'Water flux into sea ice due to runoff (frozen)'
    stdname  = 'frozen_water_flux_into_sea_ice'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Firr_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fixx_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Forr_rofi', index=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rofi')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), compocn, mapunset, 'none', rof2ocn_ice_rmapname)

    !-----------------------------
    ! wav->ocn
    !-----------------------------

    longname = 'Langmuir multiplier'
    stdname  = 'wave_model_langmuir_multiplier'
    units    = '1'
    call fldList_AddMetadata(fldname='Sw_lamult', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Sw_lamult')
    call fldList_AddFld(fldListFr(compwav), 'Sw_lamult', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn), 'Sw_lamult')
    call fldlist_AddMap(fldListFr(compwav), compocn,  fldindex=n1, mapbilnr, 'one', wav2ocn_smapfile)

    longname = 'Stokes drift u component'
    stdname  = 'wave_model_stokes_drift_eastward_velocity'
    units    = 'm/s'
    call fldList_AddMetadata(fldname='Sw_ustokes', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Sw_ustokes')
    call fldList_AddFld(fldListFr(compwav), 'Sw_ustokes', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn), 'Sw_ustokes')
    call fldlist_AddMap(fldListFr(compwav), compocn,  fldindex=n1, mapbilnr, 'one', wav2ocn_smapfile)

    longname = 'Stokes drift v component'
    stdname  = 'wave_model_stokes_drift_northward_velocity'
    units    = 'm/s'
    call fldList_AddMetadata(fldname='Sw_vstokes', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Sw_vstokes')
    call fldList_AddFld(fldListFr(compwav), 'Sw_vstokes', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn), 'Sw_vstokes')
    call fldlist_AddMap(fldListFr(compwav), compocn,  fldindex=n1, mapbilnr, 'one', wav2ocn_smapfile)

    longname = 'Stokes drift depth'
    stdname  = 'wave_model_stokes_drift_depth'
    units    = 'm'
    call fldList_AddMetadata(fldname='Sw_hstokes', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Sw_hstokes')
    call fldList_AddFld(fldListFr(compwav), 'Sw_hstokes', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn), 'Sw_hstokes')
    call fldlist_AddMap(fldListFr(compwav), compocn,  fldindex=n1, mapbilnr, 'one', wav2ocn_smapfile)

    !-----------------------------
    ! fields for history output only
    !-----------------------------

    if (do_flux_diurnal) then
       longname = 'Downward solar radiation'
       stdname  = 'surface_downward_shortwave_flux'
       units    = 'W m-2'
       call fldList_AddMetadata(fldname="Faox_swdn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('Faox_swdn', flds_xao_diurnl) ! Needed for FB initialization

       longname = 'Upward solar radiation'
       stdname  = 'surface_upward_shortwave_flux'
       units    = 'W m-2'
       call fldList_AddMetadata(fldname="Faox_swup", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('Faox_swup', flds_xao_diurnl) ! Needed for FB initialization

       longname = 'atm/ocn flux temperature bulk'
       stdname  = 'aoflux_tbulk'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tbulk_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_tbulk_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux temperature skin'
       stdname  = 'aoflux_tskin'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tskin_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString( 'So_tskin_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux temperature skin at night'
       stdname  = 'aoflux_tskin_night'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tskin_night_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_tskin_night_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux temperature skin at day'
       stdname  = 'aoflux_tskin_day'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tskin_day_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_tskin_day_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux cool skin'
       stdname  = 'aoflux_cskin'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_cskin_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_cskin_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux cool skin at night'
       stdname  = 'aoflux_cskin_night'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_cskin_night_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_cskin_night_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux warming'
       stdname  = 'aoflux_warm'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_warm_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_warm_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux salting'
       stdname  = 'aoflux_salt'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_salt_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_salt_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux speed'
       stdname  = 'aoflux_speed'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_speed_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_speed_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux regime'
       stdname  = 'aoflux_regime'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_regime_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_regime_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux warming dialy max'
       stdname  = 'aoflux_warmmax'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_warmmax_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_warmmax_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux wind daily max'
       stdname  = 'aoflux_windmax'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windmax_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_windmax_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux q-solar daily avg'
       stdname  = 'aoflux_qsolavg'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_qsolvavg_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_qsolvavg_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux wind daily avg'
       stdname  = 'aoflux_windavg'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windavg_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_windavg_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux daily max increment'
       stdname  = 'aoflux_warmmaxinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_warmmaxinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_warmmaxinc_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux wind daily max increment'
       stdname  = 'aoflux_windmaxinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windmaxinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_windmaxinc_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux q-solar increment'
       stdname  = 'aoflux_qsolinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_qsolinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_qsolinc_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux wind increment'
       stdname  = 'aoflux_windinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_windinc_diurn', flds_xao_diurnl)

       longname = 'atm/ocn flux increment counter'
       stdname  = 'aoflux_ninc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_ninc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_ConcatString('So_ninc_diurn', flds_xao_diurnl)
    end if

    !-----------------------------
    ! glc -> ocn
    !-----------------------------

    longname = 'glc liquid runoff flux to ocean'
    stdname  = 'glacier_liquid_runoff_flux_to_ocean'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Fogg_rofl', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc), 'Flgg_rofl', fldindex=n1)
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compocn,  mapunset, 'one', glc2ocn_rmapname)

    longname = 'glc frozen runoff flux to ocean'
    stdname  = 'glacier_frozen_runoff_flux_to_ocean'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Fogg_rofi', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc), 'Flgg_rofi', fldindex=n1)
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compocn,  mapunset, 'one', glc2ocn_rmapname)

    !-----------------------------
    ! glc -> ice
    !-----------------------------

    longname = 'glc frozen runoff_iceberg flux to ice'
    stdname  = 'glacier_frozen_runoff_flux_to_seaice'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Figg_rofi', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc), 'Figg_rofi', fldindex=n1)
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compice,  mapunset, 'one', glc2ice_rmapname)

    !-----------------------------
    ! glc -> lnd
    !-----------------------------

    ! for glc fields with multiple elevation classes in glc->lnd
    ! fields from glc->med do NOT have elevation classes
    ! fields from med->lnd are BROKEN into multiple elevation classes

    longname = 'Ice sheet grid coverage on global grid'
    stdname  = 'ice_sheet_grid_mask'
    units    = '1'
    call fldList_AddMetadata(fldname="Sg_icemask", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, 'Sg_icemask', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sg_icemask')
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), complnd,  mapconsf, 'one', glc2lnd_smapname)
    call fldList_ConcatString('Sg_icemask', flds_x2l_fr_glc) ! Needed for FB initialization

    longname = 'Ice sheet mask where we are potentially sending non-zero fluxes'
    stdname  = 'icemask_coupled'
    units    = '1'
    call fldList_AddMetadata(fldname="Sg_icemask_coupled_fluxes", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, 'Sg_icemask_coupled_fluxes', index=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sg_icemask_coupled_fluxes')
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), complnd,  mapconsf, 'one', glc2lnd_smapname)
    call fldList_ConcatString('Sg_icemask_coupled_fluxes', flds_x2l_fr_glc) ! Needed for FB initialization

    name = 'Sg_ice_covered'
    longname = 'Fraction of glacier area'
    stdname  = 'glacier_area_fraction'
    units    = '1'
    call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc), trim(name), fldindex=n1)
    call fldList_AddMap(FldListFr(compglc)%flds(n1), complnd, mapconsf, 'unset', glc2lnd_fmapname) ! TODO: normalization?
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname= trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, unit=units)
          call fldList_AddFld(fldListTo(complnd)   , trim(name)//trim(cnum))
          call fldList_ConcatString(trim(name)//trim(cnum), flds_x2l_fr_glc) ! Needed for FB initialization
       end do
    end if

    name = 'Sg_topo'
    longname = 'Surface height of glacier'
    stdname  = 'height'
    units    = 'm'
    call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc), trim(name), fldindex=n1)
    call fldList_AddMap(FldListFr(compglc)%flds(n1), compglc, mapconsf, 'custom', glc2lnd_fmapname)
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname= trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, unit=units)
          call fldList_AddFld(fldListTo(complnd)   , trim(name)//trim(cnum))
          call fldList_ConcatString(trim(name)//trim(cnum), flds_x2l_fr_glc) ! Needed for FB initialization
       end do
    end if

    name = 'Flgg_hflx'
    attname = name
    longname = 'Downward heat flux from glacier interior'
    stdname  = 'downward_heat_flux_in_glacier'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc), trim(name), fldindex=n1)
    call fldList_AddMap(FldListFr(compglc)%flds(n1), compglc, mapconsf, 'custom', glc2lnd_fmapname)
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname= trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, unit=units)
          call fldList_AddFld(fldListTo(complnd)   , trim(name)//trim(cnum))
          call fldList_ConcatString(trim(name)//trim(cnum), flds_x2l_fr_glc) ! Needed for FB initialization
       end do
    end if

    !-----------------------------
    ! lnd -> glc
    !-----------------------------

    ! glc fields with multiple elevation classes: lnd->glc
    ! - fields sent from lnd->med are in multiple elevation classes
    ! - fields sent from med->glc do NOT have elevation classes
    ! - need to keep track of the l2x fields destined for glc in the
    !   additional variables, l2x_to_glc. This is needed so that can set up
    !   additional fields holding accumulated quantities of just these fields.

    ! Sets a coupling field for all glc elevation classes (1:glc_nec) plus bare land (index 0).
    ! Note that, if glc_nec = 0, then we don't create any coupling fields (not even the bare land (0) index)

    name = 'Flgl_qice'
    attname = name
    longname = 'New glacier ice flux'
    stdname  = 'ice_flux_out_of_glacier'
    units    = 'kg m-2 s-1'
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname  = trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, unit=units)
          call fldList_AddFld(fldListFr(complnd), trim(name)//trim(cnum)), fldindex=n1)
          call fldList_AddFld(fldListMed_lnd_to_glc, trim(name)//trim(cnum))
       end do
    end if
    call fldList_AddMetadata(fldname= trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compglc), trim(name))
    call fldList_AddMap(FldListFr(complnd)%flds(n1), compglc, mapconsf, 'none', lnd2glc_fmapname)

    name = 'Sl_tsrf'
    attname = name
    longname = 'Surface temperature of glacier'
    stdname  = 'surface_temperature'
    units    = 'deg C'
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname  = trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, unit=units)
          call fldList_AddFld(fldListFr(complnd), trim(name)//trim(cnum)), fldindex=n1)
          call fldList_AddFld(fldListMed_l2x_to_glc, trim(name)//trim(cnum))
       end do
    end if
    call fldList_AddMetadata(fldname= trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compglc), trim(name))
    call fldList_AddMap(FldListFr(complnd)%flds(n1), compglc, mapconsf, 'none', lnd2glc_fmapname)

    ! Sl_topo is sent from lnd -> med, but is NOT sent to glc (it is only used for the
    ! remapping in the mediator)
    name = 'Sl_topo'
    attname = name
    longname = 'Surface height'
    stdname  = 'height'
    units    = 'm'
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname  = trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, unit=units)
          call fldList_AddFld(fldListFr(complnd), trim(name)//trim(cnum)), fldindex=n1)
          call fldList_AddFld(fldListMed_l2x_to_glc, trim(name)//trim(cnum))
       end do
    end if
    call fldList_AddMetadata(fldname= trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compglc), trim(name))
    call fldList_AddMap(FldListFr(complnd)%flds(n1), compglc, mapconsf, 'none', lnd2glc_fmapname)

    !-----------------------------
    ! co2 fields
    !-----------------------------

    if (flds_co2a) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2prog', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2prog', index=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2prog')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2prog')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapbilnr, 'one', atm2ocn_smapfile)

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2diag', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2diag', index=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2diag')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2diag')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapbilnr, 'one', atm2ocn_smapfile)

    else if (flds_co2b) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2prog', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2prog', index=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2prog')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2diag', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2diag', index=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2diag')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)

       longname = 'Surface flux of CO2 from land'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_land'
       units    = 'moles m-2 s-1'
       call fldList_AddMetadata(fldname='Fall_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddMetadata(fldname='Faxx_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_fco2_lnd', index=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_fco2_lnd', merge_with_weights=.true.)
       call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', atm2lnd_smapfile)

    else if (flds_co2c) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2prog', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2prog', index=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2prog')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2prog')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapbilnr, 'one', atm2ocn_smapfile)

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2diag', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2diag', index=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2diag')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2diag')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd, mapbilnr, 'one', atm2lnd_smapfile)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn, mapbilnr, 'one', atm2ocn_smapfile)

       longname = 'Surface flux of CO2 from land'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_land'
       units    = 'moles m-2 s-1'
       call fldList_AddMetadata(fldname='Fall_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddMetadata(fldname='Faxx_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_fco2_lnd', index=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_fco2_lnd', merge_with_weights=.true.)
       call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', atm2lnd_smapfile)

       longname = 'Surface flux of CO2 from ocean'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_open_sea'
       units    = 'moles m-2 s-1'
       call fldList_AddMetadata(fldname='Fall_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddMetadata(fldname='Faxx_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complcn)%flds, 'Faoo_fco2_ocn', index=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_fco2_ocn', merge_with_weights=.true.)
       call fldlist_AddMap(fldListFr(compocn)%flds(n1), compatm, mapconsf, 'one', ocn2atm_smapfile)

    endif

    !-----------------------------
    ! water isotope fields
    !-----------------------------

    ! if (flds_wiso) then
    !    longname = 'Ratio of ocean surface level abund. H2_16O/H2O/Rstd'
    !    stdname  = 'ratio_ocean_surface_16O_abund'
    !    units    = '1'
    !    call fld_add(flds_o2x, flds_o2x_map, "So_roce_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2i, flds_x2i_map, "So_roce_16O")

    !    longname = 'Ratio of ocean surface level abund. HDO/H2O/Rstd'
    !    stdname  = 'ratio_ocean_surface_HDO_abund'
    !    call fld_add(flds_o2x, flds_o2x_map, "So_roce_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2i, flds_x2i_map, "So_roce_HDO")

    !    !--------------------------------------------
    !    ! Atmospheric specific humidty at lowest level:
    !    !--------------------------------------------

    !    ! specific humidity of H216O at the lowest model level (kg/kg)
    !    longname = 'Specific humidty of H216O at the lowest model level'
    !    stdname  = 'H216OV'
    !    units    = 'kg kg-1'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Sa_shum_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Sa_shum_16O")
    !    call fld_add(flds_x2i, flds_x2i_map, "Sa_shum_16O")

    !    ! specific humidity of HD16O at the lowest model level (kg/kg)
    !    longname = 'Specific humidty of HD16O at the lowest model level'
    !    stdname  = 'HD16OV'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Sa_shum_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Sa_shum_HDO")
    !    call fld_add(flds_x2i, flds_x2i_map, "Sa_shum_HDO")

    !    ! specific humidity of H218O at the lowest model level (kg/kg)
    !    longname = 'Specific humidty of H218O at the lowest model level'
    !    stdname  = 'H218OV'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Sa_shum_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Sa_shum_18O")
    !    call fld_add(flds_x2i, flds_x2i_map, "Sa_shum_18O")

    !    ! Surface snow water equivalent (land/atm only)
    !    longname = 'Isotopic surface snow water equivalent'
    !    stdname  = 'surface_snow_water_equivalent'
    !    units    = 'm'
    !    call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_l2x, flds_l2x_map, "Sl_snowh_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh_16O")
    !    call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh_18O")
    !    call fld_add(flds_x2a, flds_x2a_map, "Sl_snowh_HDO")

    !    !--------------
    !    ! Isotopic Rain:
    !    !--------------

    !    !Isotopic Precipitation Fluxes:
    !    units    = 'kg m-2 s-1'
    !    longname = 'H216O Convective precipitation rate'
    !    stdname  = 'H2_16O_convective_precipitation_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_rainc_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc_16O")
    !    longname = 'H216O Large-scale (stable) precipitation rate'
    !    stdname  = 'H2_16O_large_scale_precipitation_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_rainl_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl_16O")
    !    longname = 'Water flux due to H216O rain' !equiv. to bulk
    !    stdname  = 'H2_16O_rainfall_flux'
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain_16O")

    !    longname = 'H218O Convective precipitation rate'
    !    stdname  = 'H2_18O_convective_precipitation_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_rainc_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc_18O")
    !    longname = 'H218O Large-scale (stable) precipitation rate'
    !    stdname  = 'H2_18O_large_scale_precipitation_flux'
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_rainl_18O")
    !    longname = 'Water flux due to H218O rain'
    !    stdname  = 'h2_18o_rainfall_flux'
    !    call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain_18O", longname=longname, stdname=stdname, units=units)

    !    longname = 'HDO Convective precipitation rate'
    !    stdname  = 'HDO_convective_precipitation_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_rainc_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainc_HDO")
    !    longname = 'HDO Large-scale (stable) precipitation rate'
    !    stdname  = 'HDO_large_scale_precipitation_flux'
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_rainl_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_rainl_HDO")
    !    longname = 'Water flux due to HDO rain'
    !    stdname  = 'hdo_rainfall_flux'
    !    call fld_add(flds_x2i, flds_x2i_map, "Faxa_rain_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_rain_HDO", longname=longname, stdname=stdname, units=units)

    !    !-------------
    !    ! Isotopic snow:
    !    !-------------

    !    longname = 'H2_16O Convective snow rate (water equivalent)'
    !    stdname  = 'H2_16O_convective_snowfall_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_snowc_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc_16O")

    !    longname = 'H2_16O Large-scale (stable) snow rate (water equivalent)'
    !    stdname  = 'H2_16O_large_scale_snowfall_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_snowl_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl_16O")

    !    longname = 'Water equiv. H216O snow flux'
    !    stdname  = 'h2_16o_snowfall_flux'
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow_16O", longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_18O Convective snow rate (water equivalent)'
    !    stdname  = 'H2_18O_convective_snowfall_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_snowc_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc_18O")

    !    longname = 'H2_18O Large-scale (stable) snow rate (water equivalent)'
    !    stdname  = 'H2_18O_large_scale_snowfall_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_snowl_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl_18O")

    !    longname = 'Isotopic water equiv. snow flux of H218O'
    !    stdname  = 'h2_18o_snowfall_flux'
    !    call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow_18O", longname=longname, stdname=stdname, units=units)

    !    longname = 'HDO Convective snow rate (water equivalent)'
    !    stdname  = 'HDO_convective_snowfall_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_snowc_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowc_HDO")

    !    longname = 'HDO Large-scale (stable) snow rate (water equivalent)'
    !    stdname  = 'HDO_large_scale_snowfall_flux'
    !    call fld_add(fldsfr_list(compatm) flds_a2x_map, "Faxa_snowl_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map, "Faxa_snowl_HDO")

    !    longname = 'Isotopic water equiv. snow flux of HDO'
    !    stdname  = 'hdo_snowfall_flux'
    !    call fld_add(flds_x2i, flds_x2i_map, "Faxa_snow_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_snow_HDO", longname=longname, stdname=stdname, units=units)

    !    !----------------------------------
    !    ! Isotopic precipitation (rain+snow):
    !    !----------------------------------

    !    longname = 'Isotopic Water flux (rain+snow) for H2_16O'
    !    stdname  = 'h2_18o_precipitation_flux'
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec_16O", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

    !    longname = 'Isotopic Water flux (rain+snow) for H2_18O'
    !    stdname  = 'h2_18o_precipitation_flux'
    !    units    = 'kg m-2 s-1'
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec_18O", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

    !    longname = 'Isotopic Water flux (rain+snow) for HD_O'
    !    stdname  = 'hdo_precipitation_flux'
    !    units    = 'kg m-2 s-1'
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_prec_HDO", longname=longname, stdname=stdname, units=units)  ! derived rain+snow

    !    !-------------------------------------
    !    ! Isotopic two meter reference humidity:
    !    !-------------------------------------

    !    ! H216O Reference specific humidity at 2 meters
    !    longname = 'Reference H216O specific humidity at 2 meters'
    !    stdname  = 'H216O_specific_humidity'
    !    units    = 'kg kg-1'
    !    call fld_add(flds_l2x, flds_l2x_map, "Sl_qref_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_i2x, flds_i2x_map, "Si_qref_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_xao, flds_xao_map, "So_qref_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a, flds_x2a_map, "Sx_qref_16O", longname=longname, stdname=stdname, units=units)

    !    ! HD16O Reference specific humidity at 2 meters
    !    longname = 'Reference HD16O specific humidity at 2 meters'
    !    stdname  = 'HD16O_specific_humidity'
    !    units    = 'kg kg-1'
    !    call fld_add(flds_l2x, flds_l2x_map, "Sl_qref_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_i2x, flds_i2x_map, "Si_qref_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_xao, flds_xao_map, "So_qref_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a, flds_x2a_map, "Sx_qref_HDO", longname=longname, stdname=stdname, units=units)

    !    ! H218O Reference specific humidity at 2 meters
    !    longname = 'Reference H218O specific humidity at 2 meters'
    !    stdname  = 'H218O_specific_humidity'
    !    units    = 'kg kg-1'
    !    call fld_add(flds_l2x, flds_l2x_map, "Sl_qref_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_i2x, flds_i2x_map, "Si_qref_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_xao, flds_xao_map, "So_qref_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a, flds_x2a_map, "Sx_qref_18O", longname=longname, stdname=stdname, units=units)

    !    !-------------------------
    !    ! Isotopic Evaporation flux:
    !    !-------------------------

    !    ! H216O Evaporation water flux
    !    longname = 'Evaporation H216O flux'
    !    stdname  = 'H216O_evaporation_flux'
    !    units    = 'kg m-2 s-1'
    !    call fld_add(flds_l2x "Fall_evap_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_i2x "Faii_evap_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_xao "Faox_evap_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a "Faxx_evap_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o "Foxx_evap_16O", longname=longname, stdname=stdname, units=units)

    !    ! HD16O Evaporation water flux
    !    longname = 'Evaporation HD16O flux'
    !    stdname  = 'HD16O_evaporation_flux'
    !    units    = 'kg m-2 s-1'
    !    call fld_add(flds_l2x, "Fall_evap_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_i2x, "Faii_evap_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_xao, "Faox_evap_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a, "Faxx_evap_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, "Foxx_evap_HDO", longname=longname, stdname=stdname, units=units)

    !    ! H218O Evaporation water flux
    !    longname = 'Evaporation H218O flux'
    !    stdname  = 'H218O_evaporation_flux'
    !    units    = 'kg m-2 s-1'
    !    call fld_add(flds_l2x, "Fall_evap_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_i2x, "Faii_evap_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_xao, "Faox_evap_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2a, "Faxx_evap_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, "Foxx_evap_18O", longname=longname, stdname=stdname, units=units)

    !    !-----------------------------
    !    ! Isotopic sea ice melting flux:
    !    !-----------------------------

    !    ! H216O Water flux from melting
    !    units    = 'kg m-2 s-1'
    !    longname = 'H2_16O flux due to melting'
    !    stdname  = 'h2_16o_surface_snow_melt_flux'
    !    call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw_16O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw_16O", longname=longname, stdname=stdname, units=units)

    !    ! H218O Water flux from melting
    !    longname = 'H2_18O flux due to melting'
    !    stdname  = 'h2_18o_surface_snow_melt_flux'
    !    call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw_18O", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw_18O", longname=longname, stdname=stdname, units=units)

    !    ! HDO Water flux from melting
    !    units    = 'kg m-2 s-1'
    !    longname = 'HDO flux due to melting'
    !    stdname  = 'hdo_surface_snow_melt_flux'
    !    call fld_add(flds_i2x, flds_i2x_map, "Fioi_meltw_HDO", longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map, "Foxx_meltw_HDO", longname=longname, stdname=stdname, units=units)

    !    !Iso-Runoff
    !    ! r2o, l2x, x2r

    !    units    = 'kg m-2 s-1'
    !    longname = 'H2_16O Water flux from land (frozen)'
    !    stdname  = 'H2_16O_frozen_water_flux_into_runoff'
    !    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi_16O', longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_18O Water flux from land (frozen)'
    !    stdname  = 'H2_18O_frozen_water_flux_into_runoff'
    !    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi_18O', longname=longname, stdname=stdname, units=units)

    !    longname = 'HDO Water flux from land (frozen)'
    !    stdname  = 'HDO_frozen_water_flux_into_runoff'
    !    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofi_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofi_HDO', longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_16O Water flux from land (liquid)'
    !    stdname  = 'H2_16O_liquid_water_flux_into_runoff'
    !    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofl_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofl_16O', longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_18O Water flux from land (liquid)'
    !    stdname  = 'H2_18O_liquid_water_flux_into_runoff'
    !    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofl_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofl_18O', longname=longname, stdname=stdname, units=units)

    !    longname = 'HDO Water flux from land (liquid)'
    !    stdname  = 'HDO_liquid_water_flux_into_runoff'
    !    call fld_add(flds_l2x, flds_l2x_map,'Flrl_rofl_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2r, flds_x2r_map,'Frxx_rofl_HDO', longname=longname, stdname=stdname, units=units)

    !    !-----------------------------
    !    ! Isotopic r2x, x2o
    !    !-----------------------------

    !    units    = 'kg m-2 s-1'
    !    longname = 'H2_16O Water flux due to liq runoff '
    !    stdname  = 'H2_16O_water_flux_into_sea_water'
    !    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl_16O', longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_18O Water flux due to liq runoff '
    !    stdname  = 'H2_18O_water_flux_into_sea_water'
    !    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl_18O', longname=longname, stdname=stdname, units=units)

    !    longname = 'HDO Water flux due to liq runoff '
    !    stdname  = 'HDO_water_flux_into_sea_water'
    !    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofl_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofl_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_r2o_liq, flds_r2o_liq_map,'Forr_rofl_HDO', longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_16O Water flux due to ice runoff '
    !    stdname  = 'H2_16O_water_flux_into_sea_water'
    !    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi_16O', longname=longname, stdname=stdname, units=units)

    !    longname = 'H2_18O Water flux due to ice runoff '
    !    stdname  = 'H2_18O_water_flux_into_sea_water'
    !    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi_18O', longname=longname, stdname=stdname, units=units)

    !    longname = 'HDO Water flux due to ice runoff '
    !    stdname  = 'HDO_water_flux_into_sea_water'
    !    call fld_add(flds_r2x, flds_r2x_map,'Forr_rofi_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2o, flds_x2o_map,'Foxx_rofi_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_r2o_ice, flds_r2o_ice_map,'Forr_rofi_HDO', longname=longname, stdname=stdname, units=units)

    !    ! r2x, x2l

    !    units    = 'kg m-2 s-1'
    !    longname = 'H2_16O waterrflux due to flooding'
    !    stdname  = 'H2_16O_flodding_water_flux_back_to_land'
    !    call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_16O')

    !    longname = 'H2_18O waterrflux due to flooding'
    !    stdname  = 'H2_18O_flodding_water_flux_back_to_land'
    !    call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_18O')

    !    longname = 'HDO Waterrflux due to flooding'
    !    stdname  = 'HDO_flodding_water_flux_back_to_land'
    !    call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_HDO')

    !    longname = 'H2_16O river channel water volume '
    !    stdname  = 'H2_16O_rtm_volr'
    !    call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr_16O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr_16O')

    !    longname = 'H2_18O river channel water volume '
    !    stdname  = 'H2_18O_rtm_volr'
    !    call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr_18O', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr_18O')

    !    longname = 'HDO river channel water volume '
    !    stdname  = 'HDO_rtm_volr'
    !    call fld_add(flds_r2x, flds_r2x_map,'Flrr_volr_HDO', longname=longname, stdname=stdname, units=units)
    !    call fld_add(flds_x2l, flds_x2l_map,'Flrr_volr_HDO')

    !    ! longname = 'H2_18O Waterrflux due to flooding'
    !    ! stdname  = 'H2_18O_flodding_water_flux_back_to_land'
    !    ! call fld_add(flds_r2x, flds_r2x_map,'Flrr_flood_HDO', longname=longname, stdname=stdname, units=units)
    !    ! call fld_add(flds_x2l, flds_x2l_map,'Flrr_flood_HDO')

    ! endif !Water isotopes

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
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(compice)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compocn)%flds, trim(name))
          call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)

          ! Net shortwave radiation
          name = 'PFioi_swpen_ifrac_' // cnum
          longname = 'net shortwave radiation penetrating into ice and ocean times ice fraction for thickness category ' // cnum
          stdname  = 'product_of_net_downward_shortwave_flux_at_sea_water_surface_and_sea_ice_area_fraction'
          units    = 'W m-2'
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(compice)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compocn)%flds, trim(name))
          call fldlist_AddMap(fldListFr(compice)%flds(n1), compocn,  mapfcopy)
       end do

       ! Fractional atmosphere coverage wrt ocean

       longname = 'fractional atmosphere coverage wrt ocean'
       stdname  = 'atmosphere_area_fraction'
       units    = '1'
       call fldList_AddMetadata(fldname='Sf_afrac', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sf_afrac')
       ! TODO: add mapping

       longname = 'fractional atmosphere coverage used in radiation computations wrt ocean'
       stdname  = 'atmosphere_area_fraction'
       units    = '1'
       call fldList_AddMetadata(fldname='Sf_afracr', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sf_afracr')
       ! TODO: add mapping

       ! Net shortwave radiation

       name = 'Foxx_swnet_afracr'
       longname = 'net shortwave radiation times atmosphere fraction'
       stdname = 'product_of_net_downward_shortwave_flux_at_sea_water_surface_and_atmosphere_area_fraction'
       units = 'W m-2'
       call fldList_AddMetadata(fldname='Foxx_swnet_afracr', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_swnet_afracr')
       ! ???? add mapping

    endif

    !-----------------------------------------------------------------------------
    ! CARMA fields
    ! if carma_flds are specified then setup fields for CLM to CAM communication
    !-----------------------------------------------------------------------------

    call shr_carma_readnl(nlfilename='drv_flds_in', carma_fields=carma_fields)
    if (carma_fields /= ' ') then
       longname = 'Volumetric soil water'
       stdname  = 'soil_water'
       units    = 'm3/m3'
       num = shr_string_listGetNum(carma_fields)
       do n = 1,num
          call shr_string_listGetName(carm_fields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', lnd2atm_smapfile)
       enddo
    endif

    !-----------------------------------------------------------------------------
    ! MEGAN emissions fluxes fields
    ! if MEGAN emission are specified then setup fields for CLM to CAM communication
    !-----------------------------------------------------------------------------

    call shr_megan_readnl(nlfilename='drv_flds_in', ID=ID, megan_fields=megan_voc_fields)
    if (shr_megan_mechcomps_n > 0) then
       longname = 'MEGAN emission fluxes'
       stdname  = 'megan'
       units    = 'molecules/m2/sec'
       do n = 1,num
          call shr_string_listGetName(megan_voc_fields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', atm2lnd_smapfile)
       enddo
    endif

    !-----------------------------------------------------------------------------
    ! Fire emissions fluxes fields
    ! if fire emission are specified then setup fields for CLM to CAM communication
    ! (emissions fluxes)
    !-----------------------------------------------------------------------------

    call shr_fire_emis_readnl(nlfilename='drv_flds_in', ID=ID, emis_fields=fire_emis_fields)
    if (shr_fire_emis_mechcomps_n>0) then
       longname = 'wild fire emission fluxes'
       stdname  = 'fire_emis'
       units    = 'kg/m2/sec'
       do n = 1,num
          call shr_string_listGetName(fire_emis_fields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', lnd2atm_smapfile)
       enddo

       name = trim(shr_fire_emis_ztop_token)
       longname = 'wild fire plume height'
       stdname  = 'fire_plume_top'
       units    = 'm'
       call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, trim(name), index=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
       call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', lnd2atm_smapfile)
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
       do n = 1,num
          call shr_string_listGetName(drydepfields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), compatm, mapconsf, 'one', lnd2atm_smapfile)
       enddo
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
       do n = 1,num
          call shr_string_listGetName(ndep_fields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(compatm)%flds, trim(name), index=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldList_AddFld(fldListTo(compocn)%flds, trim(name))
          call fldlist_AddMap(fldListFr(compatm)%flds(n1), complnd mapbilnr, 'one', atm2lnd_smapfile)
          call fldlist_AddMap(fldListFr(compatm)%flds(n1), compocn mapbilnr, 'one', atm2ocn_smapfile)
       enddo
    end if

    !----------------------------------------------------------------------------
    ! state + flux fields
    !----------------------------------------------------------------------------

    ! Determine character list of fields
    call fldList_concat(fldListFr(compatm), fldListTo(compatm), flds_a2x, flds_x2a)
    call fldList_concat(fldListFr(complnd), fldListTo(complnd), flds_l2x, flds_x2l)
    call fldList_concat(fldListFr(compice), fldListTo(compice), flds_i2x, flds_x2i)
    call fldList_concat(fldListFr(compocn), fldListTo(compocn), flds_o2x, flds_x2o)
    call fldList_concat(fldListFr(compglc), fldListTo(compglc), flds_g2x, flds_x2g)
    call fldList_concat(fldListFr(comprof), fldListTo(comprof), flds_r2x, flds_x2r)
    call fldList_concat(fldListFr(compwav), fldListTo(compwav), flds_w2x, flds_x2w)

    do n = 1,size(FldListXao_ocnalb_o)
       if (trim(flds_xao_ocnalb) == '') then
          flds_xao_ocnalb = trim(FldListXao_ocnalb_o(n)%shortname)
       else
          flds_xao_ocnalb = trim(flds_xao_ocnalb//':'//trim(FldListXao_ocnalb_o(n)%shortname)
       end if
    end do
    if (len_trim(flds_xao_ocnalb) >= CXX) then
       write(llogunit,*)'fields are = ',trim(flds_xao_ocnalb)
       write(llogunit,*)'fields length = ',len_trim(flds_xao_ocnalb)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

    do n = 1,size(FldListXao_fluxes_o)
       if (trim(flds_xao_fluxes) == '') then
          flds_xao_fluxes = trim(FldListXao_fluxes_o(n)%shortname)
       else
          flds_xao_fluxes = trim(flds_xao_fluxes//':'//trim(FldListXao_fluxes_o(n)%shortname)
       end if
    end do
    if (len_trim(flds_xao_fluxes) >= CXX) then
       write(llogunit,*)'fields are = ',trim(flds_xao_fluxes)
       write(llogunit,*)'fields length = ',len_trim(flds_xao_fluxes)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

    if (mastertask) then
       write(llogunit, "(A)") subname//': flds_a2x        = ',trim(flds_a2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2a        = ',trim(flds_x2a)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_l2x        = ',trim(flds_l2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2l        = ',trim(flds_x2l)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_i2x        = ',trim(flds_i2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2i        = ',trim(flds_x2i)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_o2x        = ',trim(flds_o2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2o        = ',trim(flds_x2o)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_g2x        = ',trim(flds_g2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2g        = ',trim(flds_x2g)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_r2x        = ',trim(flds_r2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2r        = ',trim(flds_x2r)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_w2x        = ',trim(flds_w2x)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2w        = ',trim(flds_x2w)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_xao_fluxes = ',trim(flds_xao_fluxes)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_xao_ocnalb = ',trim(flds_xao_ocnalb)
       write(llogunit, "(A)") '-------------------------------------------------'
      !write(llogunit, "(A)") subname//': flds_xao_diurnl = ',trim(flds_xao_diurnl)
    end if

  end subroutine shr_nuopc_fldList_Init

  !===============================================================================

  subroutine fldList_concat_entry(fldsFr, fldsTo, concat_src, concat_dst)
    ! Returns new concatentated colon delimited field lists

    ! input/output parameters:
    type(src_fldlist_type), intent(in) :: fldsFr
    type(dst_fldlist_type), intent(in) :: fldsTo
    character(len=*) , intent(inout) :: concat_src
    character(len=*) , intent(inout) :: concat_dst

    ! local variables
    character(len=*),parameter :: subname = '(fldList_concat) '
    !-------------------------------------------------------------------------------

    do n = 1,size(FldsFr%flds)
       if (trim(concat_src) == '') then
          concat_src = trim(FldsFr%flds(n)%shortname)
       else
          concat_src = trim(concat_dst)//':'//trim(FldsFr%flds(n)%shortname)
       end if
    end do

    if (len_trim(concat_src) >= CXX) then
       write(llogunit,*)'fields are = ',trim(concat_src)
       write(llogunit,*)'fields length = ',len_trim(concat_src)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

    do n = 1,size(FldsTo%flds)
       if (trim(concat_dst) == '') then
          concat_dst = trim(FldsTo%flds(n)%shortname)
       else
          concat_dst = trim(concat_dst)//':'//trim(FldsFr%flds(n)%shortname)
       end if
    end do

    if (len_trim(concat_src) >= CXX) then
       write(llogunit,*)'fields are = ',trim(concat_src)
       write(llogunit,*)'fields length = ',len_trim(concat_src)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

  end subroutine fldList_concat_entry

  subroutine fldList_concat_string(fldname, fldlist_string
    ! Returns new concatentated colon delimited field lists

    ! input/output parameters:
    character(len=*), intent(in) :: fldname
    character(len=*), intent(inout) :: fldlist_string

    ! local variables
    character(len=*),parameter :: subname = '(fldList_concat) '
    !-------------------------------------------------------------------------------

    if (trim(fldlist) == '') then
       fldlist_string = trim(fldname)
    else
       fldlist_string = trim(fldlist_string)//':'//trim(fldname)
    end if

    if (len_trim(fldlist_string) >= CXX) then
       write(llogunit,*)'fields are = ',trim(fldlist_string)
       write(llogunit,*)'fields length = ',len_trim(fldlist_string)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if

  end subroutine fldList_concat_string

  !===============================================================================

  subroutine fldList_AddDomain(fldlist, fldname, longname, stdname, units)

    ! Returns new concatentated field and map lists

    ! input/output parameters:
    character(len=*),intent(inout)       :: fldlist   ! output field name
    character(len=*),intent(in)          :: fldname   ! fldname to add to fldlist
    character(len=*),intent(in),optional :: longname
    character(len=*),intent(in),optional :: stdname
    character(len=*),intent(in),optional :: units

    ! local variables
    character(len=*),parameter :: subname = '(fldList_AddDomain) '
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
       call fldList_AddMetadata(trim(fldname), longname, stdname, units)
    endif

  end subroutine fldList_AddDomain

  !===============================================================================

  subroutine fldList_AddMetadata(attname , longname, stdname, units)

    ! input/output parameters:
    character(len=*), intent(in) :: attname
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: stdname
    character(len=*), intent(in) :: units

    ! local variables
    integer :: i, j
    character(len=*),parameter :: subname = '(fldList_AddMetadata) '
    !-------------------------------------------------------------------------------

    ! If the attname passed in contains colons it is assumed to be a list of fields
    ! all of which have the same names and units

    i = index(attname,':')
    j=1

    do while(i>j .and. i<=len_trim(attname))
       if (fld_metadata_entry(attname(j:i-1)) <= 0) then
          n_entries = n_entries + 1
          metadta_entry(n_entries,1) = attname(j:i-1)
          metadta_entry(n_entries,2) = trim(longname)
          metadta_entry(n_entries,3) = trim(stdname )
          metadta_entry(n_entries,4) = trim(units   )
          j=i+1
          i =  index(attname(j:),':') + j - 1
       endif
    enddo
    if (fld_metadata_entry(attname(j:i)) <= 0) then
       n_entries = n_entries + 1
       i = len_trim(attname)
       metadta_entry(n_entries,1) = attname(j:i)
       metadta_entry(n_entries,2) = trim(longname)
       metadta_entry(n_entries,3) = trim(stdname )
       metadta_entry(n_entries,4) = trim(units   )
    endif

    if (n_entries .ge. nmax) then
       write(llogunit,*)'n_entries= ',n_entries,' nmax = ',nmax,' attname= ',trim(attname)
       call shr_sys_abort(subname//'ERROR: nmax fields in metadta_entry table exceeded')
    end if

  end subroutine fldList_AddMetadata

  !================================================================================

  subroutine fldList_AddFld_Src(fldlist, stdname, transferOffer, tag, shortname, index, rc)
    ! ----------------------------------------------
    ! Set up a list of field information
    ! Use pointers to create an extensible allocatable array.
    ! o allow the size of fldlist to grow, the process for
    ! adding a new field is:
    !
    ! 1) allocate new_fldlist to be N (one element larger than fldlist)
    ! 2) copy fldlist into first N-1 elements of new_fldlist
    ! 3) newest fldlist entry is Nth element of new_fldlist
    ! 4) deallocate / nullify fldlist
    ! 5) point fldlist => new_sfo
    ! ----------------------------------------------

    type(shr_nuopc_src_fldList_type) , intent(inout)              :: fldlist(:)
    character(len=*)                 , intent(in)                 :: stdname
    character(len=*)                 , intent(in)    , optional   :: tag
    character(len=*)                 , intent(in)    , optional   :: shortname
    integer                          , intent(out)   , optional   :: index
    integer                          , intent(inout)              :: rc

    ! local variables
    integer :: n,oldsize,id
    type(shr_nuopc_src_entry_type), pointer :: new_fldList(:)
    character(len=*), parameter :: subname='(fldList_AddFld_Src)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (.not.associated(fldlist)) then
       call ESMF_LogWrite(trim(subname)//":"//trim(tag)//&
            " ERROR in fldlist, call fldList_Zero first "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    end if

    oldsize = size(fldlist%flds)
    id = oldsize + 1

    if (associated(fldlist%flds)) then
       oldsize = size(fldlist%flds)
    else
       oldsize = 0
    end if
    id = oldsize + 1

    ! 1) allocate new_fldList to be size (one element larger than this%state)
    allocate(new_fldList%flds(id))

    ! 2) copy fldList into first N-1 elements of new_state
    do n = 1,oldsize
       new_fldlist%flds(n)%stdname           = fldlist(n)%stdname
       new_fldlist(n)%flds(n)%shortname      = fldlist(n)%shortname
       new_fldlist%flds(n)(n)%transfer_offer = fldlist(n)%transfer_offer
       new_fldlist%flds(n)(n)%tag            = fldlist(n)%tag
       new_fldlist%flds(n)(n)%mapname        = fldlist(n)%mapname
       new_fldlist%flds(n)(n)%mapnorm        = fldlist(n)%mapnorm
    end do

    ! 3) deallocate / nullify fldlist
    if (oldsize >  0) then
       deallocate(fldlist%flds)
       nullify(fldlist%flds)
    end if

    ! 4) point fldlist => new_fldlist
    fldlist%flds => new_fldlist%flds

    ! 5) now update fldlist information for new entry
    if (.not. present(transfer_offer)) then
       transfer_offer= 'unset'
    end if
    if (.not. present(tag)) then
       tag = 'unset'
    end if
    if (.not. present(shortname)) then
       shortname = trim(stdname)
    end if
    fldlist%flds(id)%stdname            = trim(stdname)
    fldlist%flds(id)%shortname          = trim(shortname)
    fldlist%flds(id)%transferOffer(num) = trim(transferOffer)
    fldlist%flds(id)%tag(num)           = trim(tag)

    if (present(fldindex)) then
       fldindex = id
    end if

  end subroutine fldList_AddFld_Src

  !================================================================================

  subroutine fldList_AddFld_Dst(fldlist, stdname, transferOffer, tag, shortname,  merge_with_weights, rc)

    ! ----------------------------------------------
    ! Set up a list of field information
    ! Use pointers to create an extensible allocatable array.
    ! o allow the size of fldlist to grow, the process for
    ! adding a new field is:
    !
    ! 1) allocate new_fldlist to be N (one element larger than fldlist)
    ! 2) copy fldlist into first N-1 elements of new_fldlist
    ! 3) newest fldlist entry is Nth element of new_fldlist
    ! 4) deallocate / nullify fldlist
    ! 5) point fldlist => new_sfo
    ! ----------------------------------------------

    type (shr_nuopc_dst_fldlist_type) , intent(inout)         :: fldlist(:)
    logical                           , intent(in) , optional :: merge_with_weights
    character(len=*)                  , intent(in)            :: stdname
    character(len=*)                  , intent(in) , optional :: transferOffer
    character(len=*)                  , intent(in) , optional :: tag
    character(len=*)                  , intent(in) , optional :: shortname
    integer                           , intent(inout)         :: rc

    ! local variables
    integer :: n,oldsize,id
    type(shr_nuopc_dst_entry_type), pointer :: new_fldList(:)
    character(len=*), parameter :: subname='(fldList_AddFld_Dst)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (.not.associated(fldlist)) then
       call ESMF_LogWrite(trim(subname)//":"//trim(tag)//&
            " ERROR in fldlist, call fldList_Zero first "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    end if

    oldsize = size(fldlist%flds)
    id = oldsize + 1

    if (associated(fldlist%flds)) then
       oldsize = size(fldlist%flds)
    else
       oldsize = 0
    end if
    id = oldsize + 1

    ! 1) allocate new_fldList to be size (one element larger than this%state)
    allocate(new_fldList%flds(id))

    ! 2) copy fldList into first N-1 elements of new_state
    do n = 1,oldsize
       new_fldlist%flds(n)%stdname           = fldlist(n)%stdname
       new_fldlist(n)%flds(n)%shortname      = fldlist(n)%shortname
       new_fldlist%flds(n)(n)%transfer_offer = fldlist(n)%transfer_offer
       new_fldlist%flds(n)(n)%tag            = fldlist(n)%tag
       new_fldlist%flds(n)(n)%merge_with_weights = fldlist(n)%merge_with_weights
    end do

    ! 3) deallocate / nullify fldlist
    if (oldsize >  0) then
       deallocate(fldlist%flds)
       nullify(fldlist%flds)
    end if

    ! 4) point fldlist => new_fldlist
    fldlist%flds => new_fldlist%flds

    ! 5) now update fldlist information for new entry
    if (.not. present(transfer_offer)) then
       transfer_offer= 'unset'
    end if
    if (.not. present(tag)) then
       tag = 'unset'
    end if
    if (.not. present(shortname)) then
       shortname = trim(stdname)
    end if
    fldlist%flds(id)%stdname            = trim(stdname)
    fldlist%flds(id)%shortname          = trim(shortname)
    fldlist%flds(id)%transferOffer(num) = trim(transferOffer)
    fldlist%flds(id)%tag(num)           = trim(tag)

    if (present(merge_with_weights)) then
       fldlist%flds(id)%merge_with_weights = merge_with_weights
    end if

  end subroutine fldList_AddFld_Dst

  !================================================================================

  subroutine fldListAddMap(fld, destcomp, mapindex, mapnorm, mapfile)
    type(shr_nuopc_src_entry_type) , intent(inout) :: fld
    integer                        , intent(in)    :: destcomp
    integer                        , intent(in)    :: mapindex
    character(len=*)               , intent(in)    :: mapnorm
    character(len=*)               , intent(in)    :: mapfile

    if (mapfile == 'idmap') then
       fld%mapfile(destcomp) = 'unset'
       fld%mapindex(destcomp) = mapfcopy
    else
       fld%mapfile(destcomp) = mapfile
       fld%mapindex(destcomp) = mapindex
    end if
    fld%mapnorm(destcomp) = mapnorm

  end subroutine fldListAddMap

  !================================================================================

  subroutine shr_nuopc_fldList_Advertise(state, frFldList, toFldList, transferOffer, tag, rc)

    type(ESMF_State)               , intent(inout)     :: state
    type(shr_nuopc_src_entry_type) , pointer, optional :: frFldList(:)
    type(shr_nuopc_dst_entry_type) , pointer, optional :: toFldList(:)
    character(len=*)               , intent(in)        :: transferOffer
    character(len=*)               , intent(in)        :: tag
    integer                        , intent(inout)     :: rc

    ! local variables
    integer       :: n, nflds
    character(CS) :: units
    character(ESMF_MAXSTR), pointer :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer :: ConnectedList(:)
    character(ESMF_MAXSTR), pointer :: NameSpaceList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    character(len=*),parameter  :: subname='(shr_nuopc_fldList_Advertise)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (.not. present(frFlds) .and. .not. present(toFlds)) then
       call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR either frFlds or toFlds argument must be provided", &
            ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    if (present(frFldList)) then
       nflds = size(frFldList%flds)
    else
       nflds = size(ToFldList%flds)
    end if

    do n = 1, nflds
       if (present(frFldList)) then
          shortname = frFldList%flds(n)%shortname
          stdname = frFldList%flds(n)%stdname
       else
          shortname = toFldList%flds(n)%shortname
          shortname = toFldList%flds(n)%stdname
       end if

       call ESMF_LogWrite(subname//':'//trim(tag)//':'//trim(stdname), ESMF_LOGMSG_INFO)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       call NUOPC_Advertise(state, &
            standardName=trim(stdname), shortname=trim(shortname), name=trim(shortname), &
            TransferOfferGeomObject=transferOffer)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    enddo

  end subroutine shr_nuopc_fldList_Advertise

  !================================================================================

  subroutine shr_nuopc_fldList_Realize(state, grid, mesh, fldListFr, fldListTo, tag, rc)

    type(ESMF_State)                  , intent(inout)         :: state
    type(ESMF_Grid)                   , intent(in) , optional :: grid
    type(ESMF_Mesh)                   , intent(in) , optional :: mesh
    type(shr_nuopc_fldlist_entry_src) , pointer    , optional :: fldListFr(:)
    type(shr_nuopc_fldlist_entry_dst) , pointer    , optional :: fldListTo(:)
    character(len=*)                  , intent(in)            :: tag
    integer                           , intent(inout)         :: rc

    ! local variables
    integer                         :: n, nflds
    integer                         :: itemCount
    type(ESMF_Field)                :: field
    character(CS)                   :: shortname
    character(CS)                   :: stdname
    character(ESMF_MAXSTR)          :: transferAction
    character(ESMF_MAXSTR), pointer :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer :: ConnectedList(:)
    character(ESMF_MAXSTR), pointer :: NameSpaceList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    character(len=*),parameter  :: subname='(shr_nuopc_fldList_Realize)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (.not. present(fldListFr) .and. .not. present(fldListTo)) then
       call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR either fldListFr or fldListTo argument must be provided as input", &
            ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       return
    else if (present(fldListFr) .and. present(fldListTo) then
       call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR FldListFr and fldListTo cannot both be provided as input", &
            ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       return
    endif

    if (present(grid) .and. present(mesh)) then
       call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR both grid and mesh not allowed", &
            ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       return
    endif

    nullify(StandardNameList)
    nullify(ConnectedList)
    nullify(NameSpaceList)
    nullify(ItemNameList)

    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    write(infostr,'(i6)') itemCount

    call ESMF_LogWrite(trim(subname)//trim(tag)//" count = "//trim(infostr), ESMF_LOGMSG_INFO, rc=dbrc)
    if (itemCount > 0) then
       allocate(itemNameList(itemCount))
       call ESMF_StateGet(state, itemNameList=itemNameList, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       do n = 1,itemCount
          call ESMF_LogWrite(trim(subname)//trim(tag)//" itemNameList = "//trim(itemNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
       enddo
       deallocate(itemNameList)
    endif

#if (1 == 0)
    call NUOPC_GetStateMemberLists(state, StandardNameList=StandardNameList, ConnectedList=ConnectedList, &
         NamespaceList=NamespaceList, itemNameList=itemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    write(infostr,'(i6)') size(StandardNameList)
    call ESMF_LogWrite(trim(subname)//trim(tag)//" size = "//trim(infostr), ESMF_LOGMSG_INFO, rc=dbrc)

    do n = 1,size(StandardNameList)
       call ESMF_LogWrite(trim(subname)//trim(tag)//" StandardNameList = "//trim(StandardNameList(n)), &
            ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    do n = 1,size(ConnectedList)
       call ESMF_LogWrite(trim(subname)//trim(tag)//" ConnectedList = "//trim(ConnectedList(n)), &
            ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    do n = 1,size(NamespaceList)
       call ESMF_LogWrite(trim(subname)//trim(tag)//" NamespaceList = "//trim(NamespaceList(n)), &
            ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    do n = 1,size(ItemnameList)
       call ESMF_LogWrite(trim(subname)//trim(tag)//" ItemnameList = "//trim(ItemnameList(n)), &
            ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
#endif

    if (present(fldListFr)) then
       nflds = size(fldListFr%flds)
    else
       nflds = size(fldListTo%flds)
    end if

    do n = 1, nflds

       if (present(fldListFr)) then
          shortname = fldListFr%flds(n)%shortname
       else
          shortname = fldListTo%flds(n)%shortname
       end if
       ! call ESMF_LogWrite(subname//' fld = '//trim(shortname), ESMF_LOGMSG_INFO, rc=dbrc)

       if (NUOPC_IsConnected(state, fieldName=shortname)) then

          call ESMF_StateGet(state, field=field, itemName=trim(shortname), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          if (trim(transferAction) == "accept") then
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(shortname)//" is connected, grid/mesh TBD", &
                  ESMF_LOGMSG_INFO, rc=dbrc)

          else   ! provide

             if (shortname == trim(flds_scalar_name)) then
                call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(shortname)//" is connected on root pe", &
                     ESMF_LOGMSG_INFO, rc=dbrc)
                call shr_nuopc_fldList_SetScalarField(field, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
             elseif (present(grid)) then
                call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(shortname)//" is connected using grid", &
                     ESMF_LOGMSG_INFO, rc=dbrc)
                field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=shortname,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
             elseif (present(mesh)) then
                call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(shortname)//" is connected using mesh", &
                     ESMF_LOGMSG_INFO, rc=dbrc)
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=shortname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
             else
                call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR grid or mesh expected", &
                     ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
                rc = ESMF_FAILURE
                return
             endif

             call NUOPC_Realize(state, field=field, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

             ! call ESMF_FieldPrint(field=field, rc=rc)
             ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          endif

       else

          call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(shortname) // " is not connected.", &
               ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_StateRemove(state, (/shortname/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       end if

    end do

    call ESMF_LogWrite(subname//' done ', ESMF_LOGMSG_INFO, rc=dbrc)
  end subroutine shr_nuopc_fldList_Realize

end module shr_nuopc_fldList_mod
