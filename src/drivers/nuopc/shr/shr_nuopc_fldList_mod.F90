module shr_nuopc_fldList_mod

  use ESMF
  use NUOPC
  use shr_nuopc_fldList_types_mod
  use shr_kind_mod      , only : CX => shr_kind_CX, CXX => shr_kind_CXX, CS=>shr_kind_CS, CL=>shr_kind_CL
  use shr_sys_mod       , only : shr_sys_abort
  use seq_comm_mct      , only : llogunit => logunit
  use seq_drydep_mod    , only : seq_drydep_init, seq_drydep_readnl, lnd_drydep
  use shr_megan_mod     , only : shr_megan_readnl, shr_megan_mechcomps_n
  use shr_fire_emis_mod , only : shr_fire_emis_readnl, shr_fire_emis_mechcomps_n, shr_fire_emis_ztop_token
  use shr_carma_mod     , only : shr_carma_readnl
  use shr_ndep_mod      , only : shr_ndep_readnl
  use shr_flds_mod      , only : shr_flds_dom_coord, shr_flds_dom_other
  use shr_string_mod    , only : shr_string_listGetNum, shr_string_listGetName, shr_string_lastindex
  use glc_elevclass_mod , only : glc_elevclass_init, glc_get_num_elevation_classes, glc_elevclass_as_string

  implicit none
  public

  !----------------------------------------------------------------------------
  ! routines
  !----------------------------------------------------------------------------

  public :: shr_nuopc_fldList_SetDict
  public :: shr_nuopc_fldList_Init
  public :: shr_nuopc_fldList_Concat
  public :: shr_nuopc_fldList_Realize

  interface fldList_AddFld ; module procedure &
       fldList_AddFld_Src, &
       fldList_AddFld_Dst
  end interface fldList_AddFld
  private :: fldList_AddFld
  private :: fldList_AddDomain
  private :: fldList_AddMetadata
  private :: fldList_AddMap

  !----------------------------------------------------------------------------
  ! other field lists - column deliminated string
  !----------------------------------------------------------------------------
  ! TODO: these must move away from colon delimited fields

  character(len=CXX) :: drydep_fields       ! List of dry-deposition fields
  character(len=CXX) :: megan_voc_fields    ! List of MEGAN VOC emission fields
  character(len=CXX) :: fire_emis_fields    ! List of fire emission fields
  character(len=CX)  :: carma_fields        ! List of CARMA fields from lnd->atm
  character(len=CX)  :: ndep_fields         ! List of nitrogen deposition fields from atm->lnd/ocn
  integer            :: ice_ncat            ! number of sea ice thickness categories
  logical            :: flds_i2o_per_cat    ! .true. => select per ice thickness category fields passed from ice to ocn
  logical            :: add_ndep_fields     ! .true. => add ndep fields

  !----------------------------------------------------------------------------
  ! private data
  !----------------------------------------------------------------------------

  integer           :: dbrc
  character(len=CL) :: infostr

!================================================================================
contains
!================================================================================

  subroutine shr_nuopc_fldList_SetDict(rc)
    ! ----------------------------------------------
    ! Build NUOPC dictionary from flds data
    ! ----------------------------------------------
    integer, intent(inout)  :: rc

    ! local variables
    integer       :: n
    character(CS) :: stdname
    character(CS) :: units
    character(len=*), parameter :: subname='(shr_nuopc_fldList_SetDict)'
    !------------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1,n_entries
       stdname = trim(metadata_entry(n,3))
       units   = trim(metadata_entry(n,4))
       if (.not.NUOPC_FieldDictionaryHasEntry(stdname)) then
          call ESMF_LogWrite(subname//': Add:'//trim(stdname), ESMF_LOGMSG_INFO, rc=rc)
          call NUOPC_FieldDictionaryAddEntry(standardName=stdname, canonicalUnits=units, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
    enddo

  end subroutine shr_nuopc_fldList_SetDict

  !================================================================================

  subroutine shr_nuopc_fldList_Init(gcomp, ID, rc)

    ! input/output parameters:
    type(ESMF_GridComp)    :: gcomp
    integer, intent(in)    :: ID        ! seq_comm ID TODO: this needs to be depracated
    integer, intent(inout) :: rc

    ! local variables:
    type(ESMF_VM)          :: vm
    integer                :: localPet
    logical                :: mastertask
    character(len=CSS)     :: attname
    character(len=CSS)     :: units
    character(len=CSS)     :: longname
    character(len=CSS)     :: stdname
    integer                :: num, i, n
    integer                :: n1, n2, n3, n4
    character(len=  2)     :: cnum
    character(len=CSS)     :: name
    character(len=CL)      :: cvalue
    character(len=CX)      :: atm2ice_fmapname
    character(len=CX)      :: atm2ice_smapname
    character(len=CX)      :: atm2ice_vmapname
    character(len=CX)      :: atm2lnd_fmapname
    character(len=CX)      :: atm2lnd_smapname
    character(len=CX)      :: atm2ocn_fmapname
    character(len=CX)      :: atm2ocn_smapname
    character(len=CX)      :: atm2ocn_vmapname
    character(len=CX)      :: atm2wav_smapname
    character(len=CX)      :: glc2lnd_fmapname
    character(len=CX)      :: glc2lnd_smapname
    character(len=CX)      :: glc2ice_rmapname
    character(len=CX)      :: glc2ocn_rmapname
    character(len=CX)      :: ice2atm_fmapname
    character(len=CX)      :: ice2atm_smapname
    character(len=CX)      :: ice2wav_smapname
    character(len=CX)      :: lnd2atm_fmapname
    character(len=CX)      :: lnd2atm_smapname
    character(len=CX)      :: lnd2glc_fmapname
    character(len=CX)      :: lnd2glc_smapname
    character(len=CX)      :: lnd2rof_fmapname
    character(len=CX)      :: ocn2atm_fmapname
    character(len=CX)      :: ocn2atm_smapname
    character(len=CX)      :: ocn2wav_smapname
    character(len=CX)      :: rof2lnd_fmapname
    character(len=CX)      :: rof2ocn_fmapname
    character(len=CX)      :: rof2ocn_ice_rmapname
    character(len=CX)      :: rof2ocn_liq_rmapname
    character(len=CX)      :: wav2ocn_smapname
    character(len=CX)      :: wav2ocn_fmapname
    logical                :: flds_co2a  ! use case
    logical                :: flds_co2b  ! use case
    logical                :: flds_co2c  ! use case
    logical                :: flds_wiso  ! use case
    logical                :: do_flux_diurnal
    integer                :: glc_nec
    character(CXX)         :: concatFr, concatTo
    character(len=*), parameter :: subname='(shr_nuopc_fldList_Init)'
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

    call NUOPC_CompAttributeGet(gcomp, name='flux_diurnal', value=cvalue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    read(cvalue,*) do_flux_diurnal

    !----------------------------------------------------------
    ! Initialize mapping file names
    !----------------------------------------------------------

    ! To atm

    call NUOPC_CompAttributeGet(gcomp, name='ice2atm_fmapname', value=ice2atm_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ice2atm_fmapname = '// trim(ice2atm_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='ice2atm_smapname', value=ice2atm_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ice2atm_smapname = '// trim(ice2atm_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='lnd2atm_fmapname', value=lnd2atm_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2atm_fmapname = '// trim(lnd2atm_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_smapname', value=ocn2atm_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ocn2atm_smapname = '// trim(ocn2atm_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_fmapname', value=ocn2atm_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ocn2atm_fmapname = '// trim(ocn2atm_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='lnd2atm_smapname', value=lnd2atm_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2atm_smapname = '// trim(lnd2atm_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    ! To lnd

    call NUOPC_CompAttributeGet(gcomp, name='atm2lnd_fmapname', value=atm2lnd_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2lnd_fmapname = '// trim(atm2lnd_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='atm2lnd_smapname', value=atm2lnd_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2lnd_smapname = '// trim(atm2lnd_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='rof2lnd_fmapname', value=rof2lnd_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2lnd_fmapname = '// trim(rof2lnd_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='glc2lnd_fmapname', value=glc2lnd_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2lnd_smapname = '// trim(glc2lnd_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='glc2lnd_smapname', value=glc2lnd_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2lnd_smapname = '// trim(glc2lnd_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    ! To ice

    call NUOPC_CompAttributeGet(gcomp, name='atm2ice_fmapname', value=atm2ice_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ice_fmapname = '// trim(atm2ice_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ice_smapname', value=atm2ice_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ice_smapname = '// trim(atm2ice_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ice_vmapname', value=atm2ice_vmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ice_vmapname = '// trim(atm2ice_vmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='glc2ice_rmapname', value=glc2ice_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2ice_rmapname = '// trim(glc2ice_rmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    ! To ocn

    call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_fmapname', value=atm2ocn_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ocn_fmapname = '// trim(atm2ocn_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_smapname', value=atm2ocn_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ocn_smapname = '// trim(atm2ocn_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_vmapname', value=atm2ocn_vmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2ocn_vmapname = '// trim(atm2ocn_vmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='glc2ocn_rmapname', value=glc2ocn_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('glc2ocn_rmapname = '// trim(glc2ocn_rmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='wav2ocn_fmapname', value=wav2ocn_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('wav2ocn_fmapname = '// trim(wav2ocn_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='rof2ocn_fmapname', value=rof2ocn_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2ocn_fmapname = '// trim(rof2ocn_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='rof2ocn_liq_rmapname', value=rof2ocn_liq_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2ocn_liq_rmapname = '// trim(rof2ocn_liq_rmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='rof2ocn_ice_rmapname', value=rof2ocn_ice_rmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('rof2ocn_ice_rmapname = '// trim(rof2ocn_ice_rmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    ! To rof

    call NUOPC_CompAttributeGet(gcomp, name='lnd2rof_fmapname', value=lnd2rof_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2rof_fmapname = '// trim(lnd2rof_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    ! To glc

    call NUOPC_CompAttributeGet(gcomp, name='lnd2glc_fmapname', value=lnd2glc_fmapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2glc_fmapname = '// trim(lnd2glc_fmapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='lnd2glc_smapname', value=lnd2glc_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('lnd2glc_smapname = '// trim(lnd2glc_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    ! To wav

    call NUOPC_CompAttributeGet(gcomp, name='atm2wav_smapname', value=atm2wav_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('atm2wav_smapname = '// trim(atm2wav_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='ice2wav_smapname', value=ice2wav_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ice2wav_smapname = '// trim(ice2wav_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_CompAttributeGet(gcomp, name='ocn2wav_smapname', value=ocn2wav_smapname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_LogWrite('ocn2wav_smapname = '// trim(ocn2wav_smapname), ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------
    ! scalar information
    !----------------------------------------------------------

    longname = trim(flds_scalar_name)
    stdname  = trim(flds_scalar_name)
    units    = 'unitless'
    call fldList_AddMetadata(fldname=trim(flds_scalar_name), longname=longname, stdname=stdname, units=units)
    do n = 1,ncomps
       call fldList_AddFld(fldListFr(n)%flds, trim(flds_scalar_name))
       call fldList_AddFld(fldListTo(n)%flds, trim(flds_scalar_name))
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
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_ifrac', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Si_ifrac')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Si_ifrac')
    call fldList_AddFld(fldListTo(compwav)%flds, 'Si_ifrac')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compatm,  mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    !----------------------------------------------------------
    ! Fields from atm
    !----------------------------------------------------------

    longname = 'Height at the lowest model level'
    stdname  = 'height'
    units    = 'm'
    call fldList_AddMetadata(fldname='Sa_z', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_z', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_z')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_z')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapbilnr, 'one', atm2ice_smapname)

    longname = 'Surface height'
    stdname  = 'height'
    units    = 'm'
    call fldList_AddMetadata(fldname='Sa_topo', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_topo', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_topo')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)

    ! TODO: Sa_u and Sa_v are mapped to the ocean grid in the mediator - BUT are not sent to the ocean -
    ! They are only used in the atm/ocn flux calculation - so a special mapping will be done in the mediator
    ! for these fields

    longname = 'Zonal wind at the lowest model level'
    stdname  = 'eastward_wind'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname='Sa_u', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_u', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_u')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_u')
    call fldList_AddFld(fldListTo(compwav)%flds, 'Sa_u')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mappatch, 'one', atm2ice_vmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compwav, mapbilnr, 'one', atm2wav_smapname)

    longname = 'Meridional wind at the lowest model level'
    stdname  = 'northward_wind'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname='Sa_v', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_v', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_v')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_v')
    call fldList_AddFld(fldListTo(compwav)%flds, 'Sa_v')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mappatch, 'one', atm2ice_vmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compwav, mapbilnr, 'one', atm2wav_smapname)

    longname = 'Temperature at the lowest model level'
    stdname  = 'air_temperature'
    units    = 'K'
    call fldList_AddMetadata(fldname='Sa_tbot', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_tbot', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_tbot')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_tbot')
    call fldList_AddFld(fldListTo(compwav)%flds, 'Sa_tbot')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapbilnr, 'one', atm2ice_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compwav, mapbilnr, 'one', atm2wav_smapname)

    longname = 'Potential temperature at the lowest model level'
    stdname  = 'air_potential_temperature'
    units    = 'K'
    call fldList_AddMetadata(fldname='Sa_ptem', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_ptem', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_ptem')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_ptem')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapbilnr, 'one', atm2ice_smapname)

    longname = 'Specific humidity at the lowest model level'
    stdname  = 'specific_humidity'
    units    = 'kg kg-1'
    call fldList_AddMetadata(fldname='Sa_shum', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_shum', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_shum')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_shum')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapbilnr, 'one', atm2ice_smapname)

    longname = 'Pressure at the lowest model level'
    stdname  = 'air_pressure'
    units    = 'Pa'
    call fldList_AddMetadata(fldname='Sa_pbot', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_pbot', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_pbot')
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_pbot')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapbilnr, 'one', atm2ice_smapname)

    longname = 'Density at the lowest model level'
    stdname  = 'air_density'
    units    = 'kg m-3'
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_dens', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'Sa_dens')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice,  mapbilnr, 'one', atm2ice_smapname)

    units    = 'kg m-2 s-1'
    longname = 'Convective precipitation rate'
    stdname  = 'convective_precipitation_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_rainc', longname=longname, stdname=stdname, units=units)
    longname = 'Large-scale (stable) precipitation rate' ! water equivalent
    stdname  = 'large_scale_precipitation_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_rainc', longname=longname, stdname=stdname, units=units)
    longname = 'Water flux due to rain'
    stdname  = 'rainfall_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_rain', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_rainc', fldindex=n)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_rainc')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_rain' )
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rain' )
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Convective snow rate'
    stdname  = 'convective_snowfall_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_snowc', longname=longname, stdname=stdname, units=units)
    longname = 'Large-scale (stable) snow rate'
    stdname  = 'large_scale_snowfall_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_snowl', longname=longname, stdname=stdname, units=units)
    longname = 'Water flux due to snow'
    stdname  = 'surface_snow_melt_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_snowc', fldindex=n)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_snowc')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_snow' )
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_snow' )
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    ! total precipitation to ocean (derived rain + snow, done AFTER mappings)
    longname = 'Water flux (rain+snow)'
    stdname  = 'precipitation_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Foxx_prec', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_prec')

    longname = 'Downward longwave heat flux'
    stdname  = 'downwelling_longwave_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname='Faxa_lwdn', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Foxx_lwdn', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_lwdn', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_lwdn')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_lwdn')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_lwdn')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Direct near-infrared incident solar radiation'
    stdname  = 'surface_downward_direct_shortwave_flux_due_to_near_infrared_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swndr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swndr', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swndr')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swndr')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)

    longname = 'Direct visible incident solar radiation'
    stdname  = 'surface_downward_direct_shortwave_flux_due_to_visible_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swvdr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swvdr', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swvdr')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swvdr')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)

    longname = 'Diffuse near-infrared incident solar radiation'
    stdname  = 'surface_downward_diffuse_shortwave_flux_due_to_near_infrared_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swndf", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swndf', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swndf')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swndf')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)

    longname = 'Diffuse visible incident solar radiation'
    stdname  = 'surface_downward_diffuse_shortwave_flux_due_to_visible_radiation'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname='Faxa_swvdf', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swvdf', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_swvdf')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_swvdf')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)

    longname = 'Net shortwave radiation'
    stdname  = 'surface_net_shortwave_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Faxa_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fall_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_swnet", longname=longname, stdname=stdname, units=units)
    longname = 'Net shortwave radiation penetrating into ice and ocean'
    stdname  = 'net_downward_shortwave_flux_in_sea_ice_due_to_penetration'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname='Fioi_swpen', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_swnet", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_swnet')  ! only diagnostic
    call fldList_AddFld(fldListTo(complnd)%flds, 'Fall_swnet')  ! only diagnostic
    call fldList_AddFld(fldListTo(compice)%flds, 'Faii_swnet')  ! only diagnostic
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_swpen', fldindex=n1) ! used for Foxx_swnet (see below)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_swnet', merge_with_weights=.true.) ! derived using albedos, Faxa_sw[v,n][dr,df] and swpen
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Hydrophylic black carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Faxa_bcphidry', longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname='Foxx_bcphidry', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphidry', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphidry', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Hydrophobic black carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophobic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_bcphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphodry', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphodry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphodry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphodry', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Hydrophylic black carbon wet deposition flux'
    stdname  = 'wet_deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_bcphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphiwet', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphiwet')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphiwet')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphiwet')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Hydrophylic organic carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophylic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_bcphidry", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphidry", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_bcphidry', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_bcphidry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphidry', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Hydrophobic organic carbon dry deposition flux'
    stdname  = 'dry_deposition_flux_of_hydrophobic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_ocphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_ocphodry", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_ocphodry', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_ocphodry')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_ocphodry')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_ocphodry', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Hydrophylic organic carbon wet deposition flux'
    stdname  = 'wet_deposition_flux_of_hydrophylic_organic_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_ocphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_ocphiwet", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_ocphiwet', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_ocphiwet')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_ocphiwet')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_ocphiwet', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust wet deposition flux (size 1)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet1", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet1', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet1')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet1')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet1', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust wet deposition flux (size 2)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet2", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet2", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet2', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet2')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet2')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet2', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust wet deposition flux (size 3)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet3", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet3", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet3', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet3')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet3')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet3', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust wet deposition flux (size 4)'
    stdname  = 'wet_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstwet4", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstwet4", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstwet4', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstwet4')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstwet4')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstwet4', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust dry deposition flux (size 1)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry1", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry1', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry1')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry1')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry1', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust dry deposition flux (size 2)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry2", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry2", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry2', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry2')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry2')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry2', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust dry deposition flux (size 3)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry3", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry3", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry3', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry3')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry3')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry3', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

    longname = 'Dust dry deposition flux (size 4)'
    stdname  = 'dry_deposition_flux_of_dust'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Faxa_dstdry4", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_dstdry4", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Faxa_dstdry4', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Faxa_dstdry4')
    call fldList_AddFld(fldListTo(compice)%flds, 'Faxa_dstdry4')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_dstdry4', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapconsf, 'one', atm2lnd_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compice, mapconsf, 'one', atm2ice_fmapname)
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapconsf, 'one', atm2ocn_fmapname)

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
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_avsdr', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_avsdr', fldindex=n2)
    call fldList_AddFld(fldListMed_ocnalb_o%flds, 'So_avsdr', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_avsdr', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_smapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_smapname)
    call fldlist_AddMap(fldListMed_ocnalb_o%flds(n3), compocn, compatm, mapconsf, 'ofrac', ocn2atm_smapname)

    longname = 'Direct albedo (near-infrared radiation)'
    stdname  = 'surface_direct_albedo_due_to_near_infrared_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_anidr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_anidr', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_anidr', fldindex=n2)
    call fldList_AddFld(fldListMed_ocnalb_o%flds, 'So_anidr', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_anidr', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_ocnalb_o%flds(n3), compocn, compatm, mapconsf, 'ofrac', ocn2atm_smapname)

    longname = 'Diffuse albedo (visible radiation)'
    stdname  = 'surface_diffuse_albedo_due_to_visible_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_avsdf", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_avsdf', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_avsdf', fldindex=n2)
    call fldList_AddFld(fldListMed_ocnalb_o%flds, 'So_avsdf', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_avsdf', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_ocnalb_o%flds(n3), compocn, compatm, mapconsf, 'ofrac', ocn2atm_smapname)

    longname = 'Diffuse albedo (near-infrared radiation)'
    stdname  = 'surface_diffuse_albedo_due_to_near_infrared_radiation'
    units    = '1'
    call fldList_AddMetadata(fldname="Si_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_anidf", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_anidf', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_anidf', fldindex=n2)
    call fldList_AddFld(fldListMed_ocnalb_o%flds, 'So_anidf', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_anidf', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_ocnalb_o%flds(n3), compocn, compatm, mapconsf, 'ofrac', ocn2atm_smapname)

    longname = 'Reference temperature at 2 meters'
    stdname  = 'air_temperature'
    units    = 'K'
    call fldList_AddMetadata(fldname="Si_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_tref", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_tref', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_tref', fldindex=n2)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_tref', fldindex=n3) ! Needed only for merging
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_tref', fldindex=n3) ! Needed only for merging
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_tref', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n3), compatm, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n3), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    longname = 'Reference specific humidity at 2 meters'
    stdname  = 'specific_humidity'
    units    = 'kg kg-1'
    call fldList_AddMetadata(fldname="Si_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_qref", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_qref', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_qref', fldindex=n2)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_qref', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_qref', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_qref', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n3), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n3), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    longname = 'Surface temperature'
    stdname  = 'surface_temperature'
    units    = 'K'
    call fldList_AddMetadata(fldname="Si_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sl_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_t", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_t', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_t', fldindex=n2)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_t', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sx_t', merge_with_weights=.true.)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_t')
    call fldList_AddFld(fldListTo(compwav)%flds, 'So_t')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf , 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2), compice, compatm, mapconsf , 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListFr(compocn)%flds(n3), compocn, compatm, mapconsf , 'ofrac', ocn2atm_fmapname)
    call fldlist_AddMap(fldListFr(compocn)%flds(n3), compocn, compwav, mapbilnr , 'one'  , ocn2wav_smapname) ! This will be a custom map - need to name it however
    call fldlist_AddMap(fldListFr(compocn)%flds(n3), compocn, compice, mapfcopy , 'unset', 'unset')

    longname = 'Surface fraction velocity in land'
    stdname  = 'fraction_velocity'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname="Sl_fv", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_fv', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_fv')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    longname = 'Aerodynamic resistance'
    stdname  = 'aerodynamic_resistance'
    units    = 's/m'
    call fldList_AddMetadata(fldname="Sl_ram1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_ram1', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_ram1')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    longname = 'Surface snow water equivalent'
    stdname  = 'surface_snow_water_equivalent'
    units    = 'm'
    call fldList_AddMetadata(fldname="Sl_snowh", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Sl_snowh', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Sl_snowh')
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    longname = 'Surface snow depth'
    stdname  = 'surface_snow_thickness'
    units    = 'm'
    call fldList_AddMetadata(fldname="Si_snowh", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Si_snowh', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Si_snowh')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)

    longname = 'Surface saturation specific humidity in ocean'
    stdname  = 'specific_humidity_at_saturation'
    units    = 'kg kg-1'
    call fldList_AddMetadata(fldname="So_ssq", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_ssq', fldindex=n1)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_ssq')
    call fldList_AddFld(fldListTo(compatm)%flds , 'So_ssq')
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n1), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n1), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    longname = 'Square of exch. coeff (tracers)'
    stdname  = 'square_of_exch_coeff'
    units    = '1'
    call fldList_AddMetadata(fldname="So_re", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_re', fldindex=n1)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_re')
    call fldList_AddFld(fldListTo(compatm)%flds , 'So_re')
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n1), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n1), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    longname = '10m wind'
    stdname  = '10m_wind'
    units    = 'm'
    call fldList_AddMetadata(fldname="Sl_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Si_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="So_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Sx_u10", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Sl_u10', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Si_u10', fldindex=n2)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_u10', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_u10', fldindex=n3)
    call fldList_AddFld(fldListTo(compatm)%flds , 'Sx_u10', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n3), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n3), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    longname = 'Zonal surface stress'
    stdname  = 'surface_downward_eastward_stress'
    units    = 'N m-2'
    call fldList_AddMetadata(fldname="Fall_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_taux", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_taux', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_taux', fldindex=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_taux', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'Faox_taux', fldindex=n4)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'Faox_taux')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_taux', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n4), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n4), compatm, compocn, mapconsf, 'one'  , atm2ocn_fmapname) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Meridional surface stress'
    stdname  = 'surface_downward_northward_stress'
    units    = 'N m-2'
    call fldList_AddMetadata(fldname="Fall_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_tauy", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_tauy', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_tauy', fldindex=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_tauy', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'Faox_tauy', fldindex=n4)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'Faox_tauy')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_tauy', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n4), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n4), compatm, compocn, mapconsf, 'one'  , atm2ocn_fmapname) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Surface latent heat flux'
    stdname  = 'surface_upward_latent_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fall_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_lat", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_lat', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_lat', fldindex=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_lat', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'Faox_lat', fldindex=n4)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'Faox_lat')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_lat', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n4), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n4), compatm, compocn, mapconsf, 'one'  , atm2ocn_fmapname) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Sensible heat flux'
    stdname  = 'surface_upward_sensible_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fall_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_sen", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_sen', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_sen', fldindex=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_sen', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'Faox_sen', fldindex=n4)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'Faox_sen')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_sen', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n4), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n4), compatm, compocn, mapconsf, 'one'  , atm2ocn_fmapname) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Surface upward longwave heat flux'
    stdname  = 'surface_net_upward_longwave_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fall_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_lwup", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds , 'Fall_lwup', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_lwup', fldindex=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_lwup', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'Faox_lwup', fldindex=n4)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'Faox_lwup')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_lwup', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n4), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n4), compatm, compocn, mapconsf, 'one'  , atm2ocn_fmapname) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Evaporation water flux'
    stdname  = 'water_evaporation_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faox_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faii_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fioi_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_evap", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_evap', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds , 'Faii_evap', fldindex=n2)
    call fldList_AddFld(fldListFr(compice)%flds , 'Fioi_evap', fldindex=n3)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'Faox_evap', fldindex=n4)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'Faox_evap')
    call fldList_AddFld(fldListTo(compatm)%flds , 'Foxx_evap', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1) , complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)
    call fldlist_AddMap(fldListFr(compice)%flds(n2) , compice, compatm, mapconsf, 'ifrac', ice2atm_fmapname)
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n4), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n4), compatm, compocn, mapconsf, 'one'  , atm2ocn_fmapname) ! map atm->ocn
    call fldlist_AddMap(fldListFr(compice)%flds(n3) , compice, compocn, mapfcopy, 'unset', 'unset')

    longname = 'Dust flux (particle bin number 1)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst1", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst1", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst1', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst1', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    longname = 'Dust flux (particle bin number 2)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst2", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst2", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst2', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst2', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    longname = 'Dust flux (particle bin number 3)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst3", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst3", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst3', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst3', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    longname = 'Dust flux (particle bin number 4)'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fall_flxdst4", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Faxx_flxdst4", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_flxdst4', fldindex=n1)
    call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_flxdst4', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'lfrin', lnd2atm_fmapname)

    !-----------------------------
    ! atm<->ocn only exchange
    !-----------------------------

    longname = 'Sea level pressure'
    stdname  = 'air_pressure_at_sea_level'
    units    = 'Pa'
    call fldList_AddMetadata(fldname="Sa_pslv", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_pslv', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_pslv')
    call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapbilnr, 'one', atm2ocn_smapname)

    longname = 'Wind speed squared at 10 meters'
    stdname  = 'square_of_wind_speed'
    units    = 'm2 s-2'
    call fldList_AddMetadata(fldname="So_duu10n", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_duu10n', fldindex=n1)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_duu10n')
    call fldList_AddFld(fldListTo(compocn)%flds , 'So_duu10n')
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n1), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n1), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    longname = 'Surface fraction velocity in ocean'
    stdname  = 'fraction_velocity'
    units    = 'm s-1'
    call fldList_AddFld(fldListMed_aoflux_a%flds, 'So_ustar', fldindex=n1)
    call fldList_AddFld(fldListMed_aoflux_o%flds, 'So_ustar')
    call fldList_AddFld(fldListTo(compocn)%flds , 'So_ustar')
    call fldlist_AddMap(fldListMed_aoflux_o%flds(n1), compocn, compatm, mapconsf, 'ofrac', ocn2atm_fmapname) ! map ocn->atm
    call fldlist_AddMap(fldListMed_aoflux_a%flds(n1), compatm, compocn, mapbilnr, 'one'  , atm2ocn_fmapname) ! map atm->ocn

    !-----------------------------
    ! ice->ocn exchange
    !-----------------------------

    longname = 'Heat flux from melting'
    stdname  = 'surface_snow_melt_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fioi_melth", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_melth", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_melth', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_melth')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    longname = 'Water flux due to melting'
    stdname  = 'surface_snow_melt_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_meltw", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_meltw", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_meltw', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_meltw')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    longname = 'Salt flux'
    stdname  = 'virtual_salt_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_salt", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_salt", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_salt', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_salt')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    longname = 'Hydrophylic black carbon deposition flux'
    stdname  = 'deposition_flux_of_hydrophylic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_bcphi", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcphi", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_bcphi', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcphi')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    longname = 'Hydrophobic black carbon deposition flux'
    stdname  = 'deposition_flux_of_hydrophobic_black_carbon'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_bcpho", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_bcpho", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_bcpho', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_bcpho')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    longname = 'Dust flux'
    stdname  = 'dust_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Fioi_flxdst", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_flxdst", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compice)%flds, 'Fioi_flxdst', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_flxdst')
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    !-----------------------------
    ! ocn -> ice exchange
    !-----------------------------

    longname = 'Sea surface salinity'
    stdname  = 'sea_surface_salinity'
    units    = 'g kg-1'
    call fldList_AddMetadata(fldname="So_s", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_s', fldindex=n1)
    call fldList_AddFld(fldListFr(compice)%flds, 'So_s', fldindex=n1)
    call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

    longname = 'Ocean melt and freeze potential'
    stdname  = 'surface_snow_and_ice_melt_heat_flux'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname="Fioo_q", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'Fioo_q', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'Fioo_q')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    longname = 'Zonal sea water velocity'
    stdname  = 'eastward_sea_water_velocity'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname="So_u", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_u', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_u')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    longname = 'Meridional sea water velocity'
    stdname  = 'northward_sea_water_velocity'
    units    = 'm s-1'
    call fldList_AddMetadata(fldname="So_v", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_v', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_v')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    longname = 'Zonal sea surface slope'
    stdname  = 'sea_surface_eastward_slope'
    units    = 'm m-1'
    call fldList_AddMetadata(fldname="So_dhdx", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_dhdx', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_dhdx')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    longname = 'Meridional sea surface slope'
    stdname  = 'sea_surface_northward_slope'
    units    = 'm m-1'
    call fldList_AddMetadata(fldname="So_dhdy", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_dhdy', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_dhdy')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    longname = 'Ocean Boundary Layer Depth'
    stdname  = 'ocean_boundary_layer_depth'
    units    = 'm'
    call fldList_AddMetadata(fldname="So_bldepth", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_bldepth', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_bldepth')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    longname = 'Fraction of sw penetrating surface layer for diurnal cycle'
    stdname  = 'Fraction_of_sw_penetrating_surface_layer'
    units    = '1'
    call fldList_AddMetadata(fldname="So_fswpen", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compocn)%flds, 'So_fswpen', fldindex=n1)
    call fldList_AddFld(fldListTo(compice)%flds, 'So_fswpen')
    call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compice,  mapfcopy, 'unset', 'unset')

    !-----------------------------
    ! lnd->rof exchange
    !-----------------------------

    longname = 'Water flux from land (liquid surface)'
    stdname  = 'water_flux_into_runoff_surface'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofsur", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofsur", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofsur', fldindex=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofsur', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land (liquid glacier, wetland, and lake)'
    stdname  = 'water_flux_into_runoff_from_gwl'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofgwl", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofgwl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofgwl', fldindex=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofgwl', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land (liquid subsurface)'
    stdname  = 'water_flux_into_runoff_subsurface'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofsub", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofsub", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofsub', fldindex=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofsub', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land direct to ocean'
    stdname  = 'water_flux_direct_to_ocean'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofdto", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofdto", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofdto', fldindex=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofdto', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    longname = 'Water flux from land (frozen)'
    stdname  = 'frozen_water_flux_into_runoff'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_rofi", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_rofi", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_rofi', fldindex=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_rofi', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    ! Irrigation flux (land/rof only)
    longname = 'Irrigation flux (withdrawal from rivers)'
    stdname  = 'irrigation'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrl_irrig", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Frxx_irrig", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(complnd)%flds, 'Flrl_irrig', fldindex=n1)
    call fldList_AddFld(fldListTo(comprof)%flds, 'Frxx_irrig', merge_with_weights=.true.)
    call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, comprof, mapconsf, 'lfrin', lnd2rof_fmapname)

    !-----------------------------
    ! rof->lnd
    !-----------------------------

    longname = 'Waterflux back to land due to flooding'
    stdname  = 'flooding_water_flux'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Flrr_flood", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Flrr_flood', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Flrr_flood')
    call fldList_AddFld(fldListTo(compocn)%flds, 'Flrr_flood')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, complnd, mapconsf, 'one', rof2lnd_fmapname)
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, complnd, mapconsf, 'one', rof2ocn_fmapname)

    longname = 'River channel total water volume'
    stdname  = 'rtm_volr'
    units    = 'm'
    call fldList_AddMetadata(fldname="Flrr_volr", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Flrr_volr', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Flrr_volr')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, complnd, mapconsf, 'one', rof2lnd_fmapname)

    longname = 'River channel main channel water volume'
    stdname  = 'rtm_volrmch'
    units    = 'm'
    call fldList_AddMetadata(fldname="Flrr_volrmch", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Flrr_volrmch', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds, 'Flrr_volrmch')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, complnd, mapconsf, 'one', rof2lnd_fmapname)

    !-----------------------------
    ! rof->ocn (liquid and frozen)
    !-----------------------------

    longname = 'Water flux into sea water due to runoff (liquid)'
    stdname  = 'water_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Foxx_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Forr_rofl', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rofl')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, compocn, mapunset, 'none', rof2ocn_liq_rmapname)

    longname = 'Water flux into sea water due to runoff (frozen)'
    stdname  = 'frozen_water_flux_into_sea_water'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Forr_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Foxx_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Forr_rofi', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rofi')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, compocn, mapunset, 'none', rof2ocn_ice_rmapname)

    !-----------------------------
    ! rof->ice (frozen)
    !-----------------------------

    longname = 'Water flux into sea ice due to runoff (frozen)'
    stdname  = 'frozen_water_flux_into_sea_ice'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname="Firr_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddMetadata(fldname="Fixx_rofl", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(comprof)%flds, 'Forr_rofi', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Foxx_rofi')
    call fldlist_AddMap(fldListFr(comprof)%flds(n1), comprof, compocn, mapunset, 'none', rof2ocn_ice_rmapname)

    !-----------------------------
    ! wav->ocn
    !-----------------------------

    longname = 'Langmuir multiplier'
    stdname  = 'wave_model_langmuir_multiplier'
    units    = '1'
    call fldList_AddMetadata(fldname='Sw_lamult', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compwav)%flds, 'Sw_lamult', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Sw_lamult')
    call fldlist_AddMap(fldListFr(compwav)%flds(n1), compwav, compocn,  mapbilnr, 'one', wav2ocn_smapname)

    longname = 'Stokes drift u component'
    stdname  = 'wave_model_stokes_drift_eastward_velocity'
    units    = 'm/s'
    call fldList_AddMetadata(fldname='Sw_ustokes', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compwav)%flds, 'Sw_ustokes', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Sw_ustokes')
    call fldlist_AddMap(fldListFr(compwav)%flds(n1), compwav, compocn,  mapbilnr, 'one', wav2ocn_smapname)

    longname = 'Stokes drift v component'
    stdname  = 'wave_model_stokes_drift_northward_velocity'
    units    = 'm/s'
    call fldList_AddMetadata(fldname='Sw_vstokes', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compwav)%flds, 'Sw_vstokes', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Sw_vstokes')
    call fldlist_AddMap(fldListFr(compwav)%flds(n1), compwav, compocn, mapbilnr, 'one', wav2ocn_smapname)

    longname = 'Stokes drift depth'
    stdname  = 'wave_model_stokes_drift_depth'
    units    = 'm'
    call fldList_AddMetadata(fldname='Sw_hstokes', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compwav)%flds, 'Sw_hstokes', fldindex=n1)
    call fldList_AddFld(fldListTo(compocn)%flds, 'Sw_hstokes')
    call fldlist_AddMap(fldListFr(compwav)%flds(n1), compwav, compocn, mapbilnr, 'one', wav2ocn_smapname)

    !-----------------------------
    ! fields for history output only
    !-----------------------------

    if (do_flux_diurnal) then
       longname = 'Downward solar radiation'
       stdname  = 'surface_downward_shortwave_flux'
       units    = 'W m-2'
       call fldList_AddMetadata(fldname="Faox_swdn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'Faox_swdn') ! Needed for FB initialization

       longname = 'Upward solar radiation'
       stdname  = 'surface_upward_shortwave_flux'
       units    = 'W m-2'
       call fldList_AddMetadata(fldname="Faox_swup", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'Faox_swup') ! Needed for FB initialization

       longname = 'atm/ocn flux temperature bulk'
       stdname  = 'aoflux_tbulk'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tbulk_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_tbulk_diurn')

       longname = 'atm/ocn flux temperature skin'
       stdname  = 'aoflux_tskin'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tskin_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds,  'So_tskin_diurn')

       longname = 'atm/ocn flux temperature skin at night'
       stdname  = 'aoflux_tskin_night'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tskin_night_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_tskin_night_diurn')

       longname = 'atm/ocn flux temperature skin at day'
       stdname  = 'aoflux_tskin_day'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_tskin_day_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_tskin_day_diurn')

       longname = 'atm/ocn flux cool skin'
       stdname  = 'aoflux_cskin'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_cskin_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_cskin_diurn')

       longname = 'atm/ocn flux cool skin at night'
       stdname  = 'aoflux_cskin_night'
       units    = 'K'
       call fldList_AddMetadata(fldname="So_cskin_night_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_cskin_night_diurn')

       longname = 'atm/ocn flux warming'
       stdname  = 'aoflux_warm'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_warm_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_warm_diurn')

       longname = 'atm/ocn flux salting'
       stdname  = 'aoflux_salt'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_salt_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_salt_diurn')

       longname = 'atm/ocn flux speed'
       stdname  = 'aoflux_speed'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_speed_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_speed_diurn')

       longname = 'atm/ocn flux regime'
       stdname  = 'aoflux_regime'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_regime_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_regime_diurn')

       longname = 'atm/ocn flux warming dialy max'
       stdname  = 'aoflux_warmmax'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_warmmax_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_warmmax_diurn')

       longname = 'atm/ocn flux wind daily max'
       stdname  = 'aoflux_windmax'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windmax_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_windmax_diurn')

       longname = 'atm/ocn flux q-solar daily avg'
       stdname  = 'aoflux_qsolavg'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_qsolvavg_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_qsolvavg_diurn')

       longname = 'atm/ocn flux wind daily avg'
       stdname  = 'aoflux_windavg'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windavg_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_windavg_diurn')

       longname = 'atm/ocn flux daily max increment'
       stdname  = 'aoflux_warmmaxinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_warmmaxinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_warmmaxinc_diurn')

       longname = 'atm/ocn flux wind daily max increment'
       stdname  = 'aoflux_windmaxinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windmaxinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_windmaxinc_diurn')

       longname = 'atm/ocn flux q-solar increment'
       stdname  = 'aoflux_qsolinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_qsolinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_qsolinc_diurn')

       longname = 'atm/ocn flux wind increment'
       stdname  = 'aoflux_windinc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_windinc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_windinc_diurn')

       longname = 'atm/ocn flux increment counter'
       stdname  = 'aoflux_ninc'
       units    = 'unitless'
       call fldList_AddMetadata(fldname="So_ninc_diurn", longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(FldListMed_aoflux_diurnl%flds, 'So_ninc_diurn')
    end if

    !-----------------------------
    ! glc -> ocn
    !-----------------------------

    longname = 'glc liquid runoff flux to ocean'
    stdname  = 'glacier_liquid_runoff_flux_to_ocean'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Fogg_rofl', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, 'Flgg_rofl', fldindex=n1)
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compglc, compocn,  mapunset, 'one', glc2ocn_rmapname)

    longname = 'glc frozen runoff flux to ocean'
    stdname  = 'glacier_frozen_runoff_flux_to_ocean'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Fogg_rofi', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, 'Flgg_rofi', fldindex=n1)
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compglc, compocn,  mapunset, 'one', glc2ocn_rmapname)

    !-----------------------------
    ! glc -> ice
    !-----------------------------

    longname = 'glc frozen runoff_iceberg flux to ice'
    stdname  = 'glacier_frozen_runoff_flux_to_seaice'
    units    = 'kg m-2 s-1'
    call fldList_AddMetadata(fldname='Figg_rofi', longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, 'Figg_rofi', fldindex=n1)
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compglc, compice,  mapunset, 'one', glc2ice_rmapname)

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
    call fldList_AddFld(fldListFr(compglc)%flds   , 'Sg_icemask', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds   , 'Sg_icemask')
    call fldList_AddFld(fldListMed_x2l_fr_glc%flds, 'Sg_icemask') ! Needed for FB initialization
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compglc, complnd,  mapconsf, 'one', glc2lnd_smapname)

    longname = 'Ice sheet mask where we are potentially sending non-zero fluxes'
    stdname  = 'icemask_coupled'
    units    = '1'
    call fldList_AddMetadata(fldname="Sg_icemask_coupled_fluxes", longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds   , 'Sg_icemask_coupled_fluxes', fldindex=n1)
    call fldList_AddFld(fldListTo(complnd)%flds   , 'Sg_icemask_coupled_fluxes')
    call fldList_AddFld(fldListMed_x2l_fr_glc%flds, 'Sg_icemask_coupled_fluxes') 
    call fldlist_AddMap(fldListFr(compglc)%flds(n1), compglc, complnd,  mapconsf, 'one', glc2lnd_smapname)

    name = 'Sg_ice_covered'
    longname = 'Fraction of glacier area'
    stdname  = 'glacier_area_fraction'
    units    = '1'
    call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, trim(name), fldindex=n1)
    call fldList_AddMap(FldListFr(compglc)%flds(n1), compglc, complnd, mapconsf, 'unset', glc2lnd_fmapname) ! TODO: normalization?
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname= trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, units=units)
          call fldList_AddFld(fldListTo(complnd)%flds   , trim(name)//trim(cnum))
          call fldList_AddFld(fldListMed_x2l_fr_glc%flds, trim(name)//trim(cnum))
       end do
    end if

    name = 'Sg_topo'
    longname = 'Surface height of glacier'
    stdname  = 'height'
    units    = 'm'
    call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, trim(name), fldindex=n1)
    call fldList_AddMap(FldListFr(compglc)%flds(n1), compglc, compglc, mapconsf, 'custom', glc2lnd_fmapname)
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname= trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, units=units)
          call fldList_AddFld(fldListTo(complnd)%flds   , trim(name)//trim(cnum))
          call fldList_AddFld(fldListMed_x2l_fr_glc%flds, trim(name)//trim(cnum))
       end do
    end if

    name = 'Flgg_hflx'
    attname = name
    longname = 'Downward heat flux from glacier interior'
    stdname  = 'downward_heat_flux_in_glacier'
    units    = 'W m-2'
    call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListFr(compglc)%flds, trim(name), fldindex=n1)
    call fldList_AddMap(FldListFr(compglc)%flds(n1), compglc, compglc, mapconsf, 'custom', glc2lnd_fmapname)
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname= trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, units=units)
          call fldList_AddFld(fldListTo(complnd)%flds   , trim(name)//trim(cnum))
          call fldList_AddFld(fldListMed_x2l_fr_glc%flds, trim(name)//trim(cnum))
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
    ! Note that, if glc_nec = 0, then we don't create any coupling fields (not even the bare land (0) fldindex)

    name = 'Flgl_qice'
    longname = 'New glacier ice flux'
    stdname  = 'ice_flux_out_of_glacier'
    units    = 'kg m-2 s-1'
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname = trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds   , trim(name)//trim(cnum), fldindex=n1)
          call fldList_AddFld(fldListMed_l2x_to_glc%flds, trim(name)//trim(cnum))
       end do
    end if
    call fldList_AddMetadata(fldname= trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compglc)%flds, trim(name))
    call fldList_AddMap(FldListFr(complnd)%flds(n1), complnd, compglc, mapconsf, 'none', lnd2glc_fmapname)

    name = 'Sl_tsrf'
    longname = 'Surface temperature of glacier'
    stdname  = 'surface_temperature'
    units    = 'deg C'
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname  = trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds   , trim(name)//trim(cnum), fldindex=n1)
          call fldList_AddFld(fldListMed_l2x_to_glc%flds, trim(name)//trim(cnum))
       end do
    end if
    call fldList_AddMetadata(fldname= trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compglc)%flds, trim(name))
    call fldList_AddMap(FldListFr(complnd)%flds(n1), complnd, compglc, mapconsf, 'none', lnd2glc_fmapname)

    ! Sl_topo is sent from lnd -> med, but is NOT sent to glc (it is only used for the
    ! remapping in the mediator)
    name = 'Sl_topo'
    longname = 'Surface height'
    stdname  = 'height'
    units    = 'm'
    if (glc_get_num_elevation_classes() > 0) then
       do num = 0, glc_get_num_elevation_classes()
          cnum = glc_elevclass_as_string(num)
          call fldList_AddMetadata(fldname  = trim(name)//trim(cnum), &
               longname = trim(longname)//' of elevation class '//trim(cnum), stdname =stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds   , trim(name)//trim(cnum), fldindex=n1)
          call fldList_AddFld(fldListMed_l2x_to_glc%flds, trim(name)//trim(cnum))
       end do
    end if
    call fldList_AddMetadata(fldname= trim(name), longname=longname, stdname=stdname, units=units)
    call fldList_AddFld(fldListTo(compglc)%flds, trim(name))
    call fldList_AddMap(FldListFr(complnd)%flds(n1), complnd, compglc, mapconsf, 'none', lnd2glc_fmapname)

    !-----------------------------
    ! co2 fields
    !-----------------------------

    if (flds_co2a) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2prog', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2prog', fldindex=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2prog')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2prog')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapbilnr, 'one', atm2ocn_smapname)

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2diag', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2diag', fldindex=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2diag')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2diag')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapbilnr, 'one', atm2ocn_smapname)

    else if (flds_co2b) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2prog', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2prog', fldindex=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2prog')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2diag', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2diag', fldindex=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2diag')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)

       longname = 'Surface flux of CO2 from land'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_land'
       units    = 'moles m-2 s-1'
       call fldList_AddMetadata(fldname='Fall_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddMetadata(fldname='Faxx_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_fco2_lnd', fldindex=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_fco2_lnd', merge_with_weights=.true.)
       call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', atm2lnd_smapname)

    else if (flds_co2c) then

       longname = 'Prognostic CO2 at the lowest model level'
       stdname  = 'prognostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2prog', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2prog', fldindex=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2prog')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2prog')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapbilnr, 'one', atm2ocn_smapname)

       longname = 'Diagnostic CO2 at the lowest model level'
       stdname  = 'diagnostic_CO2_lowest_level'
       units    = '1e-6 mol/mol'
       call fldList_AddMetadata(fldname='Sa_co2diag', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(compatm)%flds, 'Sa_co2diag', fldindex=n1)
       call fldList_AddFld(fldListTo(complnd)%flds, 'Sa_co2diag')
       call fldList_AddFld(fldListTo(compocn)%flds, 'Sa_co2diag')
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
       call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapbilnr, 'one', atm2ocn_smapname)

       longname = 'Surface flux of CO2 from land'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_land'
       units    = 'moles m-2 s-1'
       call fldList_AddMetadata(fldname='Fall_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddMetadata(fldname='Faxx_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, 'Fall_fco2_lnd', fldindex=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_fco2_lnd', merge_with_weights=.true.)
       call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', atm2lnd_smapname)

       longname = 'Surface flux of CO2 from ocean'
       stdname  = 'surface_upward_flux_of_carbon_dioxide_where_open_sea'
       units    = 'moles m-2 s-1'
       call fldList_AddMetadata(fldname='Fall_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddMetadata(fldname='Faxx_fco2_lnd', longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, 'Faoo_fco2_ocn', fldindex=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, 'Faxx_fco2_ocn', merge_with_weights=.true.)
       call fldlist_AddMap(fldListFr(compocn)%flds(n1), compocn, compatm, mapconsf, 'one', ocn2atm_smapname)

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
          call fldList_AddFld(fldListFr(compice)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compocn)%flds, trim(name))
          call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')

          ! Net shortwave radiation
          name = 'PFioi_swpen_ifrac_' // cnum
          longname = 'net shortwave radiation penetrating into ice and ocean times ice fraction for thickness category ' // cnum
          stdname  = 'product_of_net_downward_shortwave_flux_at_sea_water_surface_and_sea_ice_area_fraction'
          units    = 'W m-2'
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(compice)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compocn)%flds, trim(name))
          call fldlist_AddMap(fldListFr(compice)%flds(n1), compice, compocn,  mapfcopy, 'unset', 'unset')
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
       ! TODO: add mapping

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
          call shr_string_listGetName(carma_fields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', lnd2atm_smapname)
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
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', atm2lnd_smapname)
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
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', lnd2atm_smapname)
       enddo

       name = trim(shr_fire_emis_ztop_token)
       longname = 'wild fire plume height'
       stdname  = 'fire_plume_top'
       units    = 'm'
       call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
       call fldList_AddFld(fldListFr(complnd)%flds, trim(name), fldindex=n1)
       call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
       call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', lnd2atm_smapname)
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
          call shr_string_listGetName(drydep_fields, n, name)
          call fldList_AddMetadata(fldname=trim(name), longname=longname, stdname=stdname, units=units)
          call fldList_AddFld(fldListFr(complnd)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldlist_AddMap(fldListFr(complnd)%flds(n1), complnd, compatm, mapconsf, 'one', lnd2atm_smapname)
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
          call fldList_AddFld(fldListFr(compatm)%flds, trim(name), fldindex=n1)
          call fldList_AddFld(fldListTo(compatm)%flds, trim(name))
          call fldList_AddFld(fldListTo(compocn)%flds, trim(name))
          call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, complnd, mapbilnr, 'one', atm2lnd_smapname)
          call fldlist_AddMap(fldListFr(compatm)%flds(n1), compatm, compocn, mapbilnr, 'one', atm2ocn_smapname)
       enddo
    end if

    !----------------------------------------------------------------------------
    ! creation of colon delimited field list
    !----------------------------------------------------------------------------

    ! Determine character list of fields
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(compatm), fldListTo(compatm), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': flds_a2x        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2a        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(complnd), fldListTo(complnd), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': flds_l2x        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2l        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(compice), fldListTo(compice), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': flds_i2x        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': flds_x2i        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(compocn), fldListTo(compocn), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': concatFr        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': concatTo        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(compglc), fldListTo(compglc), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': concatFr        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': concatTo        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(comprof), fldListTo(comprof), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': concatFr        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': concatTo        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if
    concatFr = ''; concatTo = ''
    call shr_nuopc_fldList_Concat(fldListFr(compwav), fldListTo(compwav), concatFr, concatTo)
    if (mastertask) then
       write(llogunit, "(A)") subname//': concatFr        = ',trim(concatFr)
       write(llogunit, "(A)") '-------------------------------------------------'
       write(llogunit, "(A)") subname//': concatTo        = ',trim(concatTo)
       write(llogunit, "(A)") '-------------------------------------------------'
    end if

  end subroutine shr_nuopc_fldList_Init

  !===============================================================================

  subroutine shr_nuopc_fldList_Concat(fldsFr, fldsTo, concat_src, concat_dst, rc)
    ! Returns new concatentated colon delimited field lists
    
    ! input/output parameters:
    type(shr_nuopc_fldList_src_type) , intent(in)    :: fldsFr
    type(shr_nuopc_fldList_dst_type) , intent(in)    :: fldsTo
    character(len=*)                 , intent(inout) :: concat_src
    character(len=*)                 , intent(inout) :: concat_dst
    integer, optional                , intent(out)   :: rc
    
    ! local variables
    integer :: n
    character(len=*),parameter :: subname = '(shr_nuopc_fldList_concat) '
    !-------------------------------------------------------------------------------
    
    do n = 1,size(FldsFr%flds)
       if (trim(FldsFr%flds(n)%shortname) /= flds_scalar_name) then
          if (trim(concat_src) == '') then
             concat_src = trim(FldsFr%flds(n)%shortname)
          else
             concat_src = trim(concat_dst)//':'//trim(FldsFr%flds(n)%shortname)
          end if
       end if
    end do
    if (len_trim(concat_src) >= CXX) then
       write(llogunit,*)'fields are = ',trim(concat_src)
       write(llogunit,*)'fields length = ',len_trim(concat_src)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if
    
    do n = 1,size(FldsTo%flds)
       if (trim(FldsTo%flds(n)%shortname) /= flds_scalar_name) then
          if (trim(concat_dst) == '') then
             concat_dst = trim(FldsTo%flds(n)%shortname)
          else
             concat_dst = trim(concat_dst)//':'//trim(FldsFr%flds(n)%shortname)
          end if
       end if
    end do
    if (len_trim(concat_src) >= CXX) then
       write(llogunit,*)'fields are = ',trim(concat_src)
       write(llogunit,*)'fields length = ',len_trim(concat_src)
       call shr_sys_abort(subname//'ERROR: maximum length of xxx or xxx has been exceeded')
    end if
    
  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine fldList_concat_string(fldname, fldlist_string)
      character(len=*), intent(in)    :: fldname
      character(len=*), intent(inout) :: fldlist_string
      
      ! local variables
      character(len=*),parameter :: subname = '(fldList_concat_string) '
      
      ! Returns new concatentated colon delimited field lists
      if (trim(fldlist_string) == '') then
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

  end subroutine shr_nuopc_fldList_Concat

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
  
  subroutine fldList_AddMetadata(fldname , longname, stdname, units)

    ! input/output parameters:
    character(len=*), intent(in) :: fldname
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: stdname
    character(len=*), intent(in) :: units

    ! local variables
    integer :: i, j
    character(len=*),parameter :: subname = '(fldList_AddMetadata) '
    !-------------------------------------------------------------------------------

    ! If the fldname passed in contains colons it is assumed to be a list of fields
    ! all of which have the same names and units

    i = index(fldname,':')
    j=1

    do while(i>j .and. i<=len_trim(fldname))
       if (get_metadata_entry(fldname(j:i-1)) <= 0) then
          n_entries = n_entries + 1
          metadata_entry(n_entries,1) = fldname(j:i-1)
          metadata_entry(n_entries,2) = trim(longname)
          metadata_entry(n_entries,3) = trim(stdname )
          metadata_entry(n_entries,4) = trim(units   )
          j=i+1
          i =  index(fldname(j:),':') + j - 1
       endif
    enddo
    if (get_metadata_entry(fldname(j:i)) <= 0) then
       n_entries = n_entries + 1
       i = len_trim(fldname)
       metadata_entry(n_entries,1) = fldname(j:i)
       metadata_entry(n_entries,2) = trim(longname)
       metadata_entry(n_entries,3) = trim(stdname )
       metadata_entry(n_entries,4) = trim(units   )
    endif

    if (n_entries >= nmax) then
       write(llogunit,*)'n_entries= ',n_entries,' nmax = ',nmax,' fldname= ',trim(fldname)
       call shr_sys_abort(subname//'ERROR: nmax fields in metadata_entry table exceeded')
    end if

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    integer function get_metadata_entry(shortname)
      character(len=*), intent(in)  :: shortname

      integer :: i,n,lnentry
      logical :: found
      character(len=CSS) :: lshortname  ! local copies
      character(len=*),parameter :: subname = '(get_metadata_entry) '
      !-------------------------------------------------------------------------------

      found = .false.
      lnentry = 0
      if (.not.found) then
         i = 1
         do while (i <= n_entries .and. .not.found)
            if (trim(shortname) == trim(metadata_entry(i,1))) then
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
            if (trim(lshortname) == trim(metadata_entry(i,1))) then
               found   = .true.
               lnentry = i
            end if
            i = i + 1
         end do
      endif
      get_metadata_entry = lnentry
    end function get_metadata_entry

  end subroutine fldList_AddMetadata

  !================================================================================

  subroutine fldList_AddFld_Src(flds, stdname, shortname, fldindex)
    ! ----------------------------------------------
    ! Add an entry to to the flds array
    ! Use pointers to create an extensible allocatable array.
    ! to allow the size of flds to grow, the process for
    ! adding a new field is:
    ! 1) allocate newflds to be N (one element larger than flds)
    ! 2) copy flds into first N-1 elements of newflds
    ! 3) newest flds entry is Nth element of newflds
    ! 4) deallocate / nullify flds
    ! 5) point flds => newflds
    ! ----------------------------------------------

    type(shr_nuopc_fldList_src_entry_type) , pointer                    :: flds(:)
    character(len=*)                       , intent(in)                 :: stdname
    character(len=*)                       , intent(in)    , optional   :: shortname
    integer                                , intent(out)   , optional   :: fldindex

    ! local variables
    integer :: n,oldsize,id
    type(shr_nuopc_fldList_src_entry_type), pointer :: newflds(:)
    character(len=*), parameter :: subname='(fldList_AddFld_Src)'
    ! ----------------------------------------------

    if (.not.associated(flds)) then
       call shr_sys_abort(trim(subname) // "flds is not associated")
    end if

    oldsize = size(flds)
    id = oldsize + 1

    if (associated(flds)) then
       oldsize = size(flds)
    else
       oldsize = 0
    end if
    id = oldsize + 1

    ! 1) allocate newfld to be size (one element larger than input flds)
    allocate(newflds(id))

    ! 2) copy flds into first N-1 elements of newflds
    do n = 1,oldsize
       newflds(n)%stdname   = flds(n)%stdname
       newflds(n)%shortname = flds(n)%shortname
       newflds(n)%mapindex  = flds(n)%mapindex
       newflds(n)%mapnorm   = flds(n)%mapnorm
    end do

    ! 3) deallocate / nullify flds
    if (oldsize >  0) then
       deallocate(flds)
       nullify(flds)
    end if

    ! 4) point flds => new_flds
    flds => newflds

    ! 5) now update flds information for new entry
    flds(id)%stdname   = trim(stdname)
    if (present(shortname)) then
       flds(id)%shortname = trim(shortname)
    else
       flds(id)%shortname = trim(stdname)
    end if

    if (present(fldindex)) then
       fldindex = id
    end if

  end subroutine fldList_AddFld_Src

  !================================================================================

  subroutine fldList_AddFld_Dst(flds, stdname, shortname, merge_with_weights)

    ! ----------------------------------------------
    ! Add an entry to to the flds array
    ! Use pointers to create an extensible allocatable array.
    ! to allow the size of flds to grow, the process for
    ! adding a new field is:
    ! 1) allocate newflds to be N (one element larger than flds)
    ! 2) copy flds into first N-1 elements of newflds
    ! 3) newest flds entry is Nth element of newflds
    ! 4) deallocate / nullify flds
    ! 5) point flds => newflds
    ! ----------------------------------------------

    type (shr_nuopc_fldList_dst_entry_type) , pointer               :: flds(:)
    character(len=*)                        , intent(in)            :: stdname
    character(len=*)                        , intent(in) , optional :: shortname
    logical                                 , intent(in) , optional :: merge_with_weights

    ! local variables
    integer :: n,oldsize,id
    type(shr_nuopc_fldList_dst_entry_type), pointer :: newflds(:)
    character(len=*), parameter :: subname='(fldList_AddFld_Dst)'
    ! ----------------------------------------------

    if (.not.associated(flds)) then
       call shr_sys_abort(trim(subname) // "flds is not associated in ")
    end if

    oldsize = size(flds)
    id = oldsize + 1

    if (associated(flds)) then
       oldsize = size(flds)
    else
       oldsize = 0
    end if
    id = oldsize + 1

    ! 1) allocate newflds to be one element larger than flds
    allocate(newflds(id))

    ! 2) copy flds into first N-1 elements of newflds
    do n = 1,oldsize
       newflds(n)%stdname           = flds(n)%stdname
       newflds(n)%shortname         = flds(n)%shortname
       newflds(n)%merge_with_weights = flds(n)%merge_with_weights
    end do

    ! 3) deallocate / nullify flds
    if (oldsize >  0) then
       deallocate(flds)
       nullify(flds)
    end if

    ! 4) point flds => newflds
    flds => newflds

    ! 5) now update flds information for new entry
    if (present(shortname)) then
       flds(id)%shortname = trim(shortname)
    else
       flds(id)%shortname = trim(stdname)
    end if

    if (present(merge_with_weights)) then
       flds(id)%merge_with_weights = merge_with_weights
    end if

  end subroutine fldList_AddFld_Dst

  !================================================================================

  subroutine fldList_AddMap(fld, srccomp, destcomp, mapindex, mapnorm, mapfile)
    type(shr_nuopc_fldList_src_entry_type) , intent(inout) :: fld
    integer                                , intent(in)    :: srccomp
    integer                                , intent(in)    :: destcomp
    integer                                , intent(in)    :: mapindex
    character(len=*)                       , intent(in)    :: mapnorm
    character(len=*)                       , intent(in)    :: mapfile
    
    ! local variables
    logical :: mapset 
    character(len=*),parameter  :: subname='(fldList_AddMap)'
    ! ----------------------------------------------

    ! Note - default values are already set for the fld entries - so only non-default
    ! values need to be set below
    ! If mapindex is mapfcopy - create a redistribution route handle
    ! If mapfile is idmap - create a redistribution route nhandle
    ! If mapfile is unset then create the mapping route handle at run time

    fld%mapindex(destcomp) = mapindex
    fld%mapfile(destcomp)  = trim(mapfile)
    fld%mapnorm(destcomp)  = trim(mapnorm)

    ! overwrite values if appropriate
    if (fld%mapindex(destcomp) == mapfcopy) then
       fld%mapfile(destcomp) = 'unset'
       fld%mapnorm(destcomp) = 'unset'
    else if (trim(fld%mapfile(destcomp)) == 'idmap') then
       fld%mapindex(destcomp) = mapfcopy
       fld%mapnorm(destcomp) = 'unset'
    end if
  end subroutine fldList_AddMap

  !================================================================================

  subroutine shr_nuopc_fldList_Realize(state, grid, mesh, fldsFr, fldsTo, tag, rc)
    type(ESMF_State)                      , intent(inout)         :: state
    type(ESMF_Grid)                       , intent(in) , optional :: grid
    type(ESMF_Mesh)                       , intent(in) , optional :: mesh
    type(shr_nuopc_fldlist_src_entry_type), pointer    , optional :: fldsFr(:)
    type(shr_nuopc_fldlist_dst_entry_type), pointer    , optional :: fldsTo(:)
    character(len=*)                      , intent(in)            :: tag
    integer                               , intent(inout)         :: rc

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

    if (.not. present(fldsFr) .and. .not. present(fldsTo)) then
       call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR either fldsFr or fldsTo argument must be provided as input", &
            ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       return
    else if (present(fldsFr) .and. present(fldsTo)) then
       call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR FldsFr and fldsTo cannot both be provided as input", &
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

    if (present(fldsFr)) then
       nflds = size(fldsFr)
    else
       nflds = size(fldsTo)
    end if

    do n = 1, nflds
       if (present(fldsFr)) then
          shortname = fldsFr(n)%shortname
       else
          shortname = fldsTo(n)%shortname
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
                call SetScalarField(field, rc=rc)
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

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine SetScalarField(field, rc)
      ! ----------------------------------------------
      ! create a field with scalar data on the root pe
      ! ----------------------------------------------
      type(ESMF_Field), intent(inout)  :: field
      integer,          intent(inout)  :: rc

      ! local variables
      type(ESMF_Distgrid) :: distgrid
      type(ESMF_Grid)     :: grid
      character(len=*), parameter :: subname='(SetScalarField)'
      ! ----------------------------------------------

      rc = ESMF_SUCCESS

      ! create a DistGrid with a single index space element, which gets mapped onto DE 0.
      distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      field = ESMF_FieldCreate(name=trim(flds_scalar_name), &
           grid=grid, &
           typekind=ESMF_TYPEKIND_R8, &
           ungriddedLBound=(/1/), &
           ungriddedUBound=(/flds_scalar_num/), rc=rc)  ! num of scalar values
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    end subroutine SetScalarField

  end subroutine shr_nuopc_fldList_Realize

end module shr_nuopc_fldList_mod
