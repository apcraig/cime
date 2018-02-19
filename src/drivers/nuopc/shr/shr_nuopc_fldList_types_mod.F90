module shr_nuopc_fldList_types_mod

  use shr_kind_mod, only : CX => shr_kind_CX, CXX => shr_kind_CXX, CS=>shr_kind_CS, CL=>shr_kind_CL
  integer, parameter :: CSS = 256  ! use longer short character
  integer, parameter :: CLL = 1024

  !-----------------------------------------------
  ! Metadata array
  !-----------------------------------------------

  character(len=*), parameter :: undef     = 'undefined'
  integer         , parameter :: nmax      = 1000        ! maximum number of entries in metadta_entry
  integer                     :: n_entries = 0           ! actual number of entries in metadta_entry
  character(len=CSS)          :: metadata_entry(nmax,4) = undef

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

  !-----------------------------------------------
  ! Component and mapping array indices
  !-----------------------------------------------

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

  integer, parameter :: nmappers=5
  integer, parameter :: mapbilnr=1
  integer, parameter :: mapconsf=2
  integer, parameter :: mapconsd=3
  integer, parameter :: mappatch=4
  integer, parameter :: mapfcopy=5
  integer, parameter :: mapunset=0

  character(len=*), parameter :: mapnames(nmappers) = (/'bilnr','consf','consd','patch','fcopy'/)

  !-----------------------------------------------
  ! Types and instantiations that determine fields, mappings, mergings
  !-----------------------------------------------

  type shr_nuopc_fldList_src_entry_type
     character(CS) :: stdname
     character(CS) :: shortname
     integer       :: mapindex(ncomps) = mapunset
     character(CS) :: mapnorm(ncomps) = 'unset'
     character(CX) :: mapfile(ncomps) = 'unset'
  end type shr_nuopc_fldList_src_entry_type

  type shr_nuopc_fldList_dst_entry_type
     character(CS) :: stdname
     character(CS) :: shortname
     logical       :: merge_with_weight = .false.
  end type shr_nuopc_fldList_dst_entry_type

  type shr_nuopc_fldList_src_type
     type (shr_nuopc_fldList_src_entry_type), pointer :: flds(:)
  end type shr_nuopc_fldList_src_type

  type shr_nuopc_fldList_dst_type
     type (shr_nuopc_fldList_dst_entry_type), pointer :: flds(:)
  end type shr_nuopc_fldList_dst_type

  ! The following is used in the advertise AND realize fields in the components and mediator
  ! Advertise  mediator fields in med.F90 routine InitializeIPDv03p1
  ! Realize    mediator fields in med.F90 routine InitializeIPDv03p3
  type (shr_nuopc_fldList_dst_type) :: fldListTo(ncomps)
  type (shr_nuopc_fldList_src_type) :: fldListFr(ncomps)

  ! Initialize mediator field bundles in med.F90 routine DataInitialize
  type (shr_nuopc_fldList_src_type) :: fldListMed_aoflux_a
  type (shr_nuopc_fldList_src_type) :: fldListMed_aoflux_o
  type (shr_nuopc_fldList_src_type) :: fldListMed_ocnalb_a
  type (shr_nuopc_fldList_src_type) :: fldListMed_ocnalb_o
  type (shr_nuopc_fldList_src_type) :: fldListMed_aoflux_diurnl
  type (shr_nuopc_fldList_src_type) :: fldListMed_l2x_to_glc
  type (shr_nuopc_fldList_src_type) :: fldListMed_x2l_fr_glc
  type (shr_nuopc_fldList_src_type) :: fldListMed_g2x_to_lnd

end module shr_nuopc_fldList_types_mod
