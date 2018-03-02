module shr_nuopc_fldList_mod

  use ESMF
  use NUOPC
  use shr_kind_mod   , only : CX => shr_kind_CX, CXX => shr_kind_CXX, CS=>shr_kind_CS, CL=>shr_kind_CL
  use shr_sys_mod    , only : shr_sys_abort
  use shr_string_mod , only : shr_string_lastindex
  use seq_comm_mct   , only : llogunit => logunit

  implicit none
  public

  integer, parameter :: CSS = 256  ! use longer short character
  integer, parameter :: CLL = 1024

  public :: shr_nuopc_fldList_Concat
  public :: shr_nuopc_fldList_Realize
  public :: shr_nuopc_fldList_AddFld
  public :: shr_nuopc_fldList_AddDomain
  public :: shr_nuopc_fldList_AddMetadata
  public :: shr_nuopc_fldList_AddMap
  public :: shr_nuopc_fldList_GetFldNames

  interface shr_nuopc_fldList_AddFld ; module procedure &
       shr_nuopc_fldList_AddFld_Src, &
       shr_nuopc_fldList_AddFld_Dst
  end interface shr_nuopc_fldList_AddFld

  !-----------------------------------------------
  ! Metadata array
  !-----------------------------------------------

  character(len=*), parameter :: undef     = 'undefined'
  integer         , parameter :: nmax      = 1000        ! maximum number of entries in metadta_entry
  integer                     :: n_entries = 0           ! actual number of entries in metadta_entry
  character(len=CSS)          :: shr_nuopc_fldList_Metadata(nmax,4) = undef

  !----------------------------------------------------------------------------
  ! scalar data
  !----------------------------------------------------------------------------

  character(len=*), parameter :: flds_scalar_name = "cpl_scalars"
  integer, parameter :: flds_scalar_index_nx                 = 1
  integer, parameter :: flds_scalar_index_ny                 = 2
  integer, parameter :: flds_scalar_index_precip_fact        = 3
  integer, parameter :: flds_scalar_index_nextsw_cday        = 4
  integer, parameter :: flds_scalar_index_dead_comps         = 5
  integer, parameter :: flds_scalar_index_rofice_present     = 6  ! does rof have iceberg coupling on
  integer, parameter :: flds_scalar_index_flood_present      = 7  ! does rof have flooding on
  integer, parameter :: flds_scalar_index_ocnrof_prognostic  = 8  ! does ocn need rof data
  integer, parameter :: flds_scalar_index_iceberg_prognostic = 9  ! does ice model support icebergs
  integer, parameter :: flds_scalar_index_glclnd_present     = 10 ! does glc have land coupling fields on
  integer, parameter :: flds_scalar_index_glcocn_present     = 11 ! does glc have ocean runoff on
  integer, parameter :: flds_scalar_index_glcice_present     = 12 ! does glc have iceberg coupling on
  integer, parameter :: flds_scalar_index_glc_valid_input    = 13 ! does glc have is valid accumulated data being sent to it?
                                                                  ! (only valid of glc_prognostic is .true.)
  integer, parameter :: flds_scalar_index_glc_coupled        = 14 ! does glc send fluxes to other components
                                                                  ! (only relevant if glc_present is .true.)
  integer, parameter :: flds_scalar_num                      = 14

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
     logical       :: merge_with_weights = .false.
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

  !----------------------------------------------------------------------------
  ! private data
  !----------------------------------------------------------------------------

  integer           :: dbrc
  character(len=CL) :: infostr

!================================================================================
contains
!================================================================================

  subroutine shr_nuopc_fldList_GetFldNames(flds, fldnames)
    type(shr_nuopc_fldList_src_entry_type), intent(in) :: flds(:)
    character(len=*), pointer :: fldnames(:)
    integer :: n

    do n = 1,size(flds)
       fldnames(n) = trim(flds(n)%shortname)
    end do
  end subroutine shr_nuopc_fldList_GetFldNames

  !================================================================================

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

  subroutine shr_nuopc_fldList_AddDomain(fldlist, fldname, longname, stdname, units)

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
       call shr_nuopc_fldList_AddMetadata(trim(fldname), longname, stdname, units)
    endif

  end subroutine shr_nuopc_fldList_AddDomain

  !===============================================================================
  
  subroutine shr_nuopc_fldList_AddMetadata(fldname , longname, stdname, units)

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
          shr_nuopc_fldList_metadata(n_entries,1) = fldname(j:i-1)
          shr_nuopc_fldList_metadata(n_entries,2) = trim(longname)
          shr_nuopc_fldList_metadata(n_entries,3) = trim(stdname )
          shr_nuopc_fldList_metadata(n_entries,4) = trim(units   )
          j=i+1
          i =  index(fldname(j:),':') + j - 1
       endif
    enddo
    if (get_metadata_entry(fldname(j:i)) <= 0) then
       n_entries = n_entries + 1
       i = len_trim(fldname)
       shr_nuopc_fldList_metadata(n_entries,1) = fldname(j:i)
       shr_nuopc_fldList_metadata(n_entries,2) = trim(longname)
       shr_nuopc_fldList_metadata(n_entries,3) = trim(stdname )
       shr_nuopc_fldList_metadata(n_entries,4) = trim(units   )
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
            if (trim(shortname) == trim(shr_nuopc_fldList_Metadata(i,1))) then
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
            if (trim(lshortname) == trim(shr_nuopc_fldList_Metadata(i,1))) then
               found   = .true.
               lnentry = i
            end if
            i = i + 1
         end do
      endif
      get_metadata_entry = lnentry
    end function get_metadata_entry

  end subroutine shr_nuopc_fldList_AddMetadata

  !================================================================================

  subroutine shr_nuopc_fldList_AddFld_Src(flds, stdname, shortname, fldindex)
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

  end subroutine shr_nuopc_fldList_AddFld_Src

  !================================================================================

  subroutine shr_nuopc_fldList_AddFld_Dst(flds, stdname, shortname, merge_with_weights)

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

  end subroutine shr_nuopc_fldList_AddFld_Dst

  !================================================================================

  subroutine shr_nuopc_fldList_AddMap(fld, srccomp, destcomp, mapindex, mapnorm, mapfile)
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
  end subroutine shr_nuopc_fldList_AddMap

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
