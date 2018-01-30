!================================================================================
module shr_nuopc_fldList_mod

  use ESMF
  use NUOPC

  use shr_kind_mod       , only : r8 => shr_kind_r8
  use shr_kind_mod       , only : CL => SHR_KIND_CL, CX => SHR_KIND_CX, CS => SHR_KIND_CS
  use shr_sys_mod        , only : shr_sys_abort
  use shr_log_mod        , only : loglev  => shr_log_Level
  use shr_log_mod        , only : logunit => shr_log_Unit
  use shr_mpi_mod        , only : shr_mpi_bcast
  use shr_string_mod     , only : shr_string_listGetNum, shr_string_listGetName
  use shr_nuopc_flds_mod , only : flds_lookup, flds_scalar_name, flds_scalar_num, ncomps

  implicit none
  private

  public :: shr_nuopc_fldList_setDict_fromflds
  public :: shr_nuopc_fldList_fromflds
  public :: shr_nuopc_fldList_Type
  public :: shr_nuopc_fldList_Zero
  public :: shr_nuopc_fldList_Add
  public :: shr_nuopc_fldList_Advertise
  public :: shr_nuopc_fldList_Realize
  public :: shr_nuopc_fldList_SetScalarField

  private :: shr_nuopc_fldList_CopyScalarToState
  private :: shr_nuopc_fldList_CopyStateToScalar

  type shr_nuopc_fldList_Type
    character(CS) :: stdname
    character(CS) :: shortname
    character(CS) :: transferOffer  ! provide or accept
    character(CS) :: mapnames(ncomps) = 'unset'
    character(CS) :: mapnorms(ncomps) = 'unset'
    character(CX) :: mapfiles(ncomps) = 'unset'
  end type shr_nuopc_fldList_Type

  integer :: dbrc
  character(len=CL) :: infostr

!================================================================================
contains
!================================================================================

  subroutine shr_nuopc_fldList_setDict_fromflds(rc)
    ! ----------------------------------------------
    ! Build NUOPC dictionary from flds data
    ! ----------------------------------------------
    integer,          intent(inout)  :: rc

    ! local variables
    integer :: n, num
    character(CS) :: stdname
    character(CS) :: units
    character(len=*), parameter :: subname='(shr_nuopc_fldList_setDict_fromflds)'

    rc = ESMF_SUCCESS

    call shr_nuopc_flds_get_num_entries(num)

    do n = 1,num
       call shr_nuopc_flds_get_entry(n,shortname=stdname,units=units)
       if (.not.NUOPC_FieldDictionaryHasEntry(stdname)) then
          call ESMF_LogWrite(subname//': Add:'//trim(stdname), ESMF_LOGMSG_INFO, rc=rc)
          call NUOPC_FieldDictionaryAddEntry(standardName=stdname, canonicalUnits=units, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
    enddo

  end subroutine shr_nuopc_fldList_setDict_fromflds

!================================================================================

  ! subroutine shr_nuopc_fldList_fromflds(fldList, flds, transferOffer, tag, flds_maps, ncomps, rc)
  !   ! ----------------------------------------------
  !   ! Build fldlist from flds and flds_maps list
  !   ! ----------------------------------------------
  !   type(shr_nuopc_fldList_Type) , pointer              :: fldList(:)
  !   character(len=*)             , intent(in)           :: flds ! colon deliminted string of fields
  !   character(len=*)             , intent(in)           :: transferOffer
  !   character(len=*)             , intent(in)           :: tag
  !   character(len=*)             , intent(in), optional :: flds_list_maps
  !   integer                      , intent(in), optional :: ncomps
  !   integer                      , intent(inout)        :: rc

  !   ! local variables
  !   integer           :: n, num_flds
  !   character(len=CS) :: stdname
  !   character(len=CS) :: mapname
  !   character(len=*), parameter :: subname='(shr_nuopc_fldList_fromflds)'

  !   rc = ESMF_SUCCESS

  !   ! num is the number of colon deliminated entries in flds_list
  !   num_flds = shr_string_listGetNum(flds_list)

  !   do n = 1,num_flds
  !     call shr_string_listGetName(flds, n, stdname)
  !     call shr_nuopc_fldList_Add(fldlist, stdname, transferOffer, tag, mapname=mapname, rc=rc)

  !     ! Add mapping names fldlist data structure (currently this is only done by the mediator)
  !     if ((present(ncomps) .and. present(flds_maps))) then
  !        allocate(fldlist(num)%mapping(ncomps)
  !        call shr_string_listGetName(flds_list_maps, n, mapname)
  !        fldlist(num)%mappings(:) = mapname
  !     end if
  !   enddo

  ! end subroutine shr_nuopc_fldList_fromflds

!================================================================================

  subroutine shr_nuopc_fldList_Zero(fldlist, rc)
    ! ----------------------------------------------
    ! Zero out list of field information
    ! ----------------------------------------------
    type(shr_nuopc_fldList_Type) , pointer :: fldlist(:)
    integer, intent(inout)  :: rc

    ! local variables
    character(len=*), parameter :: subname='(shr_nuopc_fldList_Zero)'

    rc = ESMF_SUCCESS

    if (associated(fldlist)) deallocate(fldlist)

  end subroutine shr_nuopc_fldList_Zero

!================================================================================

  subroutine shr_nuopc_fldList_Add(fldlist, stdname, transferOffer, tag, shortname, index rc)
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

    type(shr_nuopc_fldList_Type) , intent(inout)         :: fldlist(:)
    integer                      , intent(out), optional :: index
    character(len=*)             , intent(in)            :: stdname
    character(len=*)             , intent(in), optional  :: transferOffer
    character(len=*)             , intent(in), optional  :: tag
    character(len=*)             , intent(in), optional  :: shortname
    integer                      , intent(inout)         :: rc

    ! local variables
    integer :: n,oldsize,id
    type(shr_nuopc_fldList_Type), pointer :: new_fldList(:)
    character(len=*), parameter :: subname='(shr_nuopc_fldList_Add)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (.not.associated(fldlist)) then
       call ESMF_LogWrite(trim(subname)//":"//trim(tag)//&
            " ERROR in fldlist, call shr_nuopc_fldList_Zero first "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
       rc = ESMF_FAILURE
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    end if

    oldsize = size(fldlist)
    id = oldsize + 1

    if (associated(fldlist)) then
       oldsize = size(fldlist)
    else
       oldsize = 0
    end if
    id = oldsize + 1

    ! 1) allocate new_fldList to be size (one element larger than this%state)
    allocate(new_fldList(id))

    ! 2) copy fldList into first N-1 elements of new_state
    do n = 1,oldsize
       new_fldlist(n)%stdname        = fldlist(n)%stdname
       new_fldlist(n)%mapname        = fldlist(n)%mapname
       new_fldlist(n)%transfer_offer = fldlist(n)%transfer_offer
       new_fldlist(n)%tag            = fldlist(n)%tag
       new_fldlist(n)%shortname      = fldlist(n)%shortname
    end do

    ! 3) deallocate / nullify fldlist
    if (oldsize >  0) then
       deallocate(fldlist)
       nullify(fldlist)
    end if

    ! 4) point fldlist => new_fldlist
    fldlist => new_fldlist

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
    fldlist(id)%stdname = trim(stdname)
    fldlist(id)%shortname = trim(shortname)
    fldlist(id)%transferOffer(num)= trim(transferOffer)
    fldlist(id)%tag(num)= trim(tag)

    if (present(index)) then
       index = id
    end if

  end subroutine shr_nuopc_fldList_Add

!================================================================================

  subroutine shr_nuopc_fldList_Advertise(state, fldlist, tag, rc)

    type(ESMF_State), intent(inout)       :: state
    type(shr_nuopc_fldList_Type), pointer :: fldlist(:)
    character(len=*), intent(in)          :: tag
    integer,          intent(inout)       :: rc

    ! local variables
    integer       :: n
    character(CS) :: units
    character(ESMF_MAXSTR), pointer :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer :: ConnectedList(:)
    character(ESMF_MAXSTR), pointer :: NameSpaceList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    character(len=*),parameter  :: subname='(shr_nuopc_fldList_Advertise)'

    rc = ESMF_SUCCESS

    do n = 1, size(fldlist)

      call ESMF_LogWrite(subname//':'//trim(tag)//':'//trim(fldlist(n)%stdname), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! adding to dictionary here not a good idea because will only be added on this PET.  needs
      ! to be added to all PETs at the driver level.
      !      if (.not.NUOPC_FieldDictionaryHasEntry(fldlist(n)%stdname)) then
      !        call ESMF_LogWrite(subname//':DictionaryAddEntry:'//trim(tag)//':'//trim(fldlist(n)%stdname)//':'//trim(units), &
      !            ESMF_LOGMSG_INFO, rc=rc)
      !        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      !        call flds_lookup(fldlist(n)%stdname(n), units=units)
      !        call NUOPC_FieldDictionaryAddEntry(standardName=fldlist(n)%stdname(n), canonicalUnits=units, rc=rc)
      !        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      !        if (.not.NUOPC_FieldDictionaryHasEntry(fldlist(n)%shortname)) then
      !          call ESMF_LogWrite(subname//':DictionaryAddEntry:'//trim(tag)//':'//trim(fldlist(n)%shortname)//':'//trim(units), &
      !             ESMF_LOGMSG_INFO, rc=rc)
      !          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      !          call NUOPC_FieldDictionaryAddEntry(standardName=fldlist(n)%shortname(n), canonicalUnits=units, rc=rc)
      !          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      !!          if (fldlist(n)%stdname(n) /= fldlist(n)%shortname) then
      !!            call NUOPC_FieldDictionarySetSyno( (/fldlist(n)%stdname(n), fldlist(n)%shortname(n)/), rc=rc)
      !!            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      !!          endif
      !        endif
      !      endif

      call NUOPC_Advertise(state, &
        standardName=trim(fldlist(n)%stdname), &
        shortname=trim(fldlist(n)%shortname), &
        name=trim(fldlist(n)%shortname), &
        TransferOfferGeomObject=fldlist(n)%transferOffer, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    enddo

#if (1 == 0)
    if (fldlist(n)%num > 0) then
      nullify(StandardNameList)
      nullify(ConnectedList)
      nullify(NameSpaceList)
      nullify(ItemNameList)

      call NUOPC_GetStateMemberLists(state, StandardNameList=StandardNameList, ConnectedList=ConnectedList, &
        NamespaceList=NamespaceList, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do n = 1,size(StandardNameList)
        call ESMF_LogWrite(trim(subname)//trim(tag)//" StandardNameList = "//trim(StandardNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      enddo
      do n = 1,size(ConnectedList)
        call ESMF_LogWrite(trim(subname)//trim(tag)//" ConnectedList = "//trim(ConnectedList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      enddo
      do n = 1,size(NamespaceList)
        call ESMF_LogWrite(trim(subname)//trim(tag)//" NamespaceList = "//trim(NamespaceList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      enddo
      do n = 1,size(ItemnameList)
        call ESMF_LogWrite(trim(subname)//trim(tag)//" ItemnameList = "//trim(ItemnameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      enddo
    endif
#endif

  end subroutine shr_nuopc_fldList_Advertise

!================================================================================

  subroutine shr_nuopc_fldList_Realize(state, grid, mesh, fldlist, tag, rc)

    type(ESMF_State), intent(inout)             :: state
    type(ESMF_Grid), intent(in),optional        :: grid
    type(ESMF_Mesh), intent(in),optional        :: mesh
    type(shr_nuopc_fldList_Type), pointer       :: fldlist(:)
    character(len=*), intent(in)                :: tag
    integer, intent(inout)                      :: rc

    ! local variables
    integer           :: n
    integer           :: itemCount
    type(ESMF_Field)  :: field
    character(ESMF_MAXSTR)      :: transferAction
    character(ESMF_MAXSTR), pointer :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer :: ConnectedList(:)
    character(ESMF_MAXSTR), pointer :: NameSpaceList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    character(len=*),parameter  :: subname='(shr_nuopc_fldList_Realize)'

    rc = ESMF_SUCCESS

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

    do n = 1, size(fldlist)

      !      call ESMF_LogWrite(subname//' fld = '//trim(fldlist(n)%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)

      if (NUOPC_IsConnected(state, fieldName=fldlist(n)%shortname) then

        call ESMF_StateGet(state, field=field, itemName=fldlist(n)%shortname(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call NUOPC_GetAttribute(field, name="TransferActionGeomObject", value=transferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if (trim(transferAction) == "accept") then
           call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(fldlist(n)%shortname)//" is connected, grid/mesh TBD", &
                ESMF_LOGMSG_INFO, rc=dbrc)

        else   ! provide

          if (fldlist(n)%shortname(n) == trim(flds_scalar_name)) then
            call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(fldlist(n)%shortname)//" is connected on root pe", &
              ESMF_LOGMSG_INFO, rc=dbrc)
            call shr_nuopc_fldList_SetScalarField(field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          elseif (present(grid)) then
            call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(fldlist(n)%shortname)//" is connected using grid", &
              ESMF_LOGMSG_INFO, rc=dbrc)
            field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fldlist(n)%shortname(n),rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          elseif (present(mesh)) then
            call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(fldlist(n)%shortname)//" is connected using mesh", &
              ESMF_LOGMSG_INFO, rc=dbrc)
            field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=fldlist(n)%shortname(n),meshloc=ESMF_MESHLOC_ELEMENT,rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          else
             call ESMF_LogWrite(trim(subname)//trim(tag)//": ERROR grid or mesh expected", &
                  ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif

          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

!          call ESMF_FieldPrint(field=field, rc=rc)
!          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        endif

      else

        call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(fldlist(n)%shortname) // " is not connected.", &
          ESMF_LOGMSG_INFO, rc=dbrc)
        call ESMF_StateRemove(state, (/fldlist(n)%shortname(n)/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      endif

    enddo

    call ESMF_LogWrite(subname//' done ', ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine shr_nuopc_fldList_Realize

!================================================================================

  subroutine shr_nuopc_fldList_SetScalarField(field, rc)
    ! ----------------------------------------------
    ! create a field with scalar data on the root pe
    ! ----------------------------------------------
    type(ESMF_Field), intent(inout)  :: field
    integer,          intent(inout)  :: rc

    ! local variables
    type(ESMF_Distgrid) :: distgrid
    type(ESMF_Grid)     :: grid
    character(len=*), parameter :: subname='(shr_nuopc_fldList_SetScalarField)'

    rc = ESMF_SUCCESS

    ! create a DistGrid with a single index space element, which gets mapped
    ! onto DE 0.

    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    grid = ESMF_GridCreate(distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    field = ESMF_FieldCreate(name=trim(flds_scalar_name), &
         grid=grid, &
         typekind=ESMF_TYPEKIND_R8, &
         ungriddedLBound=(/1/), ungriddedUBound=(/flds_scalar_num/), & ! num of scalar values
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

  end subroutine shr_nuopc_fldList_SetScalarField

!================================================================================

  subroutine shr_nuopc_fldList_CopyStateToScalar(State, data, mpicom, rc)
    ! ----------------------------------------------
    ! Copy scalar data from State to local data on root then broadcast
    ! ----------------------------------------------
    type(ESMF_State), intent(in)     :: State
    real(r8),         intent(inout)  :: data(:)
    integer,          intent(in)     :: mpicom
    integer,          intent(inout)  :: rc

    ! local variables
    integer :: mytask
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8), pointer :: farrayptr(:)
    character(len=*), parameter :: subname='(shr_nuopc_fldList_CopyStateToScalar)'

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, mytask, rc)
    call ESMF_StateGet(State, itemName=trim(flds_scalar_name), field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (mytask == 0) then
      call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (size(data) < flds_scalar_num .or. size(farrayptr) < flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR on data size", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
      data(1:flds_scalar_num) = farrayptr(1:flds_scalar_num)
    endif
    call shr_mpi_bcast(data, mpicom)

  end subroutine shr_nuopc_fldList_CopyStateToScalar

!================================================================================

  subroutine shr_nuopc_fldList_CopyScalarToState(data, State, mpicom, rc)
    ! ----------------------------------------------
    ! Copy local scalar data into State, root only
    ! ----------------------------------------------
    real(r8),         intent(in)     :: data(:)
    type(ESMF_State), intent(inout)  :: State
    integer,          intent(in)     :: mpicom
    integer,          intent(inout)  :: rc

    ! local variables
    integer :: mytask
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8), pointer :: farrayptr(:)
    character(len=*), parameter :: subname='(shr_nuopc_fldList_CopyScalarToState)'

    rc = ESMF_SUCCESS

    call MPI_COMM_RANK(mpicom, mytask, rc)
    call ESMF_StateGet(State, itemName=trim(flds_scalar_name), field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (mytask == 0) then
      call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (size(data) < flds_scalar_num .or. size(farrayptr) < flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR on data size", ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
      farrayptr(1:flds_scalar_num) = data(1:flds_scalar_num)
    endif

  end subroutine shr_nuopc_fldList_CopyScalarToState

!================================================================================

end module shr_nuopc_fldList_mod
