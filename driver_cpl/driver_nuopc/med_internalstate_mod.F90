module med_internalstate_mod

  !-----------------------------------------------------------------------------
  ! Mediator Internal State Datatype.
  !-----------------------------------------------------------------------------

  use ESMF
  use shr_nuopc_fldList_mod, only: shr_nuopc_fldList_type

  implicit none
  
  public
  
  ! private internal state to keep instance data
  type InternalStateStruct
    integer               :: ocncntr
    integer               :: atmcntr
    integer               :: accumcntAtm ! accumulator counter
    integer               :: accumcntOcn ! accumulator counter
    integer               :: accumcntIce ! accumulator counter
    integer               :: accumcntLnd ! accumulator counter
    integer               :: accumcntRof ! accumulator counter
    integer               :: accumcntAtmOcn ! accumulator counter

    type(ESMF_State)     :: NState_AtmImp   ! Atm Import nested state
    type(ESMF_State)     :: NState_AtmExp   ! Atm Export nested state
    type(ESMF_State)     :: NState_OcnImp   ! Ocn Import nested state
    type(ESMF_State)     :: NState_OcnExp   ! Ocn Export nested state
    type(ESMF_State)     :: NState_IceImp   ! Ice Import nested state
    type(ESMF_State)     :: NState_IceExp   ! Ice Export nested state
    type(ESMF_State)     :: NState_LndImp   ! Lnd Import nested state
    type(ESMF_State)     :: NState_LndExp   ! Lnd Export nested state
    type(ESMF_State)     :: NState_RofImp   ! Rof Import nested state
    type(ESMF_State)     :: NState_RofExp   ! Rof Export nested state

    type(ESMF_FieldBundle):: FBaccumAtm  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBaccumOcn  ! accumulator of ocn export data
    type(ESMF_FieldBundle):: FBaccumIce  ! accumulator of ice export data
    type(ESMF_FieldBundle):: FBaccumLnd  ! accumulator of lnd export data
    type(ESMF_FieldBundle):: FBaccumRof  ! accumulator of lnd export data
    type(ESMF_FieldBundle):: FBaccumAtmOcn  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBAtm_a     ! Atm export data on atm grid
    type(ESMF_FieldBundle):: FBAtm_o     ! Atm export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBAtm_i     ! Atm export data mapped to ice grid
    type(ESMF_FieldBundle):: FBAtm_l     ! Atm export data mapped to lnd grid
    type(ESMF_FieldBundle):: FBAtm_h     ! Atm export data mapped to rof grid
    type(ESMF_FieldBundle):: FBOcn_a     ! Ocn export data mapped to atm grid
    type(ESMF_FieldBundle):: FBOcn_o     ! Ocn export data on ocn grid
    type(ESMF_FieldBundle):: FBOcn_i     ! Ocn export data mapped to ice grid
    type(ESMF_FieldBundle):: FBIce_a     ! Ice export data mapped to atm grid
    type(ESMF_FieldBundle):: FBIce_o     ! Ice export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBIce_i     ! Ice export data on ice grid
    type(ESMF_FieldBundle):: FBIce_if    ! Ice export data on ice grid multiplied by ice fraction
    type(ESMF_FieldBundle):: FBLnd_a     ! Lnd export data mapped to atm grid
    type(ESMF_FieldBundle):: FBLnd_l     ! Lnd export on lnd grid
    type(ESMF_FieldBundle):: FBLnd_h     ! Lnd export data mapped to rof grid
    type(ESMF_FieldBundle):: FBRof_l     ! Rof export data mapped to lnd grid
    type(ESMF_FieldBundle):: FBRof_a     ! Rof export data mapped to atm grid
    type(ESMF_FieldBundle):: FBRof_h     ! Rof export on rof grid
    type(ESMF_FieldBundle):: FBAtmOcn_o  ! Atm/Ocn flux fields on ocn grid
    type(ESMF_FieldBundle):: FBAtmOcn_a  ! Atm/Ocn flux fields on atm grid
    type(ESMF_FieldBundle):: FBforAtm    ! data storage for atm import
    type(ESMF_FieldBundle):: FBforOcn    ! data storage for ocn import
    type(ESMF_FieldBundle):: FBforIce    ! data storage for ice import
    type(ESMF_FieldBundle):: FBforLnd    ! data storage for lnd import
    type(ESMF_FieldBundle):: FBforRof    ! data storage for rof import

    type(ESMF_RouteHandle):: RH_a2o_bilnr  ! atm to ocn bilinear
    type(ESMF_RouteHandle):: RH_o2a_bilnr  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_bilnr  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_bilnr  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_bilnr  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_bilnr  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_bilnr  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_bilnr  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_bilnr  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_bilnr  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_bilnr  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_bilnr  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_consf  ! atm to ocn conservative fracarea
    type(ESMF_RouteHandle):: RH_o2a_consf  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consf  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consf  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_consf  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_consf  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_consf  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_consf  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_consf  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consf  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_consf  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_consf  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_consd  ! atm to ocn conservative dstarea
    type(ESMF_RouteHandle):: RH_o2a_consd  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consd  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consd  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_consd  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_consd  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_consd  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_consd  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_consd  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consd  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_consd  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_consd  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_patch  ! atm to ocn patch
    type(ESMF_RouteHandle):: RH_o2a_patch  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_patch  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_patch  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_patch  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_patch  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_patch  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_patch  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_patch  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_patch  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_patch  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_patch  ! rof to lnd
    type(ESMF_RouteHandle):: RH_a2o_fcopy  ! atm to ocn fcopy
    type(ESMF_RouteHandle):: RH_o2a_fcopy  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_fcopy  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_fcopy  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_fcopy  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_fcopy  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_fcopy  ! atm to rof
    type(ESMF_RouteHandle):: RH_h2a_fcopy  ! rof to atm
    type(ESMF_RouteHandle):: RH_o2i_fcopy  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_fcopy  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_fcopy  ! lnd to rof
    type(ESMF_RouteHandle):: RH_h2l_fcopy  ! rof to lnd

    logical               :: a2o_active
    logical               :: o2a_active
    logical               :: a2i_active
    logical               :: i2a_active
    logical               :: a2l_active
    logical               :: l2a_active
    logical               :: a2h_active
    logical               :: h2a_active
    logical               :: o2i_active
    logical               :: i2o_active
    logical               :: l2h_active
    logical               :: h2l_active
    integer               :: mpicom
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  type (shr_nuopc_fldList_Type) :: fldsToAtm
  type (shr_nuopc_fldList_Type) :: fldsFrAtm
  type (shr_nuopc_fldList_Type) :: fldsToOcn
  type (shr_nuopc_fldList_Type) :: fldsFrOcn
  type (shr_nuopc_fldList_Type) :: fldsToIce
  type (shr_nuopc_fldList_Type) :: fldsFrIce
  type (shr_nuopc_fldList_Type) :: fldsToLnd
  type (shr_nuopc_fldList_Type) :: fldsFrLnd
  type (shr_nuopc_fldList_Type) :: fldsToRof
  type (shr_nuopc_fldList_Type) :: fldsFrRof
  type (shr_nuopc_fldList_Type) :: fldsAtmOcn

  !-----------------------------------------------------------------------------

end module med_internalstate_mod

