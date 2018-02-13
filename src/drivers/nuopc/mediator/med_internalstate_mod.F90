module med_internalstate_mod

  !-----------------------------------------------------------------------------
  ! Mediator Internal State Datatype.
  !-----------------------------------------------------------------------------

  use ESMF
  use shr_nuopc_fldList_mod, only: ncomps, nmappers, med_coupling_active

  implicit none

  public

  ! private internal state to keep instance data
  type InternalStateStruct

    ! NState_Imp and NState_Exp are the standard NUOPC coupling datatypes
    ! FBImp and FBExp are the internal mediator datatypes
    ! NState_Exp(n) = FBExp(n), copied in the connector prep phase
    ! FBImp(n,n) = NState_Imp(n), copied in connector post phase
    ! FBImp(n,k) is the FBImp(n,n) interpolated to grid k
    ! RH(n,k,m) is a RH from grid n to grid k, map type m

    logical               :: comp_present(ncomps)               ! comp present flag
    logical               :: med_coupling_active(ncomps,ncomps) ! computes the active coupling
    type(ESMF_State)      :: NStateImp(ncomps)                  ! Import data from various component, on their grid
    type(ESMF_State)      :: NStateExp(ncomps)                  ! Export data to various component, on their grid
    type(ESMF_FieldBundle):: FBImp(ncomps,ncomps)               ! Import data from various components interpolated to various grids
    type(ESMF_FieldBundle):: FBImpAccum(ncomps)                 ! Accumulator for various components import on their grid
    integer               :: FBImpAccumcnt(ncomps)              ! Accumulator counter for each FBImpAccum
    type(ESMF_FieldBundle):: FBExp(ncomps)                      ! Export data for various components, on their grid
    type(ESMF_FieldBundle):: FBExpAccum(ncomps)                 ! Accumulator for various components export on their grid
    integer               :: FBExpAccumcnt(ncomps)              ! Accumulator counter for each FBExpAccum
    type(ESMF_FieldBundle):: FBfrac(ncomps)                     ! Fraction data for various components, on their grid
    type(ESMF_FieldBundle):: FBNormOne(ncomps,ncomps,nmappers)  ! Unity static normalization
    type(ESMF_RouteHandle):: RH(ncomps,ncomps,nmappers)         ! Routehandles for pairs of components and different mappers
    type(ESMF_FieldBundle):: FBXao_ocnalb_o                     ! Ocn albedo on ocn grid
    type(ESMF_FieldBundle):: FBXao_ocnalb_a                     ! Ocn albedo on atm grid
    type(ESMF_FieldBundle):: FBXao_fluxes_a                     ! Ocn/Atm flux fields on atm grid
    type(ESMF_FieldBundle):: FBXao_fluxes_o                     ! Ocn/Atm flux fields on ocn grid
    type(ESMF_FieldBundle):: FBXao_fluxes_o_accum               ! Ocn/Atm flux accumulator on ocn grid
    type(ESMF_FieldBundle):: FBMed_l2x_to_glc_l_accum           ! Land->glc accumulator on lnd grid
    integer               :: conn_prep_cnt(ncomps)              ! Connector prep count
    integer               :: conn_post_cnt(ncomps)              ! Connector post count
    integer               :: mpicom
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------

end module med_internalstate_mod
