!================================================================================
module shr_nuopc_data_mod

#ifdef NUOPC_INTERFACE 

  use seq_comm_mct,     only : n_ATMID    => ATMID
  use seq_infodata_mod, only : n_infodata => seq_infodata_infodata
  use seq_timemgr_mod,  only : n_EClock_a => seq_timemgr_EClock_a

  implicit none
  public

!================================================================================
contains
!================================================================================

!================================================================================
#endif

end module shr_nuopc_data_mod
