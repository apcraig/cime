module cesm_comp_mod

  !-------------------------------------------------------------------------------
  !
  ! Purpose: Main program for NCAR CESM4/cpl7. Can have different
  !          land, sea-ice, and ocean models plugged in at compile-time.
  !          These models can be either: stub, dead, data, or active
  !          components or some combination of the above.
  !
  !               stub -------- Do nothing.
  !               dead -------- Send analytic data back.
  !               data -------- Send data back interpolated from input files.
  !               prognostic -- Prognostically simulate the given component.
  !
  ! Method: Call appropriate initialization, run (time-stepping), and
  !         finalization routines.
  !
  !-------------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  ! share code & libs
  !----------------------------------------------------------------------------
  use shr_kind_mod,      only: r8 => SHR_KIND_R8
  use shr_kind_mod,      only: cs => SHR_KIND_CS
  use shr_kind_mod,      only: cl => SHR_KIND_CL
  use shr_sys_mod,       only: shr_sys_abort, shr_sys_flush
  use shr_const_mod,     only: shr_const_cday
  use shr_file_mod,      only: shr_file_setLogLevel, shr_file_setLogUnit
  use shr_file_mod,      only: shr_file_setIO, shr_file_getUnit
  use shr_scam_mod,      only: shr_scam_checkSurface
  use shr_map_mod,       only: shr_map_setDopole
  use shr_mpi_mod,       only: shr_mpi_min, shr_mpi_max
  use shr_mem_mod,       only: shr_mem_init, shr_mem_getusage
  use shr_cal_mod,       only: shr_cal_date2ymd, shr_cal_ymd2date, shr_cal_advdateInt
  use shr_orb_mod,       only: shr_orb_params
  use shr_frz_mod,       only: shr_frz_freezetemp_init
  use shr_reprosum_mod,  only: shr_reprosum_setopts
  use mct_mod            ! mct_ wrappers for mct lib
  use perf_mod
  use ESMF

  !----------------------------------------------------------------------------
  ! cpl7 modules
  !----------------------------------------------------------------------------

  ! mpi comm data & routines, plus logunit and loglevel
  use seq_comm_mct, only: CPLID, GLOID, logunit, loglevel
  use seq_comm_mct, only: ATMID, LNDID, OCNID, ICEID, GLCID, ROFID, WAVID, ESPID
  use seq_comm_mct, only: ALLATMID,ALLLNDID,ALLOCNID,ALLICEID,ALLGLCID,ALLROFID,ALLWAVID,ALLESPID
  use seq_comm_mct, only: CPLALLATMID,CPLALLLNDID,CPLALLOCNID,CPLALLICEID
  use seq_comm_mct, only: CPLALLGLCID,CPLALLROFID,CPLALLWAVID,CPLALLESPID
  use seq_comm_mct, only: CPLATMID,CPLLNDID,CPLOCNID,CPLICEID,CPLGLCID,CPLROFID,CPLWAVID,CPLESPID
  use seq_comm_mct, only: num_inst_atm, num_inst_lnd, num_inst_rof
  use seq_comm_mct, only: num_inst_ocn, num_inst_ice, num_inst_glc
  use seq_comm_mct, only: num_inst_wav, num_inst_esp
  use seq_comm_mct, only: num_inst_xao, num_inst_frc, num_inst_phys
  use seq_comm_mct, only: num_inst_total, num_inst_max
  use seq_comm_mct, only: seq_comm_iamin, seq_comm_name, seq_comm_namelen
  use seq_comm_mct, only: seq_comm_init, seq_comm_setnthreads, seq_comm_getnthreads
  use seq_comm_mct, only: seq_comm_getinfo => seq_comm_setptrs
  use seq_comm_mct, only: seq_comm_petlist

  ! clock & alarm routines and variables
  use seq_timemgr_mod, only: seq_timemgr_type
  use seq_timemgr_mod, only: seq_timemgr_clockInit
  use seq_timemgr_mod, only: seq_timemgr_clockPrint
  use seq_timemgr_mod, only: seq_timemgr_EClockGetData
  use seq_timemgr_mod, only: seq_timemgr_histavg_type
  use seq_timemgr_mod, only: seq_timemgr_type_never
  use seq_timemgr_mod, only: seq_SyncClock => seq_timemgr_SyncClock
  use seq_timemgr_mod, only: EClock_d => seq_timemgr_Eclock_d
  use seq_timemgr_mod, only: EClock_a => seq_timemgr_Eclock_a
  use seq_timemgr_mod, only: EClock_l => seq_timemgr_Eclock_l
  use seq_timemgr_mod, only: EClock_o => seq_timemgr_Eclock_o
  use seq_timemgr_mod, only: EClock_i => seq_timemgr_Eclock_i
  use seq_timemgr_mod, only: EClock_g => seq_timemgr_Eclock_g
  use seq_timemgr_mod, only: EClock_r => seq_timemgr_Eclock_r
  use seq_timemgr_mod, only: EClock_w => seq_timemgr_Eclock_w
  use seq_timemgr_mod, only: EClock_e => seq_timemgr_Eclock_e

  ! "infodata" gathers various control flags into one datatype
  use seq_infodata_mod, only: seq_infodata_putData, seq_infodata_GetData
  use seq_infodata_mod, only: seq_infodata_init, seq_infodata_exchange
  use seq_infodata_mod, only: seq_infodata_type, seq_infodata_orb_variable_year
  use seq_infodata_mod, only: seq_infodata_print, seq_infodata_init2
  use seq_infodata_mod, only: infodata=>seq_infodata_infodata

  ! i/o subroutines
  use seq_io_mod, only : seq_io_cpl_init

  ! rearrange type routines
  use cplcomp_exchange_mod, only: seq_mctext_decomp

  ! list of fields transferred between components
  use seq_flds_mod, only : seq_flds_set

  ! --- timing routines ---
  use t_drv_timers_mod

  implicit none

  private

  public cesm_pre_init1, cesm_pre_init2
  public timing_dir, mpicom_GLOID

#include <mpif.h>

  !----------------------------------------------------------------------------
  ! temporary variables
  !----------------------------------------------------------------------------

  character(len=CL) :: suffix
  logical           :: iamin_id
  logical           :: iamroot_id
  integer           :: mpicom
  character(len=seq_comm_namelen) :: compname

  !----------------------------------------------------------------------------
  ! domains & related
  !----------------------------------------------------------------------------

  !--- domain equivalent 2d grid size ---
  integer  :: atm_nx, atm_ny  ! nx, ny of 2d grid, if known
  integer  :: lnd_nx, lnd_ny
  integer  :: ice_nx, ice_ny
  integer  :: ocn_nx, ocn_ny
  integer  :: rof_nx, rof_ny
  integer  :: glc_nx, glc_ny
  integer  :: wav_nx, wav_ny

  !----------------------------------------------------------------------------
  ! Infodata: inter-model control flags, domain info
  !----------------------------------------------------------------------------

  ! tcraig moved to seq_infodata_mod for NUOPC use temporarily
  !   type (seq_infodata_type), target :: infodata ! single instance for cpl and all comps

  !----------------------------------------------------------------------------
  ! time management
  !----------------------------------------------------------------------------

  real(r8) :: days_per_year = 365.0  ! days per year

  integer  :: dtime                  ! dt of one coupling interval
  integer  :: ncpl                   ! number of coupling intervals per day
  integer  :: ymd                    ! Current date (YYYYMMDD)
  integer  :: year                   ! Current date (YYYY)
  integer  :: month                  ! Current date (MM)
  integer  :: day                    ! Current date (DD)
  integer  :: tod                    ! Current time of day (seconds)
  integer  :: ymdtmp                 ! temporary date (YYYYMMDD)
  integer  :: todtmp                 ! temporary time of day (seconds)
  character(CL) :: orb_mode          ! orbital mode
  character(CS) :: tfreeze_option    ! Freezing point calculation
  integer  :: orb_iyear              ! orbital year
  integer  :: orb_iyear_align        ! associated with model year
  integer  :: orb_cyear              ! orbital year for current orbital computation
  integer  :: orb_nyear              ! orbital year associated with currrent model year
  real(r8) :: orb_eccen              ! orbital eccentricity
  real(r8) :: orb_obliq              ! obliquity in degrees
  real(r8) :: orb_mvelp              ! moving vernal equinox long
  real(r8) :: orb_obliqr             ! Earths obliquity in rad
  real(r8) :: orb_lambm0             ! Mean long of perihelion at vernal equinox (radians)
  real(r8) :: orb_mvelpp             ! moving vernal equinox long
  real(r8) :: wall_time_limit        ! wall time limit in hours
  real(r8) :: wall_time              ! current wall time used
  character(CS) :: force_stop_at     ! force stop at next (month, day, etc)
  logical  :: force_stop             ! force the model to stop
  integer  :: force_stop_ymd         ! force stop ymd
  integer  :: force_stop_tod         ! force stop tod

  !--- for documenting speed of the model ---
  character( 8) :: dstr              ! date string
  character(10) :: tstr              ! time string
  integer       :: begStep, endStep  ! Begining and ending step number
  character(CL) :: calendar          ! calendar name
  real(r8)      :: simDays           ! Number of simulated days
  real(r8)      :: SYPD              ! Simulated years per day
  real(r8)      :: Time_begin        ! Start time
  real(r8)      :: Time_end          ! Ending time
  real(r8)      :: Time_bstep        ! Start time
  real(r8)      :: Time_estep        ! Ending time
  real(r8)      :: time_brun         ! Start time
  real(r8)      :: time_erun         ! Ending time
  real(r8)      :: cktime            ! delta time
  real(r8)      :: cktime_acc(10)    ! cktime accumulator array 1 = all, 2 = atm, etc
  integer       :: cktime_cnt(10)    ! cktime counter array
  real(r8)      :: max_cplstep_time
  character(CL) :: timing_file       ! Local path to tprof filename
  character(CL) :: timing_dir        ! timing directory
  character(CL) :: tchkpt_dir        ! timing checkpoint directory

  !----------------------------------------------------------------------------
  ! control flags
  !----------------------------------------------------------------------------

  logical  :: atm_present            ! .true.  => atm is present
  logical  :: lnd_present            ! .true.  => land is present
  logical  :: ice_present            ! .true.  => ice is present
  logical  :: ocn_present            ! .true.  => ocn is present
  logical  :: glc_present            ! .true.  => glc is present
  logical  :: glclnd_present         ! .true.  => glc is computing land coupling
  logical  :: glcocn_present         ! .true.  => glc is computing ocean runoff
  logical  :: glcice_present         ! .true.  => glc is computing icebergs
  logical  :: rofice_present         ! .true.  => rof is computing icebergs
  logical  :: rof_present            ! .true.  => rof is present
  logical  :: flood_present          ! .true.  => rof is computing flood
  logical  :: wav_present            ! .true.  => wav is present
  logical  :: esp_present            ! .true.  => esp is present

  logical  :: atm_prognostic         ! .true.  => atm comp expects input
  logical  :: lnd_prognostic         ! .true.  => lnd comp expects input
  logical  :: ice_prognostic         ! .true.  => ice comp expects input
  logical  :: iceberg_prognostic     ! .true.  => ice comp can handle iceberg input
  logical  :: ocn_prognostic         ! .true.  => ocn comp expects input
  logical  :: ocnrof_prognostic      ! .true.  => ocn comp expects runoff input
  logical  :: glc_prognostic         ! .true.  => glc comp expects input
  logical  :: rof_prognostic         ! .true.  => rof comp expects input
  logical  :: wav_prognostic         ! .true.  => wav comp expects input
  logical  :: esp_prognostic         ! .true.  => esp comp expects input

  logical  :: areafact_samegrid      ! areafact samegrid flag
  logical  :: single_column          ! scm mode logical
  real(r8) :: scmlon                 ! single column lon
  real(r8) :: scmlat                 ! single column lat
  logical  :: aqua_planet            ! aqua planet mode
  real(r8) :: nextsw_cday            ! radiation control
  logical  :: atm_aero               ! atm provides aerosol data

  character(CL) :: cpl_seq_option    ! coupler sequencing option
  logical  :: skip_ocean_run         ! skip the ocean model first pass
  logical  :: cpl2ocn_first          ! use to call initial cpl2ocn timer
  logical  :: run_barriers           ! barrier the component run calls

  character(CS) :: aoflux_grid       ! grid for a/o flux calc: atm xor ocn
  character(CS) :: vect_map          ! vector mapping type

  character(CL) :: atm_gnam          ! atm grid
  character(CL) :: lnd_gnam          ! lnd grid
  character(CL) :: ocn_gnam          ! ocn grid
  character(CL) :: ice_gnam          ! ice grid
  character(CL) :: rof_gnam          ! rof grid
  character(CL) :: glc_gnam          ! glc grid
  character(CL) :: wav_gnam          ! wav grid

  logical       :: read_restart      ! local read restart flag
  character(CL) :: rest_file         ! restart file path + filename

  logical  :: shr_map_dopole         ! logical for dopole in shr_map_mod
  logical  :: domain_check           ! .true.  => check consistency of domains
  logical  :: reprosum_use_ddpdd     ! setup reprosum, use ddpdd
  real(r8) :: reprosum_diffmax       ! setup reprosum, set rel_diff_max
  logical  :: reprosum_recompute     ! setup reprosum, recompute if tolerance exceeded

  logical  :: output_perf = .false.  ! require timing data output for this pe

  !--- history & budgets ---
  logical :: do_budgets              ! heat/water budgets on
  logical :: do_histinit             ! initial hist file
  logical :: do_hist_r2x             ! create aux files: r2x
  logical :: do_hist_l2x             ! create aux files: l2x
  logical :: do_hist_a2x24hr         ! create aux files: a2x
  logical :: do_hist_l2x1yr          ! create aux files: l2x
  logical :: do_hist_a2x             ! create aux files: a2x
  logical :: do_hist_a2x3hrp         ! create aux files: a2x 3hr precip
  logical :: do_hist_a2x3hr          ! create aux files: a2x 3hr states
  logical :: do_hist_a2x1hri         ! create aux files: a2x 1hr instantaneous
  logical :: do_hist_a2x1hr          ! create aux files: a2x 1hr
  integer :: budget_inst             ! instantaneous budget flag
  integer :: budget_daily            ! daily budget flag
  integer :: budget_month            ! monthly budget flag
  integer :: budget_ann              ! annual budget flag
  integer :: budget_ltann            ! long term budget flag for end of year writing
  integer :: budget_ltend            ! long term budget flag for end of run writing

  character(CL) :: hist_a2x_flds     = &
       'Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf'

  character(CL) :: hist_a2x3hrp_flds = &
       'Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl'

  character(CL) :: hist_a2x24hr_flds = &
       'Faxa_bcphiwet:Faxa_bcphodry:Faxa_bcphidry:Faxa_ocphiwet:Faxa_ocphidry:&
       &Faxa_ocphodry:Faxa_dstwet1:Faxa_dstdry1:Faxa_dstwet2:Faxa_dstdry2:Faxa_dstwet3:&
       &Faxa_dstdry3:Faxa_dstwet4:Faxa_dstdry4:Sa_co2prog:Sa_co2diag'

  character(CL) :: hist_a2x1hri_flds = &
       'Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf'

  character(CL) :: hist_a2x1hr_flds  = &
       'Sa_u:Sa_v'

  character(CL) :: hist_a2x3hr_flds  = &
       'Sa_z:Sa_topo:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_dens:Sa_pbot:Sa_pslv:Faxa_lwdn:&
       &Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl:&
       &Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf:&
       &Sa_co2diag:Sa_co2prog'

  ! --- other ---
  integer  :: ka,km,k1,k2,k3         ! aVect field indices
  integer  :: ocnrun_count           ! number of times ocn run alarm went on
  logical  :: exists                 ! true if file exists
  integer  :: ierr                   ! MPI error return
  integer  :: rc                     ! return code
  logical  :: cdf64                  ! true => use 64 bit addressing in netCDF files

  character(*), parameter :: NLFileName = "drv_in"  ! input namelist filename

  integer  :: info_debug = 0         ! local info_debug level

  !----------------------------------------------------------------------------
  ! memory monitoring
  !----------------------------------------------------------------------------
  real(r8) :: msize,msize0,msize1     ! memory size (high water)
  real(r8) :: mrss ,mrss0 ,mrss1      ! resident size (current memory use)

  !----------------------------------------------------------------------------
  ! threading control
  !----------------------------------------------------------------------------
  integer  :: nthreads_GLOID         ! OMP global number of threads
  integer  :: nthreads_CPLID         ! OMP cpl number of threads
  integer  :: nthreads_ATMID         ! OMP atm number of threads
  integer  :: nthreads_LNDID         ! OMP lnd number of threads
  integer  :: nthreads_ICEID         ! OMP ice number of threads
  integer  :: nthreads_OCNID         ! OMP ocn number of threads
  integer  :: nthreads_GLCID         ! OMP glc number of threads
  integer  :: nthreads_ROFID         ! OMP glc number of threads
  integer  :: nthreads_WAVID         ! OMP wav number of threads
  integer  :: nthreads_ESPID         ! OMP esp number of threads

  integer  :: pethreads_GLOID        ! OMP number of threads per task

  logical  :: drv_threading          ! driver threading control

  !----------------------------------------------------------------------------
  ! communicator groups and related
  !----------------------------------------------------------------------------
  integer  :: Global_Comm

  integer  :: mpicom_GLOID          ! MPI global communicator
  integer  :: mpicom_CPLID          ! MPI cpl communicator
  integer  :: mpicom_OCNID          ! MPI ocn communicator for ensemble member 1

  integer  :: mpicom_CPLALLATMID    ! MPI comm for CPLALLATMID
  integer  :: mpicom_CPLALLLNDID    ! MPI comm for CPLALLLNDID
  integer  :: mpicom_CPLALLICEID    ! MPI comm for CPLALLICEID
  integer  :: mpicom_CPLALLOCNID    ! MPI comm for CPLALLOCNID
  integer  :: mpicom_CPLALLGLCID    ! MPI comm for CPLALLGLCID
  integer  :: mpicom_CPLALLROFID    ! MPI comm for CPLALLROFID
  integer  :: mpicom_CPLALLWAVID    ! MPI comm for CPLALLWAVID

  integer  :: iam_GLOID             ! pe number in global id
  logical  :: iamin_CPLID           ! pe associated with CPLID
  logical  :: iamroot_GLOID         ! GLOID masterproc
  logical  :: iamroot_CPLID         ! CPLID masterproc

  logical  :: iamin_CPLALLATMID     ! pe associated with CPLALLATMID
  logical  :: iamin_CPLALLLNDID     ! pe associated with CPLALLLNDID
  logical  :: iamin_CPLALLICEID     ! pe associated with CPLALLICEID
  logical  :: iamin_CPLALLOCNID     ! pe associated with CPLALLOCNID
  logical  :: iamin_CPLALLGLCID     ! pe associated with CPLALLGLCID
  logical  :: iamin_CPLALLROFID     ! pe associated with CPLALLROFID
  logical  :: iamin_CPLALLWAVID     ! pe associated with CPLALLWAVID

  !----------------------------------------------------------------------------
  ! complist: list of comps on this pe
  !----------------------------------------------------------------------------

  ! allow enough room for names of all physical components + coupler,
  ! where each string can be up to (max_inst_name_len+1) characters
  ! long (+1 allows for a space before each name)
  character(len=(seq_comm_namelen+1)*(num_inst_phys+1)) :: complist

  !----------------------------------------------------------------------------
  ! comp_num_<comp>: unique component number for each component type
  !----------------------------------------------------------------------------
  integer, parameter :: comp_num_atm = 1
  integer, parameter :: comp_num_lnd = 2
  integer, parameter :: comp_num_ice = 3
  integer, parameter :: comp_num_ocn = 4
  integer, parameter :: comp_num_glc = 5
  integer, parameter :: comp_num_rof = 6
  integer, parameter :: comp_num_wav = 7
  integer, parameter :: comp_num_esp = 8

  !----------------------------------------------------------------------------
  ! misc
  !----------------------------------------------------------------------------

  integer, parameter :: ens1=1         ! use first instance of ensemble only
  integer, parameter :: fix1=1         ! temporary hard-coding to first ensemble, needs to be fixed
  integer :: eai, eli, eoi, eii, egi, eri, ewi, eei, exi, efi  ! component instance counters

  !----------------------------------------------------------------------------
  ! formats
  !----------------------------------------------------------------------------
  character(*), parameter :: subname = '(seq_mct_drv)'
  character(*), parameter :: F00 = "('"//subname//" : ', 4A )"
  character(*), parameter :: F0L = "('"//subname//" : ', A, L6 )"
  character(*), parameter :: F0I = "('"//subname//" : ', A, 2i8 )"
  character(*), parameter :: F01 = "('"//subname//" : ', A, 2i8, 3x, A )"
  character(*), parameter :: F0R = "('"//subname//" : ', A, 2g23.15 )"
  character(*), parameter :: FormatA = '(A,": =============== ", A41,          " ===============")'
  character(*), parameter :: FormatD = '(A,": =============== ", A20,2I8,5x,   " ===============")'
  character(*), parameter :: FormatR = '(A,": =============== ", A31,F9.3,1x,  " ===============")'
  character(*), parameter :: FormatQ = '(A,": =============== ", A20,2F10.2,1x," ===============")'
  !===============================================================================
contains
  !===============================================================================

  !===============================================================================
  !*******************************************************************************
  !===============================================================================

  subroutine cesm_pre_init1()
    use shr_pio_mod, only : shr_pio_init1, shr_pio_init2

    !----------------------------------------------------------
    !| Initialize MCT and MPI communicators and IO
    !----------------------------------------------------------

    integer, dimension(num_inst_total) :: comp_id, comp_comm, comp_comm_iam
    logical :: comp_iamin(num_inst_total)
    logical :: flag
    character(len=seq_comm_namelen) :: comp_name(num_inst_total)
    integer :: i, it

    call mpi_initialized(flag,ierr)
    call shr_mpi_chkerr(ierr,subname//' mpi_initialized')
    if (.not. flag) then
       call mpi_init(ierr)
       call shr_mpi_chkerr(ierr,subname//' mpi_init')
    endif

    Global_Comm=MPI_COMM_WORLD
    comp_comm = MPI_COMM_NULL
    time_brun = mpi_wtime()

    call shr_pio_init1(num_inst_total,NLFileName, Global_Comm)
    !
    ! If pio_async_interface is true Global_Comm is MPI_COMM_NULL on the servernodes
    ! and server nodes do not return from shr_pio_init2
    !
    !   if (Global_Comm /= MPI_COMM_NULL) then

    call seq_comm_init(Global_Comm, NLFileName)

    !--- set task based threading counts ---
    call seq_comm_getinfo(GLOID,pethreads=pethreads_GLOID,iam=iam_GLOID)
    call seq_comm_setnthreads(pethreads_GLOID)

    !--- get some general data ---
    it=1
    call seq_comm_getinfo(GLOID,mpicom=mpicom_GLOID,&
         iamroot=iamroot_GLOID,nthreads=nthreads_GLOID)
    if (iamroot_GLOID) output_perf = .true.

    call seq_comm_getinfo(CPLID,mpicom=mpicom_CPLID,&
         iamroot=iamroot_CPLID,nthreads=nthreads_CPLID,&
         iam=comp_comm_iam(it))
    if (iamroot_CPLID) output_perf = .true.

    if (iamin_CPLID) complist = trim(complist)//' cpl'

    comp_id(it)    = CPLID
    comp_comm(it)  = mpicom_CPLID
    iamin_CPLID    = seq_comm_iamin(CPLID)
    comp_iamin(it) = seq_comm_iamin(comp_id(it))
    comp_name(it)  = seq_comm_name(comp_id(it))

    do eai = 1,num_inst_atm
       it=it+1
       comp_id(it)    = ATMID(eai)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(ATMID(eai), mpicom=comp_comm(it), &
            nthreads=nthreads_ATMID, iam=comp_comm_iam(it))
       if (seq_comm_iamin(ATMID(eai))) then
          complist = trim(complist)//' '//trim(seq_comm_name(ATMID(eai)))
       endif
       if (seq_comm_iamroot(ATMID(eai))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLATMID, mpicom=mpicom_CPLALLATMID)
    iamin_CPLALLATMID = seq_comm_iamin(CPLALLATMID)

    do eli = 1,num_inst_lnd
       it=it+1
       comp_id(it)    = LNDID(eli)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(LNDID(eli), mpicom=comp_comm(it), &
            nthreads=nthreads_LNDID, iam=comp_comm_iam(it))
       if (seq_comm_iamin(LNDID(eli))) then
          complist = trim(complist)//' '//trim(seq_comm_name(LNDID(eli)))
       endif
       if (seq_comm_iamroot(LNDID(eli))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLLNDID, mpicom=mpicom_CPLALLLNDID)
    iamin_CPLALLLNDID = seq_comm_iamin(CPLALLLNDID)

    do eoi = 1,num_inst_ocn
       it=it+1
       comp_id(it)    = OCNID(eoi)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(OCNID(eoi), mpicom=comp_comm(it), &
            nthreads=nthreads_OCNID, iam=comp_comm_iam(it))
       if (seq_comm_iamin (OCNID(eoi))) then
          complist = trim(complist)//' '//trim(seq_comm_name(OCNID(eoi)))
       endif
       if (seq_comm_iamroot(OCNID(eoi))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLOCNID, mpicom=mpicom_CPLALLOCNID)
    iamin_CPLALLOCNID = seq_comm_iamin(CPLALLOCNID)

    do eii = 1,num_inst_ice
       it=it+1
       comp_id(it)    = ICEID(eii)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(ICEID(eii), mpicom=comp_comm(it), &
            nthreads=nthreads_ICEID, iam=comp_comm_iam(it))
       if (seq_comm_iamin (ICEID(eii))) then
          complist = trim(complist)//' '//trim(seq_comm_name(ICEID(eii)))
       endif
       if (seq_comm_iamroot(ICEID(eii))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLICEID, mpicom=mpicom_CPLALLICEID)
    iamin_CPLALLICEID = seq_comm_iamin(CPLALLICEID)

    do egi = 1,num_inst_glc
       it=it+1
       comp_id(it)    = GLCID(egi)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(GLCID(egi), mpicom=comp_comm(it), nthreads=nthreads_GLCID, iam=comp_comm_iam(it))
       if (seq_comm_iamin (GLCID(egi))) then
          complist = trim(complist)//' '//trim(seq_comm_name(GLCID(egi)))
       endif
       if (seq_comm_iamroot(GLCID(egi))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLGLCID, mpicom=mpicom_CPLALLGLCID)
    iamin_CPLALLGLCID = seq_comm_iamin(CPLALLGLCID)

    do eri = 1,num_inst_rof
       it=it+1
       comp_id(it)    = ROFID(eri)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(ROFID(eri), mpicom=comp_comm(it), &
            nthreads=nthreads_ROFID, iam=comp_comm_iam(it))
       if (seq_comm_iamin(ROFID(eri))) then
          complist = trim(complist)//' '//trim( seq_comm_name(ROFID(eri)))
       endif
       if (seq_comm_iamroot(ROFID(eri))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLROFID, mpicom=mpicom_CPLALLROFID)
    iamin_CPLALLROFID = seq_comm_iamin(CPLALLROFID)

    do ewi = 1,num_inst_wav
       it=it+1
       comp_id(it)    = WAVID(ewi)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(WAVID(ewi), mpicom=comp_comm(it), &
            nthreads=nthreads_WAVID, iam=comp_comm_iam(it))
       if (seq_comm_iamin(WAVID(ewi))) then
          complist = trim(complist)//' '//trim(seq_comm_name(WAVID(ewi)))
       endif
       if (seq_comm_iamroot(WAVID(ewi))) output_perf = .true.
    enddo
    call seq_comm_getinfo(CPLALLWAVID, mpicom=mpicom_CPLALLWAVID)
    iamin_CPLALLWAVID = seq_comm_iamin(CPLALLWAVID)

    do eei = 1,num_inst_esp
       it=it+1
       comp_id(it)    = ESPID(eei)
       comp_iamin(it) = seq_comm_iamin(comp_id(it))
       comp_name(it)  = seq_comm_name(comp_id(it))
       call seq_comm_getinfo(ESPID(eei), mpicom=comp_comm(it), &
            nthreads=nthreads_ESPID, iam=comp_comm_iam(it))
       if (seq_comm_iamin (ESPID(eei))) then
          complist = trim(complist)//' '//trim(seq_comm_name(ESPID(eei)))
       endif
    enddo
    ! ESP components do not use the coupler (they are 'external')

    !----------------------------------------------------------
    !| Set logging parameters both for shr code and locally
    !----------------------------------------------------------

    if (iamroot_CPLID) then
       inquire(file='cpl_modelio.nml',exist=exists)
       if (exists) then
          logunit = shr_file_getUnit()
          call shr_file_setIO('cpl_modelio.nml',logunit)
          call shr_file_setLogUnit(logunit)
          loglevel = 1
          call shr_file_setLogLevel(loglevel)
       endif
    else
       loglevel = 0
       call shr_file_setLogLevel(loglevel)
    endif

    !----------------------------------------------------------
    ! Log info about the environment settings
    !----------------------------------------------------------

    if (iamroot_CPLID) then
#ifdef USE_ESMF_LIB
       write(logunit,'(2A)') subname,' USE_ESMF_LIB is set'
#else
       write(logunit,'(2A)') subname,' USE_ESMF_LIB is NOT set, using esmf_wrf_timemgr'
#endif
       write(logunit,'(2A)') subname,' MCT_INTERFACE is set'
    endif

    !
    !  When using io servers (pio_async_interface=.true.) the server tasks do not return from
    !  shr_pio_init2
    !
    call shr_pio_init2(comp_id,comp_name,comp_iamin,comp_comm,comp_comm_iam)

  end subroutine cesm_pre_init1

  !===============================================================================
  !*******************************************************************************
  !===============================================================================

  subroutine cesm_pre_init2()
    use pio, only : file_desc_t, pio_closefile, pio_file_is_open
    use shr_const_mod, only: shr_const_tkfrz, shr_const_tktrip, &
         shr_const_mwwv, shr_const_mwdair
    use shr_wv_sat_mod, only: shr_wv_sat_set_default, shr_wv_sat_init, &
         ShrWVSatTableSpec, shr_wv_sat_make_tables

    type(file_desc_t) :: pioid
    integer :: maxthreads

    character(CS) :: wv_sat_scheme
    real(r8) :: wv_sat_transition_start
    logical :: wv_sat_use_tables
    real(r8) :: wv_sat_table_spacing
    character(CL) :: errstring

    type(ShrWVSatTableSpec) :: liquid_spec, ice_spec, mixed_spec

    real(r8), parameter :: epsilo = shr_const_mwwv/shr_const_mwdair

    !----------------------------------------------------------
    ! Print Model heading and copyright message
    !----------------------------------------------------------

    if (iamroot_CPLID) call seq_cesm_printlogheader()

    !----------------------------------------------------------
    !| Timer initialization (has to be after mpi init)
    !----------------------------------------------------------
    maxthreads = max(nthreads_GLOID,nthreads_CPLID,nthreads_ATMID, &
         nthreads_LNDID,nthreads_ICEID,nthreads_OCNID,nthreads_GLCID, &
         nthreads_ROFID, nthreads_WAVID, nthreads_ESPID, pethreads_GLOID )

    call t_initf(NLFileName, LogPrint=.true., mpicom=mpicom_GLOID, &
         MasterTask=iamroot_GLOID,MaxThreads=maxthreads)

    if (iamin_CPLID) then
       call seq_io_cpl_init()
    endif

    call t_startf('CPL:INIT')
    call t_adj_detailf(+1)

    call t_startf('CPL:cesm_pre_init2')
    !----------------------------------------------------------
    !| Memory test
    !----------------------------------------------------------

    !mt   call shr_mem_init(prt=.true.)
    call shr_mem_init(prt=iamroot_CPLID)

    !----------------------------------------------------------
    !| Initialize infodata
    !----------------------------------------------------------

    call seq_infodata_init(infodata,nlfilename, GLOID, pioid)

    !----------------------------------------------------------
    !| Initialize coupled fields (depends on infodata)
    !----------------------------------------------------------

    call seq_flds_set(nlfilename, GLOID, infodata)

    !----------------------------------------------------------
    !| Obtain infodata info
    !----------------------------------------------------------

    call seq_infodata_GetData(infodata, &
         info_debug=info_debug)

    if (info_debug > 1 .and. iamroot_CPLID) then
       write(logunit,*) ' '
       write(logunit,'(2A)') 'Status of infodata after seq_infodata_init'
       call seq_infodata_print( infodata )
       write(logunit,*) ' '
    endif

    call seq_infodata_GetData(infodata             , &
         read_restart=read_restart                 , &
         restart_file=rest_file                    , &
         timing_dir=timing_dir                     , &
         tchkpt_dir=tchkpt_dir                     , &
         info_debug=info_debug                     , &
         atm_present=atm_present                   , &
         lnd_present=lnd_present                   , &
         ice_present=ice_present                   , &
         ocn_present=ocn_present                   , &
         glc_present=glc_present                   , &
         rof_present=rof_present                   , &
         wav_present=wav_present                   , &
         esp_present=esp_present                   , &
         single_column=single_column               , &
         aqua_planet=aqua_planet                   , &
         cpl_seq_option=cpl_seq_option             , &
         drv_threading=drv_threading               , &
         do_histinit=do_histinit                   , &
         do_budgets=do_budgets                     , &
         budget_inst=budget_inst                   , &
         budget_daily=budget_daily                 , &
         budget_month=budget_month                 , &
         budget_ann=budget_ann                     , &
         budget_ltann=budget_ltann                 , &
         budget_ltend=budget_ltend                 , &
         histaux_a2x=do_hist_a2x                   , &
         histaux_a2x1hri=do_hist_a2x1hri           , &
         histaux_a2x1hr=do_hist_a2x1hr             , &
         histaux_a2x3hr =do_hist_a2x3hr            , &
         histaux_a2x3hrp=do_hist_a2x3hrp           , &
         histaux_a2x24hr=do_hist_a2x24hr           , &
         histaux_l2x=do_hist_l2x                   , &
         histaux_l2x1yr=do_hist_l2x1yr             , &
         histaux_r2x=do_hist_r2x                   , &
         run_barriers=run_barriers                 , &
         mct_usealltoall=mct_usealltoall           , &
         mct_usevector=mct_usevector               , &
         aoflux_grid=aoflux_grid                   , &
         vect_map=vect_map                         , &
         atm_gnam=atm_gnam                         , &
         lnd_gnam=lnd_gnam                         , &
         ocn_gnam=ocn_gnam                         , &
         ice_gnam=ice_gnam                         , &
         rof_gnam=rof_gnam                         , &
         glc_gnam=glc_gnam                         , &
         wav_gnam=wav_gnam                         , &
         tfreeze_option = tfreeze_option           , &
         cpl_decomp=seq_mctext_decomp              , &
         shr_map_dopole=shr_map_dopole             , &
         wall_time_limit=wall_time_limit           , &
         force_stop_at=force_stop_at               , &
         reprosum_use_ddpdd=reprosum_use_ddpdd     , &
         reprosum_diffmax=reprosum_diffmax         , &
         reprosum_recompute=reprosum_recompute, &
         max_cplstep_time=max_cplstep_time)

    ! above - cpl_decomp is set to pass the cpl_decomp value to seq_mctext_decomp
    ! (via a use statement)

    call shr_map_setDopole(shr_map_dopole)

    call shr_reprosum_setopts(&
         repro_sum_use_ddpdd_in    = reprosum_use_ddpdd, &
         repro_sum_rel_diff_max_in = reprosum_diffmax, &
         repro_sum_recompute_in    = reprosum_recompute)

    ! Check cpl_seq_option

    if (trim(cpl_seq_option) /= 'CESM1_ORIG' .and. &
         trim(cpl_seq_option) /= 'CESM1_ORIG_TIGHT' .and. &
         trim(cpl_seq_option) /= 'CESM1_MOD' .and. &
         trim(cpl_seq_option) /= 'CESM1_MOD_TIGHT' .and. &
         trim(cpl_seq_option) /= 'RASM_OPTION1' .and. &
         trim(cpl_seq_option) /= 'RASM_OPTION2' ) then
       call shr_sys_abort(subname//' invalid cpl_seq_option = '//trim(cpl_seq_option))
    endif

    !----------------------------------------------------------
    !| Test Threading Setup in driver
    !  happens to be valid on all pes for all IDs
    !----------------------------------------------------------

    if (drv_threading) then
       if (iamroot_GLOID) write(logunit,*) ' '
       if (iamroot_GLOID) write(logunit,'(2A)    ') subname,' Test Threading in driver'
       call seq_comm_setnthreads(nthreads_GLOID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_GLOID = ',&
            nthreads_GLOID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_CPLID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_CPLID = ',&
            nthreads_CPLID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_ATMID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_ATMID = ',&
            nthreads_ATMID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_LNDID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_LNDID = ',&
            nthreads_LNDID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_OCNID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_OCNID = ',&
            nthreads_OCNID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_ICEID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_ICEID = ',&
            nthreads_ICEID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_GLCID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_GLCID = ',&
            nthreads_GLCID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_ROFID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_ROFID = ',&
            nthreads_ROFID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_WAVID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_WAVID = ',&
            nthreads_WAVID,seq_comm_getnthreads()
       call seq_comm_setnthreads(nthreads_ESPID)
       if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'    nthreads_ESPID = ',&
            nthreads_ESPID,seq_comm_getnthreads()
       if (iamroot_GLOID) write(logunit,*) ' '

       call seq_comm_setnthreads(nthreads_GLOID)
    endif

    !----------------------------------------------------------
    !| Initialize time manager
    !----------------------------------------------------------

    call seq_timemgr_clockInit(seq_SyncClock, nlfilename, &
         read_restart, rest_file, pioid, mpicom_gloid,           &
         EClock_d, EClock_a, EClock_l, EClock_o,          &
         EClock_i, Eclock_g, Eclock_r, Eclock_w, Eclock_e)

    if (iamroot_CPLID) then
       call seq_timemgr_clockPrint(seq_SyncClock)
    endif

    !----------------------------------------------------------
    !| Initialize infodata items which need the clocks
    !----------------------------------------------------------
    call seq_infodata_init2(infodata, GLOID)

    call seq_infodata_getData(infodata,   &
         orb_iyear=orb_iyear,             &
         orb_iyear_align=orb_iyear_align, &
         orb_mode=orb_mode)

    !----------------------------------------------------------
    ! Initialize freezing point calculation for all components
    !----------------------------------------------------------

    call shr_frz_freezetemp_init(tfreeze_option)

    if (trim(orb_mode) == trim(seq_infodata_orb_variable_year)) then
       call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd)

       call shr_cal_date2ymd(ymd,year,month,day)
       orb_cyear = orb_iyear + (year - orb_iyear_align)

       call shr_orb_params(orb_cyear, orb_eccen, orb_obliq, orb_mvelp, &
            orb_obliqr, orb_lambm0, orb_mvelpp, iamroot_CPLID)

       call seq_infodata_putData(infodata, &
            orb_eccen=orb_eccen,           &
            orb_obliqr=orb_obliqr,         &
            orb_lambm0=orb_lambm0,         &
            orb_mvelpp=orb_mvelpp)
    endif

    call seq_infodata_getData(infodata,                   &
         wv_sat_scheme=wv_sat_scheme,                     &
         wv_sat_transition_start=wv_sat_transition_start, &
         wv_sat_use_tables=wv_sat_use_tables,             &
         wv_sat_table_spacing=wv_sat_table_spacing)

    if (.not. shr_wv_sat_set_default(wv_sat_scheme)) then
       call shr_sys_abort('Invalid wv_sat_scheme.')
    end if

    call shr_wv_sat_init(shr_const_tkfrz, shr_const_tktrip, &
         wv_sat_transition_start, epsilo, errstring)

    if (errstring /= "") then
       call shr_sys_abort('shr_wv_sat_init: '//trim(errstring))
    end if

    ! The below produces internal lookup tables in the range 175-374K for
    ! liquid water, and 125-274K for ice, with a resolution set by the
    ! option wv_sat_table_spacing.
    ! In theory these ranges could be specified in the namelist, but in
    ! practice users will want to change them *very* rarely if ever, which
    ! is why only the spacing is in the namelist.
    if (wv_sat_use_tables) then
       liquid_spec = ShrWVSatTableSpec(ceiling(200._r8/wv_sat_table_spacing), &
            175._r8, wv_sat_table_spacing)
       ice_spec = ShrWVSatTableSpec(ceiling(150._r8/wv_sat_table_spacing), &
            125._r8, wv_sat_table_spacing)
       mixed_spec = ShrWVSatTableSpec(ceiling(250._r8/wv_sat_table_spacing), &
            125._r8, wv_sat_table_spacing)
       call shr_wv_sat_make_tables(liquid_spec, ice_spec, mixed_spec)
    end if

    call seq_infodata_putData(infodata, &
         atm_phase=1,                   &
         lnd_phase=1,                   &
         ocn_phase=1,                   &
         ice_phase=1,                   &
         glc_phase=1,                   &
         wav_phase=1,                   &
         esp_phase=1)

    !----------------------------------------------------------
    !| Set aqua_planet and single_column flags
    !  If in single column mode, overwrite flags according to focndomain file
    !  in ocn_in namelist. SCAM can reset the "present" flags for lnd,
    !  ocn, ice, rof, and flood.
    !----------------------------------------------------------

    if (.not.aqua_planet .and. single_column) then
       call seq_infodata_getData( infodata, &
            scmlon=scmlon, scmlat=scmlat)

       call seq_comm_getinfo(OCNID(ens1), mpicom=mpicom_OCNID)

       call shr_scam_checkSurface(scmlon, scmlat, &
            OCNID(ens1), mpicom_OCNID,            &
            lnd_present=lnd_present,              &
            ocn_present=ocn_present,              &
            ice_present=ice_present,              &
            rof_present=rof_present,              &
            flood_present=flood_present,          &
            rofice_present=rofice_present)

       call seq_infodata_putData(infodata,  &
            lnd_present=lnd_present,        &
            ocn_present=ocn_present,        &
            ice_present=ice_present,        &
            rof_present=rof_present,        &
            flood_present=flood_present,    &
            rofice_present=rofice_present)
    endif

    if(PIO_FILE_IS_OPEN(pioid)) then
       call pio_closefile(pioid)
    endif

    call t_stopf('CPL:cesm_pre_init2')

    call t_adj_detailf(-1)
    call t_stopf('CPL:INIT')

  end subroutine cesm_pre_init2

end module cesm_comp_mod
