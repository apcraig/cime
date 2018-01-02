module shr_dmodeltime_mod

  use ESMF
  use shr_kind_mod, only: SHR_KIND_IN, SHR_KIND_R8, SHR_KIND_CL, SHR_KIND_I8
  use shr_sys_mod , only: shr_sys_abort

  implicit none

  private ! except

  public  :: shr_dmodeltime_EClockGetData     ! Get data from an ESMF clock

  private :: shr_dmodeltime_ETimeGet
  private :: shr_dmodeltime_ESMFCodeCheck

!===============================================================================
contains
!===============================================================================

  subroutine shr_dmodeltime_EClockGetData( clock, &
       curr_yr, curr_mon, curr_day, curr_tod, curr_ymd, curr_cday, StepNo, dtime)

    ! DESCRIPTION: Get various values from the clock.

    ! INPUT/OUTPUT PARAMETERS:
    type(ESMF_Clock)     , intent(in)            :: clock    ! Input clock object
    integer(SHR_KIND_IN) , intent(out), optional :: curr_yr   ! Current year
    integer(SHR_KIND_IN) , intent(out), optional :: curr_mon  ! Current month
    integer(SHR_KIND_IN) , intent(out), optional :: curr_day  ! Current day in month
    integer(SHR_KIND_IN) , intent(out), optional :: curr_tod  ! Current time of day (s)
    integer(SHR_KIND_IN) , intent(out), optional :: curr_ymd  ! Current date YYYYMMDD
    real(SHR_KIND_R8)    , intent(out), optional :: curr_cday ! current calendar day
    integer(SHR_KIND_IN) , intent(out), optional :: StepNo    ! Number of steps taken
    integer(SHR_KIND_IN) , intent(out), optional :: dtime     ! Time-step (seconds)

    !----- local -----
    type(ESMF_Time)         :: CurrentTime     ! Current time
    type(ESMF_TimeInterval) :: timeStep        ! Clock, time-step
    integer(SHR_KIND_I8)    :: advSteps        ! Number of time-steps that have advanced
    integer(SHR_KIND_IN)    :: yy, mm, dd, sec ! Return time values
    integer(SHR_KIND_IN)    :: ymd             ! Date (YYYYMMDD)
    integer(SHR_KIND_IN)    :: tod             ! time of day (sec)
    integer(SHR_KIND_IN)    :: ldtime          ! local dtime
    real(SHR_KIND_R8)       :: doy             ! day of year
    integer(SHR_KIND_IN)    :: rc              ! Return code
    character(len=*), parameter :: subname = '(shr_dmodeltime_EClockGetData) '
    !-------------------------------------------------------------------------------

    call ESMF_ClockGet( clock, currTime=CurrentTime, TimeStep=timeStep, advanceCount=advSteps,  rc=rc )
    call shr_dmodeltime_ESMFCodeCheck( rc, msg=subname//"Error from ESMF_ClockGet" )

    call ESMF_TimeGet( CurrentTime, yy=yy, mm=mm, dd=dd, s=sec, dayofyear_r8=doy, rc=rc )
    call shr_dmodeltime_ESMFCodeCheck( rc, msg=subname//"Error from ESMF_TimeGet" )

    call shr_dmodeltime_ETimeGet( CurrentTime, ymd=ymd, tod=tod )

    call ESMF_TimeIntervalGet( timeStep, s=ldtime, rc=rc )
    call shr_dmodeltime_ESMFCodeCheck( rc, msg=subname//"Error from ESMF_TimeIntervalGet" )

    if ( present(curr_yr)  ) curr_yr  = yy
    if ( present(curr_mon) ) curr_mon = mm
    if ( present(curr_day) ) curr_day = dd
    if ( present(curr_tod) ) curr_tod = tod
    if ( present(curr_ymd) ) curr_ymd = ymd
    if ( present(curr_cday)) curr_cday = doy
    if ( present(StepNo)   ) StepNo   = advSteps
    if ( present(dtime)    ) dtime    = ldtime

  end subroutine shr_dmodeltime_EClockGetData

  !===============================================================================

  subroutine shr_dmodeltime_ETimeGet( ETime, ymd, tod )

    ! !DESCRIPTION: Get the date in YYYYMMDD format from a ESMF time object.

    ! !INPUT/OUTPUT PARAMETERS:
    type(ESMF_Time) , intent(in)  :: ETime   ! Input ESMF time
    integer         , intent(out) :: ymd     ! date of day
    integer         , intent(out) :: tod     ! Time of day

    !----- local -----
    integer :: year     ! Year
    integer :: month    ! Month
    integer :: day      ! Day in month
    integer :: sec      ! Day in month
    integer :: rc       ! Return code
    character(len=*), parameter :: subname = '(shr_dmodeltime_ETimeGet) '
    !-------------------------------------------------------------------------------

    call ESMF_TimeGet( ETime, yy=year, mm=month, dd=day, s=sec, rc=rc )
    call shr_dmodeltime_ESMFCodeCheck( rc, msg=subname// ": Error from ESMF_TimeGet" )

    ! NOTE: the coded calendar date has a year zero (but no day or month zero)
    call shr_cal_ymd2date(year,month,day,ymd)
    tod = sec

  end subroutine shr_dmodeltime_ETimeGet

  !===============================================================================

  subroutine shr_dmodeltime_ESMFCodeCheck( rc, msg )

    ! !DESCRIPTION: Check ESMF return code and abort if not successful.

    ! !INPUT/OUTPUT PARAMETERS:
    integer, intent(in)  :: rc   ! return code from ESMF
    character(len=*)     :: msg  ! error message

    !----- local -----
    character(len=*),parameter :: subname = 'shr_dmodeltime_ESMFCodeCheck'
    !-------------------------------------------------------------------------------

    if ( rc == ESMF_SUCCESS ) then
       return
    else
       call shr_sys_abort(trim(subname) // trim(msg))
    end if

  end subroutine shr_dmodeltime_ESMFCodeCheck

end module shr_dmodeltime_mod
