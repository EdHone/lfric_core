!-----------------------------------------------------------------------------
! (c) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Lifecycle management of the simple timer profiling system.
!>
module driver_timer_mod

  use io_config_mod, only : subroutine_timers, &
                            timer_output_path
  use timer_mod,     only : timer, output_timer, init_timer

  implicit none

  private
  public :: init_timers, final_timers

contains

  !> Initialises timers from namelists.
  !>
  !> As well as initialising the system a "top level" timer is started
  !> which will give the time between initialisation and finalisation of
  !> the timer system.
  !>
  !> @param[in] identifier Top level timer name.
  !>
  subroutine init_timers( identifier )

    implicit none

    character(*), intent(in) :: identifier

    if (subroutine_timers) then
      call init_timer( timer_output_path )
      call timer( identifier )
    end if

  end subroutine init_timers

  !> Shuts down timers.
  !>
  !> The identifier specified when shutting down should be the same as the one
  !> given on initialisation. There is a chance to mismatch the identifiers
  !> which will cause problems but it avoids the use of a global variable.
  !>
  !> @todo Reconsider the existance of the simple timer system once the
  !>       profiler is integrated.
  !>
  !> @param[in] identifier Top level timer name.
  !>
  subroutine final_timers( identifier )

    implicit none

    character(*), intent(in) :: identifier

    if (subroutine_timers) then
      call timer( identifier )
      call output_timer()
    end if

  end subroutine final_timers

end module driver_timer_mod
