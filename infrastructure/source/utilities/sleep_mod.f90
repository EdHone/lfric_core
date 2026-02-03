!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------

!> Module containing an interface to the sleep and usleep utilities from C.
module sleep_mod

  use, intrinsic :: iso_c_binding, only : c_int, c_int32_t
  use constants_mod, only: i_def

  implicit none

  private
  public :: sleep, usleep

  !> Interface to the C usleep function
  interface
    function c_usleep(useconds) bind(c, name='usleep')
        import :: c_int, c_int32_t
        implicit none
        integer(kind=c_int32_t), value :: useconds
        integer(kind=c_int)            :: c_usleep
    end function c_usleep
  end interface

contains

  !> Sleep for a given number of seconds
  !> @param seconds Number of seconds to sleep
  subroutine sleep(seconds)
      integer(i_def), intent(in) :: seconds
      integer(kind=c_int) :: rc
      rc = c_usleep(int(seconds, kind=c_int32_t) * 1000000_c_int32_t )
  end subroutine sleep

  !> Sleep for a given number of microseconds
  !> @param microseconds Number of microseconds to sleep
  subroutine usleep(microseconds)
      integer(i_def), intent(in) :: microseconds
      integer(kind=c_int) :: rc
      rc = c_usleep(int(microseconds, kind=c_int32_t))
  end subroutine usleep

end module sleep_mod