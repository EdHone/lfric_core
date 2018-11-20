!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> @brief Define indices for the mixing ratio field vectors
!>
!> @details Define and set indices for the mixing ratio field vectors
module mr_indices_mod

  use constants_mod, only: str_short

  implicit none

  private

  public :: imr_v, imr_cl, imr_r, imr_ci, imr_s, imr_g, nummr, mr_names

  integer, parameter :: imr_v  = 1  ! vapour
  integer, parameter :: imr_cl = 2  ! liquid cloud mass
  integer, parameter :: imr_r  = 3  ! rain mass
  integer, parameter :: imr_ci = 4  ! ice cloud mass
  integer, parameter :: imr_s  = 5  ! snow mass
  integer, parameter :: imr_g  = 6  ! graupel mass
  integer, parameter :: nummr  = 6  ! Total number of mixing ratio variables

  character(str_short), parameter :: mr_names(nummr) = &
     [ 'm_v ', 'm_cl', 'm_r ', 'm_ci', 'm_s ', 'm_g ' ]

end module mr_indices_mod
