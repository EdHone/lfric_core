!-----------------------------------------------------------------------------
! (c) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Defines an object to pair reals with a unique identifier.
module id_real64_pair_mod

  use constants_mod,         only: r_double, i_def
  use id_abstract_pair_mod,  only: id_abstract_pair_type

  implicit none

  private

  ! ========================================================================== !
  ! ID-real64 Pair
  ! ========================================================================== !

  !> @brief An object pairing an real64 with a unique identifier
  !>
  type, public, extends(id_abstract_pair_type) :: id_real64_pair_type

    private

    real(kind=r_double) :: number_

  contains

    procedure, public :: initialise
    procedure, public :: get_real64

  end type id_real64_pair_type

contains

  !> @brief Initialises the id_real64_pair object
  !> @param[in] number   The number that will be stored in the paired object
  !> @param[in] id       The real64 ID to pair with the real64
  subroutine initialise( self, number, id )

    implicit none

    class(id_real64_pair_type), intent(inout) :: self
    real(kind=r_double),        intent(in)    :: number
    integer(kind=i_def),        intent(in)    :: id

    self%number_ = number
    call self%set_id(id)

  end subroutine initialise

  !> @brief Get the real64 corresponding to the paired object
  !> @param[in] self     The paired object
  !> @return             The real64
  function get_real64(self) result(number)

    implicit none

    class(id_real64_pair_type), target, intent(in) :: self
    real(kind=r_double),                pointer    :: number

    number => self%number_

  end function get_real64

end module id_real64_pair_mod
