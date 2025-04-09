!-----------------------------------------------------------------------------
! (c) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Defines an object to pair reals with a unique identifier.
module id_real32_pair_mod

  use constants_mod,         only: r_single, i_def
  use id_abstract_pair_mod,  only: id_abstract_pair_type

  implicit none

  private

  ! ========================================================================== !
  ! ID-real32 Pair
  ! ========================================================================== !

  !> @brief An object pairing an real32 with a unique identifier
  !>
  type, public, extends(id_abstract_pair_type) :: id_real32_pair_type

    private

    real(kind=r_single) :: number_

  contains

    procedure, public :: initialise
    procedure, public :: get_real32

  end type id_real32_pair_type

contains

  !> @brief Initialises the id_real32_pair object
  !> @param[in] number   The number that will be stored in the paired object
  !> @param[in] id       The real32 ID to pair with the real32
  subroutine initialise( self, number, id )

    implicit none

    class(id_real32_pair_type), intent(inout) :: self
    real(kind=r_single),        intent(in)    :: number
    integer(kind=i_def),        intent(in)    :: id

    self%number_ = number
    call self%set_id(id)

  end subroutine initialise

  !> @brief Get the real32 corresponding to the paired object
  !> @param[in] self     The paired object
  !> @return             The real32
  function get_real32(self) result(number)

    implicit none

    class(id_real32_pair_type), target, intent(in) :: self
    real(kind=r_single),                pointer    :: number

    number => self%number_

  end function get_real32

end module id_real32_pair_mod
