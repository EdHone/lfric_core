!-----------------------------------------------------------------------------
! (c) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Defines an object to pair logicals with a unique identifier.
module id_logical_pair_mod

  use constants_mod,         only: l_def, i_def
  use id_abstract_pair_mod,  only: id_abstract_pair_type

  implicit none

  private

  ! ========================================================================== !
  ! ID-Logical Pair
  ! ========================================================================== !

  !> @brief An object pairing a logical with a unique identifier
  !>
  type, public, extends(id_abstract_pair_type) :: id_logical_pair_type

    private

    logical(kind=l_def) :: bool_flag_

  contains

    procedure, public :: initialise
    procedure, public :: get_logical

  end type id_logical_pair_type

contains

  !> @brief Initialises the id_logical_pair object
  !> @param[in] bool_flag   The bool_flag that will be stored in the paired object
  !> @param[in] id       The logical ID to pair with the logical
  subroutine initialise( self, bool_flag, id )

    implicit none

    class(id_logical_pair_type), intent(inout) :: self
    logical(kind=l_def),         intent(in)    :: bool_flag
    integer(kind=i_def),         intent(in)    :: id

    self%bool_flag_ = bool_flag
    call self%set_id(id)

  end subroutine initialise

  !> @brief Get the logical corresponding to the paired object
  !> @param[in] self     The paired object
  !> @return             The logical
  function get_logical(self) result(bool_flag)

    implicit none

    class(id_logical_pair_type), target, intent(in) :: self
    logical(kind=l_def),                 pointer    :: bool_flag

    bool_flag => self%bool_flag_

  end function get_logical

end module id_logical_pair_mod
