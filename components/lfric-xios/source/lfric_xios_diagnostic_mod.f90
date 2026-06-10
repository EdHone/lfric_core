!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

module lfric_xios_diagnostic_mod
  use lfric_xios_field_mod, only: lfric_xios_field_type
  use linked_list_mod,      only: linked_list_item_type

  implicit none

private

type, public, extends(linked_list_item_type) :: lfric_xios_diagnostic_type
  private

  type(lfric_xios_field_type) :: field
  type(xios_date) :: next_operation
  logical :: was_sent_this_timestep = .false.
contains
  procedure :: lfric_xios_diagnostic_constructor
  procedure :: send
  final :: lfric_xios_diagnostic_destructor
end type lfric_xios_diagnostic_type

contains

end module lfric_xios_diagnostic_mod