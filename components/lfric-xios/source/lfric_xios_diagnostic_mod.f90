!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

module lfric_xios_diagnostic_mod

  use field_parent_mod,     only: field_parent_type
  use lfric_xios_field_mod, only: lfric_xios_field_type
  use linked_list_mod,      only: linked_list_item_type
  use xios,                 only: xios_date

  implicit none

private

type, public, extends(linked_list_item_type) :: lfric_xios_diagnostic_type
  private

  type(lfric_xios_field_type) :: field
  type(xios_date) :: next_operation
  logical :: was_sent_this_timestep = .false.
contains
  !procedure :: send
  !final :: lfric_xios_diagnostic_destructor
end type lfric_xios_diagnostic_type

interface lfric_xios_diagnostic_type
  module procedure lfric_xios_diagnostic_constructor
end interface

contains


function lfric_xios_diagnostic_constructor(field, xios_id) result(this)

  class(field_parent_type), target, intent(in) :: field
  character(len=*), optional,       intent(in) :: xios_id

  class(field_parent_type), pointer :: field_ptr
  type(lfric_xios_diagnostic_type) :: this

  field_ptr => field
  if (present(xios_id)) then
    this%field = lfric_xios_field_type(field_ptr, xios_id)
  else
    this%field = lfric_xios_field_type(field_ptr)
  end if


  nullify(field_ptr)

end function lfric_xios_diagnostic_constructor

end module lfric_xios_diagnostic_mod