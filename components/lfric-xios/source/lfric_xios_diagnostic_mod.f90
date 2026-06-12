!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> Module containing an interface to enable flexible sending of diagnostic
!> fields to XIOS, separate of file definition
module lfric_xios_diagnostic_mod

  use constants_mod,        only: str_def
  use field_parent_mod,     only: field_parent_type
  use lfric_xios_field_mod, only: lfric_xios_field_type
  use linked_list_data_mod, only: linked_list_data_type
  use log_mod,              only: log_event, log_level_debug, log_level_error
  use xios,                 only: xios_date, xios_duration,                 &
                                  xios_is_valid_field, xios_get_start_date, &
                                  xios_get_field_attr, operator(+)

  implicit none

private

type, public, extends(linked_list_data_type) :: lfric_xios_diagnostic_type
  private

  type(lfric_xios_field_type) :: field
  type(xios_date) :: next_operation
  type(xios_duration) :: frequency
  logical :: was_sent_this_timestep = .false.
contains
  procedure :: send
  procedure :: reset
  procedure :: get_xios_id
end type lfric_xios_diagnostic_type

interface lfric_xios_diagnostic_type
  module procedure lfric_xios_diagnostic_constructor
end interface

contains

!> Constructor for the lfric_xios_diagnostic_type, which takes a field and an
!> optional XIOS ID to create an instance of an lfric_xios_field_type for the
!> diagnostic.
function lfric_xios_diagnostic_constructor(field, input_xios_id) result(self)

  class(field_parent_type), target, intent(in) :: field
  character(len=*), optional,       intent(in) :: input_xios_id

  type(lfric_xios_diagnostic_type) :: self

  class(field_parent_type), pointer :: field_ptr
  type(xios_date) :: start_date
  type(xios_duration) :: freq_offset
  character(len=str_def) :: xios_id

  field_ptr => field
  if (present(input_xios_id)) then
    self%field = lfric_xios_field_type(field_ptr, input_xios_id)
  else
    self%field = lfric_xios_field_type(field_ptr)
  end if

  xios_id = self%field%get_xios_id()

  if (.not. xios_is_valid_field(trim(xios_id))) then
    call log_event( "Diagnostic field '" // trim(xios_id) // &
                    "' must have a definition in iodef.xml", &
                    log_level_error )
  end if

  call xios_get_start_date(start_date)
  call xios_get_field_attr(trim(xios_id), freq_op=self%frequency, freq_offset=freq_offset)
  self%next_operation = start_date + freq_offset

  nullify(field_ptr)

end function lfric_xios_diagnostic_constructor

!> Send field data to for the diagnostic to XIOS.
subroutine send(self, field_pointer)

  implicit none

  class(lfric_xios_diagnostic_type),           intent(inout) :: self
  class(field_parent_type), pointer, optional, intent(in)    :: field_pointer

  if (self%was_sent_this_timestep) then
    call log_event( "Diagnostic field '" // trim(self%field%get_xios_id()) // &
                    "' has already been sent this timestep - skipping.",      &
                    log_level_debug )
    return
  end if

  ! If an optional field has been passed to this subroutine, as for cases where
  ! diagnostics might be dynamically generated, then update the associated field
  ! pointer
  if (present(field_pointer)) then
    call self%field%set_model_field(field_pointer)
  end if

  ! Send data to XIOS
  call self%field%send()

  ! Set object attributes to reflect successful send operation
  self%was_sent_this_timestep = .true.
  self%next_operation = self%next_operation + self%frequency

end subroutine send

!> Resets the diagnostic's state for the next timestep, allowing it to be sent
!> again if required.
subroutine reset(self)

  implicit none

  class(lfric_xios_diagnostic_type), intent(inout) :: self

  self%was_sent_this_timestep = .false.

end subroutine reset

function get_xios_id(self) result(xios_id_out)

  implicit none

  class(lfric_xios_diagnostic_type), intent(in) :: self
  character(len=str_def), allocatable :: xios_id_out

  xios_id_out = trim(adjustl(self%field%get_xios_id()))

end function get_xios_id

end module lfric_xios_diagnostic_mod