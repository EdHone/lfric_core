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
  use xios,                 only: xios_date, xios_duration, xios_timestep,    &
                                  xios_is_valid_field, xios_get_start_date,   &
                                  xios_get_field_attr, xios_get_current_date, &
                                  xios_is_defined_field_attr, operator(+),    &
                                  operator(<=)

  implicit none

private

type, public, extends(linked_list_data_type) :: lfric_xios_diagnostic_type
  private

  type(lfric_xios_field_type) :: field
  type(xios_date) :: next_operation
  type(xios_duration) :: frequency
  logical :: enabled = .true.
  logical :: was_sent_this_timestep = .false.
contains
  procedure :: send
  procedure :: reset
  procedure :: get_xios_id
  procedure :: is_enabled
end type lfric_xios_diagnostic_type

interface lfric_xios_diagnostic_type
  module procedure lfric_xios_diagnostic_constructor
end interface

contains

!> Constructor for the lfric_xios_diagnostic_type, which takes a field and an
!> optional XIOS ID to create an instance of an lfric_xios_field_type for the
!> diagnostic.
!!
!> @param[in] field The field to be sent as a diagnostic to XIOS
!> @param[in] input_xios_id An optional XIOS ID to associate with the diagnostic field
function lfric_xios_diagnostic_constructor(field, input_xios_id) result(self)

  class(field_parent_type), target, intent(in) :: field
  character(len=*), optional,       intent(in) :: input_xios_id

  type(lfric_xios_diagnostic_type) :: self

  class(field_parent_type), pointer :: field_ptr
  type(xios_date) :: start_date
  type(xios_duration) :: freq_offset
  character(len=str_def) :: xios_id
  logical :: l_freq_op, l_freq_offset, l_enabled

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

  self%frequency = xios_timestep
  freq_offset = xios_timestep
  call xios_is_defined_field_attr( trim(xios_id), freq_op=l_freq_op, &
                                                  freq_offset=l_freq_offset,&
                                                  enabled=l_enabled )

  if (l_enabled) call xios_get_field_attr(trim(xios_id), enabled=self%enabled)
  if (l_freq_op) call xios_get_field_attr(trim(xios_id), freq_op=self%frequency)
  if (l_freq_offset) call xios_get_field_attr(trim(xios_id), freq_offset=freq_offset)

  call xios_get_start_date(start_date)
  self%next_operation = start_date + freq_offset

  nullify(field_ptr)

end function lfric_xios_diagnostic_constructor

!> Send field data to for the diagnostic to XIOS.
!>
!> @param[in] field_pointer An optional field pointer which is used as dynamic
!>                          source of field data in provided
subroutine send(self, field_pointer)

  implicit none

  class(lfric_xios_diagnostic_type),           intent(inout) :: self
  class(field_parent_type), pointer, optional, intent(in)    :: field_pointer

  type(xios_date) :: model_date

  if (self%was_sent_this_timestep) then
    call log_event( "Diagnostic field '" // trim(self%field%get_xios_id()) // &
                    "' has already been sent this timestep - skipping.",      &
                    log_level_debug )
    return
  end if

  call xios_get_current_date(model_date)
  if (self%next_operation <= model_date) then

    call log_event( "Sending diagnostic field '" //                &
                    trim(self%field%get_xios_id()) // "' to XIOS", &
                    log_level_debug )

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
  else
    call log_event( "Diagnostic field '" // trim(self%field%get_xios_id()) // &
                    "' is not due to be sent on this timestep - skipping.",   &
                    log_level_debug )
  end if

end subroutine send

!> Resets the diagnostic's state for the next timestep, allowing it to be sent
!> again if required.
subroutine reset(self)

  implicit none

  class(lfric_xios_diagnostic_type), intent(inout) :: self

  self%was_sent_this_timestep = .false.

end subroutine reset

!> Returns the XIOS ID associated with the diagnostic field
function get_xios_id(self) result(xios_id_out)

  implicit none

  class(lfric_xios_diagnostic_type), intent(in) :: self
  character(len=str_def), allocatable :: xios_id_out

  xios_id_out = trim(adjustl(self%field%get_xios_id()))

end function get_xios_id

!> Returns true if field is enabled
function is_enabled(self) result(enabled_out)

  implicit none

  class(lfric_xios_diagnostic_type), intent(in) :: self
  logical :: enabled_out

  enabled_out = self%enabled

end function is_enabled

end module lfric_xios_diagnostic_mod