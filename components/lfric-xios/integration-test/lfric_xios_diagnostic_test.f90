!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

! Simple tests for the lfric_xios_diagnostic_type
!
program lfric_xios_diagnostic_test

  use event_mod,              only: event_action
  use event_actor_mod,        only: event_actor_type
  use field_mod,              only: field_type
  use lfric_xios_action_mod,  only: advance
  use lfric_xios_context_mod, only: lfric_xios_context_type
  use lfric_xios_diagnostic_mod, only: lfric_xios_diagnostic_type
  use lfric_xios_driver_mod,  only: lfric_xios_initialise, lfric_xios_finalise
  use log_mod,                only: log_event, log_level_info
  use test_db_mod,            only: test_db_type

  use local_mesh_mod, only: local_mesh_type
  use mesh_mod, only: mesh_type

  implicit none

  type(test_db_type)                                 :: test_db
  type(lfric_xios_context_type), target, allocatable :: io_context

  class(event_actor_type),       pointer :: context_actor
  procedure(event_action),       pointer :: context_advance
  type(field_type),              pointer :: diagnostic_field
  type(lfric_xios_diagnostic_type), pointer :: diagnostic

  call test_db%initialise()
  call lfric_xios_initialise( "test", test_db%comm, .false. )

  ! =============================== Start test ================================

  allocate(io_context)
  call io_context%initialise( "test_io_context", 1, 10 )

  diagnostic_field => null()
  call test_db%fields%get_field("diagnostic_field", diagnostic_field)

  call io_context%initialise_xios_context( test_db%comm,                    &
                                           test_db%chi,  test_db%panel_id,  &
                                           test_db%clock, test_db%calendar )
  call io_context%add_diagnostic(diagnostic_field)
  call io_context%close_context_definition()

  context_advance => advance
  context_actor => io_context
  call test_db%clock%add_event( context_advance, context_actor )
  call io_context%set_active(.true.)

  diagnostic => null()
  do while (test_db%clock%tick())
    if (mod(test_db%clock%get_step(), 4) == 0) then
      diagnostic => io_context%get_diagnostic("diagnostic_field")
      call diagnostic%send()
      diagnostic => null()
    end if
  end do

  deallocate(io_context)

  ! ============================== Finish test =================================

  call lfric_xios_finalise()
  call test_db%finalise()

end program lfric_xios_diagnostic_test
