module io_demo_checkpoint_mod

  use modeldb_mod, only: modeldb_type
  use linked_list_mod, only: linked_list_type
  use lfric_xios_context_mod, only: lfric_xios_context_type
  use lfric_xios_file_mod, only: lfric_xios_file_type
  use log_mod, only: log_event, LOG_LEVEL_DEBUG

  implicit none

  private
  public :: setup_checkpoint_io

contains

  subroutine setup_checkpoint_io(modeldb)

    type(modeldb_type), intent(inout) :: modeldb

    call log_event( 'io_demo: Setting up checkpoint I/O', LOG_LEVEL_DEBUG )

    type(lfric_xios_context_type) :: tmp_io_context
    type(lfric_xios_context_type), pointer :: cp_context
    type(linked_list_type), pointer :: file_list

    call tmp_io_context%initialise( "checkpoint_context",                             &
                                    start=modeldb%config%time%timestep_start(), &
                                    stop=modeldb%config%time%timestep_end() )
    call modeldb%io_contexts%add_context(tmp_io_context)

    ! Get pointer to persistent context
    call modeldb%io_contexts%get_io_context("checkpoint_context", cp_context)
    file_list => cp_context%get_filelist()

    if (modeldb%config%io%checkpoint_write()) then
      call log_event( 'io_demo: Setting up checkpoint write', LOG_LEVEL_DEBUG )
      call file_list%add_item( lfric_xios_file_type( "restart_io_demo",        &
                                    xios_id = "checkpoint_io_demo",            &
                                    io_mode = FILE_MODE_WRITE,                 &
                                    freq = modeldb%config%time%timestep_end(), &
                                    operation = OPERATION_ONCE ) )
    end if
    if (modeldb%config%io%checkpoint_read()) then
      call log_event( 'io_demo: Setting up checkpoint read', LOG_LEVEL_DEBUG )
      call file_list%add_item( lfric_xios_file_type( "restart_io_demo",        &
                                    xios_id = "restart_io_demo",               &
                                    io_mode = FILE_MODE_READ,                  &
                                    freq = modeldb%config%time%timestep_end(), &
                                    operation = OPERATION_ONCE ) )
    end if

  end subroutine setup_checkpoint_io

end module io_demo_checkpoint_mod