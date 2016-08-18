!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!------------------------------------------------------------------------------
!
!> @brief   Object to hold intermesh connectivity between two global mesh objects.
!> @details Cell mappings are from GID of a cell in the source mesh to GIDs of
!>          overlapping cells in the target global mesh object.
module global_mesh_map_mod

  use constants_mod,   only: i_def, imdi
  use log_mod,         only: log_event, log_scratch_space                      &
                           , LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use linked_list_data_mod, only : linked_list_data_type

  implicit none

  private

  !----------------------------------------------------------------------------
  type, extends(linked_list_data_type), public :: global_mesh_map_type
    private

    integer(i_def) :: source_mesh_id   !< Id of source global mesh
    integer(i_def) :: target_mesh_id   !< Id of target global mesh
    integer(i_def) :: nsource_cells    !< Number of source cells in this object
    integer(i_def) :: ntarget_cells_per_source_cell
                                       !< Number of target cells per source cell

    integer(i_def), allocatable :: global_mesh_map(:,:)

  contains
    !> @brief  Gets the id of the global mesh object used as the source
    procedure, public :: get_source_id
    !> @brief  Gets the id of the global mesh object used as the target
    procedure, public :: get_target_id
    !> @brief  Gets the number of source cells in this object
    procedure, public :: get_nsource_cells
    !> @brief  Gets the number of target cells for each source cell
    procedure, public :: get_ntarget_cells_per_source_cell
    !> @brief  Gets the target cells ids mapped to each requested source cell
    !> @param [in]  cell_ids[:] Integer array of source cell global ids which
    !>                          require mapped target cell global ids
    !> @param [out] gid_map[::] Integer array of target cell global ids which
    !>                          map to the requested cell_ids in the source
    !>                          global mesh object.
    procedure, public :: get_cell_map

    !> @brief Forcibly clears any heap memory the object is using
    procedure, public :: clear

    !> @brief Finaliser which calls the clear routine
    final             :: global_mesh_map_destructor


  end type global_mesh_map_type

  interface global_mesh_map_type
    module procedure global_mesh_map_constructor
  end interface
  !----------------------------------------------------------------------------

contains

  !> @brief     Constructor for global mesh map object
  !> @param[in] source_id  Integer id of source global mesh object
  !> @param[in] target_id  Integer id of target global mesh object
  !> @param[in] global_mesh_map 
  !>                       Integer 2D-array of Global cell ids in target global
  !>                       mesh object for each Global cell in the source global
  !>                       mesh object. 
  !>           
  !> @return    global mesh map object

  !============================================================================
  function global_mesh_map_constructor ( source_id, target_id                  &
                                       , global_mesh_map )                     &
                                result ( instance )

    implicit none

    integer(i_def), intent(in) :: source_id
    integer(i_def), intent(in) :: target_id
    integer(i_def), intent(in) :: global_mesh_map(:,:)

    type(global_mesh_map_type) :: instance

    integer(i_def) :: global_mesh_map_id
    integer(i_def) :: ntarget_cells_per_source_cell
    integer(i_def) :: nsource_cells

    if (source_id == target_id) then
      write(log_scratch_space, '(A)')                                          &
          'Global mesh ids are the same nothing to do.'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)
      call instance%set_id(imdi)
      return
    end if

    global_mesh_map_id = 1000*source_id + target_id
    call instance%set_id(global_mesh_map_id)

    ntarget_cells_per_source_cell    = size(global_mesh_map,1)
    nsource_cells = size(global_mesh_map,2)

    ! Populate instance
    ! -----------------------------------------
    instance%source_mesh_id = source_id
    instance%target_mesh_id = target_id
    instance%nsource_cells  = nsource_cells
    instance%ntarget_cells_per_source_cell = ntarget_cells_per_source_cell

    allocate( instance%global_mesh_map( ntarget_cells_per_source_cell          &
                                      , nsource_cells ) )

    instance%global_mesh_map = global_mesh_map

    return
  end function global_mesh_map_constructor



  subroutine get_cell_map(self, cell_ids, gid_map)

    implicit none

    class(global_mesh_map_type), intent(in) :: self

    integer(i_def), intent(in)  :: cell_ids(:)
    integer(i_def), intent(out) :: gid_map(:,:)

    integer(i_def) :: i
    integer(i_def) :: ncells_request

    ncells_request = size(cell_ids)

    if ((size(gid_map,1) /= self%ntarget_cells_per_source_cell) .or.           &
        (size(gid_map,2) /= ncells_request)) then
       write(log_scratch_space, '(2(A,I0),A)')                                 &
          'Output array dimensions are incorrect, dimensions of (',            &
           self%ntarget_cells_per_source_cell, ',', ncells_request,            &
           ') required.'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      return
    end if

    do i=1, ncells_request
      gid_map(:,i) = self%global_mesh_map(:,cell_ids(i))
    end do

    return
  end subroutine get_cell_map


  function get_source_id(self) result (source_mesh_id)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: source_mesh_id

    source_mesh_id = self%source_mesh_id

  end function get_source_id

  function get_target_id(self) result (target_mesh_id)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: target_mesh_id

    target_mesh_id = self%target_mesh_id

  end function get_target_id

  function get_ntarget_cells_per_source_cell(self)                             &
      result (ntarget_cells_per_source_cell)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: ntarget_cells_per_source_cell

    ntarget_cells_per_source_cell = self%ntarget_cells_per_source_cell

  end function get_ntarget_cells_per_source_cell

  function get_nsource_cells(self) result (nsource_cells)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: nsource_cells

    nsource_cells = self%nsource_cells

  end function get_nsource_cells


  !> Clear all allocated components
  subroutine clear(self)

    implicit none

    class(global_mesh_map_type), intent(inout) :: self

    if ( allocated(self%global_mesh_map) ) deallocate(self%global_mesh_map)

  end subroutine clear


  ! Global mesh map destructor
  subroutine global_mesh_map_destructor(self)

    implicit none

    type(global_mesh_map_type), intent(inout) :: self

    call self%clear()

  end subroutine global_mesh_map_destructor


end module global_mesh_map_mod

