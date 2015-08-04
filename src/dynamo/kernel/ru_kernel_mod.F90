!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which applies boundary conditions to the rhs of the momentum equation
!> @detail Wrapper code for applying boundary conditions for the momentum
!>         equation. When the Psyclone api is updated to correctly deal with
!>         boundary dofs this can be removed
module ru_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_INC,                        &
                                    W2,                                      &
                                    CELLS
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: ru_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  W2)                               &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::ru_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface ru_kernel_type
   module procedure ru_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public ru_code
contains

type(ru_kernel_type) function ru_kernel_constructor() result(self)
  return
end function ru_kernel_constructor

!> @brief The subroutine which is called directly by the Psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf_w2 The number of degrees of freedom per cell for w2
!! @param[in] undf_w2 The number unique of degrees of freedom  for w2
!! @param[in] map_w2 Integer array holding the dofmap for the cell at the base of the column for w2
!! @param[in] boundary_value array of flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
!! @param[inout] r_u Real array the data 

subroutine ru_code(nlayers,                                                    &
                   r_u,                                                        &
                   ndf_w2, undf_w2, map_w2, boundary_value_w2                  &
                   )

  use enforce_bc_mod, only: enforce_bc_w2
  
  !Arguments
  integer, intent(in) :: nlayers
  integer, intent(in) :: ndf_w2
  integer, intent(in) :: undf_w2
  integer, dimension(ndf_w2),   intent(in) :: map_w2
  integer, dimension(ndf_w2,2), intent(in) :: boundary_value_w2

  real(kind=r_def), dimension(undf_w2), intent(inout) :: r_u

  call enforce_bc_w2(nlayers,ndf_w2,undf_w2,map_w2,boundary_value_w2,r_u)
end subroutine ru_code

end module ru_kernel_mod
