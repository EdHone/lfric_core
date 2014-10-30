!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
module find_output_cell_mod

use constants_mod,        only: r_def, large_real
use mesh_generator_mod,   only: xyz2llr 
use field_mod,            only: field_type, field_proxy_type 
use mesh_mod,             only: l_spherical
use coord_algorithms_mod, only: cartesian_distance

implicit none

contains
!>@brief  function to find which horizontal grid cell a given point lies in
!>@detail Finds the horizontal cell (out_cell) that a given point (x_in)
!>        lies within as measured by the least distance from the cell centre
!>@deprecated This is a temporary solution until a better output routine is
!>implemented as which point this routine will be reviewed to see if it will be
!>needed elsewhere in the model
!>@param[in]  chi      The 3D coordinate field
!>@param[in]  x_in     The point to find which cell it lies in
!>@param[out] out_cell The cell x_in lies in
  function find_output_cell( chi, x_in ) result( out_cell )

  type(field_type), intent(in)    :: chi(3)
  real(kind=r_def), intent(in)    :: x_in(3)
  integer                         :: out_cell

  type(field_proxy_type)          :: chi_proxy(3)
  real(kind=r_def)                :: min_distance, distance
  integer                         :: df, cell
  integer, pointer                :: map(:) => null()
  real(kind=r_def)                :: xi(3), gamma(1), chi_cell(3), x_phys(3)

! find horizontal centre point of cell  
  xi = (/ 0.5_r_def, 0.5_r_def, 0.0_r_def /)

  chi_proxy(1) = chi(1)%get_proxy()
  chi_proxy(2) = chi(2)%get_proxy()
  chi_proxy(3) = chi(3)%get_proxy()

  min_distance = large_real
  do cell = 1,chi_proxy(1)%vspace%get_ncell()
    map => chi_proxy(1)%vspace%get_cell_dofmap(cell)
    chi_cell(:) = 0.0_r_def
    do df = 1,chi_proxy(1)%vspace%get_ndf()
      gamma(:) = chi_proxy(1)%vspace%evaluate_basis(df, xi)
      chi_cell(1) = chi_cell(1) + gamma(1)*chi_proxy(1)%data(map(df))
      chi_cell(2) = chi_cell(2) + gamma(1)*chi_proxy(2)%data(map(df))
      chi_cell(3) = chi_cell(3) + gamma(1)*chi_proxy(3)%data(map(df))
    end do
    if ( l_spherical ) then
      call xyz2llr( chi_cell(1),  &
                    chi_cell(2),  &
                    chi_cell(3),  &
                    x_phys(1), & 
                    x_phys(2), & 
                    x_phys(3))
    else
      x_phys(:) = chi_cell(:)
    end if

    distance = cartesian_distance(x_in, x_phys)

    if ( distance < min_distance ) then
      out_cell = cell
      min_distance = distance
    end if
  end do

  end function find_output_cell 

end module find_output_cell_mod

