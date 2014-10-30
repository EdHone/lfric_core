!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
module evaluate_output_field_mod

use constants_mod,           only: r_def, earth_radius, large_real
use field_mod,               only: field_type, field_proxy_type
use coordinate_jacobian_mod, only: coordinate_jacobian, coordinate_jacobian_inverse
use mesh_generator_mod,      only: llr2xyz 
use mesh_mod,                only: l_spherical
use coord_algorithms_mod,    only: cartesian_distance


implicit none

contains
!>@brief evaluates a field at a point (x_in) that horizontally lies within a given cell
!>@detail Subroutine that evaluates a field at a vertical column of given points.
!>        The column of points is taken to lie within a given column of grid
!>        cells. A newton method is used to compute the exact point within each
!>        cell where a point lies and then the field is evaluated at this point
!>        using the known basis functions.
!>@deprecated This is a temporary solution until a better output routine is
!>implemented as which point this routine will be reviewed to see if it will be
!>needed elsewhere in the model
!>@param[in]  field     The field object to evaluate
!>@param[in]  chi       The 3D coordinate field
!>@param[in]  x_in      The point to evaluate the field at
!>@param[in]  cell      The horizontal cell that x_in lies within
!>@param[in]  nz        The number of vertical points to evaluate
!>@param[out] field_out The array containing field evaluates at x_in
subroutine evaluate_output_field( field, chi, x_in, cell, nz, field_out)

  type(field_type), intent(in)  :: field, chi(3) 
  integer,          intent(in)  :: cell, nz
  real(kind=r_def), intent(in)  :: x_in(3,nz)
  real(kind=r_def), intent(out) :: field_out(nz)

  type(field_proxy_type)        :: chi_proxy(3), field_proxy

  integer, pointer              :: map(:)   => null(), &
                                   map_f(:) => null()
  integer                       :: iter, ndf, ndf_f, k, df, &
                                   dir, layer, nlayers, dfk
  integer, allocatable          :: out_layer(:)
  integer,          parameter   :: newton_iters = 4
  real(kind=r_def)              :: jac(3,3), jac_inv(3,3), dj(1,1), &
                                   g_func(3), gamma(1), x_loc(3), x_out(3), &
                                   distance, min_distance
  real(kind=r_def), allocatable :: chi_cell(:,:), dgamma(:,:), x_cell(:,:)

  chi_proxy(1) = chi(1)%get_proxy()
  chi_proxy(2) = chi(2)%get_proxy()
  chi_proxy(3) = chi(3)%get_proxy()
  field_proxy  = field%get_proxy()

  map => chi_proxy(1)%vspace%get_cell_dofmap(cell)
  ndf = chi_proxy(1)%vspace%get_ndf() 
  map_f => field_proxy%vspace%get_cell_dofmap(cell)
  ndf_f = field_proxy%vspace%get_ndf() 
  nlayers = field_proxy%vspace%get_nlayers()

  allocate ( chi_cell(3,ndf), dgamma(3,ndf), x_cell(3,nlayers), out_layer(nz) )

! Compute cell centre coordinates for each layer
  x_out(:) = (/ 0.5_r_def, 0.5_r_def, 0.5_r_def /)
  do k = 1, nlayers
    x_cell(:,k) = 0.0_r_def
    do df = 1,ndf
      gamma(:) =  chi_proxy(1)%vspace%evaluate_basis(df, x_out)
      dfk = map(df) + k - 1
      do dir = 1,3
        x_cell(dir,k) =  x_cell(dir,k) &
                      + gamma(1)*chi_proxy(dir)%data(dfk)
      end do
    end do
  end do

! Find which vertical layer each output point lies in based upon the 
! shortest distance from each cell centre
  do k = 1,nz 
    x_loc(:) = x_in(:,k)
    if ( l_spherical ) call llr2xyz(x_in(1,k), x_in(2,k), x_in(3,k), &
                                    x_loc(1), x_loc(2), x_loc(3))
    min_distance = large_real
    do layer = 1,nlayers
      distance = cartesian_distance(x_loc, x_cell(:,layer))
      if ( distance < min_distance ) then
        out_layer(k) = layer
        min_distance = distance
      end if
    end do
  end do

! Find the computational coordinates (x_out in [0,1]^3) corresponding to each 
! input point using a newton method with  a fixed number of iterations
  do k = 1, nz
    do df = 1,ndf 
      chi_cell(1,df) = chi_proxy(1)%data( map(df) + out_layer(k) - 1 )
      chi_cell(2,df) = chi_proxy(2)%data( map(df) + out_layer(k) - 1 )
      chi_cell(3,df) = chi_proxy(3)%data( map(df) + out_layer(k) - 1 )
    end do
! First guess of out point in reference element
    x_out(:) = (/ 0.5_r_def, 0.5_r_def, 0.5_r_def /)
    x_loc(:) = x_in(:,k)
    if ( l_spherical ) call llr2xyz( x_in(1,k), &
                                     x_in(2,k), &
                                     x_in(3,k), &
                                     x_loc(1),  &
                                     x_loc(2),  &
                                     x_loc(3))

! Find location in computational space of point to evaluate field at  
    do iter = 1,newton_iters
      do df = 1,ndf
        dgamma(:,df) = chi_proxy(1)%vspace%evaluate_diff_basis(df, x_out)
      end do
      call coordinate_jacobian( ndf, &
                                1,   &
                                1,   &
                                chi_cell(1,:), &
                                chi_cell(2,:), &
                                chi_cell(3,:), &
                                dgamma, &
                                jac, &
                                dj)  
      call coordinate_jacobian_inverse(1, 1, jac, dj, jac_inv)
! compute g(xi^n) - [x,y,z]
      g_func = -earth_radius*x_loc(:)
      do df = 1, ndf
        gamma(:) = chi_proxy(1)%vspace%evaluate_basis(df, x_out)
        do dir = 1,3
          g_func(dir) = g_func(dir) + chi_cell(dir,df)*gamma(1)
        end do
      end do
      x_out(:) = x_out(:) - matmul(jac_inv, g_func)
    end do
! Evaluate field at xi  
    field_out(k) = 0.0_r_def
    do df = 1, ndf_f
      gamma(:) = field_proxy%vspace%evaluate_basis(df,x_out)
      dfk = map_f(df) + out_layer(k) - 1
      field_out(k) = field_out(k) + gamma(1)*field_proxy%data(dfk)
    end do
  end do

  end subroutine evaluate_output_field


end module evaluate_output_field_mod


