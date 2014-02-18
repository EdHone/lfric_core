module num_dof_mod
! computes the global and local number of dofs for 2D elements
! needs extending to 3D ( + addition of V2 space)

implicit none

! numbers of dofs in each space for an element 
integer :: nv0,nv2,nv2_edge,nv2_elem,nv3,nv3_elem,nv3_edge,nv3_vert
integer :: nv0_elem, nv0_edge, nv0_vert

! global numbers of unique dofs
integer :: nv0_g, nv2_g, nv3_g
logical ::  l_biperiodic, l_spherical

contains 

subroutine num_dof_init(ne,k)

! number of elements
integer, intent(in) :: ne
! order of RT space ( = 0 for lowest order)
integer, intent(in) :: k

! global numbers based on grid topology
integer :: nedge_g, nvert_g

l_biperiodic=.true.
l_spherical=.false.


! local values
nv3 = (k+1)*(k+1)

nv2 = 2*(k+2)*(k+1)
nv2_edge = (k+1)
nv2_elem = 2*k*(k+1)

nv0 = (k+2)*(k+2)
nv0_elem = k*k
nv0_edge = k
nv0_vert = 4


! global values
nedge_g = 2*ne
! topologically a torus
if ( l_biperiodic ) nvert_g = ne
! topologically a cube
if ( l_spherical  ) nvert_g = ne + 2

nv3_g = ne*nv3

nv2_g = ne*(nv2_elem + 2*nv2_edge)

nv0_g = ne*(nv0_elem + 2*nv0_edge) + nvert_g
 
end subroutine num_dof_init


end module num_dof_mod
