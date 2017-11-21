!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

module gungho_extrusion_mod

  use base_mesh_config_mod, only : geometry,                  &
                                   key_from_geometry,         &
                                   base_mesh_geometry_planar, &
                                   base_mesh_geometry_spherical
  use constants_mod,        only : r_def
  use extrusion_mod,        only : extrusion_type,           &
                                   uniform_extrusion_type,   &
                                   quadratic_extrusion_type, &
                                   geometric_extrusion_type, &
                                   dcmip_extrusion_type
  use extrusion_config_mod, only : method, key_from_method,    &
                                   extrusion_method_uniform,   &
                                   extrusion_method_quadratic, &
                                   extrusion_method_geometric, &
                                   extrusion_method_dcmip,     &
                                   domain_top,                 &
                                   number_of_layers
  use log_mod,              only : log_event,       &
                                   log_level_error, &
                                   log_scratch_space
  use planet_config_mod,    only : scaled_radius

  implicit none

  private
  public create_extrusion

contains

  function create_extrusion() result(new)

    implicit none

    class(extrusion_type), pointer :: new

    real(r_def) :: atmosphere_bottom

    select case (geometry)
      case (base_mesh_geometry_planar)
        atmosphere_bottom = 0.0_r_def
      case (base_mesh_geometry_spherical)
        atmosphere_bottom = scaled_radius
      case default
        write( log_scratch_space,                 &
               '("Unrecognised geometry: ", A)' ) &
             key_from_geometry( geometry )
        call log_event( log_scratch_space, log_level_error )
    end select

    select case (method)
      case (extrusion_method_uniform)
        allocate( new, source=uniform_extrusion_type( atmosphere_bottom, &
                                                      domain_top,        &
                                                      number_of_layers ) )
      case (extrusion_method_quadratic)
        allocate( new, source=quadratic_extrusion_type( atmosphere_bottom, &
                                                        domain_top,        &
                                                        number_of_layers ) )
      case (extrusion_method_geometric)
        allocate( new, source=geometric_extrusion_type( atmosphere_bottom, &
                                                        domain_top,        &
                                                        number_of_layers ) )
      case (extrusion_method_dcmip)
        allocate( new, source=dcmip_extrusion_type( atmosphere_bottom, &
                                                    domain_top,        &
                                                    number_of_layers ) )
      case default
        write( log_scratch_space,                         &
               '("Unrecognised extrusion method: ", A)' ) &
             key_from_method( method )
        call log_event( log_scratch_space, log_level_error )
    end select

  end function create_extrusion

end module gungho_extrusion_mod
