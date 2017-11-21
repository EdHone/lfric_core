!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> Provides extrusion methods for converting a 2D mesh to a unitless 3D mesh.
!>
module extrusion_mod

  use constants_mod, only : i_def, r_def

  implicit none

  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> All extrusion implementations inherit from this class.
  !>
  type, public, abstract :: extrusion_type

    private

    real(r_def)    :: atmosphere_bottom
    real(r_def)    :: atmosphere_top
    integer(i_def) :: number_of_layers

  contains

    private

    procedure, public :: get_atmosphere_bottom
    procedure, public :: get_atmosphere_top
    procedure, public :: get_number_of_layers
    procedure(extrude_method), public, deferred :: extrude

    procedure :: extrusion_constructor

  end type extrusion_type

  interface
    subroutine extrude_method( this, eta )
      import extrusion_type, r_def
      class(extrusion_type), intent(in)  :: this
      real(r_def),           intent(out) :: eta(0:this%number_of_layers)
    end subroutine extrude_method
  end interface

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes with equel distribution of layers.
  !>
  type, public, extends(extrusion_type) :: uniform_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => uniform_extrude
  end type uniform_extrusion_type

  interface uniform_extrusion_type
    module procedure uniform_extrusion_constructor
  end interface uniform_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes with layers x^2 layers.
  !>
  type, public, extends(extrusion_type) :: quadratic_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => quadratic_extrude
  end type quadratic_extrusion_type

  interface quadratic_extrusion_type
    module procedure quadratic_extrusion_constructor
  end interface quadratic_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes with "geometric" layers.
  !>
  type, public, extends(extrusion_type) :: geometric_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => geometric_extrude
  end type geometric_extrusion_type

  interface geometric_extrusion_type
    module procedure geometric_extrusion_constructor
  end interface geometric_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes using DCMIP scheme.
  !>
  type, public, extends(extrusion_type) :: dcmip_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => dcmip_extrude
  end type dcmip_extrusion_type

  interface dcmip_extrusion_type
    module procedure dcmip_extrusion_constructor
  end interface dcmip_extrusion_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Creates a uniform_extrusion_type object.
  !>
  function uniform_extrusion_constructor( atmosphere_bottom, &
                                          atmosphere_top,    &
                                          number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(uniform_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function uniform_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes the mesh to give constant delta between layers.
  !>
  subroutine uniform_extrude( this, eta )

    implicit none

    class(uniform_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = real(k,r_def)/real(this%number_of_layers,r_def)
    end do

  end subroutine uniform_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Creates a quadratic_extrusion_type object.
  !>
  function quadratic_extrusion_constructor( atmosphere_bottom, &
                                            atmosphere_top,    &
                                            number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(quadratic_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function quadratic_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes the mesh to give x^2 delta between layers.
  !>
  subroutine quadratic_extrude( this, eta )

    implicit none

    class(quadratic_extrusion_type), intent(in)  :: this
    real(r_def),                     intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = ( real(k,r_def)/real(this%number_of_layers,r_def) )**2_i_def
    end do

  end subroutine quadratic_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Creates a geometric_extrusion_type object.
  !>
  function geometric_extrusion_constructor( atmosphere_bottom, &
                                            atmosphere_top,    &
                                            number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(geometric_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function geometric_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes the mesh to give a John Thuburn ENDGame non-staggered grid.
  !>
  subroutine geometric_extrude( this, eta )

    implicit none

    class(geometric_extrusion_type), intent(in)  :: this
    real(r_def),                     intent(out) :: eta(0:this%number_of_layers)

    real(r_def), parameter :: stretching_factor = 1.03_r_def

    integer(i_def) :: k
    real(r_def)    :: delta_eta

    delta_eta = (stretching_factor - 1.0_r_def) &
                / (stretching_factor**(this%number_of_layers) - 1.0_r_def)

    eta(0) = 0.0_r_def
    do k = 1, this%number_of_layers
      eta(k) = eta(k-1) + delta_eta
      delta_eta = delta_eta*stretching_factor
    end do

  end subroutine geometric_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Creates a dcmip_extrusion_type object.
  !>
  function dcmip_extrusion_constructor( atmosphere_bottom, &
                                        atmosphere_top,    &
                                        number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(dcmip_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function dcmip_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Extrudes the mesh using the DCMIP scheme.
  !>
  !> For more information see DCMIP-TestCaseDocument_v1.7.pdf,
  !> Appendix F.2. - Eq. 229.
  !>
  subroutine dcmip_extrude( this, eta )

    implicit none

    class(dcmip_extrusion_type), intent(in)  :: this
    real(r_def),                 intent(out) :: eta(0:this%number_of_layers)

    real(r_def), parameter :: phi_flatten = 15.0_r_def

    integer(i_def) :: k
    real(r_def)    :: eta_uni

    do k = 0, this%number_of_layers
      eta_uni = real(k,r_def)/real(this%number_of_layers,r_def)
      eta(k) = ( sqrt(phi_flatten*(eta_uni**2_i_def) + 1.0_r_def) &
                      - 1.0_r_def ) / &
                    ( sqrt(phi_flatten + 1.0_r_def) - 1.0_r_def )
    end do

  end subroutine dcmip_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Initialises the extrusion base class.
  !>
  !> This method should be called from child method constructors in order to
  !> populate the parent fields.
  !>
  !> @param [in] atmosphere_bottom Bottom of the atmosphere (planet surface)
  !>                               in meters.
  !> @param [in] atmosphere_top Top of the atmosphere in meters.
  !> @param [in] number_of_layers Number of layers to split atmosphere into.
  !>
  subroutine extrusion_constructor( this,              &
                                    atmosphere_bottom, &
                                    atmosphere_top,    &
                                    number_of_layers )

    implicit none

    class(extrusion_type), intent(inout) :: this
    real(r_def),           intent(in) :: atmosphere_bottom
    real(r_def),           intent(in) :: atmosphere_top
    integer(i_def),        intent(in) :: number_of_layers

    this%atmosphere_bottom = atmosphere_bottom
    this%atmosphere_top    = atmosphere_top
    this%number_of_layers  = number_of_layers

  end subroutine extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the bottom of the atmosphere or the surface of the planet.
  !>
  !> @return Bottom of the atmosphere in meters.
  !>
  function get_atmosphere_bottom( this ) result(bottom)

    implicit none

    class(extrusion_type), intent(in) :: this
    real(r_def) :: bottom

    bottom = this%atmosphere_bottom

  end function get_atmosphere_bottom

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the top of the atmosphere.
  !>
  !> @return Top of the atmosphere in meters.
  !>
  function get_atmosphere_top( this ) result(top)

    implicit none

    class(extrusion_type), intent(in) :: this
    real(r_def) :: top

    top = this%atmosphere_top

  end function get_atmosphere_top

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the number of layers in the atmosphere.
  !>
  !> @return Number of layers.
  !>
  function get_number_of_layers( this ) result(layers)

    implicit none

    class(extrusion_type), intent(in) :: this
    integer(i_def) :: layers

    layers = this%number_of_layers

  end function get_number_of_layers

end module extrusion_mod
