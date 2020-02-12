!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2019.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!> @brief Initialise Jules fields
!> @details Non-standard Surface fields (pseudo-levels) aren't as yet not implemented in LFRic.
!> As an interim measure Higher-order W3 fields have been used to mimic psuedo-level field behaviour.
!> This code is written !> based on this interim measure and will need to be updated when
!> suitable infrastructure is available (Ticket #2081)

module initial_jules_kernel_mod

  use argument_mod, only: arg_type, &
    GH_FIELD, GH_INTEGER, GH_WRITE, GH_READ,              &
    ANY_DISCONTINUOUS_SPACE_1, ANY_DISCONTINUOUS_SPACE_2, &
    ANY_DISCONTINUOUS_SPACE_3, ANY_DISCONTINUOUS_SPACE_4, &
    CELLS
  use constants_mod, only: r_def, i_def
  use kernel_mod, only: kernel_type
  use surface_config_mod, only: surf_tile_fracs
  use idealised_config_mod, only: test, test_snow

  implicit none

  private

  !> Kernel metadata for Psyclone
  type, public, extends(kernel_type) :: initial_jules_kernel_type
      private
      type(arg_type) :: meta_args(17) = (/                           &
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), & ! tile_fraction
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_2), & ! leaf_area_index
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_2), & ! canopy_height
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), & ! tile_temperature
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), & ! tile_snow_mass
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), & ! n_snow_layers
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), & ! snow_depth
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_3), & ! soil_temperature
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_3), & ! soil_moisture
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_3), & ! unfrozen_soil_moisture
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_3), & ! frozen_soil_moisture
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_4), & ! snow_layer_thickness
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_4), & ! snow_layer_ice_mass
          arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_4), & ! snow_layer_temp
          arg_type(GH_INTEGER, GH_READ             ),                & ! n_surf_tile
          arg_type(GH_INTEGER, GH_READ             ),                & ! n_land_tile
          arg_type(GH_INTEGER, GH_READ             )                 & ! max_snow_levs
          /)
      integer :: iterates_over = CELLS

  contains
      procedure, nopass :: initial_jules_code
  end type

  public initial_jules_code
contains

  !> @param[in]  nlayers            The number of layers
  !> @param[out] tile_fraction      Surface tile fractions
  !> @param[out] leaf_area_index    Leaf Area Index
  !> @param[out] canopy_height      Canopy height (m)
  !> @param[out] tile_temperature   Surface tile temperatures (K)
  !> @param[out] tile_snow_mass     Snow mass on tiles (kg m-2)
  !> @param[out] n_snow_layers      Number of snow layers on tiles
  !> @param[out] snow_depth         Snow depth on tiles (m)
  !> @param[out] soil_temperature   Soil temperature (K)
  !> @param[out] soil_moisture      Soil moisture content (kg m-2)
  !> @param[out] unfrozen_soil_moisture Unfrozen soil moisture proportion
  !> @param[out] frozen_soil_moisture Frozen soil moisture proportion
  !> @param[out] snow_layer_thickness   Thickness of snow layers (m)
  !> @param[out] snow_layer_ice_mass    Mass of ice in snow layers (kg m-2)
  !> @param[out] snow_layer_temp        Temperature of snow layer (K)
  !> @param[in]  n_surf_tile        Number of surface tiles
  !> @param[in]  n_land_tile        Number of land tiles
  !> @param[in]  max_snow_levs      Maximum number of snow layers
  !> @param[in]  ndf_tile           Number of DOFs per cell for tiles
  !> @param[in]  undf_tile          Number of total DOFs for tiles
  !> @param[in]  map_tile           Dofmap for cell for surface tiles
  !> @param[in]  ndf_pft            Number of DOFs per cell for PFTs
  !> @param[in]  undf_pft           Number of total DOFs for PFTs
  !> @param[in]  map_pft            Dofmap for cell for PFTs
  !> @param[in]  ndf_soil           Number of DOFs per cell for soil levels
  !> @param[in]  undf_soil          Number of total DOFs for soil levels
  !> @param[in]  map_soil           Dofmap for cell for soil levels
  !> @param[in]  ndf_snow           Number of DOFs per cell for snow
  !> @param[in]  undf_snow          Number of total DOFs for snow
  !> @param[in]  map_snow           Dofmap for cell for snow fields
  subroutine initial_jules_code(nlayers,                       &
                                tile_fraction,                 &
                                leaf_area_index,               &
                                canopy_height,                 &
                                tile_temperature,              &
                                tile_snow_mass,                &
                                n_snow_layers,                 &
                                snow_depth,                    &
                                soil_temperature,              &
                                soil_moisture,                 &
                                unfrozen_soil_moisture,        &
                                frozen_soil_moisture,          &
                                snow_layer_thickness,          &
                                snow_layer_ice_mass,           &
                                snow_layer_temp,               &
                                n_surf_tile,                   &
                                n_land_tile,                   &
                                max_snow_levs,                 &
                                ndf_tile, undf_tile, map_tile, &
                                ndf_pft, undf_pft, map_pft,    &
                                ndf_soil, undf_soil, map_soil, &
                                ndf_snow, undf_snow, map_snow)

      implicit none

      ! Arguments
      integer(kind=i_def), intent(in) :: nlayers, n_surf_tile, n_land_tile, &
                                         max_snow_levs
      integer(kind=i_def), intent(in) :: ndf_tile, undf_tile
      integer(kind=i_def), intent(in) :: map_tile(ndf_tile)
      integer(kind=i_def), intent(in) :: ndf_pft, undf_pft
      integer(kind=i_def), intent(in) :: map_pft(ndf_pft)
      integer(kind=i_def), intent(in) :: ndf_soil, undf_soil
      integer(kind=i_def), intent(in) :: map_soil(ndf_soil)
      integer(kind=i_def), intent(in) :: ndf_snow, undf_snow
      integer(kind=i_def), intent(in) :: map_snow(ndf_snow)

      real(kind=r_def), intent(out) :: tile_fraction(undf_tile)
      real(kind=r_def), intent(out) :: tile_temperature(undf_tile)
      real(kind=r_def), intent(out) :: tile_snow_mass(undf_tile)
      real(kind=r_def), intent(out) :: n_snow_layers(undf_tile)
      real(kind=r_def), intent(out) :: snow_depth(undf_tile)

      real(kind=r_def), intent(out) :: leaf_area_index(undf_pft)
      real(kind=r_def), intent(out) :: canopy_height(undf_pft)

      real(kind=r_def), intent(out) :: soil_temperature(undf_soil)
      real(kind=r_def), intent(out) :: soil_moisture(undf_soil)
      real(kind=r_def), intent(out) :: unfrozen_soil_moisture(undf_soil)
      real(kind=r_def), intent(out) :: frozen_soil_moisture(undf_soil)

      real(kind=r_def), intent(out) :: snow_layer_thickness(undf_snow)
      real(kind=r_def), intent(out) :: snow_layer_ice_mass(undf_snow)
      real(kind=r_def), intent(out) :: snow_layer_temp(undf_snow)

      !Internal variables
      integer(kind=i_def) :: i, j, i_snow, indexes(max_snow_levs)

      ! Tile fraction
      do i = 1, n_surf_tile
          tile_fraction(map_tile(i)) = surf_tile_fracs(i)
      end do

      ! Leaf area index
      leaf_area_index(map_pft(1)) = 5.0_r_def
      leaf_area_index(map_pft(2)) = 4.0_r_def
      leaf_area_index(map_pft(3)) = 2.0_r_def
      leaf_area_index(map_pft(4)) = 4.0_r_def
      leaf_area_index(map_pft(5)) = 1.0_r_def

      ! Canopy height
      canopy_height(map_pft(1)) = 19.01_r_def
      canopy_height(map_pft(2)) = 16.38_r_def
      canopy_height(map_pft(3)) =  1.46_r_def
      canopy_height(map_pft(4)) =  1.26_r_def
      canopy_height(map_pft(5)) =  1.59_r_def

      if (test == test_snow) then
      ! Testing with snow present

        do i = 1, n_surf_tile
        ! Tile temperature
          tile_temperature(map_tile(i)) = 270.0_r_def
        ! Snow mass on tiles
          tile_snow_mass(map_tile(i)) = 50.0_r_def
        ! Number of snow layers on tiles
          n_snow_layers(map_tile(i)) = 3.0_r_def
        ! Snow depth on tiles
          snow_depth(map_tile(i)) = 2.0_r_def
        end do

        ! Soil temperature
        soil_temperature(map_soil(1)) = 270.0_r_def
        soil_temperature(map_soil(2)) = 270.0_r_def
        soil_temperature(map_soil(3)) = 275.0_r_def
        soil_temperature(map_soil(4)) = 275.0_r_def

        ! Soil moisture content
        soil_moisture(map_soil(1)) = 40.0_r_def
        soil_moisture(map_soil(2)) = 90.0_r_def
        soil_moisture(map_soil(3)) = 150.0_r_def
        soil_moisture(map_soil(4)) = 460.0_r_def

        ! Unfrozen soil moisture proportion
        unfrozen_soil_moisture(map_soil(1)) = 0.46896_r_def
        unfrozen_soil_moisture(map_soil(2)) = 0.46911_r_def
        unfrozen_soil_moisture(map_soil(3)) = 0.51408_r_def
        unfrozen_soil_moisture(map_soil(4)) = 0.51096_r_def

        ! Frozen soil moisture proportion
        frozen_soil_moisture(map_soil(1)) = 0.42210_r_def
        frozen_soil_moisture(map_soil(2)) = 0.33285_r_def
        frozen_soil_moisture(map_soil(3)) = 0.0_r_def
        frozen_soil_moisture(map_soil(4)) = 0.0_r_def

      else ! All other tests without snow

        do i = 1, n_surf_tile
        ! Tile temperature
          tile_temperature(map_tile(i)) = 295.0_r_def
        ! Snow mass on tiles
          tile_snow_mass(map_tile(i)) = 0.0_r_def
        ! Number of snow layers on tiles
          n_snow_layers(map_tile(i)) = 0.0_r_def
        ! Snow depth on tiles
          snow_depth(map_tile(i)) = 0.0_r_def
        end do

        ! Soil temperature
        soil_temperature(map_soil(1)) = 285.0_r_def
        soil_temperature(map_soil(2)) = 280.0_r_def
        soil_temperature(map_soil(3)) = 275.0_r_def
        soil_temperature(map_soil(4)) = 275.0_r_def

        ! Soil moisture content
        soil_moisture(map_soil(1)) = 40.0_r_def
        soil_moisture(map_soil(2)) = 90.0_r_def
        soil_moisture(map_soil(3)) = 150.0_r_def
        soil_moisture(map_soil(4)) = 460.0_r_def

        ! Unfrozen soil moisture proportion
        unfrozen_soil_moisture(map_soil(1)) = 0.89107_r_def
        unfrozen_soil_moisture(map_soil(2)) = 0.80196_r_def
        unfrozen_soil_moisture(map_soil(3)) = 0.51408_r_def
        unfrozen_soil_moisture(map_soil(4)) = 0.51236_r_def

        ! Frozen soil moisture proportion
        frozen_soil_moisture(map_soil(1)) = 0.0_r_def
        frozen_soil_moisture(map_soil(2)) = 0.0_r_def
        frozen_soil_moisture(map_soil(3)) = 0.0_r_def
        frozen_soil_moisture(map_soil(4)) = 0.0_r_def

      end if

      i_snow = 0
      do i = 1, n_land_tile

        do j = 1, max_snow_levs
          i_snow = i_snow + 1
          indexes(j) = map_snow(i_snow)
        end do

        snow_layer_thickness(indexes) = (/ 0.04_r_def, 0.12_r_def, 1.5_r_def /)
        snow_layer_ice_mass(indexes)  = (/ 8.0_r_def, 25.0_r_def, 50.0_r_def /)
        snow_layer_temp(indexes) = (/ 260.0_r_def, 265.0_r_def, 270.0_r_def /)

      end do

  end subroutine initial_jules_code

end module initial_jules_kernel_mod
