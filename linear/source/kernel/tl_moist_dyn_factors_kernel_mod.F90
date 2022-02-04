!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Compute the tangent linear for moisture-dependent factors
module tl_moist_dyn_factors_kernel_mod

    use argument_mod,                  only: arg_type,          &
                                             GH_FIELD, GH_REAL, &
                                             GH_SCALAR,         &
                                             GH_WRITE, GH_READ, &
                                             CELL_COLUMN
    use constants_mod,                 only: r_def, i_def
    use fs_continuity_mod,             only: Wtheta
    use kernel_mod,                    only: kernel_type

    implicit none

    private

    !--------------------------------------------------------------------------
    ! Public types
    !--------------------------------------------------------------------------
    !> The type declaration for the kernel. Contains the metadata needed by the Psy layer
    type, public, extends(kernel_type) :: tl_moist_dyn_factors_kernel_type
        private
        type(arg_type) :: meta_args(3) = (/                   &
             arg_type(GH_FIELD*3, GH_REAL, GH_WRITE, Wtheta), &
             arg_type(GH_FIELD*6, GH_REAL, GH_READ,  Wtheta), &
             arg_type(GH_SCALAR,  GH_REAL, GH_READ)           &
             /)
        integer :: operates_on = CELL_COLUMN
    contains
        procedure, nopass :: tl_moist_dyn_factors_code
    end type

    !--------------------------------------------------------------------------
    ! Contained functions/subroutines
    !--------------------------------------------------------------------------
    public :: tl_moist_dyn_factors_code

contains

    !> @brief Compute the tangent linear moist dynamical factors
    !! @param[in]     nlayers       Integer the number of layers
    !! @param[in,out] moist_dyn_gas Change in Gas factor (m_v / epsilon)
    !! @param[in,out] moist_dyn_tot Change in Total mass factor (sum m_x)
    !! @param[in,out] moist_dyn_fac Change in Water factor
    !! @param[in]     mr_v          Change in Water vapour mixing ratio
    !! @param[in]     mr_cl         Change in Liquid cloud mixing ratio
    !! @param[in]     mr_r          Change in Rain mixing ratio
    !! @param[in]     mr_ci         Change in Ice cloud mixing ratio
    !! @param[in]     mr_s          Change in Snow mixing ratio
    !! @param[in]     mr_g          Change in Graupel mixing ratio
    !! @param[in]     recip_epsilon Reciprocal of ratio molecular mass of
    !!                              water to dry air
    !! @param[in]     ndf_wtheta    The number of degrees of freedom per cell
    !!                              for wtheta
    !! @param[in]     udf_wtheta    The number of total degrees of freedom for
    !!                              wtheta
    !! @param[in]     map_wtheta    Integer array holding the dofmap for the
    !!                              cell at the base of the column
    subroutine tl_moist_dyn_factors_code( nlayers, moist_dyn_gas,        &
                                          moist_dyn_tot, moist_dyn_fac,  &
                                          mr_v, mr_cl, mr_r, mr_ci,      &
                                          mr_s, mr_g, recip_epsilon,     &
                                          ndf_wtheta, undf_wtheta, map_wtheta )

        implicit none

        ! Arguments
        integer(kind=i_def), intent(in) :: nlayers, ndf_wtheta, undf_wtheta

        real(kind=r_def), dimension(undf_wtheta),   intent(inout) :: moist_dyn_gas,       &
                                                                     moist_dyn_tot,       &
                                                                     moist_dyn_fac
        real(kind=r_def), dimension(undf_wtheta),   intent(in)    :: mr_v, mr_cl, mr_r
        real(kind=r_def), dimension(undf_wtheta),   intent(in)    :: mr_ci, mr_s, mr_g
        real(kind=r_def),                           intent(in)    :: recip_epsilon
        integer(kind=i_def), dimension(ndf_wtheta), intent(in)    :: map_wtheta

        ! Internal variables
        integer(kind=i_def)                 :: k, df

        real(kind=r_def)                    :: mr_v_at_dof, mr_cl_at_dof, mr_r_at_dof
        real(kind=r_def)                    :: mr_ci_at_dof, mr_s_at_dof, mr_g_at_dof

        ! compute the pointwise mr profile
        do k = 0, nlayers-1

          do df = 1, ndf_wtheta
            mr_v_at_dof  = mr_v(map_wtheta(df) + k)
            mr_cl_at_dof = mr_cl(map_wtheta(df) + k)
            mr_r_at_dof  = mr_r(map_wtheta(df) + k)
            mr_ci_at_dof = mr_ci(map_wtheta(df) + k)
            mr_s_at_dof  = mr_s(map_wtheta(df) + k)
            mr_g_at_dof  = mr_g(map_wtheta(df) + k)
            moist_dyn_gas(map_wtheta(df) + k) = recip_epsilon * mr_v_at_dof
            moist_dyn_tot(map_wtheta(df) + k) = mr_v_at_dof  +  &
                                                mr_cl_at_dof +  &
                                                mr_r_at_dof  +  &
                                                mr_ci_at_dof +  &
                                                mr_s_at_dof  +  &
                                                mr_g_at_dof
            moist_dyn_fac(map_wtheta(df) + k) = 0.0_r_def
          end do

        end do

    end subroutine tl_moist_dyn_factors_code

end module tl_moist_dyn_factors_kernel_mod
