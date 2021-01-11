! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE skeleton_initialise_skeleton_mod

IMPLICIT NONE

PRIVATE

PUBLIC :: skeleton_initialise_skeleton

CONTAINS

SUBROUTINE skeleton_initialise_skeleton()

! skeleton modules
USE skeleton_namelists_mod, ONLY: skeleton_config
!USE lfricinp_initialise_um_mod, ONLY: um_input_file
!USE um2lfric_check_input_data_mod, ONLY: um2lfric_check_input_data
!USE um2lfric_regrid_weights_mod, ONLY: um2lfric_regrid_weightsfile_ctl

! lfricinputs modules
USE lfricinp_stashmaster_mod, ONLY: lfricinp_read_stashmaster
USE lfricinp_stash_to_lfric_map_mod, ONLY: lfricinp_init_stash_to_lfric_map
USE lfricinp_um_grid_mod, ONLY: lfricinp_set_grid_from_file

IMPLICIT NONE

! Read in UM stashmaster
CALL lfricinp_read_stashmaster(skeleton_config%stashmaster_file)

! Map stashcodes to lfric field names
CALL lfricinp_init_stash_to_lfric_map()

! Check input file
!CALL um2lfric_check_input_data(um_input_file)
!
! Initialise the grid
!CALL lfricinp_set_grid_from_file(um_input_file,     &
!                                 num_snow_layers,   &
!                                 num_surface_types)

!
!! Read in and process weights files
!CALL um2lfric_regrid_weightsfile_ctl()

END SUBROUTINE skeleton_initialise_skeleton

END MODULE skeleton_initialise_skeleton_mod
