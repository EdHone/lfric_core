! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE lfricinp_finalise_lfric_mod

IMPLICIT NONE

PRIVATE

PUBLIC :: lfricinp_finalise_lfric

CONTAINS

SUBROUTINE lfricinp_finalise_lfric()

! Description:
!  Call finalise routines for associated APIs and logging system

USE log_mod,                   ONLY: finalise_logging, LOG_LEVEL_INFO, &
                                     log_event
! External libraries
USE xios,                      ONLY: xios_finalize
USE yaxt,                      ONLY: xt_finalize
USE mpi_mod,                   ONLY: finalise_comm


IMPLICIT NONE

CALL log_event( 'Calling lfric finalise routines', LOG_LEVEL_INFO )

! Finalise YAXT, XIOS, MPI, etc.
CALL xt_finalize()
CALL xios_finalize()
CALL finalise_comm()

! Finalise the logging system
CALL finalise_logging()

END SUBROUTINE lfricinp_finalise_lfric


END MODULE lfricinp_finalise_lfric_mod
