!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! UNV TO GMESH CONVERSION UTILITY
!   Convert unv format to gmsh format:
!
!> @mainpage UNV to Gmesh Converter
!!    This pre-processor provides mesh format conversion capabilities. It is
!!    designed to allow for a simple workflow from UNV to GMESH.
!
!>   This driver provides the control structure for all entities within the
!!   UNVTOGMESH utility. It parses command line input, opens the input
!!   file, and prints out the input in GMESH style.
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
PROGRAM hammercoupler
    USE globalvariables
    USE readinfunctions
    USE printoutfunctions
    USE globalfunctions
    IMPLICIT NONE

    !get command line information
    CALL readincommandline()

    !read in unv mesh information
    CALL readinunv()
    WRITE(*,'(A)')"Mesh read in without error"

    !print out gmesh
    CALL printoutgmesh()
    WRITE(*,'(A)')"Program completed without error"
END PROGRAM hammercoupler
