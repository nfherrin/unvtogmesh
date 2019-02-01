!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! UNV TO GMESH CONVERSION UTILITY
!   Global Variables Module:
!
!>    This module contains variables used to store data common to all other
!!    modules in the utility.
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
MODULE globalvariables
    IMPLICIT NONE

    !> Name of the input unv mesh file
    CHARACTER(64)::filein
    !> Name of the output gmesh mesh file
    CHARACTER(64)::fileout
    !> Temporary data holder for the whole system
    CHARACTER(64)::tempcharacter
    !> Loop control
    INTEGER::i,j
    !> File error detector
    INTEGER::ios
    !> Number of arguments in the command line
    INTEGER::arg_count
    !> Number of nodes in the mesh
    INTEGER::numnodes
    !> Number of elements in the mesh
    INTEGER::numelements
    !> Info for each element in the list, first dimension is element number,
    !! column 4 is material, columns 7-10 are nodes that make up the tet
    !! should be allocated as (numelements,10)
    INTEGER,ALLOCATABLE::elements(:,:)
    !> Conversion factor for the nodes data, (we output in units of cm)
    REAL(8)::conversion
    !> Node coordinates, x,y,z, for each node, allocated as (numnodes,3)
    REAL(8),ALLOCATABLE::nodes(:,:)
END MODULE globalvariables
