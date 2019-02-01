!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! UNV TO GMESH CONVERSION UTILITY
!   Output Functions Module:
!
!>    This module contains functionality necessary to output Gmesh format
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
MODULE printoutfunctions
    USE globalvariables
    USE globalfunctions
    IMPLICIT NONE
CONTAINS

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Output elements, node, and material data to the Gmesh file
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE printoutgmesh
        !open output file
        OPEN(UNIT=12,FILE=fileout,STATUS='REPLACE',ACTION='WRITE',IOSTAT=ios,IOMSG=tempcharacter)
        IF(ios .NE. 0)THEN
            WRITE(*,*)tempcharacter
            STOP
        END IF

        !output standard format information
        WRITE(12,'(A)')'$MeshFormat'
        WRITE(12,'(A)')'2.0 0 8'
        WRITE(12,'(A)')'$EndMeshFormat'
        WRITE(12,'(A)')'$Nodes'
        WRITE(12,'(I0)')numnodes
        DO i=1,numnodes
            WRITE(12,'(I0,3ES26.16)')i,nodes(i,:)
        END DO
        WRITE(12,'(A)')'$EndNodes'
        WRITE(12,'(A)')'$Elements'
        WRITE(12,'(I0)')numelements

        !here is the important part, libmesh doesn't use material to specify region, the elements(i,4) write does that
        DO i=1,numelements
            WRITE(12,'(I0,5I2,4I6)')i,4,3,elements(i,4)-1,0,1,elements(i,7:10)
        END DO
        WRITE(12,'(A)')'$EndElements'

        !close output file
        CLOSE(UNIT=12,IOSTAT=ios)
        IF(ios .NE. 0)THEN
            WRITE(*,*)'Could not close input file ', fileout
            STOP
        END IF
    END SUBROUTINE printoutgmesh
END MODULE printoutfunctions
