!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! UNV TO GMESH CONVERSION UTILITY
!   Input Functions Module:
!
!>    This module contains functionality necessary to ingest UNV input
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
MODULE readinfunctions
    USE globalvariables
    USE globalfunctions
    IMPLICIT NONE
CONTAINS

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Gets command line arguments, and prompts for input if command line arguments
    !! not present
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE readincommandline
        !Get number of command line args and check for proper invocation, return error if to many
        arg_count=COMMAND_ARGUMENT_COUNT()
        IF(arg_count .GT. 2)STOP 'Only two argument variables are allowed for this program! "filein" "fileout"'

        !either use or prompt for input mesh file name
        IF(arg_count .GE. 1)THEN
            CALL GET_COMMAND_ARGUMENT(1, filein)
        ELSE
            WRITE(*,'(A)')'Input file name?'
            WRITE(*,'(A)',ADVANCE='NO')'> '
            READ(*,*)filein
        END IF

        !either use or prompt for output mesh file name
        IF(arg_count .GE. 2)THEN
            CALL GET_COMMAND_ARGUMENT(2, fileout)
        ELSE
            WRITE(*,'(A)')'Output file name?'
            WRITE(*,'(A)',ADVANCE='NO')'> '
            READ(*,*)fileout
        END IF
    END SUBROUTINE readincommandline

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Extracts elements, node, and material data from the unv file
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE readinunv
        !> Delimiter to find various pieces of the mesh in the unv file
        CHARACTER(64)::delim

        !open input mesh file
        OPEN(UNIT=11,FILE=filein,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
        IF(ios .NE. 0)THEN
            WRITE(*,*)tempcharacter
            STOP
        END IF

        !get to data card
        delim="164"
        CALL finddelim(11,delim)

        !get conversion factor, code outputs units of cm
        READ(11,*)tempcharacter
        SELECT CASE(tempcharacter)
            CASE('1Meter', '3Meter')
                conversion=100.
            CASE('2Foot', '4Foot')
                conversion=30.48
            CASE('5mm', '8mm', '10mm')
                conversion=0.1
            CASE('6cm')
                conversion=1
            CASE('7Inch')
                conversion=2.54
            CASE DEFAULT
                STOP 'Not a valid set of units'
        END SELECT

        !get to nodes card
        delim="2411"
        CALL finddelim(11,delim)

        !count the number of nodes until -1 delimiter
        delim="-1"
        CALL counter(11,delim,numnodes)

        !fix count for those extra lines in format
        numnodes=(numnodes-1)/2

        !get to elements card
        delim="2412"
        CALL finddelim(11,delim)

        !count number of elements until -1 delimiter
        delim="-1"
        CALL counter(11,delim,numelements)

        !fix count for those extra lines in format
        numelements=(numelements-1)/2

        !allocate nodes and elements arrays
        ALLOCATE(nodes(numnodes,3),elements(numelements,10))

        !get to nodes card
        delim="2411"
        CALL finddelim(11,delim)

        !read in all nodes data, skipping extra lines
        DO i=1,numnodes
            READ(11,*)tempcharacter
            READ(11,*)nodes(i,:)
            !convert those nodes to cm
            DO j=1,3
                nodes(i,j)=nodes(i,j)*conversion
            END DO
        END DO

        !get to elements card
        delim="2412"
        CALL finddelim(11,delim)

        !read in all elements data, extra lines contain relevant data so don't skip them!
        DO i=1,numelements
            READ(11,*)elements(i,:)
        END DO

        !close input file
        CLOSE(UNIT=11,IOSTAT=ios)
        IF(ios .NE. 0)THEN
            WRITE(*,*)'Could not close input file ', filein
            STOP
        END IF
    END SUBROUTINE readinunv

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Find delimiter value in a unv file
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE finddelim(filenum,delim)
        !> File id number
        INTEGER,INTENT(IN)::filenum
        !> Delimiter string
        CHARACTER(64),INTENT(IN)::delim
        !> Temporary character variables
        CHARACTER(64)::tc1="",tc2=""

        !go to beginning of file
        REWIND(filenum)

        !find delimiter, if not found give error, this is tricky for unv files as the
        !delimiter may appear somewhere and be incorrect, need to also check that it
        !is proceeded by two -1 values
        tempcharacter=""
        DO WHILE(tempcharacter .NE. delim .OR. tc1 .NE. "-1" .OR. tc2 .NE. "-1")
            tc1=tc2
            tc2=tempcharacter
            READ(filenum,*,IOSTAT=ios)tempcharacter
            IF(ios .NE. 0)THEN
                WRITE(*,'(2A)') "End of file reached without finding delimiter: ", delim
                STOP
            END IF
        END DO
    END SUBROUTINE finddelim

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Count the number of values in the section of the unv file
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE counter(filenum,delim,count)
        !> File id number
        INTEGER,INTENT(IN)::filenum
        !> Delimiter string
        CHARACTER(64),INTENT(IN)::delim
        !> Number counted
        INTEGER,INTENT(OUT)::count

        !count number until delimiter
        count=0
        tempcharacter=""
        DO WHILE(tempcharacter .NE. delim)
            READ(filenum,*)tempcharacter
            count=count+1
        END DO
    END SUBROUTINE counter
END MODULE readinfunctions
