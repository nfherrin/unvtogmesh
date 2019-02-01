PROGRAM unvtogmesh
IMPLICIT NONE
CHARACTER(32)::tempcharacter,filein,fileout,arg,tc1,tc2
INTEGER::numnodes,numelements,i,j,ios
INTEGER,ALLOCATABLE::elements(:,:)
REAL(8),ALLOCATABLE::nodes(:,:)
REAL(8)::conversion

!prompt for input and output file names, unless one is provided. 
!Convention will be for fist name to be in, and second name to be out
IF(iargc() .GT. 2)STOP 'Only two argument variables are allowed for this program! "filein" "fileout"'

!either use or prompt for input file name
IF(iargc() .GE. 1)THEN
    CALL getarg(1, arg)
    READ(arg,*)filein
ELSE
    WRITE(*,'(A)')'Input file name?'
    WRITE(*,'(A)',ADVANCE='NO')'> '
    READ(*,*)filein
END IF

!either use or prompt for input file name
IF(iargc() .GE. 2)THEN
    CALL getarg(2, arg)
    READ(arg,*)fileout
ELSE
    WRITE(*,'(A)')'Output file name?'
    WRITE(*,'(A)',ADVANCE='NO')'> '
    READ(*,*)fileout
END IF

!open input file
OPEN(UNIT=11,FILE=filein,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
IF(ios .NE. 0)THEN
    WRITE(*,*)tempcharacter
    WRITE(*,*)'Could not open', filein
    STOP
END IF

!get to 164 delimiter for data card
tempcharacter=""
tc1=""
tc2=""
DO WHILE(tempcharacter .NE. "164" .OR. tc1 .NE. "-1" .OR. tc2 .NE. "-1")
    tc1=tc2
    tc2=tempcharacter
    READ(11,*)tempcharacter
END DO
READ(11,*)tempcharacter

!get conversion factor, code outputs units of cm
IF(tempcharacter .EQ. '1Meter' .OR. tempcharacter .EQ. '3Meter')THEN
    conversion=100.
ELSE IF(tempcharacter .EQ. '2Foot' .OR. tempcharacter .EQ. '4Foot')THEN
    conversion=30.48
ELSE IF(tempcharacter .EQ. '5mm' .OR. tempcharacter .EQ. '8mm' .OR. tempcharacter .EQ. '10mm')THEN
    conversion=0.1
ELSE IF(tempcharacter .EQ. '6cm')THEN
    conversion=1
ELSE IF(tempcharacter .EQ. '7Inch')THEN
    conversion=2.54
ELSE
    STOP 'Not a valid set of units'
END IF
REWIND(11)

!get to 2411 delimiter for nodes card
tempcharacter=""
tc1=""
tc2=""
DO WHILE(tempcharacter .NE. "2411" .OR. tc1 .NE. "-1" .OR. tc2 .NE. "-1")
    tc1=tc2
    tc2=tempcharacter
    READ(11,*)tempcharacter
END DO

!count the number of nodes until -1 delimiter
numnodes=0
tempcharacter=""
DO WHILE(tempcharacter .NE. "-1")
    READ(11,*)tempcharacter
    numnodes=numnodes+1
END DO
!fix count for those extra lines in format
numnodes=(numnodes-1)/2
!rewind file, cards may be out of order
REWIND(11)

!get to 2412 delimiter for elements card
tempcharacter=""
tc1=""
tc2=""
DO WHILE(tempcharacter .NE. "2412" .OR. tc1 .NE. "-1" .OR. tc2 .NE. "-1")
    tc1=tc2
    tc2=tempcharacter
    READ(11,*)tempcharacter
END DO

!count number of elements until -1 delimiter
numelements=0
tempcharacter=""
DO WHILE(tempcharacter .NE. "-1")
    READ(11,*)tempcharacter
    numelements=numelements+1
END DO
!fix count for those extra lines in format
numelements=(numelements-1)/2
!rewind file
REWIND(11)

!allocate nodes and elements arrays
ALLOCATE(nodes(numnodes,3),elements(numelements,10))

!get to nodes card
tempcharacter=""
tc1=""
tc2=""
DO WHILE(tempcharacter .NE. "2411" .OR. tc1 .NE. "-1" .OR. tc2 .NE. "-1")
    tc1=tc2
    tc2=tempcharacter
    READ(11,*)tempcharacter
END DO

!read in all nodes data, skipping extra lines
DO i=1,numnodes
    READ(11,*)tempcharacter
    READ(11,*)nodes(i,:)
    !convert those nodes to cm
    DO j=1,3
        nodes(i,j)=nodes(i,j)*conversion
    END DO
END DO
!rewind file, cards may be out of order
REWIND(11)

!get to elements card
tempcharacter=""
tc1=""
tc2=""
DO WHILE(tempcharacter .NE. "2412" .OR. tc1 .NE. "-1" .OR. tc2 .NE. "-1")
    tc1=tc2
    tc2=tempcharacter
    READ(11,*)tempcharacter
END DO

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

!open output file
OPEN(UNIT=11,FILE=fileout,STATUS='REPLACE',ACTION='WRITE',IOSTAT=ios,IOMSG=tempcharacter)
IF(ios .NE. 0)THEN
    WRITE(*,*)tempcharacter
    WRITE(*,*)'Could not open', fileout
    STOP
END IF

!output standard format information
WRITE(11,'(A)')'$MeshFormat'
WRITE(11,'(A)')'2.0 0 8'
WRITE(11,'(A)')'$EndMeshFormat'
WRITE(11,'(A)')'$Nodes'
WRITE(tempcharacter,*)numnodes
WRITE(11,'(A)')TRIM(ADJUSTL(tempcharacter))

DO i=1,numnodes
    WRITE(tempcharacter,*)i
    WRITE(11,'(A)',ADVANCE='NO')TRIM(ADJUSTL(tempcharacter))
    WRITE(11,'(3ES26.16)')nodes(i,:)
END DO

WRITE(11,'(A)')'$EndNodes'
WRITE(11,'(A)')'$Elements'
WRITE(tempcharacter,*)numelements
WRITE(11,'(A)')TRIM(ADJUSTL(tempcharacter))

!here is the important part, libmesh doesn't use material to specify region, the elements(i,4) write does that
DO i=1,numelements
    WRITE(tempcharacter,*)i
    WRITE(11,'(A)',ADVANCE='NO')TRIM(ADJUSTL(tempcharacter))
    WRITE(11,'(5I2,4I6)')4,3,elements(i,4)-1,0,1,elements(i,7:10)
END DO

WRITE(11,'(A)')'$EndElements'

WRITE(*,'(A)')'unv to gmesh file conversion completed.'

END PROGRAM unvtogmesh