      Program RomanNumerals
      USE PARAMETERS    !, only: SingleRomanValues, DoubleRomanValues  ! DEBUG
      implicit none
      
      integer(kind=2) :: Numeral1_Integer, Numeral2_Integer, Sum_Integer, i
      character(LEN=20) :: comm_arg1, comm_arg2
      character(LEN=:), allocatable :: Numeral1_String, Numeral2_String, NumeralFinal_String

      ! -----------------------  Initializations
      Numeral1_Integer = 0; Numeral2_Integer = 0
      ! -------------------------------------------^^^

      CALL GET_COMMAND_ARGUMENT(1, comm_arg1)   ! Read argument #1 and #2 from the command line, and assign it to comm_arg#
      CALL GET_COMMAND_ARGUMENT(2, comm_arg2)

      ! -----------------------  Handle Input
      !WRITE(*,*) 'This program takes the user input of two Roman Numerals and produces and outputs the sum as a Numeral'
      !WRITE(*,*) 'The valid Numeral values are I, V, X, L, C, D, and M and their lowercase'
      !WRITE(*,*) 'Invalid input include other ASCII characters.'
      
      CALL BUFFER_TO_STRING(comm_arg1, Numeral1_String)   ! Adjust to the left and trim trailing spaces from the string 
      CALL CHECK_VALIDITY(Numeral1_String)   ! Check the input for invalid ASCII characters, return true/false 
      
      CALL BUFFER_TO_STRING(comm_arg2, Numeral2_String)   ! Adjust to the left and trim trailing spaces from the string
      CALL CHECK_VALIDITY(Numeral2_String)   ! Check the input for invalid ASCII characters, return true/false 
      
      
      IF (testing == 1) THEN  ! create an ascii file
      open(12,file='ascii2.dat',access='append')
        DO i = 1, 255
          CALL ASCII_INPUT(Numeral2_String,i)
          WRITE(12,*) 'RomanNum I ',adjustl(Numeral2_String)
        ENDDO
      close(12)
      END IF

      !WRITE(*,'(A)') ROMAN(1998)

      Numeral1_Integer = DECODER(Numeral1_String)
      Numeral2_Integer = DECODER(Numeral2_String)

      Sum_Integer = Numeral1_Integer + Numeral2_Integer

      WRITE(*,*) Numeral1_Integer, Numeral2_Integer, Sum_Integer

      
      !NumeralFinal_String = ROMAN(Sum_Integer)
      !WRITE(*,'(A)') NumeralFinal_String

      





      !DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
      !      WRITE(*,*) 'Input first Roman Numeral'
      !      READ(*,*) buffer
      !      CALL BUFFER_TO_STRING(buffer,Numeral1_String)
      !      CALL CHECK_VALIDITY(Numeral1_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
      !      IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral1_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      !END DO 

      !LogicalResult = .FALSE.       ! Reset the Logical result to restart the process for Numeral#2
      
      !DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
      !      WRITE(*,*) 'Input second Roman Numeral'
      !      READ(*,*) buffer
      !      CALL BUFFER_TO_STRING(buffer,Numeral2_String)
      !      CALL CHECK_VALIDITY(Numeral2_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
      !      IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral2_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      !END DO
      ! ----------------------------------------^^^

      CONTAINS
      !==================================================================================================================
      SUBROUTINE CHECK_VALIDITY(NumString)
      USE PARAMETERS, only: ValidArray
      implicit none
      character(LEN=*), intent(in) :: NumString
      logical :: LogicalVal
      integer :: i, j

      DO i = 1, LEN(NumString)     
         DO j = 1, 14
           IF (NumString(i:i).eq.ValidArray(j)) THEN
              LogicalVal = .TRUE. 
              EXIT
           ELSE 
              LogicalVal = .FALSE.
           END IF
         END DO
      END DO
      
      IF (LogicalVal.eq..FALSE.) THEN
        WRITE(*,*) 'ERROR: ',NumString, ' contains invalid ASCII characters.'
        STOP
      END IF 

      end SUBROUTINE CHECK_VALIDITY
      !==================================================================================================================

      !==================================================================================================================
      SUBROUTINE BUFFER_TO_STRING(buffer, StringVal)
      implicit none
      character(LEN=20)            , intent(in)  :: buffer
      character(LEN=:), allocatable, intent(out) :: StringVal

      StringVal = trim(adjustL(buffer))
      
      IF (StringVal.eq.'') THEN
        WRITE(*,*) 'ERROR: command line argument empty.'
        STOP
      ENDIF
      
      end SUBROUTINE BUFFER_TO_STRING
      !==================================================================================================================
      
      !==================================================================================================================
      !SOURCE: https://rosettacode.org/wiki/Roman_numerals/Encode#Fortran
      FUNCTION ROMAN(numeral) RESULT (r)  
      implicit none
      integer, intent(in) :: numeral
      integer, parameter  :: d_max = 13
      integer             :: d
      integer             :: m
      integer             :: m_div
      character(32)       :: r
      integer,       dimension(d_max), parameter :: d_dec = (/1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1/)
      character(32), dimension(d_max), parameter :: d_rom = (/'M ','CM','D ','CD','C ','XC','L ','XL','X ','IX','V ','IV','I '/)
 
      r = ''
      m = numeral
      
      DO d = 1, d_max
        m_div = m / d_dec(d)
        r = trim(r) // repeat(trim(d_rom(d)), m_div)
        m = m - d_dec(d) * m_div
      END DO
 
      END FUNCTION ROMAN
      !==================================================================================================================
      
      
      !==================================================================================================================
      !SOURCE: https://www.rosettacode.org/wiki/Roman_numerals/DECODE#Fortran
      !The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI
      !
      FUNCTION DECODER(numeral) RESULT(numberArabic)
      implicit none
      character(LEN=*), intent(in) :: numeral
      integer :: i, n, lastval, numberArabic
      integer(kind=2), dimension(13) :: counters
      logical :: doubleVal
 
      numberArabic = 0
      lastval = 0
      doubleVal = .FALSE.
      n = 0
      counters = 0

      DO i = LEN(numeral), 1, -1    ! Start from the end and iterate the opposite way
        IF (doubleVal.eq..TRUE.) THEN 
          doubleVal = .FALSE. 
          CYCLE    !when a double valued numeral is detected, skip the current loop
        END IF
        
        !WRITE(*,*) i, numeral(i:i)  ! DEBUG
        
        SELECT CASE(numeral(i:i)) ! case structure can be used to create a jump list from a selector, where the selector can be Int or Char
        CASE ('M','m')
          n = 1000
          CALL COUNTING(13,counters)            ! COUNT M
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! distinguish CM as 900
              n = 900 
              doubleVal = .TRUE.
              CALL COUNTING(12,counters)        ! COUNT CM
              IF (counters(9).ge.counters(12)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CM and C found together.'
                STOP
              END IF
              counters(13) = counters(13) - 1
            END IF
          END IF
          
        CASE ('D','d')
          n = 500
          CALL COUNTING(11,counters)            ! COUNT D
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! distinguish CD as 400
              n = 400 
              doubleVal = .TRUE.
              CALL COUNTING(10,counters)        ! COUNT CD
              IF (counters(9).ge.counters(10)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CD and C found together.'
                STOP
              END IF
              counters(11) = counters(11) - 1
            END IF
          END IF
          
        CASE ('C','c')
          n = 100
          CALL COUNTING(9,counters)             ! COUNT C
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XC as 90
              n = 90 
              doubleVal = .TRUE.
              CALL COUNTING(8,counters)         ! COUNT XC
              IF (counters(5).ge.counters(8)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XC and X found together.'
                STOP
              END IF
              counters(9) = counters(9) - 1
            END IF
          END IF
          
        CASE ('L','l')
          n = 50
          CALL COUNTING(7,counters)             ! COUNT L
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XL as 40
              n = 40 
              doubleVal = .TRUE.
              CALL COUNTING(6,counters)         ! COUNT XL
              IF (counters(5).ge.counters(6)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XL and X found together.'
                STOP
              END IF
              counters(7) = counters(7) - 1
            END IF
          END IF
          
        CASE ('X','x')
          n = 10
          CALL COUNTING(5,counters)             ! COUNT X
          
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IX as 9
              n = 9 
              doubleVal = .TRUE.
              CALL COUNTING(4,counters)         ! COUNT IX
              IF (counters(1).ge.counters(4)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IX found together.'
                STOP
              END IF
              counters(5) = counters(5) - 1
            END IF
          END IF
          
        CASE ('V','v')
          n = 5
          CALL COUNTING(3,counters)             ! COUNT V(lad)

          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IV as 4
              n = 4 
              doubleVal = .TRUE.
              CALL COUNTING(2, counters)        ! COUNT IV
              IF (counters(1).ge.counters(2)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IV found together.'
                STOP
              END IF
              counters(3) = counters(3) - 1
            END IF
          END IF
          
          
          
        CASE ('I','i')
          n = 1
          CALL COUNTING(1,counters)             ! COUNT I
          
        CASE default    ! this will not detect invalid characters
          WRITE(*,*) 'ERROR: SELECT CASE selector wrong value...'
          n = 0
        END SELECT
          
       ! WRITE(*,*) 'i,n,lastval', i, n, lastval      ! DEBUG
        
        
        
        IF (n >= lastval) THEN
          numberArabic = numberArabic + n
          
          IF (real(lastval).eq.real(n)*4/9) THEN     
            WRITE(*,*) 'INVALID NUMERAL SUCCESSION. 2 DOUBLEVAL NUMERALS in succession'    ! if the new value is the second doubleVal in succession, abort 
            STOP
          END IF

          IF (doubleVal.eq..TRUE..and.real(n).lt.real(lastval*2)) THEN
            WRITE(*,*) 'INVALID NUMERAL SUCCESSION. DOUBLEVAL NUMERAL along with smaller numeral of the same order'    ! if
            STOP
          END IF
        
        ELSE !IF (n  lastval.and.lastval.gt.0) THEN
          WRITE(*,*) 'INVALID NUMERAL SUCCESSION' 
          STOP

        !ELSE !IF (n > lastval.and.doubleVal.eq..FALSE.) THEN
        !  numberArabic = numberArabic + n
        END IF
        lastval = n
      END DO
      END FUNCTION DECODER
      !==================================================================================================================
      
      !==================================================================================================================
      SUBROUTINE COUNTING(order,counters)
      USE PARAMETERS, only: validCount            ! total valid amount of counts for each numeral
      implicit none
      integer(kind=2), intent(in) :: order       ! the order is given by the numeral succession: I, IV, V, IX, X, .. etc
      integer(kind=2), dimension(13), intent(inout) :: counters    ! how many counts of each numeral are triggered

      counters(order) = counters(order) + 1     ! iterate for every call
      IF (counters(order).gt.validCount(order)) THEN  ! if more counts than permitted are found, abort the program with an error.
        WRITE(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
        STOP
      END IF
      
      end SUBROUTINE COUNTING
      !==================================================================================================================

      !==================================================================================================================
      SUBROUTINE ASCII_INPUT(ascii,i)
      implicit none
      character(LEN=*), intent(out) :: ascii
      integer(kind=2), intent(in) :: i

      ascii = achar(i)
      
      end SUBROUTINE ASCII_INPUT
      !==================================================================================================================


      end program RomanNumerals

      
