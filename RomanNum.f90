      Program numerals
      USE parameters, only: debugMode
      implicit none
      
      integer(kind=2) :: Number_1, Number_2, NumberSum
      character(LEN=20) :: command_line_argument_1, command_line_argument_2
      character(LEN=:), allocatable :: Numeral_1, Numeral_2, NumeralSum

      ! -----------------------  Initializations
      Number_1 = 0; Number_2 = 0
      ! -------------------------------------------^^^

      CALL GET_COMMAND_ARGUMENT(1, command_line_argument_1)   ! Read argument #1 
      CALL GET_COMMAND_ARGUMENT(2, command_line_argument_2)   ! and #2 from the command line, and assign it to comm_arg#

      ! -----------------------
      ! This program takes the user input of two Roman Numerals and produces and outputs the sum as a Numeral
      ! The valid Numeral values are I, V, X, L, C, D, and M and their lowercase
      ! Invalid input includes all other ASCII characters.
      
      CALL BUFFER_TO_STRING(command_line_argument_1, Numeral_1)   ! Adjust to the left and trim trailing spaces from the string 
      CALL CHECK_VALIDITY(Numeral_1)   ! Check the input for invalid ASCII characters, return true or STOP the program
      
      CALL BUFFER_TO_STRING(command_line_argument_2, Numeral_2)   ! Adjust to the left and trim trailing spaces from the string
      CALL CHECK_VALIDITY(Numeral_2)   ! Check the input for invalid ASCII characters, return true or STOP the program
      
      Number_1 = DECODER(Numeral_1)
      Number_2 = DECODER(Numeral_2)

      NumberSum = Number_1 + Number_2

      IF (debugMode.eq..TRUE.) WRITE(*,*) Number_1, Number_2, NumberSum

      NumeralSum = ROMAN(NumberSum)

      WRITE(*,*) NumeralSum
      



      CONTAINS
      
      
      !==================================================================================================================
      ! This routine tests whether the input string contains any invalid ASCII characters

      SUBROUTINE CHECK_VALIDITY(StringValue)
      USE PARAMETERS, only: ValidCharacters
      implicit none
      character(LEN=*), intent(in) :: StringValue
      logical                      :: isCharacterValid
      integer                      :: i, j

      DO i = 1, LEN(StringValue)            ! for each character in the input string
         DO j = 1, SIZE(ValidCharacters)    ! for each variable in ValidCharacters
           IF (StringValue(i:i).eq.ValidCharacters(j)) THEN  ! when a match is detected, EXIT the loop and start the next one.
              isCharacterValid = .TRUE. 
              EXIT
           ELSE 
              isCharacterValid = .FALSE.
           END IF
         END DO
      END DO
      
      IF (isCharacterValid.eq..FALSE.) THEN
        WRITE(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
        STOP
      END IF 

      end SUBROUTINE CHECK_VALIDITY
      !==================================================================================================================

      !==================================================================================================================
      ! This routine adjusts the string to the left, removes any trailing spaces, and allocates the new length.
      ! The resulting string cannot be empty.

      SUBROUTINE BUFFER_TO_STRING(buffer, StringValue)
      implicit none
      character(LEN=20)            , intent(in)  :: buffer
      character(LEN=:), allocatable, intent(out) :: StringValue

      StringValue = trim(adjustL(buffer))
      
      IF (StringValue.eq.'') THEN
        WRITE(*,*) 'ERROR: command line argument empty.'
        STOP
      ENDIF
      
      end SUBROUTINE BUFFER_TO_STRING
      !==================================================================================================================
      
      !==================================================================================================================
      !SOURCE: https://rosettacode.org/wiki/Roman_numerals/Encode#Fortran
      ! This function takes the input of a number and translates it in a valid Roman Numeral

      FUNCTION ROMAN(numberArabic) RESULT (numeral)
      USE PARAMETERS, only: RomanNumberValues, numeralCharacters
      implicit none
      integer(kind=2), intent(in) :: numberArabic
      integer                     :: i, numberDummy, integerDivision
      character(32)               :: numeral
      
      numeral = ''                                      ! initialise the string to null
      numberDummy = numberArabic                        ! use numberDummy to store the changing number instead of numberArabic(intent(in))
      
      DO i = 1, SIZE(RomanNumberValues)                 ! for every distinct roman numeral number, descending order (high to low)
        integerDivision = numberDummy / RomanNumberValues(i)                  ! divide number over largest integer, store result 
        numeral = trim(numeral) // repeat(trim(numeralCharacters(i)), integerDivision)     ! collate numeral characters integerDivision times 
        numberDummy = numberDummy - RomanNumberValues(i) * integerDivision    ! subtract the value integerDivision times, and update 
      END DO
 
      END FUNCTION ROMAN
      !==================================================================================================================
      
      
      !==================================================================================================================
      !SOURCE: https://www.rosettacode.org/wiki/Roman_numerals/DECODE#Fortran
      !The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI, IXI-IVI, CMCMDIXVIXVID, etc
      !
      FUNCTION DECODER(numeral) RESULT(numberArabic)
      implicit none
      character(LEN=*), intent(in)   :: numeral
      integer                        :: i, newValue, previousValue, numberArabic
      integer(kind=2), dimension(13) :: counters
      logical                        :: isDoubleValueNumeral
 
      !----- Initialisation --------------------------------
      numberArabic = 0
      previousValue = 0
      isDoubleValueNumeral = .FALSE.
      newValue = 0
      counters = 0
      !-----------------------------------------------------

      DO i = LEN(numeral), 1, -1                                          ! Start from the end and iterate the opposite way
        IF (isDoubleValueNumeral.eq..TRUE.) THEN                          ! IF the previous iteration numeral is part of double value numeral
          isDoubleValueNumeral = .FALSE.                                  ! RESET the trigger AND
          CYCLE                                                           ! skip the current loop
        END IF
        
        SELECT CASE(numeral(i:i)) 
        CASE ('M','m')                                                    ! IF the numeral is M
          newValue = 1000                                                 ! update newValue 
          CALL COUNTING(13,counters)                                      ! increase M counter by 1
          IF (i.ne.1) THEN                                                ! IF the numeral is not the last one
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! IF the numeral before forms CAN form a double value numeral
              newValue = 900                                              ! update newValue by overwriting the previous
              isDoubleValueNumeral = .TRUE.                               ! the two numerals of i and i-1 form a double value numeral
              CALL COUNTING(12,counters)                                  ! increase CM counter by 1
              IF (counters(9).ge.counters(12)) THEN                       ! IF the i-1 component of the numeral coexists with the individual numeral character
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CM and C found together.'
                STOP                                                      ! STOP the program 
              END IF
              counters(13) = counters(13) - 1                             ! IF the numeral was a double value numeral, decrease M counter by 1
            END IF
          END IF
          
        CASE ('D','d')
          newValue = 500
          CALL COUNTING(11,counters)                                      ! increase counter of D
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! distinguish CD as 400
              newValue = 400 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(10,counters)                                  ! increase counter of CD
              IF (counters(9).ge.counters(10)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CD and C found together.'
                STOP
              END IF
              counters(11) = counters(11) - 1
            END IF
          END IF
          
        CASE ('C','c')
          newValue = 100
          CALL COUNTING(9,counters)                                       ! increase counter of C
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XC as 90
              newValue = 90 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(8,counters)                                   ! increase counter of XC
              IF (counters(5).ge.counters(8)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XC and X found together.'
                STOP
              END IF
              counters(9) = counters(9) - 1
            END IF
          END IF
          
        CASE ('L','l')
          newValue = 50
          CALL COUNTING(7,counters)                                       ! increase counter of L
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XL as 40
              newValue = 40 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(6,counters)                                   ! increase counter of XL
              IF (counters(5).ge.counters(6)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XL and X found together.'
                STOP
              END IF
              counters(7) = counters(7) - 1
            END IF
          END IF
          
        CASE ('X','x')
          newValue = 10
          CALL COUNTING(5,counters)                                       ! increase counter of X
          
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IX as 9
              newValue = 9 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(4,counters)                                   ! increase counter of IX
              IF (counters(1).ge.counters(4)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IX found together.'
                STOP
              END IF
              counters(5) = counters(5) - 1
            END IF
          END IF
          
        CASE ('V','v')
          newValue = 5
          CALL COUNTING(3,counters)                                       ! increase counter of V(lad)

          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IV as 4
              newValue = 4 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(2, counters)                                  ! increase counter of IV
              IF (counters(1).ge.counters(2)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IV found together.'
                STOP
              END IF
              counters(3) = counters(3) - 1
            END IF
          END IF
          
        CASE ('I','i')
          newValue = 1
          CALL COUNTING(1,counters)                                       ! increase counter of I
          
        CASE default                                                      ! this will not detect invalid characters
          WRITE(*,*) 'ERROR: SELECT CASE selector wrong value...'
          newValue = 0
        END SELECT
        
        IF (newValue >= previousValue) THEN                               ! update the number by the newValue if it's equal or larger than the previousValue.
          numberArabic = numberArabic + newValue
          
          IF (real(previousValue).eq.real(newValue)*4/9) THEN             ! IF the new value is the second double Value Numeral in succession, STOP the program. 
            WRITE(*,*) 'INVALID NUMERAL SUCCESSION. Two double value numerals in succession'    
            STOP
          END IF

          IF (isDoubleValueNumeral.eq..TRUE..and.real(newValue).lt.real(previousValue*2)) THEN  ! IF newValue is a double value numeral AND less than double of the previousValue...
            WRITE(*,*) 'INVALID NUMERAL SUCCESSION. Double value numeral along with smaller numeral of the same order'
            STOP  ! ...STOP the program. This traps double value numerals followed by smaller numerals, which their sum would be assigned a larger value numeral (eg IXV = 9+5 = 14 = XIV)
          END IF
        
        ELSE                                                              ! IF newValue < previousValue
          WRITE(*,*) 'INVALID NUMERAL SUCCESSION' 
          STOP
        END IF
        
        previousValue = newValue                                          ! update the previous value in preparation of next iteration
      END DO
      END FUNCTION DECODER
      !==================================================================================================================
      
      !==================================================================================================================
      ! This routine iterates the counters for each roman numeral, and compares the amount to the valid count. If the 
      ! count is larger than the valid count, the program STOPs.
      
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

      
      end program numerals

      
