      MODULE PARAMETERS
      implicit none
      
      character(LEN=1) :: ValidCharacters(14) = (/ 'I','i', 'V','v', 'X','x', 'L','l', 'C','c', 'D','d', 'M','m' /) ! populate character array of 14 1-length characters, upper and lower
      character(LEN=2) :: NumeralCharacters(13) = (/'M ','CM','D ','CD','C ','XC','L ','XL','X ','IX','V ','IV','I '/) ! populate all roman numerals in one array by descending order - will need trim
      
      integer(kind=2) :: RomanNumberValues(13)= (/1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1/)
      integer(kind=2) :: validCount(13)= (/3,1,1,1,3,1,1,1,3,1,1,1,3/)  ! the valid numeral count corresponds to Roman Numerals up to 3999 in value.

      logical         :: debugMode = .TRUE.

      END MODULE