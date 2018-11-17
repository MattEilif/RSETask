      MODULE PARAMETERS
      implicit none
      
      character(LEN=1) :: ValidArray(14)
      character(LEN=2) :: ValidArrayPlus(6)
      character(LEN=2) :: ValidArrayAll(13)
      
      integer(kind=2) :: SingleRomanValues(7)= (/ 1, 5, 10, 50, 100, 500, 1000 /)   ! not used
      integer(kind=2) :: DoubleRomanValues(6)= (/ 4, 9, 40, 90, 400, 900 /)         ! not used
      integer(kind=2) :: RomanValues(13)= (/1,4,5,9,10,40,50,90,100,400,500,900,1000/)! not used
      integer(kind=2) :: validCount(13)= (/3,1,1,1,3,1,1,1,3,1,1,1,3/)
      integer(kind=1) :: testing = 0            ! this flag enables the creation of an ASCII file in the main program.

      DATA ValidArray / 'I','i', 'V','v', 'X','x', 'L','l', 'C','c', 'D','d', 'M','m' / ! populate character array of 14 1-length characters, upper and lower
      DATA ValidArrayPlus / 'IV', 'IX', 'XL', 'XC', 'CD', 'CM' / ! populate character array of 6 2-length characters
      DATA ValidArrayAll /'I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M' / ! populate all roman numerals in one array - will need trim

      END MODULE