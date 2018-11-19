      PROGRAM asciiCreateFile
      implicit none
      integer(kind=2)  :: i
      character(LEN=2) :: ascii

      ! Create a sample .dat file which contains one column of input I and one column of ASCII input.
      ! If this program is rerun, the results will be appended on the output file.

      
      open(12,file='ascii2.dat',access='append')
        DO i = 1, 255
          CALL ASCII_INPUT(ascii,i)
          WRITE(12,*) 'RomanNum I ',adjustl(ascii)
        ENDDO
      close(12)

      CONTAINS

      !==================================================================================================================
      SUBROUTINE ASCII_INPUT(ascii,i)
      implicit none
      character(LEN=2), intent(out) :: ascii
      integer(kind=2), intent(in) :: i

      ascii = achar(i)
      
      end SUBROUTINE ASCII_INPUT
      !==================================================================================================================


      
      END PROGRAM asciiCreateFile

      
