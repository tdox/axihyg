           subroutine WAIT
      
      
c     Name:      WAIT
c     Purpose:   To pause printing to the terminal until the user presses
c                return
c                                                                      |
c*******************************************************************************

      implicit    none

      print 100
100   format(/15x,' Press "RETURN" to continue')
      read(*,*)
      return
      end
      
      
