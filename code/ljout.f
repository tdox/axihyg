      subroutine LJOUT(EL,LENGTH,JACOB)
      
c     Purpose: To print the length and Jacobian.

      implicit    undefined(a-z)
      
      integer     EL
      real*8      LENGTH,JACOB
      
      write(4,100) EL,LENGTH,JACOB
100   format(///' EL=',I4,2X,'LENGTH=',F10.4,2X,'JACOB=',F10.4/)
 
      return
      end
      
