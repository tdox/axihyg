      subroutine KGOUT(EL)
      
C     Purpose: To print out the global stiffness matrix

      implicit undefined(a-z)
      
      include 'n.par'
      include 'elmdat.com'
      include 'kbig.com'
      
      integer EL
      
      
      write(4,100) EL
100   format(//' In KGOUT.    EL=',I5,//' K'//)

      call BIGOUT(K,MAXNEQ,NUMEQ,4)
      
      return
      end
      
      
