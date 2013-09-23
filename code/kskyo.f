      subroutine KSKYO(EL)
      
C     Purpose: To print out the global skyline stiffness array

      implicit none
      
      include 'io.par'
      include 'n.par'
      include 'elmdat.com'
      include 'kfsky.com'
      
      integer I,EL
      
      write(DOCFIL,100) EL
100   format(//' In KSKYO.    EL=',I5,//' KSKY'//)

      do 1 I=1,LNKSKY      
        write(DOCFIL,110) KSKY(I) 
1     continue
110   format(e12.4) 

      return
      end
      
      
