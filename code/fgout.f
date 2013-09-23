      subroutine FGOUT(EL)
      
C     Purpose: To print out the global force matrix

      implicit undefined(a-z)
      
      include 'n.par'
      include 'elmdat.com'
      include 'kfsky.com'
      
      integer I,EL

      write(4,120) 
120   format(//' F'//)
      do 2 I=1,NUMEQ
         write(4,130) F(I)
130      format(x,e12.4)
2     continue

      return
      end
      
      
