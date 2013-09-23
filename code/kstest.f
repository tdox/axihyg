      subroutine KSTEST
      
c     Purpose:  To test the skyline solver

      implicit     none

      include      'io.par'
      include      'n.par'
      include      'elmdat.com'
      include      'kfsky.com'
      include      'kbig.com'
      
      integer      I,J
      real*8       FTEST(MAXNEQ)

      write(DOCFIL,100) 
100   format(//' D'/)

      do 1 I=1,NUMEQ
         write(DOCFIL,110) D(I)
110      format(x,e11.4)
1     continue

      do 2 I=1,NUMEQ
          FTEST(I) = 0.
          do 3  J=1,NUMEQ
             FTEST(I) = FTEST(I) + K(I,J)*D(J)
3         continue
2     continue


      write(DOCFIL,120) 
120   format(//4X,' F',8X,'FTEST'/)

      do 4 I=1,NUMEQ
         write(DOCFIL,130) F(I),FTEST(I)
130      format(x,2(e11.4,X))
4     continue

      return
      end
      
