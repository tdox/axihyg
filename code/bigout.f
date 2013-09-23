      subroutine BIGOUT(A,DIM,NUMLIN,UNITNO)
      
c     Purpose: To print out a big matrix in columns.

      implicit     none
      
      integer      DIM,NUMLIN,UNITNO,NUMCOL,NUMROW,REMAN,JBEGIN,
     *             JEND,ROW,I,J,COL(10),NOCOL
      real*8       A(DIM,DIM)
      
      NUMCOL = 6

      NUMROW = int(NUMLIN/NUMCOL)
      REMAN  = mod(NUMLIN,NUMCOL)
      
      if (REMAN .ne. 0) NUMROW = NUMROW + 1  
      
      JBEGIN = 1
      JEND   = min0(NUMLIN,NUMCOL)
      do 1 ROW=1,NUMROW
         if (ROW .eq. NUMROW) then
            NOCOL = REMAN
           else
            NOCOL = NUMCOL
         end if
         do 3 J=1,NOCOL
            COL(J) = J + JBEGIN - 1
3        continue
         write(UNITNO,120) (COL(J), J=1,NOCOL)
120      format(10x,10(i2,10x))
         do 2 I=1,NUMLIN
            write(UNITNO,100) I,(A(I,J), J=JBEGIN,JEND)
2        continue
         JBEGIN = JEND + 1
         JEND   = min0(JEND + NUMCOL, NUMLIN)
         write(UNITNO,110)
1     continue

100   format(x,i2,x,10(x,e11.4))
110   format(//)

      return
      end
      
