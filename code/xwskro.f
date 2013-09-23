      subroutine XWSKRO( EL,INT,XI,WEIGHT,S,
     *                   KAPA1,KAPA2,OA1,OA2,A2D1,R,WTJR)
      
c     Purpose: To print out the above variables.

      implicit    none
      
      include     'io.par'
      
      integer     EL,INT
      real*8      XI,WEIGHT,S,KAPA1,KAPA2,OA1,OA2,A2D1,R,WTJR
      
      write(DOCFIL,100) EL,INT,XI,WEIGHT,S,KAPA1,KAPA2,OA1,OA2,
     *                  A2D1,R,WTJR
100   format(/' EL=',I4,3X,'INT=',I3,3X,'XI=',e12.5,3X,'WEIGHT=',e12.5,/
     *       ' S=',e12.5,3X,'KAPA1=',e12.5,3X,'KAPA2=',e12.5,3X,/
     *       ' OA1=',e12.5,3X,'OA2=',e12.5,3x,'A2D1=',e12.5,
     *       'R=',e12.5,/' WTJR=',e12.5/)
     
      return
      end
      
