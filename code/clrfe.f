      subroutine CLRFE(FE)
      
c     Name:     CLear F Element
c     Purpose:  To enter zeros into the element force matrix.
c     Common:   
c     Input:   
c     Output:    FE, the element force matrix with zeros.
c     Called by: KFCYLN,
c     Calls    :

      implicit     undefined(a-z)
      
      include      'n.par'
      
      integer      I,J
      real*8       FE(MAXNEN,MXNVAR)
      
      do 1 I=1,MAXNEN
         do 2 J=1,MXNVAR
            FE(I,J) = 0.
2        continue
1     continue

      return
      end
      
