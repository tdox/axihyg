      subroutine  CLRKE(KE)
      
c     Name:     CLear K Element
c     Purpose:  To enter zeros into the element stiffness matrix.
c     Common:   
c     Input:   
c     Output:    KE, the element stiffness matrix with zeros.
c     Called by: KCT121,
c     Calls    :

      implicit     undefined(a-z)
      
      include      'n.par'
      
      integer      I,J,K,L
      real*8       KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR)
            
      do 4 I=1,MAXNEN
         do 3 J=1,MAXNEN
            do 2 K=1,MXNVAR
               do 1 L=1,MXNVAR
                  KE(I,J,K,L) = 0.
1              continue
2           continue
3        continue
4     continue

      return
      end
     
