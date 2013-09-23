      subroutine CLEARB(B)
      
c     Name:      CLEAR B matrix.
c     Purpose:   To enter 0. into the B matrix
c     Common:    
c     Input:                    
c     Output:    B(NEN,MXNEPS,MXNVAR) with zeors.
c     Called by: BCY121
c     Calls    : 



      implicit     undefined(a-z)
      
      include      'n.par'
      include      'elmdat.com'
      
      
      integer      NI,I,J
      real*8       B(MAXNEN,MXNEPS,MXNVAR)
            
      do 1 NI=1,NEN
         do 2 I=1,NEPS
            do 3 J=1,NDOFPN(NI)
                 B(NI,I,J)=0.
3           continue
2        continue
1     continue
 
      return
      end
      
      
