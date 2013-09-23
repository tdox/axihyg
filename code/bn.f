      subroutine BN(B,N,XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB)
      
c     Name:      B and N matrices
c     Purpose:   To calculate the B and N matrices.  This subroutine just calls
c                the appropriate one for the given order shape functions.
c     Common:    CONTRL
c     Input:    
c                
c     Output:    
c     Called by: FORMKF
c     Calls    : BN121,...

      implicit     undefined(a-z)
      
      include      'io.par'
      include      'n.par'
      include      'contrl.com'
      
      
      real*8       B(MAXNEN,MXNEPS,MXNVAR),N(MAXNEN,MXNVAR),
     *             XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB
      
100   format(' in bn')
      
      
      if (OSHPU .eq. 1) then
         if (OSHPW .eq. 2) then
            if (OSHPD .eq. 1) then
               call BN121(B,N,XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB)
            end if
         end if
      end if
      
      return
      end

      
