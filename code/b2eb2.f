
      subroutine B2EB2(B2,E,KE22)
c     Name:      B1EB1
c     Purpose:   To perform the multiplication KE12=B1t*EMAT*B2
c     Common:
c     Input:     B1,B2 and EMAT

c     Output:    KES
c     Called by: KESHRT
c     Calls    :
c                                                                      |
c*******************************************************************************

      implicit  none

      include   'n.par'

      real*8   B2(MXNEPS,MXNVAR),
     &         KE22(MXNVAR,MXNVAR),E(MXNEPS,MXNEPS)


      KE22(1,1) = B2(9,1)**2*E(9,9)+B2(2,1)*(B2(2,1)*E(2,2)+B2(1,1)*E(1,
     1   2))+B2(1,1)*(E(1,2)*B2(2,1)+B2(1,1)*E(1,1))
      KE22(1,2) = B2(9,1)*B2(9,2)*E(9,9)+B2(2,1)*(B2(2,2)*E(2,2)+B2(1,2)
     1   *E(1,2))+B2(1,1)*(E(1,2)*B2(2,2)+E(1,1)*B2(1,2))
      KE22(2,2) = B2(9,2)**2*E(9,9)+B2(2,2)*(B2(2,2)*E(2,2)+B2(1,2)*E(1,
     1   2))+B2(1,2)*(E(1,2)*B2(2,2)+E(1,1)*B2(1,2))

      KE22(2,1) = KE22(1,2)

      return
      end







