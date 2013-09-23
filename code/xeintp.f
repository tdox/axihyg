          subroutine XEINTP
      
      
c     Name:      X Edge INTerPolate
c     Purpose:   To interpolate the positions of the edges of the finite elments
c     Input:     Prescribed element edge position data from XEDIN common
c     Output:    All interpolated element edge position to ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      implicit     none
      
      include      'n.par'
      include      'elmdat.com'
      include      'xedin.com'
      
      integer      I,J,DIFF,NF,NL
      real*8       XEINCR

      XEDGE(1) = XEDGEI(1)
      
      do 10 I=2,NXEDI
         NF = XNODEI(I-1)
         NL = XNODEI(I)
         XEDGE(NL) = XEDGEI(I)
         DIFF   = NL - NF
         XEINCR = (XEDGEI(I) - XEDGEI(I-1)) / DIFF
         
         do 15 J=1,DIFF-1
            XEDGE(NF+J) = XEDGE(NF) + XEINCR*J
15       continue

10    continue

      return
      end
      
      
