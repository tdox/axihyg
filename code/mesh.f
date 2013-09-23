        subroutine MESH
      
      
c     Name:      MESH
c     Purpose:   To input the location of the finite elment nodes
c     Input:     The position of the edge nodes and locations where the
c                temperature is precribed
c     Output:    Nodal location information into ELMDAT common
c     Variables: XNODEI(MAXELM+1) : X NODE Input, list of nodes whose positions
c                  have been prescribed.
c                XEDGEI(MAXELM+1) X Edge Input. I'th element is the posion of
c                 node XNODEI(I)
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************

      implicit    undefined(a-z)
      
      include     'n.par'
      include     'elmdat.com'
      include     'geom.com'
      include     'xedin.com'
      
      NUMEL     = MAXELM
      NXEDI     = 2
      XNODEI(1) = 1
      XEDGEI(1) = XF
      XNODEI(NXEDI) = NUMEL+1
      XEDGEI(NXEDI) = XS

      call XEINTP

      return
      end
      
      
