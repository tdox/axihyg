      function XPOS(EL,XI)
      
c     Name:      X POSition
c     Purpose:   To calculate the merdional coordinate,X, corrosponding
c                to the local (element) coordinate XI of the element EL.
c     Input:     EL, the element number; XI, the local coordinate.
c     Output:    SPOS
c     Called by: FCYMCH
c     Calls    : 

      implicit      undefined(a-z)
      
      integer       EL
      real*8        XPOS,XI
      
      include       'n.par'
      include       'elmdat.com'
      
      
      XPOS   =  XEDGE(EL) + (XI+1.)/2. * (XEDGE(EL+1) - XEDGE(EL))
      
      return
      end
      
