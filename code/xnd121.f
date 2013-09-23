      subroutine XND121
      
c     Name:      X NoDe 121
c     Purpose:   To calculate the position of all the nodes.
c     Common:    ELMDAT
c     Input:     XEDGE(NUMEL)
c     Output:    XNODE:  position of all nodes
c     Called by: IDIEN
c     Calls    : 

      implicit     none

      include      'n.par'
      include      'elmdat.com'
      
      integer      I,I2
      
comment: Interpolate to find the position of all nodes.

      do 1 I=1,NUMEL+1
         XNODE(2*I-1) = XEDGE(I)
1     continue

      do 2 I=1,NUMEL
          I2 = I*2
          XNODE(I2) = (XNODE(I2-1) + XNODE(I2+1)) * 0.5
2     continue
         
      return
      end
     
     
      

