         subroutine XEDWRT(UNITNO)
      
      
c     Name:      X EDge WRiTe
c     Purpose:   To write the positions of the element edges.
c     Input:     UNITNO, unit number to print to
c     Output:    Material property list to the unit no
c     Commons:   MATPRP
c     Called by: MATERS,XEDGES
c     Calls    : 
c                                                                      |
c*******************************************************************************

      implicit    undefined(a-z)
      
      include     'n.par'
      include     'elmdat.com'
      include     'units.com'
      
      integer     NODE,NOENOD,UNITNO

      write(UNITNO,100) X1,X1US
100   format(//' Element Edge Positions'//
     &        ' Element Edge',5x,'Position'/21x,a/18x,'(',a,')')
      NOENOD = NUMEL + 1
      do 1 NODE=1,NOENOD
         write(UNITNO, 110) NODE,XEDGE(NODE)
1     continue
110   format(2x,i5,8x,e12.5)

      return
      end
     
     
