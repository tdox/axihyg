       subroutine LAMTM
      
      

c     Name:      LAMinate Thickness, Material
c     Purpose:   To calculate the laminate thickness and enter determine
c                whitch materials are in the laminate.
c     Common:    LAYUP, MATPRP
c     Input:     Data from these commons                
c     Output:    LTHKNS and MATLOG data.
c                MATLOG(i) is true if the i'th material in MATPRP common is
c                used in the laminate.
c     Called by:
c     Calls    : 
c                                                                      |
c*******************************************************************************

      
      implicit     none
      
      include      'n.par'
      include      'layup.com'
      include      'load.com'
      include      'matprp.com'
      
      integer MAT,STK

      do 1 MAT=1,NMAT
         MATLOG(MAT) = .false.
1     continue

      LTHKNS = 0.
      
      do 2 STK=1,NSTACK
         STHKNS(STK) = PTHK(STK) * NPLY(STK)
         LTHKNS = LTHKNS + STHKNS(STK)
         MATLOG(MATNO(STK)) = .true.
2     continue

      if (SYM) LTHKNS =  LTHKNS * 2.
      THICK = LTHKNS
      
      return
      end
