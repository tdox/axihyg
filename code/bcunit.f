       subroutine BCUNIT(DISPBC,TRACBC,DSPBCU,TRCBCU)
      
c     Name:      Boundary Condition UNIT
c     Purpose:   To set the boundary condition units
c     Input:     DISPUS, RSLTUS from UNITS common
c     Output:    Physical units in DSPBCU, TRACBCU
c     Called by: BCS,INWRT
c     Calls    : 
c                                                                      |
c*******************************************************************************



      implicit     undefined(a-z)
      include      'n.par'
      include      'units.com'

      character*6  DSPBCU(MXNVAR)
      character*7  DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*8  TRCBCU(MXNVAR)
      
     
      DSPBCU(1)  = DISPUS(0)
      DSPBCU(2)  = DISPUS(0)
      DSPBCU(3)  = DISPUS(0)
      DSPBCU(4)  = DISPUS(1)
      DSPBCU(5)  = DISPUS(1)
      DSPBCU(6)  = DISPUS(1)
      DSPBCU(7)  = DISPUS(2)
      DSPBCU(8)  = DISPUS(3)
      DSPBCU(9)  = DISPUS(3)
      DSPBCU(10) = DISPUS(2)
      DSPBCU(11) = DISPUS(2)
      DSPBCU(12) = DISPUS(3)
      
      TRCBCU(1)  = RSLTUS(0)
      TRCBCU(2)  = RSLTUS(0)
      TRCBCU(3)  = RSLTUS(0)
      TRCBCU(4)  = RSLTUS(1)
      TRCBCU(5)  = RSLTUS(1)
      TRCBCU(6)  = RSLTUS(1)
      TRCBCU(7)  = RSLTUS(2)
      TRCBCU(8)  = RSLTUS(3)
      TRCBCU(9)  = RSLTUS(3)
      TRCBCU(10) = RSLTUS(2)
      TRCBCU(11) = RSLTUS(2)
      TRCBCU(12) = RSLTUS(3)

      return
      end
      
      




