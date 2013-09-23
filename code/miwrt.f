         subroutine MIWRT(UNITNO)
      
      
c     Name:      Moisture Input WRiTe 
c     Purpose:   To write the input moisture concentration data to the unit
c                (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c     Output:    Temperature data to the unit.
c     Commons:   
c     Called by: INWRT,LOADS
c                                                                      |
c*******************************************************************************


      implicit    undefined(a-z)
      include     'n.par'
      include     'elmdat.com'
      include     'loadi.com'
      include     'units.com'
      
      integer     UNITNO,I

      write(UNITNO,100) X1,X1US
100   format(//' Element Edge',5x,'Position',6x,
     &       'Inside Moist.',3x,'Middle Moist.',3x,
     &       'Outside Moist.'/
     &       20x,a,11x,'Change',10x'Change',10x,'Change'/
     &       18x,'(',a,')')
      do 1 I=1,NMSTI
         print 180, MNI(I),XEDGE(MNI(I)),MII(I),MMI(I),MOI(I)
180      format(5x,i2,8x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
