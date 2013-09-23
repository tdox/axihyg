         subroutine MSTWRT(UNITNO)
      
      
c     Name:      MoiSTure WRiTe 
c     Purpose:   To write the input moisture concentration data to the unit
c                (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c     Output:    Temperature data to the unit.
c     Commons:   
c     Called by: INWRT,LOADS
c                                                                      |
c*******************************************************************************


      implicit    none
      include     'n.par'
      include     'elmdat.com'
      include     'load.com'
      include     'loadi.com'
      include     'units.com'
      
      integer     UNITNO,NODE
      
      real*8      MI,MM,MO

      write(UNITNO,100) X1,X1US
100   format(//' Element Edge',5x,'Position',6x,
     &       'Inner Surface',3x,'Mid Surface',3x,
     &       'Outer Surface'/
     &       20x,a,9x,'Moist. Change',3x,'Moist. Change',
     &       3x,'Moist. Change'/
     &       18x,'(',a,')')
      do 1 NODE=1,NUMEL+1
            MI = MOIST0(NODE) - (THICK/2.)*MOIST1(NODE)
     *                 + (THICK*THICK/4.)*MOIST2(NODE)
            MM = MOIST0(NODE)
            MO = MOIST0(NODE) + (THICK/2.)*MOIST1(NODE)
     *                 + (THICK*THICK/4.)*MOIST2(NODE)
          write(UNITNO,110) NODE,XEDGE(NODE),MI,MM,MO
110      format(5x,i2,8x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      
