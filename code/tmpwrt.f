         subroutine TMPWRT(UNITNO)
      
      
c     Name:      TeMPerature WRiTe 
c     Purpose:   To write the input temperature data to the unit
c                (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c     Output:    Temperature data to the unit
c     Commons:   
c     Called by: INWRT,LOADS
c                                                                      |
c*******************************************************************************


      implicit    none
      include     'n.par'
      include     'elmdat.com'
      include     'loadi.com'
      include     'load.com'
      include     'units.com'
      
      integer     UNITNO,NODE
      
      real*8      TI,TM,TO
       
      write(UNITNO,100) X1,X1US,TEMPUS,TEMPUS,TEMPUS
100   format(//' Temperature Distribution'//
     &       ' Element Edge',5x,'Position',5x,
     &       'Inner Surface',4x,'Midsurface',4x,
     &       'Outer Surface'/
     &       20x,a,9x,'Temp. Change',4x'Temp. Change',
     &       4x,'Temp. Change'/
     &       18x,'(',a,')',7x,'(',a,')',9x,
     &       '(',a,')',9x,'(',a,')')
      do 1 NODE=1,NUMEL+1
            TI = TEMP0(NODE) - (THICK/2.)*TEMP1(NODE)
     *                + (THICK*THICK/4.)*TEMP2(NODE)
            TM = TEMP0(NODE)
            TO = TEMP0(NODE) + (THICK/2.)*TEMP1(NODE)
     *                + (THICK*THICK/4.)*TEMP2(NODE)
         write(UNITNO,110) NODE,XEDGE(NODE),TI,TM,TO
110      format(2x,i5,8x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      
