         subroutine TIWRT(UNITNO)
      
      
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
      include     'units.com'
      
      integer     UNITNO,I
       
      write(UNITNO,100) X1,X1US,TEMPUS,TEMPUS,TEMPUS
100   format(//' Temperature Distribution'//
     &       3x,' Position',6x,
     &       'Inside Temp.',4x,'Middle Temp.',4x,
     &       'Outside Temp'/
     &       6x,a,11x,'Change',10x'Change',10x,'Change'/
     &       4x,'(',a,')',7x,'(',a,')',9x,
     &       '(',a,')',9x,'(',a,')')
      do 1 I=1,NTMPI
         print 110, XEDGE(TNI(I)),TII(I),TMI(I),TOI(I)
110      format(x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      
