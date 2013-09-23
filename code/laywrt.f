      subroutine LAYWRT(UNITNO)
      
c     Name:      LAYup WRiTe
c     Purpose:   To write the layup data  to the unit(file, screen) UNITNO.
c     Input:     UNITNO
c                Layup data from the LAYUP common
c     Output:    The layup data to the UNITNO unit
c     Called by: INWRT,LAYUPS
c     Calls    : 
c                                                                      |
c*******************************************************************************


      implicit      none

      include      'n.par'
      include      'layup.com'
      include      'units.com'
      
      integer      UNITNO,I
      
      write(UNITNO,100) LENUS
100   format(//' Laminate Layup'//
     &       ' Stack',3x,'Material No.',3x,
     &       'No. of Plies',3x,'Ply thickness',3x,'Orientation'/
     &       43x,'(',a,')',10x,'(degrees)')
      do 2 I=1,NSTACK
         write(UNITNO,110) I,MATNO(I),NPLY(I),PTHK(I),THETA(I)
2     continue
110   format(x,i3,6x,i3,13x,i3,9x,e12.5,4x,e12.5)
      if (SYM) then
         write(UNITNO,120)
120      format(x,20('-'),' midsurface (surface of symmetry) ',
     &          15('-'),//)
      end if

      return
      end
      
      
