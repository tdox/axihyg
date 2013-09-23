       subroutine OTQWRT(UNITNO)
      
c     Name:      OuTput Quantity WRiTe
c     Purpose:   To write the output quatities to solve for
c                unit (file, screen) UNITNO.
c     Input:     UNITNO
c                RSLT,PLTSWC from CONTRL common
c     Output:    The above data to the UNITNO unit
c     Called by: OUTS,INWRT
c     Calls    : 
c                                                                      |
c*******************************************************************************



      implicit     undefined(a-z)
     
      include      'contrl.com'
      include      'io.com'
      
      integer      UNITNO
      
      if (RSLT) then
         write(UNITNO,100)
100      format(//' Output Specification'//
     &           ' Currently, the program will solve for strain',
     &           ' measures and stress resultants.')
       else
         write (UNITNO,110)
110      format(//' Output Specification'//
     &            ' Currently, the program will not solve for',
     &            ' stress resultants.')
      end if

      if (PLTSWC) then
         write(UNITNO,120)
120      format(' It will create data files for plotting.')
       else
         write (UNITNO,130)
130      format(' It will not create data files for plotting.')
      end if
      
      write(UNITNO,140) OTFILE
140   format(' The finite element output file is called ',a,'.')
      
      if (UNITNO .eq. TERM) call WAIT
      
      return
      end
      
        
