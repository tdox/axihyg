       subroutine BCWRT(UNITNO)
      
c     Name:      Boundary Condition WRiTe
c     Purpose:   To write the boundary condition data to the
c                unit (file, screen) UNITNO.
c     Input:     UNITNO
c                Boundary Condition data from the LAYUP common
c     Output:    The layup data to the UNITNO unit
c     Called by: BCS,INWRT
c     Calls    : 
c                                                                      |
c*******************************************************************************



      implicit     undefined(a-z)

      include      'n.par'
      include      'bc.com'
      include      'elmdat.com'
      include      'io.com'
      include      'units.com'
     
      character*6  DSPBCU(MXNVAR)
      character*7  DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*8  TRCBCU(MXNVAR)
      
      integer      UNITNO,I,EDGN
      
      data DISPBC/'u1','u2','w','1 Beta1','1 Beta2','1 Eta','2 Eta',
     &            '3 Beta1','3 Beta2',
     &            '2 Beta1','2 Beta2','3 Eta',' '/,
     &     TRACBC/'N11','N12','Q1','1 M11','2 M12','1 S1','2 S1',
     &            '3 M11','3 M12','2 M11','2 M12', '3 S1',' '/
     
      
      call BCUNIT(DISPBC,TRACBC,DSPBCU,TRCBCU)

      write(UNITNO,100)
100   format(////'  Boundary Conditions',//)
      
      do 10 EDGN=1,2
         write(UNITNO,110) EDGN
110      format(' Prescribed at edge number ',i2/)

         if (BCTYPE(EDGN) .eq. 0) then
            write(UNITNO,111)
111         format(' This edge has general supports.')
          else if (BCTYPE(EDGN) .eq. 1) then
            write(UNITNO,112)
112         format(' This edge is simply supported.')
          else if (BCTYPE(EDGN) .eq. 2) then
            write(UNITNO,113)
113         format(' This edge is clamped.')
          else if (BCTYPE(EDGN) .eq. 3) then
            write(UNITNO,114)
114         format(' This edge is free.')
          else if (BCTYPE(EDGN) .eq. 4) then
            write(UNITNO,115)
115         format(' This edge is symmetrically supported.')
         end if
         
         write(UNITNO,117)
117      format(/' Variable',6x,'Prescribed Value'/)
         do 1 I=1,NBC
            if (BCCODE(EDGN,I) .eq. 1) then
               write(UNITNO,120) DISPBC(I),UBC(EDGN,I),DSPBCU(I)
120            format(x,a7,5x,e12.5,x,a)
             else
               write(UNITNO,120) TRACBC(I),TBC(EDGN,I),TRCBCU(I)
            end if
1        continue
         if (UNITNO .eq. TERM) call WAIT
         write(UNITNO,125)
125      format(//)
10    continue
          

      return
      end
      
      




