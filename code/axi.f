      program AXI

c     version 1.0 1 January 1988
c     version 1.1 revised 15 Sept. 1990
      
c      implicit    undefined(a-z)
      implicit    none
      
      include     'io.par'
      include     'io.com'
      include     'contrl.com'
      
      ERRFIL = ERRRFL
      
      open (unit=ERRFIL, file= 'ERROR.DAT', status= 'new')
      open (unit=DOCFIL, file= 'DOCOUT.DAT', status= 'new')
      
      write(*,100)
100   format(' in AXI, calling INPUT')
      call INPUT
      
      write(*,110)
110   format(' calling INTDAT')
      call INTDAT
      
      write(*,120)
120   format(' calling IDIEN')      
      call IDIEN
      
      write(*,130)
130   format(' calling FORMKF')
      call FORMKF      
      
      write(*,140)
140   format(' calling SOLVE')
      call SOLVE
      
      
      if (BACK .and. RSLT) then
         write(*,150)
150      format(' calling RESULT')
         call RESULT
      end if
      
      write(*,160)
160   format(' calling OUTPUT')
      call OUTPUT
      
      close(ERRFIL)

      stop
      end
      
      



      
