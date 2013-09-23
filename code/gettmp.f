           subroutine GETTMP(TI,TM,TO)
      
      
c     Name:      GET TeMPerature Input 
c     Purpose:   To input temperture or moisture distribution at a point
c     Input:     DIST, = c if temp is const. at the point
c                      = l if temp is linear through thickness
c                      = q if temp is quadratic through thickness
c                TI,TM,TO Temp at inner, mid and outer surfacec from user
c     Output:    TI,TM,TO
c     Commons:   none
c     Called by: TEMPI
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
       implicit    undefined(a-z)
       
       include     'units.com'
       
       character*1 DIST
       real*8      TI,TM,TO
       
       
5     read(*,'(a)') DIST
      if ((DIST .ne. 'c') .and. (DIST .ne. 'l') .and. 
     &    (DIST .ne. 'q')) then
         print 105
105      format(' Please enter either "c", "l" or "q"')
         go to 5
      end if

      if ((DIST .eq. 'c') .or. (DIST .eq. 'C')) then
         print 101, TEMPUL
101      format(' At this position enter the temperature change',
     &          ' of the shell in ',a'.')
         read(*,*) TI
         TM = TI
         TO = TI
       else if ((DIST .eq. 'l') .or. (DIST .eq. 'L')) then
         print 102, TEMPUL
102      format(' At this position enter the temperature'
     &          ' change of the shell',
     &          ' at the inner and'/' outer surfaces in ',a,'.')
         read(*,*) TI,TO
         TM = (TI + TO) / 2.
       else if (DIST .eq. 'q') then
         print 103, TEMPUL
103      format(' At this position enter the temperature'
     &          ' change of the shell',
     &          ' at the inner, mid,'/' and outer surfaces in ',a,'.')
         read(*,*) TI,TM,TO
      end if

      return
      end
      
      
