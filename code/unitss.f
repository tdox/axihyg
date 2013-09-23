      subroutine UNITSS(EXIST)
      
c     Name:      UNITS Subroutine
c     Purpose:   To determine which units are to be used and set the variables
c                of UNITS common accordingly.
c     Input:     Types of units to be used.
c                
c     Output:    Variables of UNITS common
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit    undefined(a-z)
      
      include     'units.com'
      
      logical     EXIST,YES
      
      print 50
50    format(//' PHYSICAL UNITS')
      
      if (EXIST) then
      
           if (UNIT .eq. 's') then
                print 100
100             format(/' The data in the input',
     &                 ' file is in SI units.',
     &                 ' Do you want to change to English'/' units?')
                call YESNO(YES)
                if (YES) then
                   UNIT = 'e'
                end if
             else if (UNIT .eq. 'e') then
                print 110
110             format(/' The data in the input',
     &                 ' file is in English units.',
     &                 ' Do you want to change to'/' SI units?')
                call YESNO(YES)
                if (YES) then
                   UNIT = 's'
                end if
            end if
               
         else
          
            print 120
120         format(/' Which dimensional unit',
     &             ' system is used, SI or',
     &             ' English?',/,' Enter "s" for SI or "e" for',
     &             ' English.')
1           read(*,'(a)') UNIT

      end if
      
      if ((UNIT .ne. 's') .and. (UNIT .ne. 'S') .and.
     &    (UNIT .ne. 'e') .and. (UNIT .ne. 'E')) then
           print 130
130        format(' Please enter either "s" or "e".')
           go to 1
      end if
      
      call UNTSET
      
      return
      end
      
      
