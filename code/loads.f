           subroutine LOADS(EXIST)
      
      
c     Name:      LOAD Subroutine
c     Purpose:   To input the mechanical, and hygrothermal loads
c     Input:     EXIST, if true, loading information already exists
c                Loading information from user
c     Output:    The above information into LOADI common
c     Commons:   LOADI
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
 
       implicit    none
       
       include     'n.par'
       include     'contrl.com'
       include     'elmdat.com'
       include     'io.com'
       include     'loadi.com'
       include     'units.com'
       
       logical     YES,EXIST,SIMPI
       character*1 ANS
       
2      print 50
50    format(//' HYGROTHERMAL LOADS')

       print 55
55     format(//' Would you like to use the simple input',
     &          ' routine',
     &          ' or the complex'/' input routine?',/
     &          ' Enter "s" for simple or "c" for complex.')
      read(*,'(a)') ANS
      if ((ANS .eq. 's').or.(ANS .eq.'S')) then
             SIMPI = .true.
          else if ((ANS .eq. 'c').or.(ANS .eq.'C')) then
            SIMPI = .false.
          else
             print 56
56           format(' Please enter "s" or "c".')
             go to 2
       end if
       

       print 100
100    format(//' Would you like to read some information concerning',
     &        ' entering temperature and'/
     &        ' moisture concentration data?')
       call YESNO(YES)
       if (YES) then
          print 110
110       format(//' In this section of the program, the change in',
     &        ' temperature and moisture'/' concentration',
     &        ' are specified.')
     
          if (SIMPI) then
             print 125
             call WAIT
           else
             print 120
             call WAIT
           end if
             
      end if
             
120          format(/' The temperature and moisture are assumed to be',
     &        ' axisymmetric, vary piecewise'/
     &        ' linearly (linear within each element)',
     &        ' in the direction parallel to the'/' generator,',
     &        ' and vary quadratically',
     &        ' through the thickness of the shell. Thus,'/
     &        ' the temperature change of the inner',
     &        ' surface, midsurface, and outer surface'/' must be',
     &        ' determined at every element edge.  However, these',
     &        ' temperature changes'/' need not be specified at every',
     &        ' element edge because the inner, mid, and outer'/
     &        ' temperature changes at element edges',
     &        ' not specified will be',
     &        ' determined through'/' linear interpolation. Thus, as',
     &        ' a minimum, the inner, mid',
     &        ' and outer'/' temperature change',
     &        ' must be specified at the first and last element',
     &        ' edges'/' (the edges of the shell).'//
     &        ' The same holds true for moisture',
     &        ' concentration changes.')

125          format(/' The temperature and moisture are',
     &        ' assumed to be',
     &        ' axisymmetric, vary'/' linearly ',
     &        ' in the direction parallel to the generator,'
     &        ' and vary'/' quadratically',
     &        ' through the thickness of the shell. Thus,',
     &        ' the temperature '/' and moisture concentration',
     &        ' change at the inner',
     &        ' surface, midsurface, and'/' outer surface must be',
     &        ' determined at both the top and bottom',
     &        ' edges of the'/' shell.  The temperature',
     &        ' change and moisture concentration change',
     &        ' at every'/' other point of the shell are',
     &        ' determined by linear',
     &        ' interpolation.')
     
     
      
      if (.not. EXIST) then
         TCODE = .false.
         MOIST = .false.
      end if
         
         
1     continue

      if (TCODE) then
         print 130
130      format(//' The previously specified temperature',
     &            ' information is:')
         call TIWRT(TERM)
         print 131
131      format(' Do you want to see the interpolated',
     &          ' temperature change values?')
         call YESNO(YES)
         if (YES) then
            call TMPWRT(TERM)
         end if
         print 150
150      format(//' Do you want to change these?')
         call YESNO(YES)
         if (YES) then
            call TEMPI
            go to 1
         end if
       else
         print 160
160      format(' Temperature data does not exist.  Do you want',
     &          ' to input some?')
         call YESNO(YES)
         if (YES) then
            call TEMPI
            TCODE = .true.
            go to 1
         end if
      end if
         
      
5     continue

      if (MOIST) then
         print 170
170      format(//' The previously specified moisture concentration',
     &          ' information is:')
         call MIWRT(TERM)
         print 173
173      format(' Do you want to see the interpolated',
     &          ' moisture concentration values?')
         call YESNO(YES)
         if (YES) then
            call MSTWRT(TERM)
         end if
         print 190
190      format(//' Do you want to change these?')
         call YESNO(YES)
         if (YES) then
            call MOISTI
            go to 5
         end if
       else
         print 200
200      format(' Moisture data does not exist.  Do you want',
     &          ' to input some?')
         call YESNO(YES)
         if (YES) then
            call MOISTI
            MOIST = .true.
            go to 5
         end if
      end if
      
      return
      end
         
      
