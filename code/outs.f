           subroutine OUTS(EXIST)
      
      
c     Name:      OUTput Subroutine
c     Purpose:   To specify which quantities to solve for and print
c     Input:     Answers from the user
c     Output:    RSLT, if true, solve the strains and resultants
c                PLTSWC, if true, create data files with plotting information
c     Commons:   CONTRL
c     Called by: CREATE,MODIFY
c     Calls : 
c                                                                      |
c*******************************************************************************

 
 
      implicit    undefined(a-z)

      include     'contrl.com'
      include     'io.com'
      
      logical     EXIST,YES
      
      print 90
90    format(//' OUTPUT QUANTITIES')
      
      BACK = .true.
1     continue

      if (EXIST) then
         call OTQWRT(TERM)
         print 50
50       format(/' Do you want to change this information?')
         call YESNO(YES)
         if (.not. YES) return
      end if
      
      print 100
100   format(/' Do you want to solve for the strain measures',
     &' and stress resultants?')
      call YESNO(RSLT)
      
      print 110
110   format(/' Do you want to create data files for use',
     &       ' in plotting?'/' (Note: This program currently has no',
     &       ' plotting capabilities of its own.)')
      call YESNO(PLTSWC)

      if (EXIST) then
         print 120, OTFILE
120      format(' The name of the finite element output file is ',a,
     &          '.  Do you want to'/' change it?')
         call YESNO(YES)
         if (.not. YES) go to 10
      end if
      
      print 130
130   format(' Enter the name of the file in which you want',
     &       ' solutions printed.  The name must'/' be less than',
     &       ' 11 characters long.')
      read(*,'(a)') OTFILE
     

10    continue
      
      EXIST = .true.
      go to 1
      
      end
      
      
