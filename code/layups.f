          subroutine LAYUPS(EXIST)
      
      
c     Name:      LAYUP Subroutine
c     Purpose:   To input the layup of the laminate
c     Input:     EXIST, if true, layup information already exists
c                LAYUP information from user
c     Output:    The above information into LAYUP common
c     Commons:   LAYUP
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      implicit     undefined(a-z)

      include      'n.par'
      include      'io.com'
      include      'layup.com'
      include      'units.com'
      
      logical      EXIST,YES,NSTEXT
      character*1  SORN
      integer      I,STKNO
      
      print 85
85    format(//' LAMINATE LAYUP')
      print 90
90    format(/' Would you like to read some information concerning',
     &       ' layup data input?')
      call YESNO(YES)
      if (YES) then
         print 101
101      format(//' In this section of the program, the layup of the',
     &       ' laminate is specified.  The'/
     &       ' following definitions are',
     &       ' used:'//4x,'A "stack" is a group of plies with the',
     &       ' same material properties and'/4x'orientation.'//
     &       4x,'The',
     &       ' "orientation" of a stack is the angle in degrees',
     &       ' between the fibers'/
     &       4x,'and the generator of the shell. ',
     &       ' The angle is positive if the fibers are'/
     &       4x,'rotated in',
     &       ' the counter-clockwise direction (looking at the',
     &       ' shell from the'/4x,'outside).'//
     &       4x,'Stack numbering starts',
     &       ' with the inside stack.'//
     &       4x,'If the laminate is',
     &       ' symmetric about the midsurface, only half of the'/
     &       4x,'laminate is to be specified and the "Number of',
     &       ' Stacks" refers to the'/4x'number of stacks in half of',
     &       ' the laminate.'/)
         call WAIT
      end if
     
1     if (EXIST) then
         NSTEXT = .true.
        else
         NSTEXT = .false.
      end if
     
      if (EXIST) then
         if (SYM) then
              print 100, NSTACK
100           format(///' The current layup is symmetric and has ',
     &               i2,' stacks.')
           else
              print 110, NSTACK
110           format(///' The current layup is not symmetric and has ',
     &               i2,' stacks.')
         end if


         call LAYWRT(TERM)


         print 130
130      format(/' Do you want to change these?')
         call YESNO(YES)
         if (.not. YES) then
            call LAMTM
            return
         end if
      end if
         
         
         
      if (EXIST) then
         if (SYM) then
            print 140
140         format(' The laminate is symmetric.')
           else
            print 150
150         format(' The laminate is not symmetric.')
         end if
         print 160
160      format(' Do you want to change this?')
         call YESNO(YES)
         if (YES) then
            SYM    = .not. SYM
         end if
        
       else
         print 162
162      format(/' Is the laminate symmetric or not?  Enter "s" for',
     &          ' symmetric or "n" for not'/' symmetric.')
10       read(*,'(a)') SORN
         if ((SORN .eq. 's') .or. (SORN .eq. 'S')) then
            SYM = .true.
           else if ((SORN .eq. 'n') .or. (SORN .eq. 'N')) then
            SYM = .false.
           else
            print 164
164         format(' Please enter either "s" or "n".')
            go to 10
         end if
         
      end if
               
         
      if (EXIST) then
           print 170, NSTACK
170        format(' There are ',i2,' stacks.'/'  Do you want to change',
     &            ' the number of stacks?')
           call YESNO(YES)
           if (YES) then
              NSTEXT = .false.
             else
              NSTEXT = .true.
           end if
      end if
      
      
      if (.not. NSTEXT) then
         print 180
180      format(' Enter the number of stacks.')
20       read(*,*) NSTACK
         if (NSTACK .le. 0) then
             print 190
190          format(' Please enter a number greater than 0.')
             go to 20
          end if
           
          do 30 I=1,NSTACK
            call STKINI(I)
30        continue
       else
60        print 200
200       format(' Enter the number of the stack you wish',
     &           ' to change or 0 to change all of'/
     &           ' the stacks.')
50        read(*,*) STKNO
          if (STKNO .eq. 0) then
             do 40 I=1,NSTACK
                call STKINI(I)
40           continue
           else if (STKNO .gt. NSTACK) then
             print 210, NSTACK
210          format(' Please enter a number between 0 and ',i2,'.')
             go to 50
           else
             call STKINI(STKNO)
          end if
          print 220
220       format(' Do you want to change the properties of another',
     &           ' stack?')
          call YESNO(YES)
          if (YES) then
             go to 60
          end if
          
      end if
      
      
      EXIST = .true.
      call LAMTM
      go to 1
      
      end
      
      
