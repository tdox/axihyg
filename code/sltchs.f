           subroutine SLTCHS(EXIST)
      
      
c     Name:      SoLution TeCHnique Subroutine
c     Purpose:   To specify which solution techniques to use
c     Input:     Solution techniques from the user
c     Output:    Solution techniques to the commons
c     Commons:   CONTRL
c     Called by: CREATE,MODIFY
c     Calls : 
c                                                                      |
c*******************************************************************************

 
 
      implicit    none
      
      include     'n.par'
      include     'contrl.com'
      include     'elmdat.com'
      include     'geom.com'
      include     'io.com'

      logical     EXIST,YES
      
      print 90
90    format(//' SOLUTION TECHNIQUES')
1     continue

      if (EXIST) then
         call SLTWRT(TERM)
         print 55
55       format(/' Do you want to change the solution technique?')
         call YESNO(YES)
         if (.not. YES) return
      end if
      
      print 100
100   format(/' This program can be used to generate solutions',
     &' based on several different'/
     &' levels of approximation (see the references listed in the',
     &' introduction).  This'/
     &' program uses the highest order approximation unless otherwise',
     &' specified by'/
     &' the user.  Using lower order approximations may result',
     &' in incorrect solutions.'/
     &' Do you want to use the highest order of approximation?'/)
      call YESNO(YES)
      if (YES) then
         THEORY = 8
         TORORD = 2
         NINT   = 2
         
         if ((SHAPE .eq. 'cyln') .or. (SHAPE .eq. 'cone')) then
            RIGID = .false.
            NRBM  = 0
          else
            RIGID = .true.
            NRBM  = 1
         end if
         
         KSHORT   = .true.
         
       else
       
         print 110
110      format(/' Warning: Changing solution techniques may be',
     &           ' hazardous to your data.  You should'/' be familiar',
     &           ' with the inner workings of the code to',
     &           ' make useful changes.'/' Proceed with caution.')
     
         if (EXIST) then
            print 120, THEORY
120         format(/' The current shell theory employed is theory',
     &              ' number ',i1,'.  Do you want to change'/
     &              ' the theory number?')
            call YESNO(YES)
            if (.not. YES) go to 20
         end if
         
10       print 130
130      format(/' Enter the Theory Number.  (Default: 8)')
         read(*,*) THEORY
         if ((THEORY .lt. 1) .or. (THEORY .gt. 8)) then
            print 140
140         format(' The Theory Number must be an integer between',
     &             '1 and 8.')
            go to 10
         end if
         

20       continue
         if (EXIST) then
            print 150, TORORD
150         format(/' Currently, the program includes terms',
     &             ' (t/R)^',i1,' when calculating the element'/
     &             ' stiffness matrices.  Do you want to change',
     &             ' the order of (t/R) that is included'/
     &             ' in the calculations?')
            call YESNO(YES)
            if (.not. YES) go to 40
         end if
         
         print 160
160      format(/' Enter the order of (t/R) that is to be included in',
     &           ' the calculations.'/' (Default: 2)')
30       read(*,*) TORORD
         if ((TORORD .lt. 0) .or. (TORORD .gt. 2)) then
            print 170
170         format(' Please enter an integer between 0 and 2.')
            go to 30
         end if
         

40       continue
         if (EXIST) then
            print 180, NINT
180         format(/' Currently, the program uses ',i1,' integration',
     &             ' points per element.'/'  Do you want to change',
     &             ' this number?')
            call YESNO(YES)
            if (.not. YES) go to 60
         end if
         
         print 190
190      format(/' Enter the number of integration points per',
     &           ' element. (Default: 2)')
50       read(*,*) NINT
         if ((NINT .lt. 1) .or. (NINT .gt. 2)) then
            print 200
200         format(' Please enter either 1 or 2.')
            go to 50
         end if
         
         
60       continue
         if ((SHAPE .ne. 'cyln') .and. (SHAPE .ne. 'cone')) then
            if (EXIST) then
               if (RIGID) then
                  print 210
210               format(/' Rigid body motion is currently included in',
     &                   ' the element shape functions.')
                 else
                  print 220
220               format(/' Rigid body motion is not currently included',
     &                   ' in the element shape functions.')
               end if
            end if
            print 230
230         format(/' Do you want to include rigid body motion in',
     &              ' the element shape functions?'/' (Default: YES)')
            call YESNO(RIGID)
          else 
            RIGID = .false.
         end if
         
         
70       continue
         if (EXIST) then
            if (KSHORT) then
               print 240
240            format(/' The global stiffness matrix is calcualted',
     &                ' using a quicker algorithm.')
              else
               print 250
250            format(/' The global stiffness matrix is calcualted',
     &                ' using a slower algorithm.')
            end if
         end if
         print 260
260      format(' Do you want to use a quicker algorithm?',
     &          '  (Default: YES)')
         call YESNO(KSHORT)
         
      end if
      
      call THRSET
      
      OSHPU = 1
      OSHPW = 2
      OSHPD = 1
      
      EXIST = .true.
      
      go to 1
      
      end
      
      
