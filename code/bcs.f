           subroutine BCS(EXIST)
      
      
c     Name:      Boundary Condition Subroutine
c     Purpose:   To input the boundary conditions
c     Input:     EXIST, if true, b.c. information already exists
c                Boundary condition information from user
c     Output:    The above information into BC common
c     Commons:   BC
c     Called by: CREATE,MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************

 
 
      implicit    none
       
      include     'n.par'
      include     'bc.com'
      include     'elmdat.com'
      include     'io.com'
       
      logical     EXIST,YES,EEXIST(2)
      
      character*6 DSPBCU(MXNVAR)
      character*7 DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*8 TRCBCU(MXNVAR)
      
      integer     EDGN,DOF
      
      real*8      BC(2,MXNBC)
      
      data DISPBC/'u1','u2','w','1 Beta1','1 Beta2','1 Eta','2 Eta',
     &            '3 Beta1','3 Beta2','2 Beta1','2 Beta2','3 Eta',' '/,
     &     TRACBC/'N11','N12','Q1','1 M11','2 M12','1 S1','2 S1',
     &            '3 M11','3 M12','2 M11','2 M12', '3 S1',' '/
      
      call BCUNIT(DISPBC,TRACBC,DSPBCU,TRCBCU)
      
      print 90
90    format(//' BOUNDARY CONDITIONS')

      print 100
100   format(//' Would you like to read some information concerning',
     &        ' entering boundary'/' conditions?')
      call YESNO(YES)
      if (YES) then
          print 110
110       format(//' In this section of the program, boundary',
     &           ' condition information is input.'/
     &           ' Since the shell is axisymmetric, there are only',
     &           ' two edges: the top edge'/' (edge #1) and the',
     &           ' bottom edge (edge #2).',
     &           ' At these',
     &           ' edges either the'/' displacements or the'
     &           ' corresponding tractions must be specified.',
     &           '  You have'/' the option of'
     &           ' prescribing a standard boundary condition',
     &           ' (simply supported,'/' clamped, or free;',
     &           ' or having a',
     &           ' symmetric boundary) or prescribing the'/
     &           ' displacements and tractions individually.')
         call WAIT
      end if
      
      
1     continue

      if (EXIST) then
         print 120
120      format(/' The previously specified boundary',
     &          ' conditions are:'/)
         call BCWRT(TERM)
         print 130
130      format(//' Do you want to change the boundary conditions?')
         call YESNO(YES)
         if (.not. YES) return
       else
        EEXIST(1) = .false.
        EEXIST(2) = .false.
      end if
      
         
      do 10 EDGN=1,2
         if (EEXIST(EDGN)) then
            print 140,EDGN
140         format(/' Do you want to change the boundary conditions for',
     &             ' edge number ',i2,'?')
            call YESNO(YES)
            if (.not. YES) go to 10
         end if
         print 150,EDGN
150      format(/' How is the edge number ',i2,' supported?  Enter:'/
     &           ' 1 for simply supported'/' 2 for clamped'/
     &           ' 3 for free'/
     &           ' 4 for symmetric boundary'/' 0 for general.')
         read(*,*) BCTYPE(EDGN)


         if (BCTYPE(EDGN) .eq. 0) then
      
            do 20 DOF=1,NBC
         
               if (EXIST) then
                  if (BCCODE(EDGN,DOF) .eq. 1) then
                     print 160,EDGN,DOF,DISPBC(DOF),
     &                         UBC(EDGN,DOF),DSPBCU(DOF)
160                  format(/' At edge number ',i1,' for the boundary',
     &                      ' condition corrosponding to degree of'/
     &                      ' freedom ',i2,', the displacement is',
     &                      ' specified.'/
     &                      3x,a,' = ',e12.5,x,a)
                    else                  
                     print 170,EDGN,DOF,TRACBC(DOF),
     &                         TBC(EDGN,DOF),TRCBCU(DOF)
170                  format(/' At edge number ',i1,' for the boundary',
     &                      ' condition corrosponding to degree of',
     &                      ' freedom ',i2,', the traction is',
     &                      ' specified.'/
     &                      3x,a,' = ',e12.5,x,a)
                  end if               
                  print 180
180               format(/' Do you want to change this boundary',
     &                   ' condition?')
                  call YESNO(YES)
                  if (.not. YES) then
                     go to 20
                  end if
               end if
            
               print 190, EDGN,DOF,DISPBC(DOF),TRACBC(DOF),DISPBC(DOF),
     &                    TRACBC(DOF)
190            format(/' At edge number ',i1,' for the boundary',
     &                ' condition corrosponding to degree of'/
     &                ' freedom ',i2,
     &                ', do you want to prescribe ',a,' or ',a,'?'/
     &                ' Enter either 1 for ',a,' or 2 for ',a,'.')
30             read(*,*) BCCODE(EDGN,DOF)
               if (BCCODE(EDGN,DOF) .eq. 1) then
                  print 200, DISPBC(DOF),DSPBCU(DOF)
200               format(/' Enter the value of 'a', in ',a,'.')
                  read(*,*) BC(EDGN,DOF)
                 else if (BCCODE(EDGN,DOF) .eq. 2) then
                  print 200, TRACBC(DOF),TRCBCU(DOF)
                  read(*,*) BC(EDGN,DOF)
                 else
                  print 210
210               format(/' Please enter "1" or "2".')
                  go to 30
               end if
            
20          continue
          
         end if
         
         
10    continue

      call BCSET(BC)

      EXIST = .true.
      
      go to 1
      
      end
      





