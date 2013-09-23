           subroutine STKINI(STKNO)
      
      
c     Name:      STacK INformation Input
c     Purpose:   To input the information for stack no. I into LAYUP common
c     Input:     STKNO, the stack no
c                Stack information from the user
c     Output:    The above information into LAYUP common
c     Commons:   LAYUP
c     Called by: LAYUPS
c     Calls    : None
c                                                                      |
c*******************************************************************************


      implicit     undefined(a-z)

      include      'n.par'
      include      'layup.com'
      include      'matprp.com'
      include      'units.com'
      
      integer      STKNO
      
      
      print 100, STKNO
100   format(//' Now enter information for stack number',
     &       i2,'.'//' Of what material is this stack made?',
     &       '  Enter the material no. or 0 to see the'/
     &       ' list of possible choices.')
10    read(*,*) MATNO(STKNO)
      if (MATNO(STKNO) .eq. 0) then
         call MATLST
         print 110
110      format(/' Enter the material number of this stack.')
         go to 10
       else if ((MATNO(STKNO) .lt. 0) .or.
     &          (MATNO(STKNO) .gt. NMAT)) then
         print 120, NMAT
120      format(' Please enter a number between 0',
     &          ' and ',i2,'.')
         go to 10
      end if
              
      print 130
130   format(' How many plies are in this stack?')
20    read(*,*) NPLY(STKNO)
      if (NPLY(STKNO) .le. 0) then
          print 140
140       format(' Please enter an integer greater than 0.')
          go to 20
      end if
    
      print 150, LENUL
150   format(' How thick is each ply?  Enter the thickness',
     &       ' in ',a,'.')
       read(*,*) PTHK(STKNO)
30     if (PTHK(STKNO) .le. 0.) then
          print 160
160       format(' Please enter a number greater than 0.')
          go to 30
       end if
              
      print 170
170   format(' Enter the orientation of the plies of this',
     &       ' stack in degrees.')
      read(*,*) THETA(STKNO)
      
      return
      end
      
      
