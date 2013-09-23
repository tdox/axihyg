         subroutine NUMELS(EXIST,XEDEXT)
      
      
c     Name:      NUMber of ELement input Subroutine
c     Purpose:   To input the of finite elments
c     Input:     EXIST, if true, number of elements information already exists
c                NUMEL, no. of elements from user
c     Output:    The above information into ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      implicit     none
      
      include      'n.par'
      include      'elmdat.com'
      
      logical      TRU,EXIST,XEDEXT
      
      
      if (EXIST) then
         print 100, NUMEL
100      format(/' There are 'i2' elements in the mesh.'/'  Do you',
     &          ' want to change this number?')
         call YESNO(TRU)
         
         if (.not. TRU) then
            XEDEXT = .true.
            return
         end if
         
      end if
         
      XEDEXT = .false.
         
      
      print 110, MAXELM
110   format(/' Enter the number of elements for this mesh.  This',
     &       ' number must be less than'/' or equal to ',i2,'.')
1     read(*,*) NUMEL
      
      if (NUMEL .gt. MAXELM) then
         print 120, MAXELM
120      format(' Please enter a number less than or',
     &          ' equal to ',i2,'.')
         go to 1
      end if
      
      if (NUMEL .le. 0) then
         print 125
125      format(' Please enter a number greater than 0')
         go to 1
      end if
      
      return
      end
      
