         subroutine VIEWMT(MATNO)
      
      
c     Name:      VIEW MaTerial data
c     Purpose:   To view the material properties in the data base MAT.DAT
c     Input:     Material no. of the material to view
c                If a 0 is passed, get the material no. from the user.
c     Output:    Material property data to the screen
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************


      implicit     undefined(a-z)
      
      include      'n.par'
      include      'io.com'
      include      'matprp.com'
      include      'units.com'
      
      integer      MAT,MATNO
 
      if (MATNO .eq. 0) then
5        print 100
100      format(/' Enter the number of the material whose',
     &          ' properties you wish to view or 0 to see'/
     &          ' the list of materials.')
10       read(*,*) MAT
         if ((MAT .lt. 0) .or. (MAT .gt. NMAT)) then
            print 150, NMAT
150         format(' Please enter a material number between',
     &             ' 0 and ',i2,'.')
            go to 10
          else if (MAT .eq. 0) then
            call MATLST
            go to 5
         end if
       else
         MAT = MATNO
      end if
      
      call WRTMAT(TERM,MAT)
      
      return
      end
       
       
