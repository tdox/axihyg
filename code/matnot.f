          subroutine MATNOT
      
      
c     Name:      MATerial property NOTation
c     Purpose:   To print information concerning notation
c     Input:     Whether to print information or not, from user
c     Output:    Material property notation information to the screen.
c     Commons:   none
c     Called by: VIEWMT,CHGMAT,ADDMAT
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
      implicit    none

      logical     YES

      print 40
40    format(//' Would you like to read some information concerning',
     &       ' notation used to describe'/' material properties?')
      call YESNO(YES)
      if (YES) then
          print 50
50       format(//' In this section, the following standard notation',
     &           ' is used:'/
     &        2x,' E',7x,'denotes Young''s modulus',
     &           ' (stiffness)'/
     &        2x,' Nu',6x,'denotes Poisson''s ratio'/
     &        2x,' G',7x,'denotes shear modulus'/
     &        2x,' Alpha T denotes coefficient of',
     &           ' thermal expansion'/
     &        2x,' Alpha M denotes coefficient of',
     &           ' moisture swelling expansion'/)
         print 51
51      format(' The numbers following these coefficients denote',
     &          ' the direction or plane to which'/
     &          ' the coefficient refers.'/
     &          2x,' The number 1 corresponds to the fiber',
     &          ' direction of the lamina.'/
     &          2x,' The number 2 corresponds to the',
     &          ' direction perpendicular to the fiber and in'/
     &          5x,' the  plane of the lamina.'/
     &          2x,' The number 3',
     &          ' corresponds to the direction perpendicular to the',
     &          ' plane of the'/5x' lamina.'/)
         print 52
52       format(' Examples:'/4x,'E1   Young''s modulus of',
     &          ' the lamina in the fiber direction.'//
     &          4x,'G13  Shear modulus in the plane formed',
     &          ' by a line parallel to the'/
     &          9x,'fiber and a line perpendicular to the lamina.'//
     &          4x,'Nu12 Poisson''s ratio for transverse',
     &          ' strain in the 2 direction when'/
     &          9x,'stressed in the fiber direction.')
     
      end if

      return
      end

