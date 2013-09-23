          subroutine CHECKX(X,OK)
      
      
c     Name:      CHECK X position
c     Purpose:   To check whether X is a valid nodal position for the given
c                geometry
c     Input:     X, the nodal position
c     Output:    Message to user about validity of nodal position
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : none
c                                                                      |
c*******************************************************************************


      implicit     none

      include      'pi.par'
      include      'geom.com'
      
      logical       OK
      real*8       X,R
      
      OK = .true.
      
      if (SHAPE .eq. 'cone') then
          R = X*sin(ALPHAD*PI/180.) + R0
          if (R .lt. 0.) then
             print 180
180          format(' R is less than 0.  Try a larger s.')
             OK = .false.
          end if
        else if (SHAPE .eq. 'torp') then
          R =  RC + RPHI*sin(X*PI/180.)
          if (R .lt. 0.) then
             print 190
190          format(' R is less than 0.  Try a larger phi.')
             OK = .false.
          end if
        else if (SHAPE .eq. 'para') then
          if ((X .lt. 0.) .or. (X .ge. 90.)) then
             print 200
200          format(' Phi must ge greater than 0 and less than 90.')
             OK = .false.
          end if
         
      end if
      
      return
      end
      
      
