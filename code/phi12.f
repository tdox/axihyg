         subroutine PHI12(EXIST)
      
      
c     Name:      Phi 1 and 2
c     Purpose:   To input Phi1 and Phi2, the angles at the edges 
c     Input:     EXIST, if true, dimension information already exits
c                various dimentions
c     Output:    Phi1 and Phi 2 to the GEOM common
c     Commons:   GEOM
c     Called by: DIMS
c     Calls    : None
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      
      include      'geom.com'
      
      logical   EXIST,TRU,OK
      
70       if (EXIST) then
             print 310, PHI1
310          format(/' The angle between a line segment parallel',
     &              ' to the axis of the shell'/' and a line segment',
     &              ' normal to the midsurface at the first edge',
     &              ' is'/' Phi_top = ',e12.5,x,'degrees.',/
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 80
         end if
         
75       print 320
320      format(' Enter the angle between a line segment parallel',
     &          ' to the axis of the shell'/' and a line segment',
     &          ' normal to the midsurface at the first edge',/
     &          ' (Phi_top) in degrees.')
     
         read(*,*) PHI1

         call CHECKX(PHI1,OK)
         if (.not. OK) go to 75

            
           
80       if (EXIST) then
             print 330, PHI2
330          format(/' The angle between a line segment parallel',
     &              ' to the axis of the shell'/' and a line segment',
     &              ' normal to the midsurface at the second edge',
     &              ' is'/' Phi_bot = ',e12.5,x,'degrees.',/,
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 90
         end if
         
         print 340
340      format(' Enter the angle between a line segment parallel',
     &          ' to the axis of the shell'/' and a line segment',
     &          ' normal to the midsurface at the second edge'/
     &          ' (Phi_bot) in degrees.')
     
85       read(*,*) PHI2

         call CHECKX(PHI2,OK)
         if (.not. OK) go to 85
         
90       if (PHI2 .le. PHI1) then
            print 350
350         format(/' Phi 2 must be greater than Phi 1')
            EXIST = .true.
            go to 70
         end if
           
         return
         end
         
         
