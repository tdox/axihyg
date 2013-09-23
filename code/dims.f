         subroutine DIMS(EXIST)
      
      
c     Name:      DIMenstion Subroutine
c     Purpose:   To input the dimensions of the shell 
c     Input:     EXIST, if true, dimension information already exits
c                various dimentions
c     Output:    various dimesions to the GEOM common
c     Commons:   GEOM
c     Called by: GEOM
c     Calls    : undefined(a-z)
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      
      
      include      'geom.com'
      include      'units.com'
      include      'pi.par'
      
      logical      TRU,EXIST
      
      if (SHAPE .eq. 'cyln') then
         if (EXIST) then
             print 100, RAD,LENUS
100          format(/' The radius of the cylinder midsurface is'/
     &              ' R = ',e12.5,x,a,'.',/
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) go to 10
         end if
         
         print 110, LENUL
110      format(' Enter the radius of the midsurface of the shell in ',
     &            a,'.')
5        read(*,*) RAD
         if (RAD .le. 0.) then
            print 120
120         format(' The radius must be greater than 0.  Please',
     &             ' reenter the radius.')
            go to 5 
         end if
    
10       if (EXIST) then
            print 130, CYLEN,LENUS
130          format(/' The length of the cylinder is',
     &           ' L = ',e12.5,x,a,'.',/,' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) return
         end if
         
         print 140, LENUL
140      format(' Enter the length of the shell in ',
     &            a,'.')
15       read(*,*) CYLEN
         if (CYLEN .le. 0.) then
            print 150
150         format(' The length must be greater than 0.  Please',
     &             ' reenter the length.')
            go to 15
         end if
       
         XF = 0.
         XS = CYLEN
       
             
        else if (SHAPE .eq. 'cone') then
        
         if (EXIST) then
             print 160, R0,LENUS
160          format(/' The radius of the cone midsurface at the top',
     &              ' of the cone is'/' Ro = ',e12.5,x,a,'.'/
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) go to 30
         end if
         
         print 170, LENUL
170      format(' Enter the radius of the cone midsurface at the top',
     &          ' of the'/' cone (Ro) in ',a,'.')
25        read(*,*) R0

         if (R0 .lt. 0.) then
            print 180
180         format(' The radius must be greater than or equal to 0.',
     &             ' Please reenter the number.')
            go to 25
         end if
         
         
30       if (EXIST) then
             print 190, ALPHAD
190          format(/' The angle formed by the axis',
     &              ' and generator alpha = ',e12.5,' degrees.',/,
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) go to 40
         end if
         
         print 200
200      format(' Enter the the angle formed by the axis',
     &          ' and generator (alpha) in degrees.')
35        read(*,*) ALPHAD
         
         if ((ALPHAD .le. 0.) .or. (ALPHAD .gt. 90.)) then
            print 210
210         format(' The angle must be greater than 0 and',
     &             ' and less than or equal 90.',/
     &             ' Please reenter the angle.')
            go to 35
         end if
         
         
         
40       if (EXIST) then
             print 220, CONEHT,LENUS
220          format(/' The height of the cone is H = ',e12.5,x,a,'.'/
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) return
         end if
         
         print 230, LENUL
230      format(' Enter the height of the cone (H) in ',a,'.')
45       read(*,*) CONEHT

         if (R0 .lt. 0.) then
            print 240
240         format(' The height must be greater than or equal to 0.',
     &             ' Please reenter the number.')
            go to 45
         end if
        
         XF = 0.
c         XS = CONEHT/cos(ALPHAD)
         XS = CONEHT/cos(ALPHAD*PIODEG)
       
        
        
        
        
        else if (SHAPE .eq. 'torp') then
           
         if (EXIST) then
             print 250, RC,LENUS
250          format(/' The distance between the center of the',
     &              ' toroidal section and the axis'/
     &              ' is Rc = ',e12.5,x,a,'.'/
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 60
         end if
         
        print 260, LENUL
260      format(' Enter the distance between',
     &          ' the center of the toroidal',
     &          ' section and the'/' axis (Rc) in ',a,'.')
55        read(*,*) RC

         if (RC .lt. 0.) then
            print 270
270         format(' This distance must be greater than or equal to 0.',
     &             ' Please reenter the number.')
            go to 55
         end if
         
         
60       if (EXIST) then
             print 280, RPHI,LENUS
280          format(/' The radius of curvature in the meridional',
     &              ' direction is'/' R phi = ',e12.5,x,a,'.',/,
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 70
         end if
         
         print 290, LENUL
290      format(' Enter the radius of curvature in the meridional',
     &              ' direction (R phi) in ',a,'.')
     
65        read(*,*) RPHI

         if (RPHI .le. 0.) then
            print 300
300         format(' The radius must be greater than 0.',
     &             ' Please reenter the number.')
            go to 65
         end if
         
70       call PHI12(EXIST)
           
         XF = PHI1
         XS = PHI2
         
           
           
       else if (SHAPE .eq. 'para') then
         if (EXIST) then
             print 350, RAT0,LENUS
350          format(/' The radius of curvature at the vertex of the',
     &              ' paraboloid'
     &              /' (R at phi=0) is R_nu = ',e12.5,x,a,'.'/
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 90
         end if
         
         print 360, LENUL
360      format(' Enter the radius of curvature at the vertex of the',
     &          ' paraboloid (R_nu, R at phi=0) in '/,x,a,'.')
95       read(*,*) RAT0

         if (RAT0 .le. 0.) then
            print 370
370         format(' The radius must be greater than 0.',
     &             ' Please reenter the number.')
            go to 95
         end if
         
         
90       if (EXIST) then
             print 380, OFFSET,LENUS
380          format(/' The distance the midsurface of the shell',
     &               ' is offset from the paraboloid'/
     &                ' is d = ',e12.5,x,a,'.',/,
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 98
         end if
         
         print 390, LENUL
390      format(' Enter the distance the midsurface of the shell',
     &          ' is offset from the'/' paraboloid (OFFSET) in ',a,'.')
         read(*,*) OFFSET
         
98       call PHI12(EXIST)
         
         XF = PHI1
         XS = PHI2


    
      end if
            
      
      return
      end
      
  
