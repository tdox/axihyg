        subroutine SHAPES(EXIST,DIMEXT)
      
      
c     Name:      SHAPE Subroutine
c     Purpose:   To input the shape of the shell 
c     Input:     EXIST, if true, shape information exists already
c                SHAPE, from user
c     Output:    SHAPE
c                DIMEXT, logical ; if .true. then dimesion information exits
c     Commons:   GEOM
c     Called by: GEOM
c     Calls    : none
c                                                                      |
c*******************************************************************************
 
 
      implicit     none
      
      
      include      'geom.com'
      include      'units.com'
      
      logical      TRU,EXIST,DIMEXT
      character*2  SHAPS
      character*17 SHAPEL
      
      if (EXIST) then
         if (SHAPE .eq. 'cyln') then
             SHAPEL = 'cylinder'
           else if (SHAPE .eq. 'cone') then
             SHAPEL = 'cone'
           else if (SHAPE .eq. 'torp') then
             SHAPEL = 'toroidal section'
           else if (SHAPE .eq. 'para') then
             SHAPEL = 'paraboloid'
         end if
            
         print 100, SHAPEL
100      format(//' The current shell is a 'a,'.'/
     &          ' Do you want to change the shape?')
     
         call YESNO(TRU)
         
         if (.not. TRU) then
            DIMEXT = .true.
            return
         end if
            
      end if 
      
      DIMEXT = .false.
      
      print 110
110   format(//' Enter the shape of the shell generator (see figure):'/
     &       ' Enter "cy" for a cylinder.'/
     &       '       "co" for a cone.'/
     &       '       "to" for a toroidal section.'/
     &       '       "pa" for a paraboloid.')
1     read(*,'(a)') SHAPS
      
      if ((SHAPS .eq. 'cy') .or. (SHAPS .eq. 'CY')) then
           SHAPE = 'cyln'
           X1    = 's'
           X1US  = LENUS
           X1UL  = LENUL
         else if ((SHAPS .eq. 'co') .or. (SHAPS .eq. 'CO')) then
           SHAPE = 'cone'
           X1    = 's'           
           X1US  = LENUS
           X1UL  = LENUL
         else if ((SHAPS .eq. 'to') .or. (SHAPS .eq. 'TO')) then
           SHAPE = 'torp'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
         else if ((SHAPS .eq. 'pa') .or. (SHAPS .eq. 'PA')) then
           SHAPE = 'para'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
         else
           print 120
120        format(' Please enter "cy", "co", "to", or "pa".')
           go to 1
      end if        
      
      return
      end
      
 
