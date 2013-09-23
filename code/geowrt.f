      subroutine GEOWRT(UNITNO)
      
c     Name:      GEOmetry WRiTe
c     Purpose:   To write the geometry data  to the unit(file, screen) UNITNO.
c     Input:     UNITNO
c                Geometry data from the Geometry common
c     Output:    The geometry  data to the UNITNO unit
c     Called by: INWRT,LAYUPS
c     Calls    : 
c                                                                      |
c*******************************************************************************


      implicit      none
      
      include      'geom.com'
      include      'units.com'
      
      character*20 SHAPEL
      
      integer      UNITNO
      
      
      if (SHAPE .eq. 'cyln') then
          SHAPEL = 'cylinder'
        else if (SHAPE .eq. 'cone') then
          SHAPEL = 'cone'
        else if (SHAPE .eq. 'torp') then
          SHAPEL = 'toroidal section'
        else if (SHAPE .eq. 'para') then
          SHAPEL = 'paraboloid'
        else if (SHAPE .eq. 'torr') then
          SHAPEL = 'toorroidal section'
        else if (SHAPE .eq. 'cyls') then
          SHAPEL = 'cylindrical section'
      end if
      
      write(UNITNO,100) SHAPEL
100   format(///' Shell Geometry'//' Shape of the shell:  ',a,/)

      if (SHAPE .eq. 'cyln') then
           write(UNITNO,110) RAD,LENUS,CYLEN,LENUS
110        format(' Radius = ',e12.5,x,a/
     &            ' Length = ',e12.5,x,a)
         else if (SHAPE .eq. 'cone') then
           write(UNITNO,120) R0,LENUS,ALPHAD,CONEHT,LENUS
120        format(' Radius (@ s=0) = ',e12.5,x,a/
     *            ' Alpha          = ',e12.5,' degrees'/
     &            ' Height         = ',e12.5,x,a)
         else if (SHAPE .eq. 'torp') then
           write(UNITNO,130) RC,LENUS,RPHI,LENUS,PHI1,PHI2
130        format(' Radius of center = ',e12.5,x,a/
     *            ' R phi            = ',e12.5,x,a/
     &            ' Phi_top          = ',e12.5,' degrees'/
     &            ' Phi_bot          = ',e12.5,' degrees')
         else if (SHAPE .eq. 'torr') then
           write(UNITNO,135) RC,LENUS,RPHI,LENUS,PHI0D
135        format(' Radius of center = ',e12.5,x,a/
     *            ' R phi            = ',e12.5,x,a/
     *            ' Phi (@ s=0)      = ',e12.5,' degrees')
         else if (SHAPE .eq. 'para') then
           write(UNITNO,140) RAT0,LENUS,OFFSET,LENUS,PHI1,PHI2
140        format(' Radius 1 @ phi=0. = ',e12.5,x,a/
     *            ' Offset            = ',e12.5,x,a/
     &            ' Phi_top           = ',e12.5,' degrees'/
     &            ' Phi_bot           = ',e12.5,' degrees')
         else if (SHAPE .eq. 'cyls') then
           write(UNITNO,150) RAD,LENUS,PHI1,PHI2
150        format(' Radius  = ',e12.5,x,a/
     &            ' Phi_top = ',e12.5,' degrees'/
     &            ' Phi_bot = ',e12.5,' degrees')
      end if

      return
      end
      
      
