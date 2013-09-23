      subroutine DIMENS(KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,S,CALCK)
      
c     Name:      DIMENSions
c     Purpose:   To calculate the curvatures of the shell at the meridional
c                 coordinate, S. 
c                
c     Input:     S, the meridional coordinate. S may be arclength,
c                 or angle phi in degrees, depending on shape.
c     Output:    KAPA1,KAPA2,OA1,OA2,A2D1,R,Z
c     Called by: FORMKF,FEEDGT,SHPPLT,RESULT,POSPLT
c     Calls    : 


      implicit     none
      
      include      'pi.par'
      include      'geom.com'
      
      logical      CALCK
      real*8       S,KAPA1,KAPA2,OA1,OA2,R,Z,PHI,A2D1,
     *             ALPHA,SA,CA,
     *             PHI0,SPHI,CPHI,C2PHI,C3PHI,TPHI,RTHETA
      
      if (SHAPE .eq. 'cyln') then
           R     = RAD
           if (CALCK) then
              KAPA1 = 0.
              KAPA2 = 1./R
              OA1   = 1.
              OA2   = 1./R
           end if
           Z     = S
           PHI   = PI/2.
           A2D1    = 0.
       else if (SHAPE .eq. 'cone') then
           ALPHA  = ALPHAD/180. * PI
           SA     = sin(ALPHA)
           CA     = cos(ALPHA)
           R      = R0 + S*SA
           if (CALCK) then
              KAPA1  = 0.
              KAPA2  = CA/R
              OA1   = 1.
              OA2   = 1./R
           end if
           Z      = S*CA
           PHI    = PI/2. - ALPHA
           A2D1     = SA
       else if (SHAPE .eq. 'torr') then
           PHI0   = PHI0D/180. * PI
           PHI    = PHI0 + S/RPHI
           SPHI   = sin(PHI)
           CPHI   = cos(PHI)
           R      = RC + RPHI*SPHI
           Z      = -RPHI*CPHI
           if (CALCK) then
              KAPA1  = 1./RPHI
              KAPA2  = SPHI/R
              OA1   = 1.
              OA2   = 1./R
           end if
           A2D1     = CPHI
       else if (SHAPE .eq. 'torp') then
           PHI    = S*PIODEG
c           PHI    = S
           SPHI   = sin(PHI)
           CPHI   = cos(PHI)
           R      = RC + RPHI*SPHI
           Z      = -RPHI*CPHI
           if (CALCK) then
              RTHETA = RC/SPHI + RPHI
              KAPA1  = 1./RPHI
              KAPA2  = 1./RTHETA
              OA1   = KAPA1/PIODEG
c              OA1   = KAPA1
              OA2   = 1./R
           end if
           A2D1     = RPHI*CPHI*PIODEG
c           A2D1     = RPHI*CPHI
       else if (SHAPE .eq. 'para') then
           if (S .ge. 90.) then
              call ERROR('DIMENS    ','S is greater than 90 degrees   ')
           end if
c          PHI Over Degrees = pi/180.   used to convert degrees to radians
           PHI    = S*PIODEG
           SPHI   = sin(PHI)
           CPHI   = cos(PHI)
           C2PHI  = CPHI*CPHI
           C3PHI  = C2PHI*CPHI
           TPHI   = SPHI/CPHI
           R      = RAT0*TPHI + OFFSET*SPHI
           Z      = R*R/(2.*RAT0) + OFFSET*(1. - CPHI)
           if (CALCK) then
              KAPA1  = C3PHI/(RAT0 + OFFSET*C3PHI)
              KAPA2  = CPHI/(RAT0 + OFFSET*CPHI)
              OA1   = KAPA1/PIODEG
              OA2   = 1./R
           end if
           A2D1 = (RAT0/C2PHI + OFFSET*CPHI)*PIODEG
           
           
       else if (SHAPE .eq. 'cyls') then
           R     = 1.
           if (CALCK) then
              KAPA1 = 1./RAD
              KAPA2 = 0.
              OA1   = KAPA1/PIODEG
              OA2   = 1.
           end if
           Z     = S
           PHI   = PI/2.
           A2D1    = 0.
        else       
           call ERROR('DIMENS    ','Improper SHAPE.                ')
      end if
      
      if (R .lt. 0.) then
        call ERROR('DIMENS    ',' R < 0.                        ')
      end if

      return
      end
      
      
