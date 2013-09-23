      subroutine STRRO(BETP11,BETP22,BETP33,T,
     &                 BETA11,BETA22,BETA33,BETA12)
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      STRess or STRain ROtation                                    *
c     Purpose:   To calculate the rotated stresses or strains; or expansion   *
c                stress or strain coefficients                                *  
c     Input:     BETa Prime ij, the coefficients in the primed coordinate     *
c                system                                                       *
c                T, (Theta)  The angle through which the primed coordinate    *
c                 system is rotated (counter clockwise about the x3 axis).    *
c     Output:    BETA ij, the coefficients in the unprimed coordinate system  *
c                shell.                                                       *
c     Called by: LAMCOF                                                       *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************

      
      implicit     undefined(a-z)


      real*8       BETP11,BETP22,BETP33,T,BETA11,BETA22,BETA33,BETA12,
     &             C2T,S2T,HB1PB2,HB1MB2
     
      C2T = cos(2.*T)
      S2T = sin(2.*T)
     
      HB1PB2 = (BETP11 + BETP22)/2.
      HB1MB2 = (BETP11 - BETP22)/2.
      
      BETA11 = HB1PB2 + HB1MB2*C2T
      BETA22 = HB1PB2 - HB1MB2*C2T
      BETA33 = BETP33
      BETA12 = HB1MB2*S2T
      
100   format(/' Theta = ',e12.5,/
     &      ' BETP11= ',e12.5,3x,'BETP22= ',e12.5,3x,'BETP33= ',e12.5,/
     &      ' BETA11= ',e12.5,3x,'BETA22= ',e12.5,3x,'BETA33= ',e12.5,/
     &      ' BETA12= ',e12.5/)

      return
      end
      
      
