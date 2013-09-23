      subroutine STIFRO(CP1111,CP2222,CP3333,CP1122,CP1133,CP2233,
     &                  CP1212,CP1313,CP2323,T,
     &                  C1111, C2222, C3333,
     &                  C1122, C1133, C2233, C1233,
     &                  C1212, C1112, C2212, C1313, C2323, C1323)
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      STIFfness ROtation                                           *
c     Purpose:   To calculate the rotated stiffness coefficients              *  
c     Input:     CPIJKL, the stiffness in the primed coordinate system        *
c                T, (Theta)  The angle which the primed coordinate system is  *
c                  rotated (about the x3 axix) through.                       *
c     Output:    CIJKL, the stiffness in the unprimed coordinate system       *
c                shell.                                                       *
c     Called by:                                                              *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************

      implicit     undefined(a-z)
      

      real*8       CP1111,CP2222,CP3333,CP1122,CP1133,CP2233,CP1212,
     &             CP1313,CP2323,T,
     &             C1111, C2222, C3333, C1122,
     &             C1133, C2233, C1233,
     &             C1212, C1112, C2212, C1313, C2323, C1323,
     &             C2T,C4T,S2T,S4T,
     &             U1,U2,U3,U4,U5,U8,U9,U10,U11

      C2T = cos(2.*T)
      C4T = cos(4.*T)
      S2T = sin(2.*T)
      S4T = sin(4.*T)
      
      U1  = (3.*CP1111 + 3.*CP2222 + 2.*CP1122 + 4.*CP1212)/8.
      U2  = (CP1111 - CP2222)/2.
      U3  = (CP1111 + CP2222 - 2.*CP1122 - 4.*CP1212)/8.
      U4  = (CP1111 + CP2222 + 6.*CP1122 - 4.*CP1212)/8.
      U5  = (CP1111 + CP2222 - 2.*CP1122 + 4.*CP1212)/8.
      U8  = (CP1133 + CP2233)/2.
      U9  = (CP1133 - CP2233)/2.
      U10 = (CP1313 + CP2323)/2.
      U11 = (CP1313 - CP2323)/2.
      
      C1111 = U1 + U2*C2T + U3*C4T
      C2222 = U1 - U2*C2T + U3*C4T 
      C3333 = CP3333
      C1122 = U4 - U3*C4T
      C1133 = U8 + U9*C2T
      C2233 = U8 - U9*C2T
      C1233 = U9*S2T
      C1212 = U5 - U3*C4T
      C1313 = U10 + U11*C2T
      C2323 = U10 - U11*C2T
      C1323 = U11*S2T
      C1112 = U2*S2T/2. + U3*S4T
      C2212 = U2*S2T/2. - U3*S4T
      
100   format(/' Theta = ',e12.5,/
     &       ' CP1111= ',e12.5,3x,'CP2222= ',e12.5,3x,'CP3333= ',e12.5/
     &       ' CP1122= ',e12.5,3x,'CP1133= ',e12.5,3x,'CP2233= ',e12.5/
     &       ' CP1212= ',e12.5,3x,'CP1313= ',e12.5,3x,'CP2323= ',e12.5//
     &       ' C1111 = ',e12.5,3x,'C2222 = ',e12.5,3x,'C3333 = ',e12.5/
     &       ' C1122 = ',e12.5,3x,'C1133 = ',e12.5,3x,'C2233 = ',e12.5/
     &       ' C1233 = ',e12.5,/
     &       ' C1212 = ',e12.5,3x,'C1112 = ',e12.5,3x,'C2212 = ',e12.5/
     &       ' C1313 = ',e12.5,3x,'C2323 = ',e12.5,3x,'C1323 = ',e12.5/)

      
      return
      end
      
