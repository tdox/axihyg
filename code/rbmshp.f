       subroutine RBMSHP(RBMSH,RBMPSH,X,X1,X2)
      
c     Name:      Rigid Body Mode SHaPe function.
c     Purpose:   To calculate the values of the rigid body shape functions at the
c                postion X
c     Common:    
c     Input:     X, meridional coordinate in degrees
c                X1,X2, meridional coordinates of the edges of the element
c     Output:    RBMSH(2) (Rigid Body Mode SHape function array),
c                  The I'th element is the
c                  value at X of the I'th rigid body mode shape function.
c                RBMPSH(2) Rigid Body Mode Primed SHape function array),
c                  The I'th element
c                  is the value at X of the derivative of the I'th linear
c                  isoparametric shape function.
c     Called by: BN121
c     Calls    : 

      implicit     undefined(a-z)
      
      include   'n.par'
      include   'pi.par'
      
      real*8    RBMSH(MXNRBM),RBMPSH(MXNRBM),X,X1,X2,
     *          SX,CX,SX1,CX1,SX2,CX2,CONST1,CONST2,XR,X1R,X2R

      XR  = X*PIODEG
      X1R = X1*PIODEG
      X2R = X2*PIODEG

c      XR  = X
c      X1R = X1
c      X2R = X2

      SX  = sin(XR)
      CX  = cos(XR)
      SX1 = sin(X1R)
      CX1 = cos(X1R)
      SX2 = sin(X2R)
      CX2 = cos(X2R)
      
      CONST1 = (SX2 - SX1)/(X2R -X1R)
      CONST2 = (CX2 - CX1)/(X2R -X1R)

comment: the factor PIODEG in RBMPSH(i) are required since X is in
c        degrees.  d sin(phi)/d phi = PI/180. * cos(phi)  9/15/90      
      RBMSH(1)  =  - SX + SX1  + CONST1 * (XR - X1R) 
c      RBMPSH(1) =  - CX        + CONST1        [error in version 1.0]
      RBMPSH(1) =  (- CX + CONST1)*PIODEG
      RBMSH(2)  =    CX - CX1 - CONST2 * (XR - X1R) 
c      RBMPSH(2) =  - SX       - CONST2         [error in version 1.0]
      RBMPSH(2) =  (- SX  - CONST2)*PIODEG
      
      return
      end
      
 
