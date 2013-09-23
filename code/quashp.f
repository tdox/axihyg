      subroutine QUASHP(QSH,QPSH,XI)
      
c     Name:      QUAdratic SHaPe function.
c     Purpose:   To calculate the values of the quadratic shape function at the
c                position XI.
c     Common:    
c     Input:     XI, local element coordinate
c     Output:    QSH(3) (Quadratic SHape function array), The I'th element is the
c                  value at XI of the I'th quadratic isoparametric shape function.
c                QPSH(3) (Quadratic Primed SHape function array), The I'th element
c                  is the value at XI of the derivative of the I'th quadratic
c                  isoparametric shape function.
c     Called by: BCY121
c     Calls    : 

      implicit     none
      
      real*8       QSH(3),QPSH(3),XI,XI2
      
      XI2 = XI*XI
      
      QSH(1) = (XI2 - XI)/2.
      QSH(2) = 1. - XI2
      QSH(3) = (XI2 + XI)/2.
      
      
      QPSH(1) = XI - 0.5
      QPSH(2) = - 2.*XI
      QPSH(3) = XI + 0.5
      
      return
      end
      
      
