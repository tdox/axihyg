      subroutine LINSHP(LSH,LPSH,XI)
      
c     Name:      LINear SHAPe function.
c     Purpose:   To calculat the values of the linear shape function at the
c                postion XI
c     Common:    
c     Input:     XI, local element coordinate
c     Output:    LSH(2) (Linear SHape function array), The I'th element is the
c                  value at XI of the I'th linear isoparametric shape function.
c                LPSH(2) (Linear Primed SHape function array), The I'th element
c                  is the value at XI of the derivative of the I'th linear
c                  isoparametric shape function.
c     Called by: BCY121
c     Calls    : 

      implicit     undefined(a-z)
      
      real*8       LSH(2),LPSH(2),XI
      
      LSH(1) = (1. - XI)/2.
      LSH(2) = (1. + XI)/2.
      
      LPSH(1) = -0.5
      LPSH(2) = +0.5
      
      return
      end
      
      
