      subroutine ZEROR2(A,M,N)
      
c     Name:      ZERO 2 dimensional Real array
c     Purpose:   To set the values of the elements of a two dimensional array to
c                zero.
c     Input:     M,N the dimensions of A
c     Output:    A(M,N) with zero valued elements.
c     Called by:
c     Calls    : 
c     Common:

      implicit    undefined(a-z)
      
      integer     M,N,I,J
      
      real*8      A(M,N)
      
      do 1 I=1,M
         do 2 J=1,N
            A(I,J) = 0.
2        continue
1     continue

      return
      end
      
