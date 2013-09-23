      subroutine SYMTZR(A,N)

c     Name:      Symmetrize (real)
c     Purpose:   To symmetrize a real matrix
c     Input:     A (An N by N matrix with non empty elements in the
c                upper triangle)
c                N 
c     Output:    A (The matrix with the lower triangle filled)
c     Called by: EISO


      implicit    none
      integer     N,I,J
      real*8      A(N,N)
      
      do 1  I=1,N
           do 1 J=I+1,N
                A(J,I) = A(I,J)
1     continue
      
      return
      end
