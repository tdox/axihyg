       subroutine KESHRT(KE,B,EMAT,WTJR)
      
c     Name:      K (Element stiffness matrix) SHoRT subroutine
c     Purpose:   To calculate Ke by multiplying Bt*EMAT*B * Weight*J*R using
c                fewer calculations
c     Common:    
c     Input:     B(NEN,MXNEPS,MXNVAR), B matrix
c                EMAT(MXNEPS,MXNEPS), stiffness matrix
c                WTJ, weight*j*R
c     Output:    KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR), K Element 
c     Called by: FORMKF
c     Calls    : 

      implicit     undefined(a-z)
      
      include      'n.par'
      include      'elmdat.com'
      
      integer      I,J
      real*8       KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR),WTJR,
     *             B(MAXNEN,MXNEPS,MXNVAR),EMAT(MXNEPS,MXNEPS),
     *             B1(MXNEPS,MXNVAR),B2(MXNEPS,MXNVAR),
     *             B3(MXNEPS,MXNVAR),KES(MXNVAR,MXNVAR)
 
      do 1 I=1,MXNEPS
         do 2 J=1,MXNVAR
            B1(I,J) = 0.
            B2(I,J) = 0.
            B3(I,J) = 0.
2        continue
1     continue

      do 4 I=1,NEPS
         do 5 J=1,NDOFPN(1)
            B1(I,J) = B(1,I,J)
5        continue
         do 6 J=1,NDOFPN(2)
            B2(I,J) = B(2,I,J)
6        continue
         do 7 J=1,NDOFPN(3)
            B3(I,J) = B(3,I,J)
7        continue
4     continue

      call B1EB1(B1,EMAT,KES)
      do 8 I=1,NDOFPN(1)
         do 9 J=1,NDOFPN(1)
            KE(1,1,I,J) = KE(1,1,I,J) + KES(I,J)*WTJR
9        continue
8     continue

      call B1EB2(B1,B2,EMAT,KES)
      do 10 I=1,NDOFPN(1)
         do 11 J=1,NDOFPN(2)
            KE(1,2,I,J) = KE(1,2,I,J) + KES(I,J)*WTJR
11        continue
10     continue


      call B1EB3(B1,B3,EMAT,KES)
      do 12 I=1,NDOFPN(1)
         do 13 J=1,NDOFPN(3)
            KE(1,3,I,J) = KE(1,3,I,J) + KES(I,J)*WTJR
13        continue
12     continue


      call B2EB2(B2,EMAT,KES)
      do 14 I=1,NDOFPN(2)
         do 15 J=1,NDOFPN(2)
            KE(2,2,I,J) = KE(2,2,I,J) + KES(I,J)*WTJR
15        continue
14     continue

      call B1EB2(B3,B2,EMAT,KES)
      do 16 I=1,NDOFPN(2)
         do 17 J=1,NDOFPN(3)
            KE(2,3,I,J) = KE(2,3,I,J) + KES(J,I)*WTJR
17        continue
16     continue

 
      call B1EB1(B3,EMAT,KES)
      do 18 I=1,NDOFPN(3)
         do 19 J=1,NDOFPN(3)
            KE(3,3,I,J) = KE(3,3,I,J) + KES(I,J)*WTJR
19       continue
18    continue

      return
      end
      
      
 
