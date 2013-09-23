      subroutine BN121(B,N,XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB)
      
c     Name:      B and N (OSHPU=1,OSHPW=2,OSHPD=1)
c     Purpose:   To calculate the B and N matrices (OSHPU=1,OSHPW=2,
c                OSHPD=1) 
c     Common:    
c     Input:     KAPA1,KAPA2,OA1,OA2,A2D1,R,JACOB    
c     Output:    B(NEN,MXNEPS,MXNVAR),N(NEN,MXNVAR)
c     Called by: BN
c     Calls    : CLEARB,QUASHP,LINSHP

      implicit     undefined(a-z)
      
      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      
      integer      J
      real*8       B(MAXNEN,MXNEPS,MXNVAR),N(MAXNEN,MXNVAR),
     *             XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB,
     *             LSH(2),LPSH(2),QSH(3),QPSH(3),
     *             RBMSH(MXNRBM),RBMPSH(MXNRBM),C1,
     *             L1C1,L2C1,L1K1,L1K2,L2K1,L2K2,L1POA1,L2POA1,
     *             Q1K1,Q1K2,Q1POA1,Q2K1,Q2K2,Q2POA1,Q3K1,Q3K2,Q3POA1
     
100   format(' in bn121')
      
      call CLEARB(B)
      
      call QUASHP(QSH,QPSH,XI)
      call LINSHP(LSH,LPSH,XI)
      
      
      C1 = A2D1*OA1*OA2
      
      L1C1   = LSH(1)*C1
      L2C1   = LSH(2)*C1
      L1K1   = LSH(1)*KAPA1
      L1K2   = LSH(1)*KAPA2
      L2K1   = LSH(2)*KAPA1
      L2K2   = LSH(2)*KAPA2
      L1POA1 = LPSH(1)*OA1/JACOB
      L2POA1 = LPSH(2)*OA1/JACOB
      
      Q1K1   = QSH(1)*KAPA1
      Q1K2   = QSH(1)*KAPA2
      Q1POA1 = QPSH(1)*OA1/JACOB
      Q2K1   = QSH(2)*KAPA1
      Q2K2   = QSH(2)*KAPA2
      Q2POA1 = QPSH(2)*OA1/JACOB
      Q3K1   = QSH(3)*KAPA1
      Q3K2   = QSH(3)*KAPA2
      Q3POA1 = QPSH(3)*OA1/JACOB
            
      
      B(1,1,1)   = L1POA1
      B(1,1,3)   = Q1K1
      B(1,2,1)   = L1C1
      B(1,2,3)   = Q1K2
      B(1,3,2)   = -L1C1
      B(1,4,2)   = L1POA1
      B(1,5,4)   = L1POA1
      B(1,5,6)   = L1K1
      B(1,6,4)   = L1C1
      B(1,6,6)   = L1K2
      B(1,7,5)   = -L1C1
      B(1,8,5)   = L1POA1
      B(1,9,1)   = -L1K1
      B(1,9,3)   = Q1POA1
      B(1,9,4)   = LSH(1)
      B(1,10,2)  = -L1K2
      B(1,10,5)  = LSH(1)
      B(1,11,6)  = LSH(1)

      if (W1CHI) then
         B(1,12,6)  =  L1POA1
         B(1,12,10) = 2.*LSH(1)
         B(1,13,11) = 2.*LSH(1)
      else
         B(1,12,6)  = 0.
         B(1,12,10) = 0.
         B(1,13,11) = 0.
      endif

      B(1,14,7)  = 2.*LSH(1)
      B(1,15,7)  = L1K1
      B(1,15,10) = L1POA1
      B(1,16,7)  = L1K2
      B(1,16,10) = L1C1
      B(1,17,11) = -L1C1
      B(1,18,11) = L1POA1
      B(1,19,8)  = L1POA1
      B(1,19,12) = L1K1
      B(1,20,8)  = L1C1
      B(1,20,12) = L1K2
      B(1,21,9)  = -L1C1
      B(1,22,9)  = L1POA1
      B(1,23,7)  = L1POA1
      B(1,23,8)  = 3.*LSH(1)
      B(1,23,10) = L1K1
      B(1,24,9)  = 3.*LSH(1)
      B(1,24,11) = L1K2
      B(1,25,8)  = 2.*L1K1
      B(1,26,9)  = 2.*L1K2
      B(1,27,12) = 3.*LSH(1)
      
      B(2,1,1)  = Q2K1
      B(2,2,1)  = Q2K2
      B(2,9,1)  = Q2POA1
      
      B(3,1,1)   = L2POA1
      B(3,1,3)   = Q3K1
      B(3,2,1)   = L2C1
      B(3,2,3)   = Q3K2
      B(3,3,2)   = -L2C1
      B(3,4,2)   = L2POA1
      B(3,5,4)   = L2POA1
      B(3,5,6)   = L2K1
      B(3,6,4)   = L2C1
      B(3,6,6)   = L2K2
      B(3,7,5)   = -L2C1
      B(3,8,5)   = L2POA1
      B(3,9,1)   = -L2K1
      B(3,9,3)   = Q3POA1
      B(3,9,4)   = LSH(2)
      B(3,10,2)  = -L2K2
      B(3,10,5)  = LSH(2)
      B(3,11,6)  = LSH(2)

      if (W1CHI) then
         B(3,12,6)  = L2POA1
         B(3,12,10) = 2.*LSH(2)
         B(3,13,11) = 2.*LSH(2)
      else    
         B(3,12,6)  = 0.
         B(3,12,10) = 0.
         B(3,13,11) = 0.
      endif

      B(3,14,7)  = 2.*LSH(2)
      B(3,15,7)  = L2K1
      B(3,15,10) = L2POA1
      B(3,16,7)  = L2K2
      B(3,16,10) = L2C1
      B(3,17,11) = -L2C1
      B(3,18,11) = L2POA1
      B(3,19,8)  = L2POA1
      B(3,19,12) = L2K1
      B(3,20,8)  = L2C1
      B(3,20,12) = L2K2
      B(3,21,9)  = -L2C1
      B(3,22,9)  = L2POA1
      B(3,23,7)  = L2POA1
      B(3,23,8)  = 3.*LSH(2)
      B(3,23,10) = L2K1
      B(3,24,9)  = 3.*LSH(2)
      B(3,24,11) = L2K2
      B(3,25,8)  = 2.*L2K1
      B(3,26,9)  = 2.*L2K2
      B(3,27,12) = 3.*LSH(2)
      
      if (FLAT) then
         B(1,2,2)   = .5
         B(3,2,2)   = .5
      end if
      
      if (RIGID) then
         call RBMSHP(RBMSH,RBMPSH,X,X1,X2)
         
         B(2,1,2) = OA1*RBMPSH(1) + KAPA1*RBMSH(2)
         B(2,2,2) = C1*RBMSH(1) + KAPA2*RBMSH(2)
         B(2,9,2) = - KAPA1*RBMSH(1) + OA1*RBMPSH(2)
      end if
      
      do 1 J=1,NVAR
        N(1,J) = LSH(1)
1     continue
      N(1,3) = QSH(1)
         
      N(2,1) = QSH(2) 
      do 3 J=2,NVAR
         N(2,J) = 0.
3     continue

      if (RIGID) then
         do 4 J=1,NRBM
            N(2,J+NVAR) = RBMSH(J)
4        continue
      end if

      do 5 J=1,NVAR
        N(3,J) = LSH(2)
5     continue
      N(3,3) = QSH(3)

                
      return
      end
      
      
