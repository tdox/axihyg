      subroutine ELAM(EMAT,KAPA1,KAPA2)

c     Name:      E LAMinate
c     Purpose:   To calculate the E (stiffness matrix for laminated materials.
c     Common:    LAMSCF
c     Input:     KAPA1, KAPA2 and integrated laminate coefficients
c                (from LAMSCF common)       
c     Output:    E(MXNEPS,MXNEPS) stiffness matrix at a point
c     Called by: ESUB
c     Calls    : SYMTZR

      implicit     none
      
      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'lamscf.com'
      include      'layup.com'
      
      
      integer      I,J
      real*8       KAPA1,KAPA2,EMAT(MXNEPS,MXNEPS),K1MK2,K2MK1,
     &             K1K1MK2,K2K2MK1,H,H2,H4,H6,
     &             H2U4,H2U8,H4U16,H4U32,H6U64,
     &             I1,I2,IB1,IB2,K1,K2,K1PK2,K1K2,
c     
     &             B1111(0:6),B1112(0:6),B1121(0:6),B1122(0:6),
     &             B1212(0:6),B1221(0:6),B1222(0:6),B2121(0:6),
     &             B2122(0:6),B2222(0:6),
c     
     &             B1133(0:5),B2233(0:5),B1233(0:5),B2133(0:5),
c
     &             B3333(0:4),
c
     &             B1313(0:6),B2323(0:6),B1323(0:6)
     

      H  = LTHKNS
      H2 = H*H
      H4 = H2*H2
      H6 = H4*H2
      
      H2U4 = 4./H2
      H2U8 = 8./H2
      H4U16 = 16./H4
      H4U32 = 32./H4
      H6U64 = 64./H6
      
      
      K1MK2 = KAPA1 - KAPA2
      K2MK1 = - K1MK2
      

      K1K1MK2 = KAPA1*K1MK2
      K2K2MK1 = KAPA2*K2MK1
      
      if (TORORD .eq. 0) then
      
         I1  = 0.
         I2  = 0.
         IB1 = 0.
         IB2 = 0.
         K1  = 0.
         K2  = 0.
         K1PK2 = 0.
         K1K2  = 0.
      
        else if (TORORD .eq. 1) then
        
         I1  = K2MK1
         I2  = K1MK2
         IB1 = 0.
         IB2 = 0.
         K1  = KAPA1
         K2  = KAPA2
         K1PK2 = KAPA1 + KAPA2
         K1K2  = 0.
      
        else if (TORORD .eq. 2) then
        
         I1  = K2MK1
         I2  = K1MK2
         IB1 = K1K1MK2
         IB2 = K2K2MK1
         K1  = KAPA1
         K2  = KAPA2
         K1PK2 = KAPA1 + KAPA2
         K1K2  = KAPA1*KAPA2
         
      end if
      
      
      do 1 I=0,6
         B1111(I) = CN1111(I) + I1*CN1111(I+1) + IB1*CN1111(I+2)
         B1112(I) = CN1112(I)
         B1121(I) = CN1112(I) + I1*CN1112(I+1) + IB1*CN1112(I+2)
         B1122(I) = CN1122(I)
         B1212(I) = CN1212(I) + I2*CN1212(I+1) + IB2*CN1212(I+2)
         B1221(I) = CN1212(I)
         B1222(I) = CN2212(I) + I2*CN2212(I+1) + IB2*CN2212(I+2)
         B2121(I) = CN1212(I) + I1*CN1212(I+1) + IB1*CN1212(I+2)
         B2122(I) = CN2212(I)
         B2222(I) = CN2222(I) + I2*CN2222(I+1) + IB2*CN2222(I+2)
1     continue


      do 2 I=0,5
         B1133(I) = CN1133(I) + K2*CN1133(I+1)
         B2233(I) = CN2233(I) + K1*CN1133(I+1)
         B1233(I) = CN1233(I) + K2*CN1233(I+1)
         B2133(I) = CN1233(I) + K1*CN1233(I+1)
2     continue


      do 3 I=0,4
         B3333(I) = CN3333(I) + K1PK2*CN3333(I+1) + K1K2*CN3333(I+2)
3     continue
         
      
      if (SHEARC) then
      
         B1313(0) = (25./16.)
     &             * ( (CN1313(0) - H2U8*CN1313(2) + H4U16*CN1313(4))
     &          + I1 * (CN1313(1) - H2U8*CN1313(3) + H4U16*CN1313(5))
     &          + IB1* (CN1313(2) - H2U8*CN1313(4) + H4U16*CN1313(6)) )
                
         B2323(0) = (25./16.)
     &             * ( (CN2323(0) - H2U8*CN2323(2) + H4U16*CN2323(4))
     &          + I2 * (CN2323(1) - H2U8*CN2323(3) + H4U16*CN2323(5))
     &          + IB2* (CN2323(2) - H2U8*CN2323(4) + H4U16*CN2323(6)) )
     
         B1323(0) = (25./16.)
     &             * (CN1323(0) - H2U8*CN1323(2) + H4U16*CN1323(4))
              
                
         B1313(1) = (49./64.)
     &             * ( (H2U4*CN1313(2) - H4U32*CN1313(4)
     &                                 + H6U64*CN1313(6))
     &          + I1 * (H2U4*CN1313(3) - H4U32*CN1313(5)
     &                                 + H6U64*CN1313(7))
     &          + IB1* (H2U4*CN1313(4) - H4U32*CN1313(6)
     &                                 + H6U64*CN1313(8)) )
                
         B2323(1) = (49./64.)
     &             * ( (H2U4*CN2323(2) - H4U32*CN2323(4)
     &                                 + H6U64*CN2323(6))
     &          + I2 * (H2U4*CN2323(3) - H4U32*CN2323(5)
     &                                 + H6U64*CN2323(7))
     &          + IB2* (H2U4*CN2323(4) - H4U32*CN2323(6)
     &                                 + H6U64*CN2323(8)) )
     
         B1323(1) = (49./64.)
     &             *  (H2U4*CN1323(2) - H4U32*CN1323(4))
     
     
       else
       
         do 4 I=0,6
            B1313(I) = CN1313(I) + I1*CN1313(I+1) + IB1*CN1313(I+2)
            B2323(I) = CN2323(I) + I2*CN2323(I+1) + IB2*CN2323(I+2)
            B1323(I) = CN1323(I)
4        continue


      end if
         
      
      do 5 I=1,MXNEPS
           do 6 J=I,MXNEPS
                EMAT(I,J) = 0.
6          continue
5     continue
      
      EMAT(1,1)  = B1111(0)
      EMAT(1,2)  = B1122(0)
      EMAT(1,3)  = B1112(0)
      EMAT(1,4)  = B1121(0)
      EMAT(1,5)  = B1111(1)
      EMAT(1,6)  = B1122(1)
      EMAT(1,7)  = B1112(1)
      EMAT(1,8)  = B1121(1)
      EMAT(1,11) = B1133(0)
      EMAT(1,14) = B1133(1)
      
      EMAT(2,2)  = B2222(0)
      EMAT(2,3)  = B1222(0)
      EMAT(2,4)  = B2122(0)
      EMAT(2,5)  = B1122(1)
      EMAT(2,6)  = B2222(1)
      EMAT(2,7)  = B1222(1)
      EMAT(2,8)  = B2122(1)
      EMAT(2,11) = B2233(0)
      EMAT(2,14) = B2233(1)

      EMAT(3,3)  = B1212(0)
      EMAT(3,4)  = B1221(0)
      EMAT(3,5)  = B1112(1)
      EMAT(3,6)  = B1222(1)
      EMAT(3,7)  = B1212(1)
      EMAT(3,8)  = B1221(1)
      EMAT(3,11) = B1233(0)
      EMAT(3,14) = B1233(1)
      
      EMAT(4,4)  = B2121(0)
      EMAT(4,5)  = B1121(1)
      EMAT(4,6)  = B2122(1)
      EMAT(4,7)  = B1221(1)
      EMAT(4,8)  = B2121(1)
      EMAT(4,11) = B2133(0)
      EMAT(4,14) = B2133(1)
            
      EMAT(5,5)  = B1111(2)
      EMAT(5,6)  = B1122(2)
      EMAT(5,7)  = B1112(2)
      EMAT(5,8)  = B1121(2)
      EMAT(5,11) = B1133(1)
      EMAT(5,14) = B1133(2)
      
      EMAT(6,6)  = B2222(2)
      EMAT(6,7)  = B1222(2)
      EMAT(6,8)  = B2122(2)
      EMAT(6,11) = B2233(1)
      EMAT(6,14) = B2233(2)

      EMAT(7,7)  = B1212(2)
      EMAT(7,8)  = B1221(2)
      EMAT(7,11) = B1233(1)
      EMAT(7,14) = B1233(2)
      
      EMAT(8,8)  = B2121(2)
      EMAT(8,11) = B2133(1)
      EMAT(8,14) = B2133(2)
      
      
      EMAT(9,9)  = B1313(0)
      EMAT(9,10) = B1323(0)
      EMAT(9,12) = B1313(1)
      EMAT(9,13) = B1323(1)
      
      EMAT(10,10) = B2323(0)
      EMAT(10,12) = B1323(1)
      EMAT(10,13) = B2323(1)

      
      EMAT(11,11) = B3333(0)
      EMAT(11,14) = B3333(1)
      
      EMAT(12,12) = B1313(2)
      EMAT(12,13) = B1323(2)
      
      EMAT(13,13) = B2323(2)
      
      EMAT(14,14) = B3333(2)
      
      if (NEPS .ge. 15) then
         
         EMAT(1,15)  = B1111(2)
         EMAT(1,16)  = B1122(2)
         EMAT(1,17)  = B1112(2)
         EMAT(1,18)  = B1121(2)
         EMAT(1,19)  = B1111(3)         
         EMAT(1,20)  = B1122(3)
         EMAT(1,21)  = B1112(3)
         EMAT(1,22)  = B1121(3)
         EMAT(1,27)  = B1133(2)
         
         EMAT(2,15)  = B1122(2)
         EMAT(2,16)  = B2222(2)
         EMAT(2,17)  = B1222(2)
         EMAT(2,18)  = B2122(2)
         EMAT(2,19)  = B1122(3)
         EMAT(2,20)  = B2222(3)
         EMAT(2,21)  = B1222(3)
         EMAT(2,22)  = B2122(3)
         EMAT(2,27)  = B2233(2)
         
         EMAT(3,15)  = B1112(2)
         EMAT(3,16)  = B1222(2)
         EMAT(3,17)  = B1212(2)
         EMAT(3,18)  = B1221(2)
         EMAT(3,19)  = B1112(3)
         EMAT(3,20)  = B1222(3)
         EMAT(3,21)  = B1212(3)
         EMAT(3,22)  = B1221(3)
         EMAT(3,27)  = B1233(2)
         
         EMAT(4,15)  = B1121(2)
         EMAT(4,16)  = B2122(2)
         EMAT(4,17)  = B1221(2)
         EMAT(4,18)  = B2121(2)
         EMAT(4,19)  = B1121(3)
         EMAT(4,20)  = B2122(3)
         EMAT(4,21)  = B1221(3)
         EMAT(4,22)  = B2121(3)
         EMAT(4,27)  = B2133(2)
         
         EMAT(5,15)  = B1111(3)
         EMAT(5,16)  = B1122(3)
         EMAT(5,17)  = B1112(3)
         EMAT(5,18)  = B1121(3)
         EMAT(5,19)  = B1111(4)         
         EMAT(5,20)  = B1122(4)
         EMAT(5,21)  = B1112(4)
         EMAT(5,22)  = B1121(4)
         EMAT(5,27)  = B1133(3)
                  
         EMAT(6,15)  = B1122(3)
         EMAT(6,16)  = B2222(3)
         EMAT(6,17)  = B1222(3)
         EMAT(6,18)  = B2122(3)
         EMAT(6,19)  = B1122(4)
         EMAT(6,20)  = B2222(4)
         EMAT(6,21)  = B1222(4)
         EMAT(6,22)  = B2122(4)
         EMAT(6,27)  = B2233(3)
         
         EMAT(7,15)  = B1112(3)
         EMAT(7,16)  = B1222(3)
         EMAT(7,17)  = B1212(3)
         EMAT(7,18)  = B1221(3)
         EMAT(7,19)  = B1112(4)
         EMAT(7,20)  = B1222(4)
         EMAT(7,21)  = B1212(4)
         EMAT(7,22)  = B1221(4)
         EMAT(7,27)  = B1233(3)
         
         EMAT(8,15)  = B1121(3)
         EMAT(8,16)  = B2122(3)
         EMAT(8,17)  = B1221(3)
         EMAT(8,18)  = B2121(3)
         EMAT(8,19)  = B1121(4)
         EMAT(8,20)  = B2122(4)
         EMAT(8,21)  = B1221(4)
         EMAT(8,22)  = B2121(4)
         EMAT(8,27)  = B2133(3)      
         
         EMAT(9,23)  = B1313(2)
         EMAT(9,24)  = B1323(2)
         EMAT(9,25)  = B1313(3)
         EMAT(9,26)  = B1323(3)
         
         EMAT(10,23) = B1323(2)
         EMAT(10,24) = B2323(2)
         EMAT(10,25) = B1323(3)
         EMAT(10,26) = B2323(3)
         
         EMAT(11,15) = B1133(2)
         EMAT(11,16) = B2233(2)
         EMAT(11,17) = B1233(2)
         EMAT(11,18) = B2133(2)
         EMAT(11,19) = B1133(3)
         EMAT(11,20) = B2233(3)
         EMAT(11,21) = B1233(3)
         EMAT(11,22) = B2133(3)
         EMAT(11,27) = B3333(2)
         
         EMAT(12,23) = B1313(3)
         EMAT(12,24) = B1323(3)
         EMAT(12,25) = B1313(4)
         EMAT(12,26) = B1323(4)
         
         EMAT(13,23) = B1323(3)
         EMAT(13,24) = B2323(3)
         EMAT(13,25) = B1323(4)
         EMAT(13,26) = B2323(4)
         
         EMAT(14,15) = B1133(3)
         EMAT(14,16) = B2233(3)
         EMAT(14,17) = B1233(3)
         EMAT(14,18) = B2133(3)
         EMAT(14,19) = B1133(4)
         EMAT(14,20) = B2233(4)
         EMAT(14,21) = B1233(4)
         EMAT(14,22) = B2133(4)
         EMAT(14,27) = B3333(3)
         
         EMAT(15,15) = B1111(4)
         EMAT(15,16) = B1122(4)
         EMAT(15,17) = B1112(4)
         EMAT(15,18) = B1121(4)
         EMAT(15,19) = B1111(5)
         EMAT(15,20) = B1122(5)
         EMAT(15,21) = B1112(5)
         EMAT(15,22) = B1121(5)
         EMAT(15,27) = B1133(4)
         
         EMAT(16,16) = B2222(4)
         EMAT(16,17) = B1222(4)
         EMAT(16,18) = B2122(4)
         EMAT(16,19) = B1122(5)
         EMAT(16,20) = B2222(5)
         EMAT(16,21) = B1222(5)
         EMAT(16,22) = B2122(5)
         EMAT(16,27) = B2233(4)
         
         EMAT(17,17) = B1212(4)
         EMAT(17,18) = B1221(4)
         EMAT(17,19) = B1112(5)
         EMAT(17,20) = B1222(5)
         EMAT(17,21) = B1212(5)
         EMAT(17,22) = B1221(5)
         EMAT(17,27) = B1233(4)
         
         EMAT(18,18) = B2121(4)
         EMAT(18,19) = B1121(5)
         EMAT(18,20) = B2122(5)
         EMAT(18,21) = B1221(5)
         EMAT(18,22) = B2121(5)
         EMAT(18,27) = B2133(4)
         
         EMAT(19,19) = B1111(6)
         EMAT(19,20) = B1122(6)
         EMAT(19,21) = B1112(6)
         EMAT(19,22) = B1121(6)
         EMAT(19,27) = B1133(5)
         
         EMAT(20,20) = B2222(6)
         EMAT(20,21) = B1222(6)
         EMAT(20,22) = B2122(6)
         EMAT(20,27) = B2233(5)
         
         EMAT(21,21) = B1212(6)
         EMAT(21,22) = B1221(6)
         EMAT(21,27) = B1233(5)
         
         EMAT(22,22) = B2121(6)
         EMAT(22,27) = B2133(5)
         
         EMAT(23,23) = B1313(4)
         EMAT(23,24) = B1323(4)
         EMAT(23,25) = B1313(5)
         EMAT(23,26) = B1323(5)
         
         EMAT(24,24) = B2323(4)
         EMAT(24,25) = B1323(5)
         EMAT(24,26) = B2323(5)

         EMAT(25,25) = B1313(6)
         EMAT(25,26) = B1323(6)
         
         EMAT(26,26) = B2323(6)
         
         EMAT(27,27) = B3333(4)
         
         
      end if
      
      call SYMTZR(EMAT,MXNEPS)
      
      return
      end
