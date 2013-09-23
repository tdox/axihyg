      subroutine EHOMO(EMAT,KAPA1,KAPA2)

c     Name:      E HOmogenious
c     Purpose:   To calculate the E (stiffness matrix for homogeneous
c                (through the thickness) materials.
c     Common:    HOMMAT
c     Input:     KAPA1, KAPA2 and E, NU (from ISOMAT common)       
c     Output:    E(MXNEPS,MXNEPS) stiffness matrix at a point
c     Called by: ESUB
c     Calls    : SYMTZR

      implicit     undefined(a-z)
      
      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'hommat.com'
      
      
      integer      I,J
      real*8       KAPA1,KAPA2,EMAT(MXNEPS,MXNEPS),K1MK2,K2MK1,
     *             H,H2,H3,H5,H7,H9,H5O80,H7O448,H9OBIG,
     *             H2O28,H3O12,K1K1MK2,K2K2MK1,
     *             CONST11,CONST12,CONST21,CONST22,CONST31,
     *             CONST32,CONST41,CONST42,
c     
     *             H3K1,H3K2,H5K1,H5K2,H7K1,H7K2,H3K1MK2,H3K2MK1,
     *             H5K1MK2,H5K2MK1,H7K1MK2,H7K2MK1,H3K1K2,H5K1K2,H7K1K2,
     *             H3K1PK2,H5K1PK2,CSH11,CSH12,CSH21,CSH22
     
      real*8       B01111,B02222,B01122,B01212,B02121,B01221,
     *             B11111,B12222,B11122,B11212,B12121,B11221,
     *             B21111,B22222,B21122,B21212,B22121,B21221,
     *             B31111,B32222,B31122,B31212,B32121,B31221,
     *             B41111,B42222,B41122,B41212,B42121,B41221,
     *             B51111,B52222,B51122,B51212,B52121,B51221,
     *             B61111,B62222,B61122,B61212,B62121,B61221,
c     
     *             B01133,B02233,B11133,B12233,B21133,B22233,
     *             B31133,B32233,B41133,B42233,B51133,B52233,
c
     *             B03333,B13333,B23333,B33333,B43333,
c
     *             B01313,B02323,B11313,B12323,B21313,B22323,
     *             B31313,B32323,B41313,B42323,B51313,B52323,
     *             B61313,B62323
     

     
      H     = THCKNS
      H2    = H*H
      H3    = H2*H
      H5    = H2*H3
      H7    = H2*H5
      H9    = H2*H7
      
      K1MK2 = KAPA1 - KAPA2
      K2MK1 = - K1MK2
      
      H3O12  = H3/12.
      H5O80  = H5/80.
      H7O448 = H7/448.
      H9OBIG = H9/2304.
      
      H2O28  = H2/28.

      K1K1MK2 = KAPA1*K1MK2
      K2K2MK1 = KAPA2*K2MK1
      
      if (TORORD .eq. 0) then
      
         CONST11      = H
         CONST12      = H
         CONST21      = H3O12
         CONST22      = H3O12
         CONST31      = H5O80
         CONST32      = H5O80
         CONST41      = H7O448
         CONST42      = H7O448
         
         H3K1 = 0.
         H3K2 = 0.
         H5K1 = 0.
         H5K2 = 0.         
         
         H3K1MK2 = 0.
         H3K2MK1 = 0.
         H5K1MK2 = 0.
         H5K2MK1 = 0.
         H7K1MK2 = 0.
         H7K2MK1 = 0.

         H3K1K2  = 0.
         H5K1K2  = 0.
         H7K1K2  = 0.
         H3K1PK2 = 0.
         H5K1PK2 = 0.

         CSH11 = 1.
         CSH12 = 1.
         CSH21 = H
         CSH22 = H
      
      
        else if (TORORD .eq. 1) then
      
         CONST11      = H
         CONST12      = H
         CONST21      = H3O12
         CONST22      = H3O12
         CONST31      = H5O80
         CONST32      = H5O80
         CONST41      = H7O448
         CONST42      = H7O448
         
         H3K1 = H3O12*KAPA1
         H3K2 = H3O12*KAPA2
         H5K1 = H5O80*KAPA1
         H5K2 = H5O80*KAPA2         
         
         H3K1MK2 = H3O12*K1MK2
         H3K2MK1 = H3O12*K2MK1
         H5K1MK2 = H5O80*K1MK2
         H5K2MK1 = H5O80*K2MK1
         H7K1MK2 = H7O448*K1MK2
         H7K2MK1 = H7O448*K2MK1

         H3K1K2  = 0.
         H5K1K2  = 0.
         H7K1K2  = 0.
         
         H3K1PK2 = H3O12*(KAPA1 + KAPA2)
         H5K1PK2 = H5O80*(KAPA1 + KAPA2)

         CSH11 = 1.
         CSH12 = 1.
         CSH21 = H
         CSH22 = H
      
        else if (TORORD .eq. 2) then
      
         CONST11      = H     + H3O12   * K1K1MK2
         CONST12      = H     + H3O12   * K2K2MK1
         CONST21      = H3O12 + H5O80   * K1K1MK2
         CONST22      = H3O12 + H5O80   * K2K2MK1
         CONST31      = H5O80 + H7O448  * K1K1MK2
         CONST32      = H5O80 + H7O448  * K2K2MK1
         CONST41      = H7O448 + H9OBIG * K1K1MK2
         CONST42      = H7O448 + H9OBIG * K2K2MK1
         
         H3K1 = H3O12*KAPA1
         H3K2 = H3O12*KAPA2
         H5K1 = H5O80*KAPA1
         H5K2 = H5O80*KAPA2     
         H7K1 = H7O448*KAPA1
         H7K2 = H7O448*KAPA2    
         
         H3K1MK2 = H3O12*K1MK2
         H3K2MK1 = H3O12*K2MK1
         H5K1MK2 = H5O80*K1MK2
         H5K2MK1 = H5O80*K2MK1
         H7K1MK2 = H7O448*K1MK2
         H7K2MK1 = H7O448*K2MK1

         H3K1K2  = H3O12*KAPA1*KAPA2
         H5K1K2  = H5O80*KAPA1*KAPA2
         H7K1K2  = H7O448*KAPA1*KAPA2
         
         H3K1PK2 = H3O12*(KAPA1 + KAPA2)
         H5K1PK2 = H5O80*(KAPA1 + KAPA2)
         
         CSH11 = 1. + H2O28*K1K1MK2
         CSH12 = 1. + H2O28*K2K2MK1
         CSH21 = H  + H3O12*K1K1MK2
         CSH22 = H  + H3O12*K2K2MK1
         
      end if
            
      B01111 = C1111*CONST11
      B02222 = C2222*CONST12
      B01122 = C1122*H
      B01212 = C1212*CONST12
      B02121 = C1212*CONST11
      B01221 = C1212*H
      
      B11111 = C1111*H3K2MK1
      B12222 = C2222*H3K1MK2
      B11122 = 0.
      B11212 = C1212*H3K1MK2
      B12121 = C1212*H3K2MK1
      B11221 = 0.
      
      B21111 = C1111*CONST21
      B22222 = C2222*CONST22
      B21122 = C1122*H3O12
      B21212 = C1212*CONST22
      B22121 = C1212*CONST21
      B21221 = C1212*H3O12

      B31111 = C1111*H5K2MK1
      B32222 = C2222*H5K1MK2
      B31122 = 0.
      B31212 = C1212*H5K1MK2
      B32121 = C1212*H5K2MK1
      B31221 = 0.      
      
      B41111 = C1111*CONST31
      B42222 = C2222*CONST32
      B41122 = C1122*H5O80
      B41212 = C1212*CONST32
      B42121 = C1212*CONST31
      B41221 = C1212*H5O80
      
      B51111 = C1111*H7K2MK1
      B52222 = C2222*H7K1MK2
      B51122 = 0.
      B51212 = C1212*H7K1MK2
      B52121 = C1212*H7K2MK1
      B51221 = 0.      
      
      B61111 = C1111*CONST41
      B62222 = C2222*CONST42
      B61122 = C1122*H7O448
      B61212 = C1212*CONST42
      B62121 = C1212*CONST41
      B61221 = C1212*H7O448
      
      
      B01133 = C1133*H
      B02233 = C2233*H
      
      B11133 = C1133*H3K2
      B12233 = C2233*H3K1
      
      B21133 = C1133*H3O12
      B22233 = C2233*H3O12
      
      B31133 = C1133*H5K2
      B32233 = C2233*H5K1
      
      B41133 = C1133*H5O80
      B42233 = C2233*H5O80
      
      B51133 = C1133*H7K2
      B52233 = C2233*H7K1
      
      B03333 = C3333*(H + H3K1K2)
      B13333 = C3333*H3K1PK2
      B23333 = C3333*(H3O12 + H5K1K2)
      B33333 = C3333*H5K1PK2
      B43333 = C3333*(H5O80 + H7K1K2)
      
      if (SHEARC) then
         B01313 = 5./6.   *C1313*H*CSH11
         B02323 = 5./6.   *C2323*H*CSH12
         B21313 = 7./120. *C1313 * CSH21
         B22323 = 7./120. *C2323 * CSH22
       else
         B01313 = C1313*CONST11
         B02323 = C2323*CONST12
         
         B11313 = C1313*H3K2MK1
         B12323 = C2323*H3K1MK2
         
         B21313 = C1313*CONST21
         B22323 = C2323*CONST22
         
         B31313 = C1313*H5K2MK1
         B32323 = C2323*H5K1MK2
         
         B41313 = C1313*CONST31
         B42323 = C2323*CONST32
         
         B51313 = C1313*H7K2MK1
         B52323 = C2323*H7K1MK2
         
         B61313 = C1313*CONST41
         B62323 = C2323*CONST42
         
         
      end if
         
      
      do 1 I=1,MXNEPS
           do 1 J=I,MXNEPS
                EMAT(I,J) = 0.
1     continue
      
      EMAT(1,1)  = B01111
      EMAT(1,2)  = B01122
      EMAT(1,5)  = B11111
      EMAT(1,6)  = B11122
      EMAT(1,11) = B01133
      EMAT(1,14) = B11133
      
      EMAT(2,2)  = B02222
      EMAT(2,5)  = B11122
      EMAT(2,6)  = B12222
      EMAT(2,11) = B02233
      EMAT(2,14) = B12233
      
      EMAT(3,3)  = B01212
      EMAT(3,4)  = B01221
      EMAT(3,7)  = B11212
      EMAT(3,8)  = B11221
      
      EMAT(4,4)  = B02121
      EMAT(4,7)  = B11221
      EMAT(4,8)  = B12121
      
      EMAT(5,5)  = B21111
      EMAT(5,6)  = B21122
      EMAT(5,11) = B11133
      EMAT(5,14) = B21133
      
      EMAT(6,6)  = B22222
      EMAT(6,11) = B12233
      EMAT(6,14) = B22233
      
      EMAT(7,7)  = B21212
      EMAT(7,8)  = B21221
      
      EMAT(8,8)  = B22121
      
      EMAT(9,9)  = B01313
      EMAT(9,12) = B11313
      
      EMAT(10,10) = B02323
      EMAT(10,13) = B12323

      
      EMAT(11,11) = B03333
      EMAT(11,14) = B13333
      
      EMAT(12,12) = B21313
      
      EMAT(13,13) = B22323
      
      EMAT(14,14) = B23333
      
      if (NEPS .ge. 15) then
         
         EMAT(1,15)  = B21111
         EMAT(1,16)  = B21122
         EMAT(1,19)  = B31111
         EMAT(1,20)  = B31122
         EMAT(1,27)  = B21133
         
         EMAT(2,15)  = B21122
         EMAT(2,16)  = B22222
         EMAT(2,20)  = B32222
         EMAT(2,27)  = B22233
         
         EMAT(3,17)  = B21212
         EMAT(3,18)  = B21221
         EMAT(3,21)  = B31212
         EMAT(3,22)  = B31221
         
         EMAT(4,17)  = B21221
         EMAT(4,18)  = B22121
         EMAT(4,21)  = B31221
         EMAT(4,22)  = B32121
         
         EMAT(5,15)  = B31111
         EMAT(5,16)  = B31122
         EMAT(5,19)  = B41111
         EMAT(5,20)  = B41122
         EMAT(5,27)  = B31133
         
         EMAT(6,15)  = B31122
         EMAT(6,16)  = B32222
         EMAT(6,19)  = B41122
         EMAT(6,20)  = B42222
         EMAT(6,27)  = B32233
         
         EMAT(7,17)  = B31212
         EMAT(7,18)  = B31221
         EMAT(7,21)  = B41212
         EMAT(7,22)  = B41221
         
         EMAT(8,17)  = B31221
         EMAT(8,18)  = B32121
         EMAT(8,21)  = B41221
         EMAT(8,22)  = B42121
         
         EMAT(9,23)  = B21313
         EMAT(9,25)  = B31313
         
         EMAT(10,24) = B22323
         EMAT(10,26) = B32323
         
         EMAT(11,15) = B21133
         EMAT(11,16) = B22233
         EMAT(11,19) = B31133
         EMAT(11,20) = B32233
         EMAT(11,27) = B23333
         
         EMAT(12,23) = B31313
         EMAT(12,25) = B41313
         
         EMAT(13,24) = B32323
         EMAT(13,26) = B42323
         
         EMAT(14,15) = B31133
         EMAT(14,16) = B32233
         EMAT(14,19) = B41133
         EMAT(14,20) = B42233
         EMAT(14,27) = B33333
         
         EMAT(15,15) = B41111
         EMAT(15,16) = B41122
         EMAT(15,19) = B51111
         EMAT(15,20) = B51122
         EMAT(15,27) = B41133
         
         EMAT(16,16) = B42222
         EMAT(16,19) = B51122
         EMAT(16,20) = B52222
         EMAT(16,27) = B42233
         
         EMAT(17,17) = B41212
         EMAT(17,18) = B41221
         EMAT(17,21) = B51212
         EMAT(17,22) = B51221
         
         EMAT(18,18) = B42121
         EMAT(18,21) = B51221
         EMAT(18,22) = B52121
         
         EMAT(19,19) = B61111
         EMAT(19,20) = B61122
         EMAT(19,27) = B51133
         
         EMAT(20,20) = B62222
         EMAT(20,27) = B52233
         
         EMAT(21,21) = B61212
         EMAT(21,22) = B61221
         
         EMAT(22,22) = B62121
         
         EMAT(23,23) = B41313
         EMAT(23,25) = B51313
         
         EMAT(24,24) = B42323
         EMAT(24,26) = B52323

         EMAT(25,25) = B61313
         
         EMAT(26,26) = B62323
         
         EMAT(27,27) = B43333
         
         
      end if
      
      call SYMTZR(EMAT,MXNEPS)
      
      return
      end
