      subroutine FTHLAM(THERM,KAPA1,KAPA2,T0,T1,T2,F)
      
c     Name:      Force  vector due to THerMal loads for HOmogenious (through
c                the thickness) materials
c     Purpose:   To calculate the thermal load vector for homgeneous materials
c     Input:     KAPA1,KAPA2: the curvatures at the point.
c                THET0S,THET1S: THETa 0 at S and THETa 1 at S.
c                BETA: The coefficient of thermal expansion for isotropic 
c                 materials.
c     Output:    F, the thermal (if THERM=.true) or
c                moisture (if THERM = .false) expansion load vector.
c     Called by: FTHERM
c     Calls    : 

      implicit      undefined(a-z)
      include       'n.par'
      include       'contrl.com'
      include       'elmdat.com'
      include       'lamecf.com'
      
      logical       THERM
      integer       I

      real*8        F(MXNEPS),T0,T1,T2,KAPA1,KAPA2,
     &              K1,K2,K1K2,K1PK2,
     &              TR11(0:3),TR12(0:3),TR21(0:3),TR22(0:3),
     &              TR33(0:2),
     &              B11(0:6),B22(0:6),B12(0:6),B33(0:6)
      
100   format(' in fthmis')
      
      do 1 I=1,MXNEPS
         F(I) = 0.
1     continue

     
            
      if (TORORD .eq. 2) then
        K1    = KAPA1
        K2    = KAPA2
        K1PK2 = KAPA1 + KAPA2
        K1K2  = KAPA1*KAPA2
       else if (TORORD .eq. 1) then
        K1    = KAPA1
        K2    = KAPA2
        K1PK2 = KAPA1 + KAPA2
        K1K2  = 0.
       else if (TORORD .eq. 0) then
        K1    = 0.
        K2    = 0.
        K1PK2 = 0.
        K1K2  = 0.
       else
         call ERROR('FTHHOM',' TORORD must be 0,1,2')
      
      end if
      
      if (THERM) then
         do 4 I=0,6
            B11(I) = BTTN11(I)
            B22(I) = BTTN22(I)
            B12(I) = BTTN12(I)
            B33(I) = BTTN33(I)
4        continue
       else
         do 5 I=0,6      
            B11(I) = BTMN11(I)
            B22(I) = BTMN22(I)
            B12(I) = BTMN12(I)
            B33(I) = BTMN33(I)
5        continue
      end if
      
      do 2 I=0,3
      
         TR11(I) = T0*B11(I) + (K2*T0 + T1)*B11(I+1)
     &                       + (K2*T1 + T2)*B11(I+2)
     &                       + (K2*T2)     *B11(I+3)
         TR12(I) = T0*B12(I) + (K2*T0 + T1)*B12(I+1)
     &                       + (K2*T1 + T2)*B12(I+2)
     &                       + (K2*T2)     *B12(I+3)
         TR21(I) = T0*B12(I) + (K1*T0 + T1)*B12(I+1)
     &                       + (K1*T1 + T2)*B12(I+2)
     &                       + (K1*T2)     *B12(I+3)
         TR22(I) = T0*B22(I) + (K1*T0 + T1)*B22(I+1)
     &                       + (K1*T1 + T2)*B22(I+2)
     &                       + (K1*T2)     *B22(I+3)
  
2     continue


            
      F(1) = TR11(0)
      F(2) = TR22(0)
      F(3) = TR21(0)
      F(4) = TR12(0)
      F(5) = TR11(1)
      F(6) = TR22(1)
      F(7) = TR21(1)
      F(8) = TR12(1)
      
      if (NEPS .ge. 11) then
      
         do 3 I=0,2
         
            TR33(I) =    T0                      *B33(I)
     &                + (T1 + K1PK2*T0)          *B33(I+1)
     &                + (T2 + K1PK2*T1 + K1K2*T0)*B33(I+2)
     &                + (     K1PK2*T2 + K1K2*T1)*B33(I+3)
     &                + (                K1K2*T2)*B33(I+4)
         
3        continue


         F(11) = TR33(0)
         F(14) = TR33(1)
         
         
         if (NEPS .ge. 15) then
      
            
            F(15) = TR11(2)
            F(16) = TR22(2)
            F(17) = TR21(2)
            F(18) = TR12(2)
            F(19) = TR11(3)
            F(20) = TR22(3)
            F(21) = TR21(3)
            F(22) = TR12(3)            
            F(27) = TR33(2)
            
         end if
            
      end if
           
      return
      end

      

