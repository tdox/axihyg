      subroutine LAMCOF
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      LAMinate COeFficients                                        *
c     Purpose:   To calculate the laminate stiffness and expansion            *
c                coefficients                                                 *
c     Input:     Material Properties from the file MAT.DAT                    *
c     Output:    The laminate coefficients in the common LAMCOF               *
c     Called by: INPUT                                                        *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************
 

      implicit    none
      
      include     'n.par'
      include     'io.par'
      include     'pi.par'
      include     'contrl.com'
      include     'layup.com'
      include     'matprp.com'
      include     'lamecf.com'
      include     'lamscf.com'
      
      integer     STK,N,MAT
      
      real*8      CP1111,CP2222,CP3333,
     &            CP1122,CP1133,CP2233,CP1212,CP1313,CP2323,
     &            C1111,C2222,C3333,
     &            C1122,C1133,C1233,C2233,C1212,C1112,C2212,
     &            C1313,C2323,C1323,
     &            BTPT11,BTPT22,BTPT33,BTPM11,BTPM22,BTPM33,T,
     &            BETT11,BETT22,BETT12,BETT33,
     &            BETM11,BETM22,BETM12,BETM33,
     &            ZK,ZK2,ZK3,ZK4,ZK5,ZK6,ZK7,ZK8,ZK9,
     &            ZKM1,ZKM12,ZKM13,ZKM14,ZKM15,ZKM16,
     &            ZKM17,ZKM18,ZKM19,
     &            IZ(0:8)
      
      do 1 N=0,8
         CN1111(N) = 0.
         CN2222(N) = 0.
         CN1122(N) = 0.
         CN1212(N) = 0.
         CN1112(N) = 0.
         CN2212(N) = 0.
1     continue

      do 2 N=0,8
         CN1313(N) = 0.
         CN2323(N) = 0.
         CN1323(N) = 0.
2     continue

      do 3 N=0,6
         CN1133(N) = 0.
         CN2233(N) = 0.
         CN1233(N) = 0.
3     continue

      do 4 N=0,6
         CN3333(N) = 0.
4     continue

      do 5 N=0,6
         BTTN11(N) = 0.
         BTTN22(N) = 0.
         BTTN12(N) = 0.
         BTTN33(N) = 0.
         BTMN11(N) = 0.
         BTMN22(N) = 0.
         BTMN12(N) = 0.
         BTMN33(N) = 0.
5     continue
      
      

      
      ZK     = - LTHKNS / 2.

      do 7 STK=1,NSTACK
      
         ZKM1 = ZK
         ZK   = ZKM1 + STHKNS(STK)
         
         MAT = MATNO(STK)
         
         if (MAT .gt. NMAT) then
            call ERROR('LAMCOF','That MATerial NO. is not defined  ')
         end if
         
comment: Calculate the elasticity constants (in the primed (lamina) coordinate
c        system) from the engineering constants

         call ENGTOC(E1(MAT),E2(MAT),E3(MAT),NU12(MAT),NU13(MAT),
     &               NU23(MAT),G12(MAT),G13(MAT),G23(MAT),
     &                  CP1111,CP2222,CP3333,CP1122,CP1133,CP2233,
     &                  CP1212,CP1313,CP2323)
     
comment: BTPT11: BeTa Prime Thermal  11;
c        BTPM11: BeTa Prime Moisture 11;
c        ALPHT1(I): ALPHa Thermal 1 for stack I

         call ALTOBT(BTPT11,BTPT22,BTPT33,
     &               ALPHT1(MAT),ALPHT2(MAT),ALPHT3(MAT),
     &               CP1111,CP2222,CP3333,CP1122,CP1133,CP2233)
     
         call ALTOBT(BTPM11,BTPM22,BTPM33,
     &               ALPHM1(MAT),ALPHM2(MAT),ALPHM3(MAT),
     &               CP1111,CP2222,CP3333,CP1122,CP1133,CP2233)
     
comment: If plane stress is assumed, calculate the reduced
c        stiffnesses
         
         if (.not. V3CODE) then
            CP1111  = CP1111 - CP1133*CP1133/CP3333
            CP2222  = CP2222 - CP2233*CP2233/CP3333
            CP1122  = CP1122 - CP1133*CP2233/CP3333
           
            BTPT11 = BTPT11 - CP1133*BTPT33/CP3333
            BTPT22 = BTPT22 - CP2233*BTPT33/CP3333
            
            BTPM11 = BTPM11 - CP1133*BTPM33/CP3333
            BTPM22 = BTPM22 - CP2233*BTPM33/CP3333
         end if

comment: Calculate the coefficients in the global coordinate system.
          
         T = THETA(STK) *PI/180.

         call STIFRO(CP1111,CP2222,CP3333,CP1122,CP1133,CP2233,
     &                  CP1212,CP1313,CP2323,T,
     &                  C1111, C2222, C3333,
     &                  C1122, C1133, C2233, C1233,
     &                  C1212, C1112, C2212, C1313, C2323, C1323)
     
         call STRRO(BTPT11,BTPT22,BTPT33,T,
     &              BETT11,BETT22,BETT33,BETT12)
     
         call STRRO(BTPM11,BTPM22,BTPM33,T,
     &              BETM11,BETM22,BETM33,BETM12)
     
     
         ZK2 = ZK*ZK
         ZK3 = ZK2*ZK
         ZK4 = ZK2*ZK2
         ZK5 = ZK3*ZK2
         ZK6 = ZK3*ZK3
         ZK7 = ZK4*ZK3
         ZK8 = ZK4*ZK4
         ZK9 = ZK5*ZK4
         
         ZKM12 = ZKM1*ZKM1
         ZKM13 = ZKM12*ZKM1
         ZKM14 = ZKM12*ZKM12
         ZKM15 = ZKM13*ZKM12
         ZKM16 = ZKM13*ZKM13
         ZKM17 = ZKM14*ZKM13
         ZKM18 = ZKM14*ZKM14
         ZKM19 = ZKM15*ZKM14
         
         IZ(0) = ZK - ZKM1
         IZ(1) = ZK2 - ZKM12
         IZ(2) = ZK3 - ZKM13
         IZ(3) = ZK4 - ZKM14
         IZ(4) = ZK5 - ZKM15
         IZ(5) = ZK6 - ZKM16
         IZ(6) = ZK7 - ZKM17
         IZ(7) = ZK8 - ZKM18
         IZ(8) = ZK9 - ZKM19
         
         if (SYM) then
         
            do 8 N=0,8,2
               CN1111(N) = CN1111(N) + C1111*IZ(N)
               CN2222(N) = CN2222(N) + C2222*IZ(N)
               CN1122(N) = CN1122(N) + C1122*IZ(N)
               CN1212(N) = CN1212(N) + C1212*IZ(N)
               CN1112(N) = CN1112(N) + C1112*IZ(N)
               CN2212(N) = CN2212(N) + C2212*IZ(N)
8           continue

            do 9 N=0,8,2
               CN1313(N) = CN1313(N) + C1313*IZ(N)
               CN2323(N) = CN2323(N) + C2323*IZ(N)
               CN1323(N) = CN1323(N) + C1323*IZ(N)
9           continue

            do 10 N=0,6,2
               CN1133(N) = CN1133(N) + C1133*IZ(N)
               CN2233(N) = CN2233(N) + C2233*IZ(N)
               CN1233(N) = CN1233(N) + C1233*IZ(N)
10          continue

            do 11 N=0,6,2
               CN3333(N) = CN3333(N) + C3333*IZ(N)
11          continue

            do 12 N=0,6,2
               BTTN11(N) = BTTN11(N) + BETT11*IZ(N)
               BTTN22(N) = BTTN22(N) + BETT22*IZ(N)
               BTTN12(N) = BTTN12(N) + BETT12*IZ(N)
               BTTN33(N) = BTTN33(N) + BETT33*IZ(N)
   
               BTMN11(N) = BTMN11(N) + BETM11*IZ(N)
               BTMN22(N) = BTMN22(N) + BETM22*IZ(N)
               BTMN12(N) = BTMN12(N) + BETM12*IZ(N)
               BTMN33(N) = BTMN33(N) + BETM33*IZ(N)
12          continue
            
          else
            
            do 13 N=0,8
               CN1111(N) = CN1111(N) + C1111*IZ(N)
               CN2222(N) = CN2222(N) + C2222*IZ(N)
               CN1122(N) = CN1122(N) + C1122*IZ(N)
               CN1212(N) = CN1212(N) + C1212*IZ(N)
               CN1112(N) = CN1112(N) + C1112*IZ(N)
               CN2212(N) = CN2212(N) + C2212*IZ(N)
13          continue

            do 14 N=0,8
               CN1313(N) = CN1313(N) + C1313*IZ(N)
               CN2323(N) = CN2323(N) + C2323*IZ(N)
               CN1323(N) = CN1323(N) + C1323*IZ(N)
14          continue

            do 15 N=0,6
               CN1133(N) = CN1133(N) + C1133*IZ(N)
               CN2233(N) = CN2233(N) + C2233*IZ(N)
               CN1233(N) = CN1233(N) + C1233*IZ(N)
15          continue

            do 16 N=0,6
               CN3333(N) = CN3333(N) + C3333*IZ(N)
16          continue

            do 17 N=0,6
               BTTN11(N) = BTTN11(N) + BETT11*IZ(N)
               BTTN22(N) = BTTN22(N) + BETT22*IZ(N)
               BTTN12(N) = BTTN12(N) + BETT12*IZ(N)
               BTTN33(N) = BTTN33(N) + BETT33*IZ(N)
   
               BTMN11(N) = BTMN11(N) + BETM11*IZ(N)
               BTMN22(N) = BTMN22(N) + BETM22*IZ(N)
               BTMN12(N) = BTMN12(N) + BETM12*IZ(N)
               BTMN33(N) = BTMN33(N) + BETM33*IZ(N)
17          continue
            
         end if
         
7      continue   
       
         
       if (SYM) then
          
         do 18 N=0,8,2
            CN1111(N) = CN1111(N) * 2.0  / (N+1)
            CN2222(N) = CN2222(N) * 2.0  / (N+1)
            CN1122(N) = CN1122(N) * 2.0  / (N+1)
            CN1212(N) = CN1212(N) * 2.0  / (N+1)
            CN1112(N) = CN1112(N) * 2.0  / (N+1)
            CN2212(N) = CN2212(N) * 2.0  / (N+1)
18       continue

         do 19 N=0,8,2
            CN1313(N) = CN1313(N) * 2.0  / (N+1)
            CN2323(N) = CN2323(N) * 2.0  / (N+1)
            CN1323(N) = CN1323(N) * 2.0  / (N+1)
19       continue

         do 20 N=0,6,2
            CN1133(N) = CN1133(N) * 2.0  / (N+1)
            CN2233(N) = CN2233(N) * 2.0  / (N+1)
            CN1233(N) = CN1233(N) * 2.0  / (N+1)
20       continue

         do 21 N=0,6,2
            CN3333(N) = CN3333(N) * 2.0  / (N+1)
21       continue

         do 22 N=0,6,2
            BTTN11(N) = BTTN11(N) * 2.0  / (N+1)
            BTTN22(N) = BTTN22(N) * 2.0  / (N+1)
            BTTN12(N) = BTTN12(N) * 2.0  / (N+1)
            BTTN33(N) = BTTN33(N) * 2.0  / (N+1)
   
            BTMN11(N) = BTMN11(N) * 2.0  / (N+1)
            BTMN22(N) = BTMN22(N) * 2.0  / (N+1)
            BTMN12(N) = BTMN12(N) * 2.0  / (N+1)
            BTMN33(N) = BTMN33(N) * 2.0  / (N+1)
22       continue

       else

         do 23 N=0,8
            CN1111(N) = CN1111(N)  / (N+1)
            CN2222(N) = CN2222(N)  / (N+1)
            CN1122(N) = CN1122(N)  / (N+1)
            CN1212(N) = CN1212(N)  / (N+1)
            CN1112(N) = CN1112(N)  / (N+1)
            CN2212(N) = CN2212(N)  / (N+1)
23       continue

         do 24 N=0,8
            CN1313(N) = CN1313(N)  / (N+1)
            CN2323(N) = CN2323(N)  / (N+1)
            CN1323(N) = CN1323(N)  / (N+1)
24       continue

         do 25 N=0,6
            CN1133(N) = CN1133(N)  / (N+1)
            CN2233(N) = CN2233(N)  / (N+1)
            CN1233(N) = CN1233(N)  / (N+1)
25       continue

         do 26 N=0,6
            CN3333(N) = CN3333(N)  / (N+1)
26       continue

         do 27 N=0,6
            BTTN11(N) = BTTN11(N)  / (N+1)
            BTTN22(N) = BTTN22(N)  / (N+1)
            BTTN12(N) = BTTN12(N)  / (N+1)
            BTTN33(N) = BTTN33(N)  / (N+1)
   
            BTMN11(N) = BTMN11(N)  / (N+1)
            BTMN22(N) = BTMN22(N)  / (N+1)
            BTMN12(N) = BTMN12(N)  / (N+1)
            BTMN33(N) = BTMN33(N)  / (N+1)
27       continue
        
      end if
      
      do 50 N=0,8
      
         write(DOCFIL,100) N,CN1111(N),N,CN2222(N),N,CN1122(N),
     &                     N,CN1212(N),N,CN1112(N),N,CN2212(N),
     &                     N,CN1313(N),N,CN2323(N),N,CN1323(N)
100      format(//' CN1111(',i1,') = ',e12.5,2x,
     &            ' CN2222(',i1,') = ',e12.5,2x,
     &            ' CN1122(',i1,') = ',e12.5,/
     &            ' CN1212(',i1,') = ',e12.5,2x,
     &            ' CN1112(',i1,') = ',e12.5,2x,
     &            ' CN2212(',i1,') = ',e12.5,/
     &            ' CN1313(',i1,') = ',e12.5,2x,
     &            ' CN2323(',i1,') = ',e12.5,2x,
     &            ' CN1323(',i1,') = ',e12.5)
50    continue


      do 55 N=0,6
      
         write(DOCFIL,110) N,CN1133(N),N,CN2233(N),N,CN1233(N),
     &                     N,CN3333(N)
110      format(//' CN1133(',i1,') = ',e12.5,2x,
     &            ' CN2233(',i1,') = ',e12.5,2x,
     &            ' CN1233(',i1,') = ',e12.5,/
     &            ' CN3333(',i1,') = ',e12.5)
55    continue

      do 60 N=0,6
      
         write(DOCFIL,120) N,BTTN11(N),N,BTTN22(N),
     &                     N,BTTN12(N),N,BTTN33(N),
     &                     N,BTMN11(N),N,BTMN22(N),
     &                     N,BTMN12(N),N,BTMN33(N)
120      format(//' BTTN11(',i1,') = ',e12.5,2x,
     &            ' BTTN22(',i1,') = ',e12.5,2x,
     &            ' BTTN12(',i1,') = ',e12.5,/
     &            ' BTTN33(',i1,') = ',e12.5//
     &            ' BTTM11(',i1,') = ',e12.5,2x,
     &            ' BTTM22(',i1,') = ',e12.5,2x,
     &            ' BTTM12(',i1,') = ',e12.5,/
     &            ' BTTM33(',i1,') = ',e12.5)
60    continue
       
      return
      end
      
      
      
