      subroutine OUTPUT

      implicit     none

      include      'io.par'
      include      'n.par'
      include      'bc.com'
      include      'contrl.com'
      include      'elmdat.com'
      include      'geom.com'
      include      'hommat.com'
      include      'io.com'
      include      'layup.com'
      include      'load.com'
      include      'matcod.com'
      include      'matprp.com'
      include      'out.com'
      include      'strses.com'
      include      'title.com'
      include      'units.com'
      
      integer      NODE,I,J,VAR,SPTNO,NOENOD,EL,DOF
      character*17 SHAPEL
         
      open (unit=OUTFIL, file= OTFILE, status= 'new')

      call INWRT(OUTFIL)
      

1240  format(//' Number of nodes per element:',i4,/,
     *         ' Number of nodes:            ',i4//)

1250  format(' IEN transpose')
1260       format(20I4)
20    continue

1270  format(//' ID transpose')
25    continue

      write(OUTFIL,1280) NUMEQ
1280  format(/' No. of equations:',I5)
1290  format(/' NDOFPN')
1300  format(20I5)

1310  format(//' FDOF')
1320  format(20I5)
30    continue

      write(OUTFIL,1325) LNKSKY
1325  format(//' Length of K skyline = ',I5)
1330  format(//' JDIAG')
     
35    continue

      if (.not. BACK) then
         write(OUTFIL,1331)
1331     format(////' Not solving for displacements.')
         return
      end if
      


      write(OUTFIL,1340)
1340  format(///'   Displacements'//)


      
      write(OUTFIL,1350) X1,X1US,DISPUS(0),DISPUS(0),DISPUS(0),
     *                   DISPUS(1),DISPUS(1)
1350  format(x,'Node',4x,a,10x,'U 1',10x,'U 2',10x,'W',
     *      10x,'1 Beta1',6x,'1 Beta2'/9x,a,6x,a,4(7x,a))
     
      do 40 NODE=1,NUMNOD
         write(OUTFIL,1360) NODE, XNODE(NODE),
     *                     (DISP(NODE,VAR), VAR=1,5)
40    continue
1360  format(i3,20(x,e12.5))



      if (NVAR .ge. 12) then
       
           write(OUTFIL,1370) X1,X1US,DISPUS(1),DISPUS(2),DISPUS(3)
1370       format(//x,'Node',4x,a,10x,'1 Eta ',
     *            7x,'2 Eta',7x,'3 Eta'/9x,a,6x,a,2(7x,a))

           do 50 NODE=1,NUMNOD
              write(OUTFIL,1360) NODE, XNODE(NODE),
     *                         (DISP(NODE,VAR), VAR=6,7),DISP(NODE,12)
50         continue


        else if (NVAR .ge. 7) then
       
           write(OUTFIL,1380) X1,X1US,DISPUS(1),DISPUS(2)
1380       format(//x,'Node',4x,a,10x,'1 Eta ',7x,'2 Eta'/ 
     &            9x,a,6x,a,(7x,a))

           do 55 NODE=1,NUMNOD
              write(OUTFIL,1360) NODE, XNODE(NODE),
     *                         (DISP(NODE,VAR), VAR=6,7)
55         continue


        else if (NVAR .ge. 6) then
      
           write(OUTFIL,1390) X1,X1US,DISPUS(1)
1390       format(//x, 'Node',5x,a,10x,'1 Eta'/
     &            9x,a,6x,a)

           do 60 NODE=1,NUMNOD
              write(OUTFIL,1360) NODE, XNODE(NODE),DISP(NODE,6)
60         continue

      end if





       if (NVAR .ge. 11) then
       
           write(OUTFIL,1400)X1,X1US,DISPUS(2),DISPUS(2),DISPUS(3),
     *                   DISPUS(3)
1400       format(//x,'Node',4x,a,9x,'2 Beta1',6x,'2 Beta2',
     *          6x,'3 Beta1',6x,'3 Beta2'/
     &          9x,a,6x,a,4(7x,a))
         
           do 65 NODE=1,NUMNOD
              write(OUTFIL,1360) NODE, XNODE(NODE),
     *                          (DISP(NODE,VAR), VAR=10,11),
     *                          (DISP(NODE,VAR), VAR=8,9)
65         continue

         else if (NVAR .ge. 9) then
         
           write(OUTFIL,1410) X1,X1US,DISPUS(3),DISPUS(3)
1410       format(//x,'Node',4x,a,9x,'3 Beta1',6x,'3 Beta2'/
     &             9x,a,6x,a,2(7x,a))
         
          do 70 NODE=1,NUMNOD
             write(OUTFIL,1360) NODE, XNODE(NODE),
     *                          (DISP(NODE,VAR), VAR=8,9)
70        continue
        
      end if
               

      if (RIGID) then
      
         write(OUTFIL,1411) DISPUS(0)
1411     format(//' Rigid Body Motion Degrees of Freedom'//
     *             2x,'Element',7x,'Uz'/17x,a)
         do 71 EL=1,NUMEL
            NODE = IEN(2,EL)
            write(OUTFIL,1412) EL,(DISP(NODE,DOF), DOF=NVAR+1,NVAR+NRBM)
1412        format(3x,i3,10(3x,e12.5))
71       continue

      end if



      
      if (RSLT) then
      
         write(OUTFIL,1420)
1420     format(////'   Strains'//)      

         write(OUTFIL,1430) X1,X1US,DISPUS(1),DISPUS(1),DISPUS(1),
     &                      DISPUS(1)
1430     format(8x,a,8x,'Gama11',7x,'Gama22',7x,'Gama21'7x,'Gama12'/
     &          6x,a,4(7x,a))

         do 75 SPTNO = 1,NOSPT
            write(OUTFIL,1440) SOPT(SPTNO), (STRAIN(SPTNO,I), I=1,4)
75      continue
1440     format(x,5(x,e12.5))


         write(OUTFIL,1450) X1,X1US,DISPUS(2),DISPUS(2),DISPUS(2),
     &                      DISPUS(2)
1450     format(//,8x,a,8x,'1 Kapa11',5x,'1 Kapa22',5x,
     *                        '1 Kapa21',5x,'1 Kapa12'/
     &          6x,a,4(7x,a))
      
         do 80 SPTNO = 1,NOSPT
            write(OUTFIL,1440) SOPT(SPTNO), (STRAIN(SPTNO,I), I=5,8)
80      continue


         if (NEPS .ge. 22) then
         
            write(OUTFIL,1460) X1,X1US,DISPUS(3),DISPUS(3),DISPUS(3),
     &                      DISPUS(3)
1460        format(//,8x,a,9x,'2 Kapa11',5x,'2 Kapa22',5x,'2 Kapa21',
     *                    5x,'2 Kapa12'/
     &                    6x,a,4(7x,a))
      
            do 85 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRAIN(SPTNO,I), I=15,18)
85         continue

            write(OUTFIL,1470) X1,X1US,DISPUS(4),DISPUS(4),DISPUS(4),
     &                      DISPUS(4)
1470        format(//,8x,a,8x,'3 Kapa11',5x,'3 Kapa22',5x,'3 Kapa21',
     *                       5x,'3 Kapa12'/
     &                    6x,a,4(7x,a))
      
            do 90 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRAIN(SPTNO,I), I=19,22)
90         continue

         end if
      
      
      
      
         if (NEPS .ge. 27) then 

            write(OUTFIL,1480)X1,X1US,DISPUS(1),DISPUS(2),DISPUS(3)
1480        format(//,8x,a,8X,'0 Lambda',5x,'1 Lambda',5x,'2 Lambda'/
     &          6x,a,3(7x,a))
            do 95 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), STRAIN(SPTNO,11),
     *                           STRAIN(SPTNO,14),STRAIN(SPTNO,27)
95         continue


          else if (NEPS .ge. 14) then
       
            write(OUTFIL,1490)X1,X1US,DISPUS(1),DISPUS(2)
1490        format(//,8x,a,8X,'0 Lambda',5x,'1 Lambda'/
     &          6x,a,2(7x,a))
            do 100 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), STRAIN(SPTNO,11),
     *                           STRAIN(SPTNO,14)
100         continue

          else if (NEPS .ge. 11) then
       
            write(OUTFIL,1500)X1,X1US,DISPUS(1)
1500        format(//,8x,a,8X,'0 Lambda'/
     &          6x,a,7x,a)
            do 105 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), STRAIN(SPTNO,11)
105         continue

          end if




          if (NEPS .ge. 26) then
      
            write(OUTFIL,1510) X1,X1US,DISPUS(1),DISPUS(1),
     &                         DISPUS(2),DISPUS(2)
1510        format(//,8x,a,8x,'1 Omega1',5x,'1 Omega2',6x,
     *                   '1 Xi1',8x,'1 Xi2'/ 
     &          6x,a,4(7x,a))
      
             do 110 SPTNO = 1,NOSPT
                write(OUTFIL,1440) SOPT(SPTNO),
     *                             (STRAIN(SPTNO,I), I=9,10),
     *                             (STRAIN(SPTNO,I), I=12,13)
110          continue
      
             write(OUTFIL,1520) X1,X1US,DISPUS(3),DISPUS(3),
     &                         DISPUS(4),DISPUS(4)
1520         format(//8x,a,10x,'2 Xi',9x,'2 Xi2',
     *               7x,'3 Xi1',8x,'3 Xi2'/
     &               6x,a,4(7x,a))
      
             do 120 SPTNO = 1,NOSPT
                  write(OUTFIL,1440) SOPT(SPTNO),
     *                               (STRAIN(SPTNO,I), I=23,26)
120          continue
         
         
           else if (NEPS .ge. 13) then
       
             write(OUTFIL,1530) X1,X1US,DISPUS(1),DISPUS(1),
     &                         DISPUS(2),DISPUS(2)
1530         format(//,8x,a,8x,'1 Omega1',5x,'1 Omega2',6x,
     *                           '1 Xi1',8x,'1 Xi2'/ 
     &               6x,a,4(7x,a))
      
             do 125 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRAIN(SPTNO,I), I=9,10),
     *                            (STRAIN(SPTNO,I), I=12,13)
125          continue
      

           else if (NEPS .ge. 10) then
       
             write(OUTFIL,1540)X1,X1US,DISPUS(1),DISPUS(1)
1540         format(//,8x,a,8x,'1 Omega1',5x,'1 Omega2'/
     &               6x,a,2(7x,a))
      
             do 130 SPTNO = 1,NOSPT
                write(OUTFIL,1440) SOPT(SPTNO),
     *                             (STRAIN(SPTNO,I), I=9,10)
130          continue
       
         end if
     
     

         
        write(OUTFIL,1550)
1550    format(////'   Stress Resultants'//)

      
      
       
        write(OUTFIL,1560) X1,X1US,RSLTUS(0),RSLTUS(0),
     &                     RSLTUS(0),RSLTUS(0)
1560    format(8x,a,9x,'N11',10x,'N22',10x,'N21'10x,'N12'/
     &               5x,a,2x,4(5x,a))

        do 135 SPTNO = 1,NOSPT
           write(OUTFIL,1440) SOPT(SPTNO), (STRESR(SPTNO,I), I=1,4)
135     continue


         write(OUTFIL,1570) X1,X1US,RSLTUS(1),RSLTUS(1),
     &                      RSLTUS(1),RSLTUS(1)
1570     format(//,8x,a,9x,'1 M11',8x,'1 M22',8x,'1 M21',8x,'1 M12'/
     &               5x,a,2x,4(5x,a))
      
         do 140 SPTNO = 1,NOSPT
            write(OUTFIL,1440) SOPT(SPTNO), (STRESR(SPTNO,I), I=5,8)
140      continue




         if (NEPS .ge. 22) then
      
            write(OUTFIL,1580)X1,X1US,RSLTUS(2),RSLTUS(2),
     &                      RSLTUS(2),RSLTUS(2)
1580         format(//,8x,a,9x,'2 M11',8x,'2 M22',8x,'2 M21',
     *                   8x,'2 M12'/
     &               5x,a,2x,4(5x,a))
      
            do 145 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRESR(SPTNO,I), I=15,18)
145         continue

            write(OUTFIL,1590)X1,X1US,RSLTUS(3),RSLTUS(3),
     &                      RSLTUS(3),RSLTUS(3)
1590        format(//,8x,a,9x,'3 M11',8x,'3 M22',8x,'3 M21',
     *                8x,'3 M12'/
     &               5x,a,2x,4(5x,a))
      
            do 160 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRESR(SPTNO,I), I=19,22)
160         continue

         end if
      
      
      
         if (NEPS .ge. 27) then
      
            write(OUTFIL,1600)X1,X1US,RSLTUS(0),RSLTUS(1),
     &                      RSLTUS(2)
1600        format(//,8x,a,9x,'0 T',10x,'1 T',10x,'2 T'/
     &               5x,a,2x,3(5x,a))
            do 170 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), STRESR(SPTNO,11),
     *                       STRESR(SPTNO,14), STRESR(SPTNO,27)
170         continue

           else if (NEPS .ge. 14) then   
         
            write(OUTFIL,1610)X1,X1US,RSLTUS(0),RSLTUS(1)
1610        format(//,8x,a,9x,'0 T',10x,'1 T'/
     &               5x,a,2x,2(5x,a))
            do 180 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), STRESR(SPTNO,11),
     *                           STRESR(SPTNO,14)
180         continue


          else if (NEPS .ge. 11) then   
         
            write(OUTFIL,1620)X1,X1US,RSLTUS(0)
1620        format(//,8x,a,9x,'0 T'/
     &               5x,a,2x,5x,a)
            do 185 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), STRESR(SPTNO,11)
185         continue

         end if
      
      
      
         if (NEPS .ge. 26) then
      
            write(OUTFIL,1630)X1,X1US,RSLTUS(0),RSLTUS(0),
     &                        RSLTUS(1),RSLTUS(1)
1630        format(//,8x,a,10x,'1 Q1',9x,'1 Q2',9X,'1 S1',9X,'1 S2',/      
     &               5x,a,2x,4(5x,a))
            do 190 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRESR(SPTNO,I), I=9,10),
     *                            (STRESR(SPTNO,I), I=12,13)
190         continue

            write(OUTFIL,1640)X1,X1US,RSLTUS(2),RSLTUS(2),
     &                        RSLTUS(3),RSLTUS(3)
1640        format(//,8x,a,10x,'2 S',9x,'2 S2',9x,'3 S1',9x,'3 S2'/
     &               5x,a,2x,4(5x,a))
      
            do 200 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRESR(SPTNO,I), I=23,26)
200         continue


          else if (NEPS .ge. 13) then
      
            write(OUTFIL,1650)X1,X1US,RSLTUS(0),RSLTUS(0),
     &                        RSLTUS(1),RSLTUS(1)
1650        format(//,8x,a,10x,'1 Q1',9x,'1 Q2',9X,'1 S1',9X,'1 S2',/
     &               5x,a,2x,4(5x,a))
      
            do 205 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO),
     *                            (STRESR(SPTNO,I), I=9,10),
     *                            (STRESR(SPTNO,I), I=12,13)
205         continue


          else if (NEPS .ge. 10) then
      
            write(OUTFIL,1660)X1,X1US,RSLTUS(0),RSLTUS(0)
1660        format(//,8x,a,10x,'1 Q1',9x,'1 Q2'/
     &               5x,a,2x,2(5x,a))
       
            do 210 SPTNO = 1,NOSPT
               write(OUTFIL,1440) SOPT(SPTNO), (STRESR(SPTNO,I), I=9,10),
210         continue


          end if

      
      end if
      
      
      
      close(OUTFIL)
      
      if (PLTSWC) call PLTOUT
      
      return
      end
      
      
