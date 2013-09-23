          subroutine CHGMAT(MATNO)
      
      
c     Name:      CHanGe MATerial data
c     Purpose:   To change the material properties in the data base MAT.DAT
c     Input:     Material no. of the material to view from the user and
c                material property data
c                If a 0 is passed, get the material no. from the user.
c     Output:    Material property data to the MATPRP common
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************


      implicit     undefined(a-z)
      
      include      'n.par'
      include      'matprp.com'
      include      'units.com'
      
      logical      YES
      character*20 TYPE
      integer      MAT,MATNO
      
      if (MATNO .eq. 0) then
5        print 100
100      format(/' Enter the number of the material whose',
     &          ' properties you wish to change or 0 to'/
     &          ' see the list of materials.')
10       read(*,*) MAT
         if ((MAT .lt. 0) .or. (MAT .gt. NMAT)) then
            print 105, NMAT
105         format(' Please enter a material number between',
     &             ' 0 and ',i2,'.')
            go to 10
          else if (MAT .eq. 0) then
            call MATLST
            go to 5
         end if
       else
         MAT = MATNO
      end if

      
      
1     print 110, MAT,MATNAM(MAT)
110   format(' The name of material no. ',i2,' is ',a,'.'/
     &       ' Do you want to change its name?')
      call YESNO(YES)
      if (YES) then
         print 120
120      format(' Enter the name of the material. It must be less',
     &          ' than 30 characters long.')
         read(*,'(a)') MATNAM(MAT)
      end if
      
      
      
      
      if (MATTYP(MAT) .eq. 1) then
         TYPE = 'isotropic'
       else if (MATTYP(MAT) .eq. 2) then
         TYPE = 'transverse isotropic'
       else if (MATTYP(MAT) .eq. 3) then
         TYPE = 'rhombic'
      end if
                         
      print 130, TYPE
130   format(' The material is ',a,'.'/
     &       ' Do you want to change its type?')
      call YESNO(YES)
      if (YES) then
20       print 140
140      format(' Enter:',/5x,'1 if the material is isotropic'/
     &          5x,'2 if the material is transverse isotropic'/
     &          5x,'3 if the material is rhombic')
         read(*,*) MATTYP(MAT)
         if ((MATTYP(MAT) .ne. 1) .and. (MATTYP(MAT) .ne. 2) .and.
     &       (MATTYP(MAT) .ne. 3)) then
            print 150
150         format(' Please enter 1,2, or 3')
            go to 20
         end if
      end if
      
      
      if (MATTYP(MAT) .eq. 1) then
      
         print 160, E1(MAT),STRSUS
160      format(' E = ',e12.5,x,a)
         print 170
170      format(' Do you want to change this value?')
         call YESNO(YES)
         if (YES) then
            print 180, STRSUL
180         format(' Enter the new value of E in ',a,'.')
            read(*,*) E1(MAT)
         end if
             
         print 190, NU12(MAT)
190      format(' Nu = ',e12.5)
         print 170
         call YESNO(YES)
         if (YES) then
            print 210
210         format(' Enter the new value of Nu.')
            read(*,*) NU12(MAT)
         end if
             
         print 220, ALPHT1(MAT),ALPTUS
220      format(' Alpha T = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 230, ALPTUL
230         format(' Enter the new value of Alpha T in ',a,'.')
            read(*,*) ALPHT1(MAT)
         end if
             
         print 240, ALPHM1(MAT),ALPMUS
240      format(' Alpha M = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 250, ALPMUL
250         format(' Enter the new value of Alpha M in ',a,'.')
            read(*,*) ALPHM1(MAT)
         end if
         
         
       else if (MATTYP(MAT) .eq. 2) then
                          
         print 260, E1(MAT),STRSUS
260      format(' E1 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 270, STRSUL
270         format(' Enter the new value of E1 in ',a,'.')
            read(*,*) E1(MAT)
         end if
             
         print 280, E2(MAT),STRSUS
280      format(' E2 = E3 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 290, STRSUL
290         format(' Enter the new value of E2 (= E3) in ',a,'.')
            read(*,*) E2(MAT)
         end if
             
         print 300, NU12(MAT)
300      format(' Nu12 = Nu13 = ',e12.5)
         print 170
         call YESNO(YES)
         if (YES) then
            print 310
310         format(' Enter the new value of Nu12 (= Nu13).')
            read(*,*) NU12(MAT)
         end if

         print 312, NU23(MAT)
312      format(' Nu23 = ',e12.5)
         print 170
         call YESNO(YES)
         if (YES) then
            print 314
314         format(' Enter the new value of Nu23.')
            read(*,*) NU23(MAT)
         end if
             
         print 320, G12(MAT),STRSUS
320      format(' G12 = G13 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 330, STRSUL
330         format(' Enter the new value of G12 (= G13) in ',a,'.')
            read(*,*) G12(MAT)
         end if
             
         print 340, ALPHT1(MAT),ALPTUS
340      format(' Alpha T 1 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 350, ALPTUL
350         format(' Enter the new value of Alpha T 1 in ',a,'.')
            read(*,*) ALPHT1(MAT)
         end if
             
         print 360, ALPHT2(MAT),ALPTUS
360      format(' Alpha T 2 = Alpha T 3 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 370, ALPTUL
370         format(' Enter the new value of',
     &             ' Alpha T 2 (= Alpha T 3) in ',a,'.')
            read(*,*) ALPHT2(MAT)
         end if
             
         print 380, ALPHM1(MAT),ALPMUS
380      format(' Alpha M 1 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 390, ALPMUL
390         format(' Enter the new value of Alpha M 1 in ',a,'.')
            read(*,*) ALPHM1(MAT)
         end if
             
         print 400, ALPHM2(MAT),ALPMUS
400      format(' Alpha M 2 = Alpha M 3 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 410, ALPMUL
410         format(' Enter the new value of'
     &             ' Alpha M 2 (= Alpha M 3) in ',a,'.')
            read(*,*) ALPHM2(MAT)
         end if
         



       else if (MATTYP(MAT) .eq. 3) then
                          
         print 420, E1(MAT),STRSUS
420      format(' E1 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 430, STRSUL
430         format(' Enter the new value of E1 in ',a,'.')
            read(*,*) E1(MAT)
         end if
             
         print 440, E2(MAT),STRSUS
440      format(' E2 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 450, STRSUL
450         format(' Enter the new value of E2 in ',a,'.')
            read(*,*) E2(MAT)
         end if

         print 460, E3(MAT),STRSUS
460      format(' E3 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 470, STRSUL
470         format(' Enter the new value of E3 in ',a,'.')
            read(*,*) E3(MAT)
         end if
             
         print 480, NU12(MAT)
480      format(' Nu12 = ',e12.5)
         print 170
         call YESNO(YES)
         if (YES) then
            print 490
490         format(' Enter the new value of Nu12.')
            read(*,*) NU12(MAT)
         end if
             
         print 500, NU13(MAT)
500      format(' Nu13 = ',e12.5)
         print 170
         call YESNO(YES)
         if (YES) then
            print 510
510         format(' Enter the new value of Nu13.')
            read(*,*) NU13(MAT)
         end if

         print 520, NU23(MAT)
520      format(' Nu23 = ',e12.5)
         print 170
         call YESNO(YES)
         if (YES) then
            print 530
530         format(' Enter the new value of Nu23.')
            read(*,*) NU23(MAT)
         end if
         
         print 540, G12(MAT),STRSUS
540      format(' G12 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 550, STRSUL
550         format(' Enter the new value of G12 in ',a,'.')
            read(*,*) G12(MAT)
         end if
         
         print 560, G13(MAT),STRSUS
560      format(' G13 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 570, STRSUL
570         format(' Enter the new value of G13 in ',a,'.')
            read(*,*) G13(MAT)
         end if
             
         print 580, G13(MAT),STRSUS
580      format(' G23 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 590, STRSUL
590         format(' Enter the new value of G23 in ',a,'.')
            read(*,*) G23(MAT)
         end if

         print 600, ALPHT1(MAT),ALPTUS
600      format(' Alpha T 1 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 610, ALPTUL
610         format(' Enter the new value of Alpha T 1 in ',a,'.')
            read(*,*) ALPHT1(MAT)
         end if
             
         print 620, ALPHT2(MAT),ALPTUS
620      format(' Alpha T 2 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 630, ALPTUL
630         format(' Enter the new value of',
     &             ' Alpha T 2 in ',a,'.')
            read(*,*) ALPHT2(MAT)
         end if
         
         print 640, ALPHT3(MAT),ALPTUS
640      format(' Alpha T 3 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 650, ALPTUL
650         format(' Enter the new value of'
     &             ' Alpha T 3 in ',a,'.')
            read(*,*) ALPHT3(MAT)
         end if
             
         print 660, ALPHM1(MAT),ALPMUS
660      format(' Alpha M 1 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 670, ALPMUL
670         format(' Enter the new value of Alpha M 1 in ',a,'.')
            read(*,*) ALPHM1(MAT)
         end if
             
         print 680, ALPHM2(MAT),ALPMUS
680      format(' Alpha M 2 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 690, ALPMUL
690         format(' Enter the new value of'
     &             ' Alpha M 2 in ',a,'.')
            read(*,*) ALPHM2(MAT)
         end if
         
         print 700, ALPHM3(MAT),ALPMUS
700      format(' Alpha M 3 = ',e12.5,x,a)
         print 170
         call YESNO(YES)
         if (YES) then
            print 710, ALPMUL
710         format(' Enter the new value of',
     &             ' Alpha M 3 in ',a,'.')
            read(*,*) ALPHM3(MAT)
         end if
         
      end if
      
      
      print 720
720   format(///' The material data entered is:')
      call VIEWMT(MAT)
      print 730
730   format(//' Is this data correct?')
      call YESNO(YES)
      if (.not. YES) then 
         go to 1
        else
         return
      end if
      
      end
                          
                                       
