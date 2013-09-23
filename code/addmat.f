c****************************************************************************
      subroutine ADDMAT
      
      
c     Name:      ADD MATerial data
c     Purpose:   To add material properties to the data base MAT.DAT
c     Input:     Material property data
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
      integer      MAT
      
      
      NMAT = NMAT + 1
      MAT  = NMAT
      print 120
120   format(' Enter the name of the material. It must be less',
     &       ' than 30 characters long.')
      read(*,'(a)') MATNAM(MAT)
      
      
                         
20    print 140
140   format(' Enter:',/5x,'1 if the material is isotropic'/
     &       5x,'2 if the material is transverse isotropic'/
     &       5x,'3 if the material is rhombic')
      read(*,*) MATTYP(MAT)
      if ((MATTYP(MAT) .ne. 1) .and. (MATTYP(MAT) .ne. 2) .and.
     &    (MATTYP(MAT) .ne. 3)) then
         print 150
150      format(' Please enter 1,2, or 3')
         go to 20
      end if
      
      
      if (MATTYP(MAT) .eq. 1) then
      
        print 180, STRSUL
180     format(' Enter the new value of E in ',a,'.')
        read(*,*) E1(MAT)
             
         print 210
210      format(' Enter the new value of Nu.')
         read(*,*) NU12(MAT)
             
         print 230, ALPTUL
230      format(' Enter the new value of Alpha T in ',a,'.')
         read(*,*) ALPHT1(MAT)
             
         print 250, ALPMUL
250      format(' Enter the new value of Alpha M in ',a,'.')
         read(*,*) ALPHM1(MAT)
         
         
       else if (MATTYP(MAT) .eq. 2) then
                          
         print 270, STRSUL
270      format(' Enter the new value of E1 in ',a,'.')
         read(*,*) E1(MAT)
             
         print 290, STRSUL
290      format(' Enter the new value of E2 (= E3) in ',a,'.')
         read(*,*) E2(MAT)
             
         print 310
310      format(' Enter the new value of Nu12 (= Nu13).')
         read(*,*) NU12(MAT)

         print 314
314      format(' Enter the new value of Nu23.')
         read(*,*) NU23(MAT)
             
         print 330, STRSUL
330      format(' Enter the new value of G12 (= G13) in ',a,'.')
         read(*,*) G12(MAT)
             
         print 350, ALPTUL
350      format(' Enter the new value of Alpha T 1 in ',a,'.')
         read(*,*) ALPHT1(MAT)
             
         print 370, ALPTUL
370      format(' Enter the new value of',
     &          ' Alpha T 2 (= Alpha T 3) in ',a,'.')
         read(*,*) ALPHT2(MAT)
             
         print 390, ALPMUL
390      format(' Enter the new value of Alpha M 1 in ',a,'.')
         read(*,*) ALPHM1(MAT)
             
         print 410, ALPMUL
410      format(' Enter the new value of',
     &          ' Alpha M 2 (= Alpha M 3) in ',a,'.')
         read(*,*) ALPHM2(MAT)
         



       else if (MATTYP(MAT) .eq. 3) then
                          
         print 430, STRSUL
430      format(' Enter the new value of E1 in ',a,'.')
         read(*,*) E1(MAT)
             
         print 450, STRSUL
450      format(' Enter the new value of E2 in ',a,'.')
         read(*,*) E2(MAT)

         print 470, STRSUL
470      format(' Enter the new value of E3 in ',a,'.')
         read(*,*) E3(MAT)
             
         print 490
490      format(' Enter the new value of Nu12.')
         read(*,*) NU12(MAT)
             
         print 510
510      format(' Enter the new value of Nu13.')
         read(*,*) NU13(MAT)

         print 530
530      format(' Enter the new value of Nu23.')
         read(*,*) NU23(MAT)
         
         print 550, STRSUL
550      format(' Enter the new value of G12 in ',a,'.')
         read(*,*) G12(MAT)
         
         print 570, STRSUL
570      format(' Enter the new value of G13 in ',a,'.')
         read(*,*) G13(MAT)
             
         print 590, STRSUL
590      format(' Enter the new value of G23 in ',a,'.')
         read(*,*) G23(MAT)

         print 610, ALPTUL
610      format(' Enter the new value of Alpha T 1 in ',a,'.')
         read(*,*) ALPHT1(MAT)
             
         print 630, ALPTUL
630      format(' Enter the new value of',
     &          ' Alpha T 2 in ',a,'.')
         read(*,*) ALPHT2(MAT)
         
         print 650, ALPTUL
650      format(' Enter the new value of',
     &          ' Alpha T 3 in ',a,'.')
         read(*,*) ALPHT3(MAT)
             
         print 670, ALPMUL
670      format(' Enter the new value of Alpha M 1 in ',a,'.')
         read(*,*) ALPHM1(MAT)
             
         print 690, ALPMUL
690      format(' Enter the new value of',
     &          ' Alpha M 2 in ',a,'.')
         read(*,*) ALPHM2(MAT)
         
         print 710, ALPMUL
710      format(' Enter the new value of',
     &          ' Alpha M 3 in ',a,'.')
         read(*,*) ALPHM3(MAT)
         
      end if
      
      
      print 720
720   format(///' The material data entered is:')
      call VIEWMT(MAT)
      print 730
730   format(//' Is this data correct?')
      call YESNO(YES)
      if (.not. YES) then 
         call CHGMAT(MAT)
         return
        else
         return
      end if
      
      end
                          
                                       




