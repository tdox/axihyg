      program USRFRD
      
      

c     Name:      USeR FRienDly
c     Purpose:   To create the INPUT.DAT file for the AXI program via a user
c                friendly interface.
c     Common:    NONE
c     Input:     Data from the file 'INPUT.DAT', and the user
c     Output:    The data files INPUT.DAT and MAT.DAT
c     Calls    : 
c                                                                      |
c*******************************************************************************


 
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      

      logical      YES
      character*1  ANS
      
      print 100
100   format(//,80('*'),//,36x,'AXIHYG',/
     &       34x,'Version 1.1'/
     &       31x,'15 September 1990',//
     &       ' A program to calculate the hygrothermal deformations of',
     &       ' axisymmetric,'/' laminated, composite shells due to',
     &       ' axisymmetric temperature and moisture'/,
     &       ' distributions.',//
     &       22x,' L. E. Doxsee, Jr. and G. S. Springer',//,19x,
     &       ' Department of Aeronautics and Astronautics',/,29x,
     &       ' Stanford University',/,31x,'Stanford CA 94305',//
     &       32x,'(415) 723-4135'//
     &       27x,'c 1987, 1988, 1989, 1990',//
     &       ' This program is a research tool in',
     &       ' the development stage and is supplied "as'/' is" for',
     &       ' the purpose of scientific collaboration.'//,x,
     &       79('*'))
     
      print 101
101   format(//' Do you want to read an introduction to this program?')
      call YESNO(YES)
      if (YES) call INTRO
      
      print 105
105   format(//' Enter the FORTRAN unit number which corresponds to',
     &         ' terminal output for the'/' particular',
     &         ' version of FORTRAN with which',
     &         ' this program was compiled.'/' (Usually 6)')
      read(*,*) TERM
      ERRFIL = TERM
      
      print 110
110   format(//' Do you wish to create a new "input data set"',
     &       ' or to modify an old one?'/' Please type "c" for',
     &       ' create or "m" for modify.')
     
5     read(*,'(a)') ANS
      
      if ((ANS .eq. 'c') .or. (ANS .eq. 'C')) then
           call CREATE
        else if ((ANS .eq. 'm') .or. (ANS .eq. 'M')) then 
           call LODATS
           call MODIFY
        else
           print 150
150        format(' Please enter either "c" or "m".')
           go to 5
      end if
      
      end
      
      
      
      subroutine dummy
      integer*4 array(10000)
      common array
      end
      
      
      
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


      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
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
                          
                                       




           subroutine BCS(EXIST)
      
      
c     Name:      Boundary Condition Subroutine
c     Purpose:   To input the boundary conditions
c     Input:     EXIST, if true, b.c. information already exists
c                Boundary condition information from user
c     Output:    The above information into BC common
c     Commons:   BC
c     Called by: CREATE,MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************

 
 
       
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer     BCTYPE(2),BCCODE(2,MXNBC)
      real*8      UBC(2,MXNVAR),TBC(2,MXNVAR)
      common /BC/ BCTYPE,BCCODE,UBC,TBC
      save /BC/
      
      
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
       
      logical     EXIST,YES,EEXIST(2)
      
      character*6 DSPBCU(MXNVAR)
      character*7 DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*8 TRCBCU(MXNVAR)
      
      integer     EDGN,DOF
      
      real*8      BC(2,MXNBC)
      
      data DISPBC/'u1','u2','w','1 Beta1','1 Beta2','1 Eta','2 Eta',
     &            '3 Beta1','3 Beta2','2 Beta1','2 Beta2','3 Eta',' '/,
     &     TRACBC/'N11','N12','Q1','1 M11','2 M12','1 S1','2 S1',
     &            '3 M11','3 M12','2 M11','2 M12', '3 S1',' '/
      
      call BCUNIT(DISPBC,TRACBC,DSPBCU,TRCBCU)
      
      print 90
90    format(//' BOUNDARY CONDITIONS')

      print 100
100   format(//' Would you like to read some information concerning',
     &        ' entering boundary'/' conditions?')
      call YESNO(YES)
      if (YES) then
          print 110
110       format(//' In this section of the program, boundary',
     &           ' condition information is input.'/
     &           ' Since the shell is axisymmetric, there are only',
     &           ' two edges: the top edge'/' (edge #1) and the',
     &           ' bottom edge (edge #2).',
     &           ' At these',
     &           ' edges either the'/' displacements or the'
     &           ' corresponding tractions must be specified.',
     &           '  You have'/' the option of'
     &           ' prescribing a standard boundary condition',
     &           ' (simply supported,'/' clamped, or free;',
     &           ' or having a',
     &           ' symmetric boundary) or prescribing the'/
     &           ' displacements and tractions individually.')
         call WAIT
      end if
      
      
1     continue

      if (EXIST) then
         print 120
120      format(/' The previously specified boundary',
     &          ' conditions are:'/)
         call BCWRT(TERM)
         print 130
130      format(//' Do you want to change the boundary conditions?')
         call YESNO(YES)
         if (.not. YES) return
       else
        EEXIST(1) = .false.
        EEXIST(2) = .false.
      end if
      
         
      do 10 EDGN=1,2
         if (EEXIST(EDGN)) then
            print 140,EDGN
140         format(/' Do you want to change the boundary conditions for',
     &             ' edge number ',i2,'?')
            call YESNO(YES)
            if (.not. YES) go to 10
         end if
         print 150,EDGN
150      format(/' How is the edge number ',i2,' supported?  Enter:'/
     &           ' 1 for simply supported'/' 2 for clamped'/
     &           ' 3 for free'/
     &           ' 4 for symmetric boundary'/' 0 for general.')
         read(*,*) BCTYPE(EDGN)


         if (BCTYPE(EDGN) .eq. 0) then
      
            do 20 DOF=1,NBC
         
               if (EXIST) then
                  if (BCCODE(EDGN,DOF) .eq. 1) then
                     print 160,EDGN,DOF,DISPBC(DOF),
     &                         UBC(EDGN,DOF),DSPBCU(DOF)
160                  format(/' At edge number ',i1,' for the boundary',
     &                      ' condition corrosponding to degree of'/
     &                      ' freedom ',i2,', the displacement is',
     &                      ' specified.'/
     &                      3x,a,' = ',e12.5,x,a)
                    else                  
                     print 170,EDGN,DOF,TRACBC(DOF),
     &                         TBC(EDGN,DOF),TRCBCU(DOF)
170                  format(/' At edge number ',i1,' for the boundary',
     &                      ' condition corrosponding to degree of',
     &                      ' freedom ',i2,', the traction is',
     &                      ' specified.'/
     &                      3x,a,' = ',e12.5,x,a)
                  end if               
                  print 180
180               format(/' Do you want to change this boundary',
     &                   ' condition?')
                  call YESNO(YES)
                  if (.not. YES) then
                     go to 20
                  end if
               end if
            
               print 190, EDGN,DOF,DISPBC(DOF),TRACBC(DOF),DISPBC(DOF),
     &                    TRACBC(DOF)
190            format(/' At edge number ',i1,' for the boundary',
     &                ' condition corrosponding to degree of'/
     &                ' freedom ',i2,
     &                ', do you want to prescribe ',a,' or ',a,'?'/
     &                ' Enter either 1 for ',a,' or 2 for ',a,'.')
30             read(*,*) BCCODE(EDGN,DOF)
               if (BCCODE(EDGN,DOF) .eq. 1) then
                  print 200, DISPBC(DOF),DSPBCU(DOF)
200               format(/' Enter the value of 'a', in ',a,'.')
                  read(*,*) BC(EDGN,DOF)
                 else if (BCCODE(EDGN,DOF) .eq. 2) then
                  print 200, TRACBC(DOF),TRCBCU(DOF)
                  read(*,*) BC(EDGN,DOF)
                 else
                  print 210
210               format(/' Please enter "1" or "2".')
                  go to 30
               end if
            
20          continue
          
         end if
         
         
10    continue

      call BCSET(BC)

      EXIST = .true.
      
      go to 1
      
      end
      





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


      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
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
                          
                                       
      subroutine CREATE
      
c     Name:      CREATE
c     Purpose:   To create the INPUT.DAT file for the AXI program via a user
c                friendly interface.
c     Input:     
c                
c     Output:    The data files INPUT.DAT and MAT.DAT
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      
      logical      EXIST
      
      logical       SAVED
      common /SAVE/ SAVED
      save   /SAVE/

      

      call INIT
      EXIST = .false.
      call TITLES(EXIST)
      EXIST = .false.
      call UNITSS(EXIST)
      EXIST = .false.
      call GEOMS(EXIST)
      EXIST = .false.
      call NODES(EXIST)
      EXIST = .false.
      call MATERS
      EXIST = .false.
      call LAYUPS(EXIST)
      EXIST = .false.
      call LOADS(EXIST)
      EXIST = .false.
      call BCS(EXIST)
      EXIST = .false.
      call OUTS(EXIST)
      EXIST = .false.
      call SLTCHS(EXIST)
      call MODIFY
      
      SAVED = .false.
      
      stop
      end
      
      
         subroutine DIMS(EXIST)
      
      
c     Name:      DIMenstion Subroutine
c     Purpose:   To input the dimensions of the shell 
c     Input:     EXIST, if true, dimension information already exits
c                various dimentions
c     Output:    various dimesions to the GEOM common
c     Commons:   GEOM
c     Called by: GEOM
c                                                                      |
c*******************************************************************************
 
 
      
      
      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      real*8     PI,PIODEG
      parameter (PI=3.14159265, PIODEG = 0.0174532952)
      
      
      
      logical      TRU,EXIST
      
      if (SHAPE .eq. 'cyln') then
         if (EXIST) then
             print 100, RAD,LENUS
100          format(/' The radius of the cylinder midsurface is'/
     &              ' R = ',e12.5,x,a,'.',/
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) go to 10
         end if
         
         print 110, LENUL
110      format(' Enter the radius of the midsurface of the shell in ',
     &            a,'.')
5        read(*,*) RAD
         if (RAD .le. 0.) then
            print 120
120         format(' The radius must be greater than 0.  Please',
     &             ' reenter the radius.')
            go to 5 
         end if
    
10       if (EXIST) then
            print 130, CYLEN,LENUS
130          format(/' The length of the cylinder is',
     &           ' L = ',e12.5,x,a,'.',/,' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) return
         end if
         
         print 140, LENUL
140      format(' Enter the length of the shell in ',
     &            a,'.')
15       read(*,*) CYLEN
         if (CYLEN .le. 0.) then
            print 150
150         format(' The length must be greater than 0.  Please',
     &             ' reenter the length.')
            go to 15
         end if
       
         XF = 0.
         XS = CYLEN
       
             
        else if (SHAPE .eq. 'cone') then
        
         if (EXIST) then
             print 160, R0,LENUS
160          format(/' The radius of the cone midsurface at the top',
     &              ' of the cone is'/' Ro = ',e12.5,x,a,'.'/
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) go to 30
         end if
         
         print 170, LENUL
170      format(' Enter the radius of the cone midsurface at the top',
     &          ' of the'/' cone (Ro) in ',a,'.')
25        read(*,*) R0

         if (R0 .lt. 0.) then
            print 180
180         format(' The radius must be greater than or equal to 0.',
     &             ' Please reenter the number.')
            go to 25
         end if
         
         
30       if (EXIST) then
             print 190, ALPHAD
190          format(/' The angle formed by the axis',
     &              ' and generator alpha = ',e12.5,' degrees.',/,
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) go to 40
         end if
         
         print 200
200      format(' Enter the the angle formed by the axis',
     &          ' and generator (alpha) in degrees.')
35        read(*,*) ALPHAD
         
         if ((ALPHAD .le. 0.) .or. (ALPHAD .gt. 90.)) then
            print 210
210         format(' The angle must be greater than 0 and',
     &             ' and less than or equal 90.',/
     &             ' Please reenter the angle.')
            go to 35
         end if
         
         
         
40       if (EXIST) then
             print 220, CONEHT,LENUS
220          format(/' The height of the cone is H = ',e12.5,x,a,'.'/
     &              ' Do you want to change it?')
             call YESNO(TRU)
             if (.not. TRU) return
         end if
         
         print 230, LENUL
230      format(' Enter the height of the cone (H) in ',a,'.')
45       read(*,*) CONEHT

         if (R0 .lt. 0.) then
            print 240
240         format(' The height must be greater than or equal to 0.',
     &             ' Please reenter the number.')
            go to 45
         end if
        
         XF = 0.
c         XS = CONEHT/cos(ALPHAD)
         XS = CONEHT/cos(ALPHAD*PIODEG)
       
        
        
        
        
        else if (SHAPE .eq. 'torp') then
           
         if (EXIST) then
             print 250, RC,LENUS
250          format(/' The distance between the center of the',
     &              ' toroidal section and the axis'/
     &              ' is Rc = ',e12.5,x,a,'.'/
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 60
         end if
         
        print 260, LENUL
260      format(' Enter the distance between',
     &          ' the center of the toroidal',
     &          ' section and the'/' axis (Rc) in ',a,'.')
55        read(*,*) RC

         if (RC .lt. 0.) then
            print 270
270         format(' This distance must be greater than or equal to 0.',
     &             ' Please reenter the number.')
            go to 55
         end if
         
         
60       if (EXIST) then
             print 280, RPHI,LENUS
280          format(/' The radius of curvature in the meridional',
     &              ' direction is'/' R phi = ',e12.5,x,a,'.',/,
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 70
         end if
         
         print 290, LENUL
290      format(' Enter the radius of curvature in the meridional',
     &              ' direction (R phi) in ',a,'.')
     
65        read(*,*) RPHI

         if (RPHI .le. 0.) then
            print 300
300         format(' The radius must be greater than 0.',
     &             ' Please reenter the number.')
            go to 65
         end if
         
70       call PHI12(EXIST)
           
         XF = PHI1
         XS = PHI2
         
           
           
       else if (SHAPE .eq. 'para') then
         if (EXIST) then
             print 350, RAT0,LENUS
350          format(/' The radius of curvature at the vertex of the',
     &              ' paraboloid'
     &              /' (R at phi=0) is R_nu = ',e12.5,x,a,'.'/
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 90
         end if
         
         print 360, LENUL
360      format(' Enter the radius of curvature at the vertex of the',
     &          ' paraboloid (R_nu, R at phi=0) in '/,x,a,'.')
95       read(*,*) RAT0

         if (RAT0 .le. 0.) then
            print 370
370         format(' The radius must be greater than 0.',
     &             ' Please reenter the number.')
            go to 95
         end if
         
         
90       if (EXIST) then
             print 380, OFFSET,LENUS
380          format(/' The distance the midsurface of the shell',
     &               ' is offset from the paraboloid'/
     &                ' is d = ',e12.5,x,a,'.',/,
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 98
         end if
         
         print 390, LENUL
390      format(' Enter the distance the midsurface of the shell',
     &          ' is offset from the'/' paraboloid (OFFSET) in ',a,'.')
         read(*,*) OFFSET
         
98       call PHI12(EXIST)
         
         XF = PHI1
         XS = PHI2


    
      end if
            
      
      return
      end
      
  
       subroutine GEOMS(EXIST)
      
      
c     Name:      GEOMetry Subroutine
c     Purpose:   To input the geometry of the shell 
c     Input:     EXIST, if true, geometry information exists already
c                SHAPE, from user
c                various dimenstions of the shell
c     Output:    SHAPE and various dimensions into GEOM common
c     Commons:   GEOM
c     Called by: CREATE
c                                                                      |
c*******************************************************************************
 
 
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      
      logical      EXIST,DIMEXT,YES
      
      print 50
50    format(//' GEOMETRY')
      
1     continue
      
      if (EXIST) then
         call GEOWRT(TERM)
         print 100
100      format(/' Do you want to change the geometry?')
         call YESNO(YES)
         if (.not. YES) return
      end if
      
      call SHAPES(EXIST,DIMEXT)
      call DIMS(DIMEXT)
      
      EXIST = .true.
      go to 1
      
      end
      
      
    
      
           subroutine GETMST(TI,TM,TO)
      
      
c     Name:      GET MoiSTure concentration  
c     Purpose:   To input moisture distribution at a point
c     Input:     DIST, = c if temp is const. at the point
c                      = l if temp is linear through thickness
c                      = q if temp is quadratic through thickness
c                TI,TM,TO Moist. at inner, mid and outer surfacec from user
c     Output:    TI,TM,TO
c     Commons:   none
c     Called by: MOISTI
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
              
       character*1 DIST
       real*8      TI,TM,TO
       
       
5     read(*,'(a)') DIST
      if ((DIST .ne. 'c') .and. (DIST .ne. 'l') .and. 
     &    (DIST .ne. 'q')) then
         print 105
105      format(' Please enter either "c", "l" or "q"')
         go to 5
      end if

      if ((DIST .eq. 'c') .or. (DIST .eq. 'C')) then
         print 101
101      format(' At this position enter the moisture change',
     &          ' of the shell.')
         read(*,*) TI
         TM = TI
         TO = TI
       else if ((DIST .eq. 'l') .or. (DIST .eq. 'L')) then
         print 102
102      format(' At this position enter the moisture'
     &          ' change of the shell',
     &          ' at the inner and'/' outer surfaces.')
         read(*,*) TI,TO
         TM = (TI + TO) / 2.
       else if (DIST .eq. 'q') then
         print 103
103      format(' At this position enter the moisture'
     &          ' change of the shell',
     &          ' at the inner, mid,'/' and outer surfaces.')
         read(*,*) TI,TM,TO
      end if

      return
      end
      
      
           subroutine GETTMP(TI,TM,TO)
      
      
c     Name:      GET TeMPerature Input 
c     Purpose:   To input temperture or moisture distribution at a point
c     Input:     DIST, = c if temp is const. at the point
c                      = l if temp is linear through thickness
c                      = q if temp is quadratic through thickness
c                TI,TM,TO Temp at inner, mid and outer surfacec from user
c     Output:    TI,TM,TO
c     Commons:   none
c     Called by: TEMPI
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
       
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
       
       character*1 DIST
       real*8      TI,TM,TO
       
       
5     read(*,'(a)') DIST
      if ((DIST .ne. 'c') .and. (DIST .ne. 'l') .and. 
     &    (DIST .ne. 'q')) then
         print 105
105      format(' Please enter either "c", "l" or "q"')
         go to 5
      end if

      if ((DIST .eq. 'c') .or. (DIST .eq. 'C')) then
         print 101, TEMPUL
101      format(' At this position enter the temperature change',
     &          ' of the shell in ',a'.')
         read(*,*) TI
         TM = TI
         TO = TI
       else if ((DIST .eq. 'l') .or. (DIST .eq. 'L')) then
         print 102, TEMPUL
102      format(' At this position enter the temperature'
     &          ' change of the shell',
     &          ' at the inner and'/' outer surfaces in ',a,'.')
         read(*,*) TI,TO
         TM = (TI + TO) / 2.
       else if (DIST .eq. 'q') then
         print 103, TEMPUL
103      format(' At this position enter the temperature'
     &          ' change of the shell',
     &          ' at the inner, mid,'/' and outer surfaces in ',a,'.')
         read(*,*) TI,TM,TO
      end if

      return
      end
      
      
      subroutine INIT
      
c     Name:      INITialize
c     Purpose:   To initialize certain data
c     Input:     none
c                
c     Output:    Data to certain commons
c     Called by: CREATE
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*10    OTFILE
      logical         BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI
      integer         THEORY,TORORD,OSHPU,OSHPW,OSHPD, NINT,NINTW,NINTD
      common /CONTRL/ BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI,
     *                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,NINTW,NINTD,
     *                OTFILE
      save   /CONTRL/
      
      
       
       NBC = MXNBC
       FCODE = .false.
       
       return
       end
      subroutine INTRO
      
c     Name:      INTROduction
c     Purpose:   To print an introduction to the program
c     Input:     none
c                
c     Output:    An introdution output to the screen.
c     Called by: USRFRD,MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      

      print 100
100   format(//'INTRODUCTION'//
     &' The program AXIHYG calculates the deformations of',
     &' axisymmetric, laminated'/' composite shells due to',
     &' changes in moisture',
     &' concentration and temperature. '/' The problem to be solved',
     &' must be compatible with the following assumptions:'//
     &'  1.  The material properties are linearly elastic and',
     &' independent'/
     &'      of temperature and moisture concentration.'/
     &'  2.  The shell''s geometry and material properties are',
     &' axisymmetric.'/
     &'  3.  The thickness of the shell is constant.'/
     &'  4.  Moisture concentration and temperature change are',
     &' axisymmetric and may'/
     &'      be either constant, or vary',
     &' linearly or quadratically through the'/,6x,'thickness.')
     
      print 105
105   format('  5.  Deflections, rotations and strains are small.'//
     &' The program is based on a higher order shell theory that',
     &' incorporates both'/
     &' transverse shear and normal strains.',
     &' The governing boundary value problem'/
     &' is one-dimensional',
     &' (axisymmetric) and is solved using a one-dimensional,'/ 
     &' displacement method finite element algorithm.  Further',
     &' information'/' regarding this code may be found in',
     &' "Hygrothermal Deformations of'/' Laminated Composite Shells"',
     &' and "A Higher Order Theory of the Hygrothermal'/' Behavior of',
     &' Composite Shells" by Doxsee and in "Hygrothermal'/
     &' Stresses and Strains in Axisymmetric Composite Shells"',
     &' and "Temperature'/' and Moisture Induced Deformations in',
     &' Fiber-reinforced Composite'/' Shells" by Doxsee and',
     &' Springer.'/)

      call WAIT

      print 110
110   format(
     &' Currently you are running the interactive user-friendly',
     &' interface "AXIHYGUF"'/
     &' which is used to create new "input data sets" or to load',
     &' and modify existing'/ 
     &' ones.  Once the input data set has been created, it may be',
     &' stored to a file.'/
     &' This file, if it is given the name "INPUT.DAT", becomes the',
     &' input file to the'/
     &' finite element program "AXIHYGFE".')
       
     
       call WAIT

      print 130
130   format('   User-Friendly Interface:'/
     &' The user-friendly interface is used',
     &' to either create new or to modify old'/' "input data',
     &' sets".  The "input data sets" are',
     &' created and modified in sections:'/)
     
      print 140
140   format('  1.  Title of the "input data set".'/
     &'  2.  Physical units to which the data refer (SI or English).'/
     &'  3.  Geometry of the shell (shape and dimensions of',
     &' the generator).'/
     &'  4.  Material properties (which are stored',
     &' in a different data base in the file'/
     &'      "MAT.DAT").'/
     &'  5.  Layup of the shell''s laminate.'/
     &'  6.  Hygrothermal loads applied to the shell.'/
     &'  7.  Boundary conditions at the two edges.'/
     &'  8.  Output selection (quantities for which to solve and',
     &' print).'/
     &'  9.  Nodal coordinates (positions of the edges',
     &' of the finite elements along'/
     &'      the generator).'/
     &' 10.  Solution techniques specification.'/)
    
      print 150
150   format(' These data can be viewed, modified or',
     &' stored at any point in the course of'/
     &' running the program.  Introductions and',
     &' instructions are provided at the'/
     &' beginning of key sections.  Figures which accompany',
     &' the code are sometimes'/' referenced.'/)

      call WAIT
      
      print 160
160   format(' Since a shell theory is the basis of the',
     &' analysis, the program solves for'/
     &' midsurface quantities from which',
     &' non-midsurface quantities can be calculated.'//
     &' The displacement of a point U(x1,x2,z) is',
     &' assumed to be in the form:'//
     &' U1(x1,x2,z) = u1(x1) +  z 1_beta_1(x1)',
     &'  +  z^2 2_beta_1(x1)  + z^3 3_beta_1(x1)'/
     &' U2(x1,x2,z) = u2(x1) +  z 1_beta_2(x1)',
     &'  +  z^2 2_beta_2(x1)  + z^3 3_beta_2(x1)'/
     &' U3(x1,x2,z) = w(x1)  +  z 1_eta(x1)   ',
     &'  +  z^2 2_eta(x1)     + z^3 3_eta(x1)'/)
      
      print 170
170   format(' where x1 is the meridional coordinate,',
     &' x2 is the circumferential coordinate,'/
     &' and z is the normal coordinate.',
     &'  The program solves for the midsurface'/
     &' displacements (u1, u2, w) and "rotations"',
     &' (n_beta_i,n_eta) from which the'/
     &' displacement of any point in the',
     &' shell can be calculated according to the'/
     &' above equations.  Midsurface strains and',
     &' curvature changes and stress'/
     &' resultants are also given as output.')
     
      call WAIT

      return
      end
      
      
          subroutine LAYUPS(EXIST)
      
      
c     Name:      LAYUP Subroutine
c     Purpose:   To input the layup of the laminate
c     Input:     EXIST, if true, layup information already exists
c                LAYUP information from user
c     Output:    The above information into LAYUP common
c     Commons:   LAYUP
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************



      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      logical           SYM
      integer           NSTACK,MATNO(MAXSTK),NPLY(MAXSTK)
      real*8            LTHKNS,PTHK(MAXSTK),THETA(MAXSTK),STHKNS(MAXSTK)
     
      common  /LAYUP/   SYM,
     &                  NSTACK,MATNO,NPLY,
     &                  LTHKNS,PTHK,THETA,STHKNS
 
      save    /LAYUP/
      
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      logical      EXIST,YES,NSTEXT
      character*1  SORN
      integer      I,STKNO
      
      print 85
85    format(//' LAMINATE LAYUP')
      print 90
90    format(/' Would you like to read some information concerning',
     &       ' layup data input?')
      call YESNO(YES)
      if (YES) then
         print 101
101      format(//' In this section of the program, the layup of the',
     &       ' laminate is specified.  The'/
     &       ' following definitions are',
     &       ' used:'//4x,'A "stack" is a group of plies with the',
     &       ' same material properties and'/4x'orientation.'//
     &       4x,'The',
     &       ' "orientation" of a stack is the angle in degrees',
     &       ' between the fibers'/
     &       4x,'and the generator of the shell. ',
     &       ' The angle is positive if the fibers are'/
     &       4x,'rotated in',
     &       ' the counter-clockwise direction (looking at the',
     &       ' shell from the'/4x,'outside).'//
     &       4x,'Stack numbering starts',
     &       ' with the inside stack.'//
     &       4x,'If the laminate is',
     &       ' symmetric about the midsurface, only half of the'/
     &       4x,'laminate is to be specified and the "Number of',
     &       ' Stacks" refers to the'/4x'number of stacks in half of',
     &       ' the laminate.'/)
         call WAIT
      end if
     
1     if (EXIST) then
         NSTEXT = .true.
        else
         NSTEXT = .false.
      end if
     
      if (EXIST) then
         if (SYM) then
              print 100, NSTACK
100           format(///' The current layup is symmetric and has ',
     &               i2,' stacks.')
           else
              print 110, NSTACK
110           format(///' The current layup is not symmetric and has ',
     &               i2,' stacks.')
         end if


         call LAYWRT(TERM)


         print 130
130      format(/' Do you want to change these?')
         call YESNO(YES)
         if (.not. YES) then
            call LAMTM
            return
         end if
      end if
         
         
         
      if (EXIST) then
         if (SYM) then
            print 140
140         format(' The laminate is symmetric.')
           else
            print 150
150         format(' The laminate is not symmetric.')
         end if
         print 160
160      format(' Do you want to change this?')
         call YESNO(YES)
         if (YES) then
            SYM    = .not. SYM
         end if
        
       else
         print 162
162      format(/' Is the laminate symmetric or not?  Enter "s" for',
     &          ' symmetric or "n" for not'/' symmetric.')
10       read(*,'(a)') SORN
         if ((SORN .eq. 's') .or. (SORN .eq. 'S')) then
            SYM = .true.
           else if ((SORN .eq. 'n') .or. (SORN .eq. 'N')) then
            SYM = .false.
           else
            print 164
164         format(' Please enter either "s" or "n".')
            go to 10
         end if
         
      end if
               
         
      if (EXIST) then
           print 170, NSTACK
170        format(' There are ',i2,' stacks.'/'  Do you want to change',
     &            ' the number of stacks?')
           call YESNO(YES)
           if (YES) then
              NSTEXT = .false.
             else
              NSTEXT = .true.
           end if
      end if
      
      
      if (.not. NSTEXT) then
         print 180
180      format(' Enter the number of stacks.')
20       read(*,*) NSTACK
         if (NSTACK .le. 0) then
             print 190
190          format(' Please enter a number greater than 0.')
             go to 20
          end if
           
          do 30 I=1,NSTACK
            call STKINI(I)
30        continue
       else
60        print 200
200       format(' Enter the number of the stack you wish',
     &           ' to change or 0 to change all of'/
     &           ' the stacks.')
50        read(*,*) STKNO
          if (STKNO .eq. 0) then
             do 40 I=1,NSTACK
                call STKINI(I)
40           continue
           else if (STKNO .gt. NSTACK) then
             print 210, NSTACK
210          format(' Please enter a number between 0 and ',i2,'.')
             go to 50
           else
             call STKINI(STKNO)
          end if
          print 220
220       format(' Do you want to change the properties of another',
     &           ' stack?')
          call YESNO(YES)
          if (YES) then
             go to 60
          end if
          
      end if
      
      
      EXIST = .true.
      call LAMTM
      go to 1
      
      end
      
      
           subroutine LOADS(EXIST)
      
      
c     Name:      LOAD Subroutine
c     Purpose:   To input the mechanical, and hygrothermal loads
c     Input:     EXIST, if true, loading information already exists
c                Loading information from user
c     Output:    The above information into LOADI common
c     Commons:   LOADI
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
 
       
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      character*10    OTFILE
      logical         BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI
      integer         THEORY,TORORD,OSHPU,OSHPW,OSHPD, NINT,NINTW,NINTD
      common /CONTRL/ BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI,
     *                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,NINTW,NINTD,
     *                OTFILE
      save   /CONTRL/
      
      
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      logical         SIMP
      integer         NTMPI,NMSTI,TNI(MAXELM+1),MNI(MAXELM+1)
      real*8          TXI(MAXELM+1),
     &                TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &                MXI(MAXELM+1),
     &                MII(MAXELM+1),MMI(MAXELM+1),MOI(MAXELM+1)
     
      common /LOADI/  TXI,TII,TMI,TOI,
     &                MXI,MII,MMI,MOI,
     &                NTMPI,NMSTI,TNI,MNI,
     &                SIMP
      save   /LOADI/
      
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
       
       logical     YES,EXIST,SIMPI
       character*1 ANS
       
2      print 50
50    format(//' HYGROTHERMAL LOADS')

       print 55
55     format(//' Would you like to use the simple input',
     &          ' routine',
     &          ' or the complex'/' input routine?',/
     &          ' Enter "s" for simple or "c" for complex.')
      read(*,'(a)') ANS
      if ((ANS .eq. 's').or.(ANS .eq.'S')) then
             SIMPI = .true.
          else if ((ANS .eq. 'c').or.(ANS .eq.'C')) then
            SIMPI = .false.
          else
             print 56
56           format(' Please enter "s" or "c".')
             go to 2
       end if
       

       print 100
100    format(//' Would you like to read some information concerning',
     &        ' entering temperature and'/
     &        ' moisture concentration data?')
       call YESNO(YES)
       if (YES) then
          print 110
110       format(//' In this section of the program, the change in',
     &        ' temperature and moisture'/' concentration',
     &        ' are specified.')
     
          if (SIMPI) then
             print 125
             call WAIT
           else
             print 120
             call WAIT
           end if
             
      end if
             
120          format(/' The temperature and moisture are assumed to be',
     &        ' axisymmetric, vary piecewise'/
     &        ' linearly (linear within each element)',
     &        ' in the direction parallel to the'/' generator,',
     &        ' and vary quadratically',
     &        ' through the thickness of the shell. Thus,'/
     &        ' the temperature change of the inner',
     &        ' surface, midsurface, and outer surface'/' must be',
     &        ' determined at every element edge.  However, these',
     &        ' temperature changes'/' need not be specified at every',
     &        ' element edge because the inner, mid, and outer'/
     &        ' temperature changes at element edges',
     &        ' not specified will be',
     &        ' determined through'/' linear interpolation. Thus, as',
     &        ' a minimum, the inner, mid',
     &        ' and outer'/' temperature change',
     &        ' must be specified at the first and last element',
     &        ' edges'/' (the edges of the shell).'//
     &        ' The same holds true for moisture',
     &        ' concentration changes.')

125          format(/' The temperature and moisture are',
     &        ' assumed to be',
     &        ' axisymmetric, vary'/' linearly ',
     &        ' in the direction parallel to the generator,'
     &        ' and vary'/' quadratically',
     &        ' through the thickness of the shell. Thus,',
     &        ' the temperature '/' and moisture concentration',
     &        ' change at the inner',
     &        ' surface, midsurface, and'/' outer surface must be',
     &        ' determined at both the top and bottom',
     &        ' edges of the'/' shell.  The temperature',
     &        ' change and moisture concentration change',
     &        ' at every'/' other point of the shell are',
     &        ' determined by linear',
     &        ' interpolation.')
     
     
      
      if (.not. EXIST) then
         TCODE = .false.
         MOIST = .false.
      end if
         
         
1     continue

      if (TCODE) then
         print 130
130      format(//' The previously specified temperature',
     &            ' information is:')
         call TIWRT(TERM)
         print 131
131      format(' Do you want to see the interpolated',
     &          ' temperature change values?')
         call YESNO(YES)
         if (YES) then
            call TMPWRT(TERM)
         end if
         print 150
150      format(//' Do you want to change these?')
         call YESNO(YES)
         if (YES) then
            call TEMPI
            go to 1
         end if
       else
         print 160
160      format(' Temperature data does not exist.  Do you want',
     &          ' to input some?')
         call YESNO(YES)
         if (YES) then
            call TEMPI
            TCODE = .true.
            go to 1
         end if
      end if
         
      
5     continue

      if (MOIST) then
         print 170
170      format(//' The previously specified moisture concentration',
     &          ' information is:')
         call MIWRT(TERM)
         print 173
173      format(' Do you want to see the interpolated',
     &          ' moisture concentration values?')
         call YESNO(YES)
         if (YES) then
            call MSTWRT(TERM)
         end if
         print 190
190      format(//' Do you want to change these?')
         call YESNO(YES)
         if (YES) then
            call MOISTI
            go to 5
         end if
       else
         print 200
200      format(' Moisture data does not exist.  Do you want',
     &          ' to input some?')
         call YESNO(YES)
         if (YES) then
            call MOISTI
            MOIST = .true.
            go to 5
         end if
      end if
      
      return
      end
         
      
       subroutine LODATS
      
c     Name:      LOad DATa Set
c     Purpose:   To load in the input data set.
c     Input:     Input data in file
c                
c     Output:    The input data to the proper common blocks
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      
      logical       SAVED
      common /SAVE/ SAVED
      save   /SAVE/

      
      logical      INEXT      
      character*1  ANS
      character*10 INFILE
      
      
 10   print 120
120   format(' Enter the name of the file which contains the',
     &       ' "input data set" you wish to'/' modify.')
      read(*,'(a)') INFILE
      inquire(file=INFILE,exist=INEXT)
      if (.not. INEXT) then
          print 130, INFILE
130       format(' The file "',a,'" does not exist. ',
     &           ' Do you want to use another file,'/
     &           ' create a new',
     &           ' input file, or quit?  Enter "a" for another file,',
     &           ' "c" for create,'/' or "q" for quit.')
15       read(*,'(a)') ANS
         if ((ANS .eq. 'c') .or. (ANS .eq. 'C')) then
             call CREATE
           else if ((ANS .eq. 'a') .or. (ANS .eq. 'A')) then 
             go to 10
           else if ((ANS .eq. 'q') .or. (ANS .eq. 'Q')) then 
             call QUIT
           else
             print 140
140          format(' Please enter either "c","a" or "q".')
             go to 15
         end if
      end if
      
      call INRED(INFILE)
      SAVED = .false.
      
      return
      end
      
      
        subroutine MATERS
      
      
c     Name:      MATERial data Subroutine
c     Purpose:   To input the material properties in the data base MAT.DAT
c     Input:     Material property data from MAT.DAT and the user.
c     Output:    Material property data to MAT.DAT
c     Commons:   MATPRP
c     Called by: CREATE
c     Calls    : MATRED
c                                                                      |
c*******************************************************************************



      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/


      logical      YES,MATEXT
 
      print 90
90    format(//' MATERIAL PROPERTIES')
      
      print 100
100   format(///' Material property data is stored in the file',
     &       ' "MAT.DAT". In this section of'/' the program, the data',
     &       ' in this file may be changed and/or new data',
     &       ' added'/' if the file already exists; or if',
     &       ' the file does not yet exist it may be'/' created.')
     
      inquire(file='MAT.DAT',exist=MATEXT)
     
      if (MATEXT) then
         print 120
120      format(/' The file MAT.DAT exists.'/)
         call WAIT
         call MATRED
         call MATLST
         call MATNOT

         print 130
130      format(/' Would you like to view the properties of',
     &           ' any of these materials?')
15       call YESNO(YES)
         if (YES) then
             call VIEWMT(0)            
             print 140
140          format(/' Do you wish to view the properties of another',
     &               ' material?')
             go to 15
         end if


         print 150
150      format(/' Would you like to change any of the existing',
     &           ' material properties?')
25       call YESNO(YES)
         if (YES) then
            call CHGMAT(0)
            print 160
160         format(/' Would you like to change any other material',
     &              ' properties?')
            go to 25          
          end if



40        print 170
170       format(/' Would you like to add additional material',
     &           ' properties to the data base?')
          call YESNO(YES)
          if (YES) then
             call ADDMAT
             go to 40
            else
             call MATWRT
             return
          end if
          
       end if
       
       
       print 180
180    format(/' MAT.DAT does not exist so there are no material',
     &        ' properties defined.'/' You must define some now.')
       NMAT = 0
       call ADDMAT
50     print 170
       call YESNO(YES)
       if (YES) then
          call ADDMAT
          go to 50
         else
          call MATWRT
          return
       end if
          
          
       end
       
         
        subroutine MATLST
      
      
c     Name:      MATERial LiST
c     Purpose:   To list the material properties in the data base MAT.DAT
c     Input:     
c     Output:    Material property list to the screen
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************

      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

      
      integer      I
         
      print 110, NMAT
110   format(//' There are ',i2,' materials whose properties have',
     &         ' already been defined.  They are:',//
     &         ' Material no.',3x,'Material name'/)
      do 10 I=1,NMAT
         print 120, I,MATNAM(I)
120      format(6x,i2,9x,a)
10    continue

      return
      end
      
      
          subroutine MATNOT
      
      
c     Name:      MATerial property NOTation
c     Purpose:   To print information concerning notation
c     Input:     Whether to print information or not, from user
c     Output:    Material property notation information to the screen.
c     Commons:   none
c     Called by: VIEWMT,CHGMAT,ADDMAT
c     Calls    : 
c                                                                      |
c*******************************************************************************
 

      logical     YES

      print 40
40    format(//' Would you like to read some information concerning',
     &       ' notation used to describe'/' material properties?')
      call YESNO(YES)
      if (YES) then
          print 50
50       format(//' In this section, the following standard notation',
     &           ' is used:'/
     &        2x,' E',7x,'denotes Young''s modulus',
     &           ' (stiffness)'/
     &        2x,' Nu',6x,'denotes Poisson''s ratio'/
     &        2x,' G',7x,'denotes shear modulus'/
     &        2x,' Alpha T denotes coefficient of',
     &           ' thermal expansion'/
     &        2x,' Alpha M denotes coefficient of',
     &           ' moisture swelling expansion'/)
         print 51
51      format(' The numbers following these coefficients denote',
     &          ' the direction or plane to which'/
     &          ' the coefficient refers.'/
     &          2x,' The number 1 corresponds to the fiber',
     &          ' direction of the lamina.'/
     &          2x,' The number 2 corresponds to the',
     &          ' direction perpendicular to the fiber and in'/
     &          5x,' the  plane of the lamina.'/
     &          2x,' The number 3',
     &          ' corresponds to the direction perpendicular to the',
     &          ' plane of the'/5x' lamina.'/)
         print 52
52       format(' Examples:'/4x,'E1   Young''s modulus of',
     &          ' the lamina in the fiber direction.'//
     &          4x,'G13  Shear modulus in the plane formed',
     &          ' by a line parallel to the'/
     &          9x,'fiber and a line perpendicular to the lamina.'//
     &          4x,'Nu12 Poisson''s ratio for transverse',
     &          ' strain in the 2 direction when'/
     &          9x,'stressed in the fiber direction.')
     
      end if

      return
      end

        subroutine MATWRT
      
      
c     Name:      MATERial WRiTe subroutine
c     Purpose:   To write the material properties to the data base MAT.DAT
c     Input:     Material property data from MATPRP common
c     Output:    Material property data to MAT.DAT
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************



      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/


      integer      I

      open(unit=1,file='MAT.DAT',status ='UNKNOWN')

      write(1,100) NMAT
100   format(i2,5x,':NMAT, Number of materials')

      do 1 I=1,NMAT
      
         write(1,110) MATNAM(I)
110      format(a)
         write(1,120) MATTYP(I)
120      format(i1,5x,':MATTYP, (1:iso., 2:trnsv. iso., 3:rhombic)')

         if (MATTYP(I) .eq. 1) then

            write(1,130) E1(I)
130         format(e11.4,5x,':E')
            write(1,135) NU12(I)
135         format(e11.4,5x,':Nu')
            write(1,140) ALPHT1(I)
140         format(e11.4,5x,':Alpha Thermal')
            write(1,145) ALPHM1(I)
145         format(e11.4,5x,':Alpha Moisture ')

          else if (MATTYP(I) .eq. 2) then
            write(1,150) E1(I),E2(I)
150         format(2(e11.4,x),':E1, E2')
            write(1,152) NU12(I),NU23(I)
152         format(2(e11.4,x),':Nu12, Nu23')
            write(1,154) G12(I)
154         format(e11.4,13x,':G12')
            write(1,160) ALPHT1(I),ALPHT2(I)
160         format(2(e11.4,x),':Alpha thermal 11, At22')
            write(1,165) ALPHM1(I),ALPHM2(I)
165         format(2(e11.4,x),':Am11, Am22')
  
          else if (MATTYP(I) .eq. 3) then
            write(1,170) E1(I),E2(I),E3(I)
170         format(3(e11.4,x),5x,':E1, E2, E3')
            write(1,180) NU12(I),NU13(I),NU23(I)
180         format(3(e11.4,x),5x,':Nu12, Nu13, Nu23')
            write(1,190) G12(I),G13(I),G23(I)
190         format(3(e11.4,x),5x,':G12, G13, G23')
            write(1,200) ALPHT1(I),ALPHT2(I),ALPHT3(I)
200         format(3(e11.4,x),5x,':At11, At22, At33')
            write(1,210) ALPHM1(I),ALPHM2(I),ALPHM3(I)
210         format(3(e11.4,x),5x,':Am11, Am22, Am33')

         end if

1     continue

      close(1)

      return
      end
        subroutine MESH
      
      
c     Name:      MESH
c     Purpose:   To input the location of the finite elment nodes
c     Input:     The position of the edge nodes and locations where the
c                temperature is precribed
c     Output:    Nodal location information into ELMDAT common
c     Variables: XNODEI(MAXELM+1) : X NODE Input, list of nodes whose positions
c                  have been prescribed.
c                XEDGEI(MAXELM+1) X Edge Input. I'th element is the posion of
c                 node XNODEI(I)
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************

      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      integer        NXEDI,XNODEI(MAXELM+1)
      real*8         XEDGEI(MAXELM+1)
      common /XEDIN/ NXEDI,XNODEI,XEDGEI
      save   /XEDIN/

      
      NUMEL     = MAXELM
      NXEDI     = 2
      XNODEI(1) = 1
      XEDGEI(1) = XF
      XNODEI(NXEDI) = NUMEL+1
      XEDGEI(NXEDI) = XS

      call XEINTP

      return
      end
      
      
         subroutine MIWRT(UNITNO)
      
      
c     Name:      Moisture Input WRiTe 
c     Purpose:   To write the input moisture concentration data to the unit
c                (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c     Output:    Temperature data to the unit.
c     Commons:   
c     Called by: INWRT,LOADS
c                                                                      |
c*******************************************************************************


      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      logical         SIMP
      integer         NTMPI,NMSTI,TNI(MAXELM+1),MNI(MAXELM+1)
      real*8          TXI(MAXELM+1),
     &                TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &                MXI(MAXELM+1),
     &                MII(MAXELM+1),MMI(MAXELM+1),MOI(MAXELM+1)
     
      common /LOADI/  TXI,TII,TMI,TOI,
     &                MXI,MII,MMI,MOI,
     &                NTMPI,NMSTI,TNI,MNI,
     &                SIMP
      save   /LOADI/
      
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      integer     UNITNO,I

      write(UNITNO,100) X1,X1US
100   format(//' Element Edge',5x,'Position',6x,
     &       'Inside Moist.',3x,'Middle Moist.',3x,
     &       'Outside Moist.'/
     &       20x,a,11x,'Change',10x'Change',10x,'Change'/
     &       18x,'(',a,')')
      do 1 I=1,NMSTI
         print 180, MNI(I),XEDGE(MNI(I)),MII(I),MMI(I),MOI(I)
180      format(5x,i2,8x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      subroutine MODIFY
      
c     Name:      MODIFY
c     Purpose:   To modiy the INPUT.DAT file for the AXI program via a user
c                friendly interface.
c     Input:     
c                
c     Output:    The data files INPUT.DAT and MAT.DAT
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      logical       SAVED
      common /SAVE/ SAVED
      save   /SAVE/

      
      logical      EXIST
      character*1  ANS
      
5     print 130
130   format(//' Which part of the input data do you want to change?'/
     &       ' Enter:'/
     &       5x,'u: for physical units'/
     &       5x,'t: for title'/
     &       5x,'g: for shell geometry'/
     &       5x,'n: for nodal position'/
     &       5x,'m: for material properties'/
     &       5x,'l: for laminate layup'/
     &       5x,'h: for hygrothermal data (temperature',
     &              ' and moisture distributions)'/
     &       5x,'b: for boundary conditions'/
     &       5x,'o: for output specification'/
     &       5x,'s: for solution techniques'//
     &       2x,'or v: to view the input data'/
     &       5x,'w: to write input data to a named file and quit'/
     &       5x,'r: to read in an existing input data set'/
     &       5x,'c: to create a new input data set'/
     &       5x,'i: to view the introduction'/
     &       5x,'q: to quit the program'/)
      read(*,'(a)') ANS
      if ((ANS .eq. 'u') .or. (ANS .eq. 'U')) then
           print 140
140        format(/' Warning: Changing the physical units',
     &            ' may affect',
     &            ' other input data such as'/
     &            ' geometry, nodal coordinates,',
     &            '  material properties, hygrothermal data, and'/,
     &            ' boundary conditions.  Be sure to change these if',
     &            ' neccesary.')
           EXIST = .true.
           call UNITSS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 't') .or. (ANS .eq. 'T')) then
           EXIST = .true.
           call TITLES(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'g') .or. (ANS .eq. 'G')) then
           print 150
150        format(/' Warning: Changing the geometry may affect',
     &            ' other input data such as'/' nodal coordinates,',
     &            ' hygrothermal data, and',
     &            ' boundary conditions.  Be sure to'/
     &            ' change these if necessary.')
           EXIST = .true.
           call GEOMS(.true.)
           SAVED = .false.
        else if ((ANS .eq. 'n') .or. (ANS .eq. 'N')) then
           print 160
160        format(/' Warning: Changing the nodal coordinates may affect',
     &            ' other input data such as'/' hygrothermal data, and',
     &            ' boundary conditions.  Be sure to change these if'/
     &            ' necessary.')
           EXIST = .true.
           call NODES(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'm') .or. (ANS .eq. 'M')) then
           call MATERS
           SAVED = .false.
        else if ((ANS .eq. 'l') .or. (ANS .eq. 'L')) then
           EXIST = .true.
           call LAYUPS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'h') .or. (ANS .eq. 'H')) then
           EXIST = .true.
           call LOADS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'b') .or. (ANS .eq. 'B')) then
           EXIST = .true.
           call BCS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'o') .or. (ANS .eq. 'O')) then
           EXIST = .true.
           call OUTS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 's') .or. (ANS .eq. 'S')) then
           EXIST = .true.
           call SLTCHS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'v') .or. (ANS .eq. 'V')) then
           call INWRT(TERM)
           SAVED = .false.
        else if ((ANS .eq. 'w') .or. (ANS .eq. 'W')) then
           call WRITE
        else if ((ANS .eq. 'r') .or. (ANS .eq. 'R')) then
           call LODATS
        else if ((ANS .eq. 'c') .or. (ANS .eq. 'C')) then
           call CREATE
        else if ((ANS .eq. 'i') .or. (ANS .eq. 'I')) then
           call INTRO
        else if ((ANS .eq. 'q') .or. (ANS .eq. 'Q')) then
           call QUIT
        else
           print 170
170        format(' Please enter either "u", "t", "g", "n", "m", "l",'/
     &            ' "b", "o", "s", "v", "w", "r", "c", "i" or "q".')
           go to 5
      
      end if
      go to 5
      
      end

            subroutine MOISTI
      
      
c     Name:      MOISTure concentration Input 
c     Purpose:   To input moisture concentration distribution
c     Input:     Moisture concentration information from user
c     Output:    The above information into LOADI common
c     Commons:   LOADI
c     Called by: LOADS
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
       
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      real*8           THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0(MAXELM+1), TEMP1(MAXELM+1),
     *                 TEMP2(MAXELM+1),
     *                 MOIST0(MAXELM+1),MOIST1(MAXELM+1),
     *                 MOIST2(MAXELM+1)
     
      common /LOAD/    THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0,TEMP1,TEMP2,
     *                 MOIST0,MOIST1,MOIST2
      save   /LOAD/
      
      
      logical         SIMP
      integer         NTMPI,NMSTI,TNI(MAXELM+1),MNI(MAXELM+1)
      real*8          TXI(MAXELM+1),
     &                TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &                MXI(MAXELM+1),
     &                MII(MAXELM+1),MMI(MAXELM+1),MOI(MAXELM+1)
     
      common /LOADI/  TXI,TII,TMI,TOI,
     &                MXI,MII,MMI,MOI,
     &                NTMPI,NMSTI,TNI,MNI,
     &                SIMP
      save   /LOADI/
      
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      

       logical     YES
       integer     MN,I
       real*8      MIIS,MMIS,MOIS

      do 1 I=1,MAXELM+1
         MNI(I) = 0
         MII(I) = 0.
         MMI(I) = 0.
         MOI(I) = 0.
1     continue
     
      MNI(1) = 1
      print 100, X1,XF,X1US
100   format(' At the first edge of the shell,',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' moisture constant',
     &       ' through the thickness or does',
     &       ' it vary linearly or'/' quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
      
      
      call GETMST(MII(1),MMI(1),MOI(1))
      
      print 110, X1,XS,X1US
110   format(' At the second edge of the shell',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' moisture constant',
     &       ' through the thickness or does',
     &       ' it vary linearly'/' or quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
     
      call GETMST(MIIS,MMIS,MOIS)
      
      if (SIMP) then
         NMSTI = 2
         MNI(NMSTI) = NUMEL+1
         MII(NMSTI) = MIIS
         MMI(NMSTI) = MMIS
         MOI(NMSTI) = MOIS
         call THTINT(THICK,MNI,MII,MMI,MOI,MOIST0,MOIST1,MOIST2)
         return
      end if
         


      print 101
101   format(/' For reference, the nodal positions',
     &       ' previously specified are:')
     
      call XEDWRT(TERM)
      
         
      NMSTI = 1
                  
         
10    continue

         NMSTI = NMSTI+1
           
         print 120
120      format(' Do you want to enter the moisture at another',
     &         ' position of the shell?')
         call YESNO(YES)
         if (YES) then
20          print 130
130         format(' Enter node number at which you want'
     &             ' to specify the',
     &             ' moisture distribution.')
            read(*,*) MN
     	   if (MN .le. MNI(NMSTI-1)) then
     	        print 140, MNI(NMSTI-1)
140             format(' The element edge number must be greater',
     &                 ' than ',i2,'.')
                go to 20
             else if (MN .gt. NUMEL+1) then
                print 150, NUMEL+1
150             format(' The element edge number must be less',
     &               ' than or equal to ',i2,'.')
              go to 20
         end if
         
           MNI(NMSTI) = MN
           print 153
153        format(' Is the moisture constant',
     &             ' through the thickness',
     &             ' or does it vary linearly'/' or quadratically',
     &             ' through the thickness at this position?'/
     &             ' Enter "c" for constant, "l" for',
     &             ' linearly,or "q" for quadratically.')

           call GETMST(MII(NMSTI),MMI(NMSTI),MOI(NMSTI))
           go to 10
          else    
           MNI(NMSTI) = NUMEL+1
           MII(NMSTI) = MIIS
           MMI(NMSTI) = MMIS
           MOI(NMSTI) = MOIS
           call THTINT(THICK,MNI,MII,MMI,MOI,MOIST0,MOIST1,MOIST2)
           return
        end if
               
      end
            
      
 
         subroutine NODES(EXIST)
      
      
c     Name:      NODE input Subroutine
c     Purpose:   To input the location of the finite elment nodes
c     Input:     EXIST, if true, nodal location information already exists
c                NUMEL, no. of elements from user
c                XEDGE(i), edge node postions from user
c     Output:    The above information into ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      
      logical      EXIST,XEDEXT,YES
      
      print 50
50    format(//' FINITE ELEMENT MESH')

      print 100
100   format(' In this section you may either prescribe',
     &       ' the finite element mesh'/' nodal coordinates',
     &       ' yourself or you may let the computer choose a',
     &       ' mesh for you.'/' Do you want to prescribe the',
     &       ' mesh yourself?')
      call YESNO(YES)
      if (YES) then
         call NUMELS(EXIST,XEDEXT)
         call XEDGES(XEDEXT)
       else
         call MESH
      end if
      
      return
      end
      
      
         subroutine NUMELS(EXIST,XEDEXT)
      
      
c     Name:      NUMber of ELement input Subroutine
c     Purpose:   To input the of finite elments
c     Input:     EXIST, if true, number of elements information already exists
c                NUMEL, no. of elements from user
c     Output:    The above information into ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      
      logical      TRU,EXIST,XEDEXT
      
      
      if (EXIST) then
         print 100, NUMEL
100      format(/' There are 'i2' elements in the mesh.'/'  Do you',
     &          ' want to change this number?')
         call YESNO(TRU)
         
         if (.not. TRU) then
            XEDEXT = .true.
            return
         end if
         
      end if
         
      XEDEXT = .false.
         
      
      print 110, MAXELM
110   format(/' Enter the number of elements for this mesh.  This',
     &       ' number must be less than'/' or equal to ',i2,'.')
1     read(*,*) NUMEL
      
      if (NUMEL .gt. MAXELM) then
         print 120, MAXELM
120      format(' Please enter a number less than or',
     &          ' equal to ',i2,'.')
         go to 1
      end if
      
      if (NUMEL .le. 0) then
         print 125
125      format(' Please enter a number greater than 0')
         go to 1
      end if
      
      return
      end
      
           subroutine OUTS(EXIST)
      
      
c     Name:      OUTput Subroutine
c     Purpose:   To specify which quantities to solve for and print
c     Input:     Answers from the user
c     Output:    RSLT, if true, solve the strains and resultants
c                PLTSWC, if true, create data files with plotting information
c     Commons:   CONTRL
c     Called by: CREATE,MODIFY
c     Calls : 
c                                                                      |
c*******************************************************************************

 
 

      character*10    OTFILE
      logical         BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI
      integer         THEORY,TORORD,OSHPU,OSHPW,OSHPD, NINT,NINTW,NINTD
      common /CONTRL/ BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI,
     *                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,NINTW,NINTD,
     *                OTFILE
      save   /CONTRL/
      
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      
      logical     EXIST,YES
      
      print 90
90    format(//' OUTPUT QUANTITIES')
      
      BACK = .true.
1     continue

      if (EXIST) then
         call OTQWRT(TERM)
         print 50
50       format(/' Do you want to change this information?')
         call YESNO(YES)
         if (.not. YES) return
      end if
      
      print 100
100   format(/' Do you want to solve for the strain measures',
     &' and stress resultants?')
      call YESNO(RSLT)
      
      print 110
110   format(/' Do you want to create data files for use',
     &       ' in plotting?'/' (Note: This program currently has no',
     &       ' plotting capabilities of its own.)')
      call YESNO(PLTSWC)

      if (EXIST) then
         print 120, OTFILE
120      format(' The name of the finite element output file is ',a,
     &          '.  Do you want to'/' change it?')
         call YESNO(YES)
         if (.not. YES) go to 10
      end if
      
      print 130
130   format(' Enter the name of the file in which you want',
     &       ' solutions printed.  The name must'/' be less than',
     &       ' 11 characters long.')
      read(*,'(a)') OTFILE
     

10    continue
      
      EXIST = .true.
      go to 1
      
      end
      
      
         subroutine PHI12(EXIST)
      
      
c     Name:      Phi 1 and 2
c     Purpose:   To input Phi1 and Phi2, the angles at the edges 
c     Input:     EXIST, if true, dimension information already exits
c                various dimentions
c     Output:    Phi1 and Phi 2 to the GEOM common
c     Commons:   GEOM
c     Called by: DIMS
c     Calls    : None
c                                                                      |
c*******************************************************************************
 
 
      
      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      
      logical   EXIST,TRU,OK
      
70       if (EXIST) then
             print 310, PHI1
310          format(/' The angle between a line segment parallel',
     &              ' to the axis of the shell'/' and a line segment',
     &              ' normal to the midsurface at the first edge',
     &              ' is'/' Phi_top = ',e12.5,x,'degrees.',/
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 80
         end if
         
75       print 320
320      format(' Enter the angle between a line segment parallel',
     &          ' to the axis of the shell'/' and a line segment',
     &          ' normal to the midsurface at the first edge',/
     &          ' (Phi_top) in degrees.')
     
         read(*,*) PHI1

         call CHECKX(PHI1,OK)
         if (.not. OK) go to 75

            
           
80       if (EXIST) then
             print 330, PHI2
330          format(/' The angle between a line segment parallel',
     &              ' to the axis of the shell'/' and a line segment',
     &              ' normal to the midsurface at the second edge',
     &              ' is'/' Phi_bot = ',e12.5,x,'degrees.',/,
     &              ' Do you want to change this value?')
             call YESNO(TRU)
             if (.not. TRU) go to 90
         end if
         
         print 340
340      format(' Enter the angle between a line segment parallel',
     &          ' to the axis of the shell'/' and a line segment',
     &          ' normal to the midsurface at the second edge'/
     &          ' (Phi_bot) in degrees.')
     
85       read(*,*) PHI2

         call CHECKX(PHI2,OK)
         if (.not. OK) go to 85
         
90       if (PHI2 .le. PHI1) then
            print 350
350         format(/' Phi 2 must be greater than Phi 1')
            EXIST = .true.
            go to 70
         end if
           
         return
         end
         
         
      subroutine QUIT
      
c     Name:      QUIT
c     Purpose:   To quit the  user-friendly interface.
c     Input:     
c                
c     Output:    
c     Called by: MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      
      logical       SAVED
      common /SAVE/ SAVED
      save   /SAVE/

      
      logical YES

      if (.not. SAVED) then
         print 100
100      format(/' Quitting does not save',
     &          ' the changes made to the input',
     &          ' data.'/'  Do you want to save the changes?')
         call YESNO(YES)
         if (YES) then
            call WRITE
            stop
          else
            stop
         end if
      end if
      
      stop
      end
      
      
        subroutine SHAPES(EXIST,DIMEXT)
      
      
c     Name:      SHAPE Subroutine
c     Purpose:   To input the shape of the shell 
c     Input:     EXIST, if true, shape information exists already
c                SHAPE, from user
c     Output:    SHAPE
c                DIMEXT, logical ; if .true. then dimesion information exits
c     Commons:   GEOM
c     Called by: GEOM
c                                                                      |
c*******************************************************************************
 
 
      
      
      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      logical      TRU,EXIST,DIMEXT
      character*2  SHAPS
      character*17 SHAPEL
      
      if (EXIST) then
         if (SHAPE .eq. 'cyln') then
             SHAPEL = 'cylinder'
           else if (SHAPE .eq. 'cone') then
             SHAPEL = 'cone'
           else if (SHAPE .eq. 'torp') then
             SHAPEL = 'toroidal section'
           else if (SHAPE .eq. 'para') then
             SHAPEL = 'paraboloid'
         end if
            
         print 100, SHAPEL
100      format(//' The current shell is a 'a,'.'/
     &          ' Do you want to change the shape?')
     
         call YESNO(TRU)
         
         if (.not. TRU) then
            DIMEXT = .true.
            return
         end if
            
      end if 
      
      DIMEXT = .false.
      
      print 110
110   format(//' Enter the shape of the shell generator (see figure):'/
     &       ' Enter "cy" for a cylinder.'/
     &       '       "co" for a cone.'/
     &       '       "to" for a toroidal section.'/
     &       '       "pa" for a paraboloid.')
1     read(*,'(a)') SHAPS
      
      if ((SHAPS .eq. 'cy') .or. (SHAPS .eq. 'CY')) then
           SHAPE = 'cyln'
           X1    = 's'
           X1US  = LENUS
           X1UL  = LENUL
         else if ((SHAPS .eq. 'co') .or. (SHAPS .eq. 'CO')) then
           SHAPE = 'cone'
           X1    = 's'           
           X1US  = LENUS
           X1UL  = LENUL
         else if ((SHAPS .eq. 'to') .or. (SHAPS .eq. 'TO')) then
           SHAPE = 'torp'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
         else if ((SHAPS .eq. 'pa') .or. (SHAPS .eq. 'PA')) then
           SHAPE = 'para'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
         else
           print 120
120        format(' Please enter "cy", "co", "to", or "pa".')
           go to 1
      end if        
      
      return
      end
      
 
           subroutine SLTCHS(EXIST)
      
      
c     Name:      SoLution TeCHnique Subroutine
c     Purpose:   To specify which solution techniques to use
c     Input:     Solution techniques from the user
c     Output:    Solution techniques to the commons
c     Commons:   CONTRL
c     Called by: CREATE,MODIFY
c     Calls : 
c                                                                      |
c*******************************************************************************

 
 
      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      character*10    OTFILE
      logical         BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI
      integer         THEORY,TORORD,OSHPU,OSHPW,OSHPD, NINT,NINTW,NINTD
      common /CONTRL/ BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI,
     *                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,NINTW,NINTD,
     *                OTFILE
      save   /CONTRL/
      
      
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      

      logical     EXIST,YES
      
      print 90
90    format(//' SOLUTION TECHNIQUES')
1     continue

      if (EXIST) then
         call SLTWRT(TERM)
         print 55
55       format(/' Do you want to change the solution technique?')
         call YESNO(YES)
         if (.not. YES) return
      end if
      
      print 100
100   format(/' This program can be used to generate solutions',
     &' based on several different'/
     &' levels of approximation (see the references listed in the',
     &' introduction).  This'/
     &' program uses the highest order approximation unless otherwise',
     &' specified by'/
     &' the user.  Using lower order approximations may result',
     &' in incorrect solutions.'/
     &' Do you want to use the highest order of approximation?'/)
      call YESNO(YES)
      if (YES) then
         THEORY = 8
         TORORD = 2
         NINT   = 2
         
         if ((SHAPE .eq. 'cyln') .or. (SHAPE .eq. 'cone')) then
            RIGID = .false.
            NRBM  = 0
          else
            RIGID = .true.
            NRBM  = 1
         end if
         
         KSHORT   = .true.
         
       else
       
         print 110
110      format(/' Warning: Changing solution techniques may be',
     &           ' hazardous to your data.  You should'/' be familiar',
     &           ' with the inner workings of the code to',
     &           ' make useful changes.'/' Proceed with caution.')
     
         if (EXIST) then
            print 120, THEORY
120         format(/' The current shell theory employed is theory',
     &              ' number ',i1,'.  Do you want to change'/
     &              ' the theory number?')
            call YESNO(YES)
            if (.not. YES) go to 20
         end if
         
10       print 130
130      format(/' Enter the Theory Number.  (Default: 8)')
         read(*,*) THEORY
         if ((THEORY .lt. 1) .or. (THEORY .gt. 8)) then
            print 140
140         format(' The Theory Number must be an integer between',
     &             '1 and 8.')
            go to 10
         end if
         

20       continue
         if (EXIST) then
            print 150, TORORD
150         format(/' Currently, the program includes terms',
     &             ' (t/R)^',i1,' when calculating the element'/
     &             ' stiffness matrices.  Do you want to change',
     &             ' the order of (t/R) that is included'/
     &             ' in the calculations?')
            call YESNO(YES)
            if (.not. YES) go to 40
         end if
         
         print 160
160      format(/' Enter the order of (t/R) that is to be included in',
     &           ' the calculations.'/' (Default: 2)')
30       read(*,*) TORORD
         if ((TORORD .lt. 0) .or. (TORORD .gt. 2)) then
            print 170
170         format(' Please enter an integer between 0 and 2.')
            go to 30
         end if
         

40       continue
         if (EXIST) then
            print 180, NINT
180         format(/' Currently, the program uses ',i1,' integration',
     &             ' points per element.'/'  Do you want to change',
     &             ' this number?')
            call YESNO(YES)
            if (.not. YES) go to 60
         end if
         
         print 190
190      format(/' Enter the number of integration points per',
     &           ' element. (Default: 2)')
50       read(*,*) NINT
         if ((NINT .lt. 1) .or. (NINT .gt. 2)) then
            print 200
200         format(' Please enter either 1 or 2.')
            go to 50
         end if
         
         
60       continue
         if ((SHAPE .ne. 'cyln') .and. (SHAPE .ne. 'cone')) then
            if (EXIST) then
               if (RIGID) then
                  print 210
210               format(/' Rigid body motion is currently included in',
     &                   ' the element shape functions.')
                 else
                  print 220
220               format(/' Rigid body motion is not currently included',
     &                   ' in the element shape functions.')
               end if
            end if
            print 230
230         format(/' Do you want to include rigid body motion in',
     &              ' the element shape functions?'/' (Default: YES)')
            call YESNO(RIGID)
          else 
            RIGID = .false.
         end if
         
         
70       continue
         if (EXIST) then
            if (KSHORT) then
               print 240
240            format(/' The global stiffness matrix is calcualted',
     &                ' using a quicker algorithm.')
              else
               print 250
250            format(/' The global stiffness matrix is calcualted',
     &                ' using a slower algorithm.')
            end if
         end if
         print 260
260      format(' Do you want to use a quicker algorithm?',
     &          '  (Default: YES)')
         call YESNO(KSHORT)
         
      end if
      
      call THRSET
      
      OSHPU = 1
      OSHPW = 2
      OSHPD = 1
      
      EXIST = .true.
      
      go to 1
      
      end
      
      
           subroutine STKINI(STKNO)
      
      
c     Name:      STacK INformation Input
c     Purpose:   To input the information for stack no. I into LAYUP common
c     Input:     STKNO, the stack no
c                Stack information from the user
c     Output:    The above information into LAYUP common
c     Commons:   LAYUP
c     Called by: LAYUPS
c     Calls    : None
c                                                                      |
c*******************************************************************************



      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      logical           SYM
      integer           NSTACK,MATNO(MAXSTK),NPLY(MAXSTK)
      real*8            LTHKNS,PTHK(MAXSTK),THETA(MAXSTK),STHKNS(MAXSTK)
     
      common  /LAYUP/   SYM,
     &                  NSTACK,MATNO,NPLY,
     &                  LTHKNS,PTHK,THETA,STHKNS
 
      save    /LAYUP/
      
      
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      integer      STKNO
      
      
      print 100, STKNO
100   format(//' Now enter information for stack number',
     &       i2,'.'//' Of what material is this stack made?',
     &       '  Enter the material no. or 0 to see the'/
     &       ' list of possible choices.')
10    read(*,*) MATNO(STKNO)
      if (MATNO(STKNO) .eq. 0) then
         call MATLST
         print 110
110      format(/' Enter the material number of this stack.')
         go to 10
       else if ((MATNO(STKNO) .lt. 0) .or.
     &          (MATNO(STKNO) .gt. NMAT)) then
         print 120, NMAT
120      format(' Please enter a number between 0',
     &          ' and ',i2,'.')
         go to 10
      end if
              
      print 130
130   format(' How many plies are in this stack?')
20    read(*,*) NPLY(STKNO)
      if (NPLY(STKNO) .le. 0) then
          print 140
140       format(' Please enter an integer greater than 0.')
          go to 20
      end if
    
      print 150, LENUL
150   format(' How thick is each ply?  Enter the thickness',
     &       ' in ',a,'.')
       read(*,*) PTHK(STKNO)
30     if (PTHK(STKNO) .le. 0.) then
          print 160
160       format(' Please enter a number greater than 0.')
          go to 30
       end if
              
      print 170
170   format(' Enter the orientation of the plies of this',
     &       ' stack in degrees.')
      read(*,*) THETA(STKNO)
      
      return
      end
      
      
           subroutine TEMPI
      
      
c     Name:      TEMPerature Input 
c     Purpose:   To input temperture distribution
c     Input:     Temperature information from user
c     Output:    The above information into LOADI common
c     Commons:   LOADI
c     Called by: LOADS
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
       
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      real*8           THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0(MAXELM+1), TEMP1(MAXELM+1),
     *                 TEMP2(MAXELM+1),
     *                 MOIST0(MAXELM+1),MOIST1(MAXELM+1),
     *                 MOIST2(MAXELM+1)
     
      common /LOAD/    THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0,TEMP1,TEMP2,
     *                 MOIST0,MOIST1,MOIST2
      save   /LOAD/
      
      
      logical         SIMP
      integer         NTMPI,NMSTI,TNI(MAXELM+1),MNI(MAXELM+1)
      real*8          TXI(MAXELM+1),
     &                TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &                MXI(MAXELM+1),
     &                MII(MAXELM+1),MMI(MAXELM+1),MOI(MAXELM+1)
     
      common /LOADI/  TXI,TII,TMI,TOI,
     &                MXI,MII,MMI,MOI,
     &                NTMPI,NMSTI,TNI,MNI,
     &                SIMP
      save   /LOADI/
      
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
       
       logical     YES
       integer     TN, I
       real*8      TIIS,TMIS,TOIS

      do 1 I=1,MAXELM+1
         TNI(I) = 0
         TII(I) = 0.
         TMI(I) = 0.
         TOI(I) = 0.
1     continue
     
      TNI(1) = 1
      print 100, X1,XF,X1US
100   format(' At the first edge of the shell,',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' temperature constant',
     &       ' through the thickness or does',
     &       ' it vary linearly or'/' quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
      
      
      call GETTMP(TII(1),TMI(1),TOI(1))
      
      print 110, X1,XS,X1US
110   format(' At the second edge of the shell',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' temperature constant',
     &       ' through the thickness or does',
     &       ' it vary linearly'/' or quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
     
      call GETTMP(TIIS,TMIS,TOIS)
      
      if (SIMP) then
         NTMPI = 2
         TNI(NTMPI) = NUMEL+1
         TII(NTMPI) = TIIS
         TMI(NTMPI) = TMIS
         TOI(NTMPI) = TOIS
         call THTINT(THICK,TNI,TII,TMI,TOI,TEMP0,TEMP1,TEMP2)
         return
      end if
         

      print 101
101   format(/' For reference, the nodal positions',
     &       ' previously specified are:')
     
      call XEDWRT(TERM)
      
      
      NTMPI = 1
                  
         
10    continue

         NTMPI = NTMPI+1
        
         print 120
120      format(' Do you want to enter the temperature at another',
     &         ' position of the shell?')
         call YESNO(YES)
         if (YES) then
20          print 130
130         format(' Enter node number at which you want'
     &             ' to specify the',
     &             ' temperature distribution.')
            read(*,*) TN
     	    if (TN .le. TNI(NTMPI-1)) then
     	        print 140, TNI(NTMPI-1)
140             format(' The element edge number must be greater',
     &               ' than ',i2,'.')
                go to 20
             else if (TN .gt. NUMEL+1) then
                print 150, NUMEL+1
150             format(' The element edge number must be less',
     &               ' than or equal to ',i2,'.')
                go to 20
            end if
          
           TNI(NTMPI) = TN
           print 153
153       format(' Is the temperature constant',
     &             ' through the thickness',
     &             ' or does it vary linearly'/' or quadratically',
     &             ' through the thickness at this position?'/
     &             ' Enter "c" for constant, "l" for',
     &             ' linearly,or "q" for quadratically.')

           call GETTMP(TII(NTMPI),TMI(NTMPI),TOI(NTMPI))
           go to 10
          else
            TNI(NTMPI) = NUMEL+1
            TII(NTMPI) = TIIS
            TMI(NTMPI) = TMIS
            TOI(NTMPI) = TOIS
            call THTINT(THICK,TNI,TII,TMI,TOI,TEMP0,TEMP1,TEMP2)
            return
       end if     

      end
      
      
      subroutine TITLES(EXIST)
      
      
c     Name:      TITLE Subroutine
c     Purpose:   To input the TITLE for the current input file
c     Input:     EXIST, if true, title exists already
c                TITLE, from user
c     Output:    The TITLE in the TITLE common
c     Commons:   TITLE
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************
 
 
      
      character*80     TITLE
      common /TITLE/   TITLE
      save   /TITLE/

      
      logical   TRU,EXIST
      
      print 50
50    format(//' TITLE')

      if (EXIST) then
         print 100, TITLE
100      format(/' The current title is:',//,a,//
     &          ' Do you want to change it?')
         call YESNO(TRU)
         if ( .not. TRU) return
      end if 
      
      print 110
110   format(/' Enter the title of this case. It must be less than 80',
     &       ' characters.')
      read(*,'(a)') TITLE
      
      return
      end
      
      

       


         subroutine TIWRT(UNITNO)
      
      
c     Name:      TeMPerature WRiTe 
c     Purpose:   To write the input temperature data to the unit
c                (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c     Output:    Temperature data to the unit
c     Commons:   
c     Called by: INWRT,LOADS
c                                                                      |
c*******************************************************************************


      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      logical         SIMP
      integer         NTMPI,NMSTI,TNI(MAXELM+1),MNI(MAXELM+1)
      real*8          TXI(MAXELM+1),
     &                TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &                MXI(MAXELM+1),
     &                MII(MAXELM+1),MMI(MAXELM+1),MOI(MAXELM+1)
     
      common /LOADI/  TXI,TII,TMI,TOI,
     &                MXI,MII,MMI,MOI,
     &                NTMPI,NMSTI,TNI,MNI,
     &                SIMP
      save   /LOADI/
      
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      integer     UNITNO,I
       
      write(UNITNO,100) X1,X1US,TEMPUS,TEMPUS,TEMPUS
100   format(//' Temperature Distribution'//
     &       3x,' Position',6x,
     &       'Inside Temp.',4x,'Middle Temp.',4x,
     &       'Outside Temp'/
     &       6x,a,11x,'Change',10x'Change',10x,'Change'/
     &       4x,'(',a,')',7x,'(',a,')',9x,
     &       '(',a,')',9x,'(',a,')')
      do 1 I=1,NTMPI
         print 110, XEDGE(TNI(I)),TII(I),TMI(I),TOI(I)
110      format(x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      
      subroutine UNITSS(EXIST)
      
c     Name:      UNITS Subroutine
c     Purpose:   To determine which units are to be used and set the variables
c                of UNITS common accordingly.
c     Input:     Types of units to be used.
c                
c     Output:    Variables of UNITS common
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      logical     EXIST,YES
      
      print 50
50    format(//' PHYSICAL UNITS')
      
      if (EXIST) then
      
           if (UNIT .eq. 's') then
                print 100
100             format(/' The data in the input',
     &                 ' file is in SI units.',
     &                 ' Do you want to change to English'/' units?')
                call YESNO(YES)
                if (YES) then
                   UNIT = 'e'
                end if
             else if (UNIT .eq. 'e') then
                print 110
110             format(/' The data in the input',
     &                 ' file is in English units.',
     &                 ' Do you want to change to'/' SI units?')
                call YESNO(YES)
                if (YES) then
                   UNIT = 's'
                end if
            end if
               
         else
          
            print 120
120         format(/' Which dimensional unit',
     &             ' system is used, SI or',
     &             ' English?',/,' Enter "s" for SI or "e" for',
     &             ' English.')
1           read(*,'(a)') UNIT

      end if
      
      if ((UNIT .ne. 's') .and. (UNIT .ne. 'S') .and.
     &    (UNIT .ne. 'e') .and. (UNIT .ne. 'E')) then
           print 130
130        format(' Please enter either "s" or "e".')
           go to 1
      end if
      
      call UNTSET
      
      return
      end
      
      
         subroutine VIEWMT(MATNO)
      
      
c     Name:      VIEW MaTerial data
c     Purpose:   To view the material properties in the data base MAT.DAT
c     Input:     Material no. of the material to view
c                If a 0 is passed, get the material no. from the user.
c     Output:    Material property data to the screen
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************


      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      
      integer      MAT,MATNO
 
      if (MATNO .eq. 0) then
5        print 100
100      format(/' Enter the number of the material whose',
     &          ' properties you wish to view or 0 to see'/
     &          ' the list of materials.')
10       read(*,*) MAT
         if ((MAT .lt. 0) .or. (MAT .gt. NMAT)) then
            print 150, NMAT
150         format(' Please enter a material number between',
     &             ' 0 and ',i2,'.')
            go to 10
          else if (MAT .eq. 0) then
            call MATLST
            go to 5
         end if
       else
         MAT = MATNO
      end if
      
      call WRTMAT(TERM,MAT)
      
      return
      end
       
       
       subroutine WRITE
      
      

c     Name:      WRITE input file
c     Purpose:   To write input data to a file.
c     Common:    TITLE,ELMDAT1,ELMDAT2,ISOMAT,LAMMAT,GEOM,MATCOD,LOAD,BC,CONTRL
c     Input:     Input data from the commons
c                
c     Output:    Data to the input file.
c     Called by:
c     Calls    : NODEIN,NODECALC,ERROR
c                                                                      |
c*******************************************************************************
      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer     BCTYPE(2),BCCODE(2,MXNBC)
      real*8      UBC(2,MXNVAR),TBC(2,MXNVAR)
      common /BC/ BCTYPE,BCCODE,UBC,TBC
      save /BC/
      
      
      character*10    OTFILE
      logical         BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI
      integer         THEORY,TORORD,OSHPU,OSHPW,OSHPD, NINT,NINTW,NINTD
      common /CONTRL/ BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI,
     *                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,NINTW,NINTD,
     *                OTFILE
      save   /CONTRL/
      
      
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      logical           SYM
      integer           NSTACK,MATNO(MAXSTK),NPLY(MAXSTK)
      real*8            LTHKNS,PTHK(MAXSTK),THETA(MAXSTK),STHKNS(MAXSTK)
     
      common  /LAYUP/   SYM,
     &                  NSTACK,MATNO,NPLY,
     &                  LTHKNS,PTHK,THETA,STHKNS
 
      save    /LAYUP/
      
      
      real*8           THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0(MAXELM+1), TEMP1(MAXELM+1),
     *                 TEMP2(MAXELM+1),
     *                 MOIST0(MAXELM+1),MOIST1(MAXELM+1),
     *                 MOIST2(MAXELM+1)
     
      common /LOAD/    THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0,TEMP1,TEMP2,
     *                 MOIST0,MOIST1,MOIST2
      save   /LOAD/
      
      
      logical         SIMP
      integer         NTMPI,NMSTI,TNI(MAXELM+1),MNI(MAXELM+1)
      real*8          TXI(MAXELM+1),
     &                TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &                MXI(MAXELM+1),
     &                MII(MAXELM+1),MMI(MAXELM+1),MOI(MAXELM+1)
     
      common /LOADI/  TXI,TII,TMI,TOI,
     &                MXI,MII,MMI,MOI,
     &                NTMPI,NMSTI,TNI,MNI,
     &                SIMP
      save   /LOADI/
      
      
      logical          HOMOGN
      character*3      MATCODE      
      common /MATCOD/  HOMOGN,MATCODE
      save   /MATCOD/


      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

      real*8          DISP(MAXNOD,MXNVAR)
      
      common /OUT/    DISP
      save   /OUT/


      logical       SAVED
      common /SAVE/ SAVED
      save   /SAVE/

      character*80     TITLE
      common /TITLE/   TITLE
      save   /TITLE/

      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      integer        NXEDI,XNODEI(MAXELM+1)
      real*8         XEDGEI(MAXELM+1)
      common /XEDIN/ NXEDI,XNODEI,XEDGEI
      save   /XEDIN/

      
      logical      OUTEXT
      
      character*1  ANS
      character*3  STAT
      character*7  DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*10 OUTFILE
      
      integer      I,EDGENO,OUTFIL
      parameter(OUTFIL=3)
   
      data DISPBC/'u1','u2','w','1 Beta1','1 Beta2','1 Eta','2 Eta',
     &            '3 Beta1','3 Beta2','2 Beta1','2 Beta2','3 Eta',' '/,
     &     TRACBC/'N11','N12','Q1','1 M11','2 M12','1 S1','2 S1',
     &            '3 M11','3 M12','2 M11','2 M12', '3 S1',' '/

      
1     print 105
105   format(' Enter the name of the file to which you want to save',
     &       ' this input data set.')
      read(*,'(a)') OUTFILE
      inquire(file=OUTFILE,exist=OUTEXT)
      
      if (OUTEXT) then
         print 106, OUTFILE
106      format(/' The file "'a'" already exists.')
2        print 107, OUTFILE
107      format(' Do you want to write',
     &      ' over the existing "'a'" or',
     &      ' choose a different'/' file name ? ',
     &      ' Enter "w" to write or "n" to name a new file.')
         read(*,'(a)') ANS
         if (ANS .eq. 'n') then
               go to 1
             else if (ANS .eq. 'w') then
               STAT = 'old'
             else
               print 108
108            format(' Please answer "w" or "n".')
               go to 2
         end if
       else
         STAT = 'new'
      end if

      open (unit=OUTFIL, file= OUTFILE, status= STAT)
      
      write(OUTFIL,'(a)')     TITLE
      
      write(OUTFIL,100) UNIT
100   format(a,18x,':UNIT, "s" for SI, "e" for English')

      write(OUTFIL,110) THEORY
110   format(i1,18x,':THEORY;  1,2,3,4,5,6,7,8')
     
      write(OUTFIL,120) RIGID
120   format(l1,18x,':RIGID if TRUE, rigid',
     &       ' body displacements included')
      
      write(OUTFIL,130) TORORD
130   format(i1,18x,':TORORD; t/R order 0,1,or2')
      
      write(OUTFIL,140) OSHPU,OSHPW,OSHPD
140   format(i1,2(x,i1),14x,':OSHPU,OSHPW,OSHPD(integers);',
     &      'Order SHape function (U,W,D)')
     
      write(OUTFIL,150) NINT
150   format(i1,18x,':NINT (Number of INTegration points')

      write(OUTFIL,160) KSHORT
160   format(l1,18x,':KSHORT; if TRUE, quick alogorithm for',
     &       ' stiffness')
      
      write(OUTFIL,170) SHAPE
170   format(a,15x,':SHAPE(character*4); either CYLN, CONE, SPHR,',
     &       'PARB')
     
      if (SHAPE .eq. 'cyln') then
           write(OUTFIL,180) RAD,CYLEN
180        format(2(x,e12.5),2x,':Radius, Length')
         else if (SHAPE .eq. 'cone') then
           write(OUTFIL,190) R0,ALPHAD,CONEHT
190        format(3(x,e12.5),7x,':Ro,Alpha,Height')
         else if (SHAPE .eq. 'torr') then
           write(OUTFIL,200) RC,RPHI,PHI0D
200        format(3(x,e12.5),7x,':Rc,Rphi,Phi0')
         else if (SHAPE .eq. 'torp') then
           write(OUTFIL,201) RC,RPHI,XF,XS
201        format(4(x,e12.5),7x,':Rc,Rphi,Phi_top,Phi_bot')
         else if (SHAPE .eq. 'para') then
           write(OUTFIL,210) RAT0,OFFSET,XF,XS
210        format(4(x,e12.5),7x,':R0,Offset,Phi_top,Phi_bot')
         else if (SHAPE .eq. 'cyls') then
           write(OUTFIL,180) RAD
      end if
      
      write(OUTFIL,220) NUMEL
220   format(i2,17x,':NUMEL(integer); number of elements')

      do 10 I=1,NXEDI      
          write(OUTFIL,230) XNODEI(I),XEDGEI(I)
230       format(i2,x,e12.5,4x,':Node,X(Node)')
10    continue
                
      
      write(OUTFIL,240)  HOMOGN
240   format(l1,18x,':HOMOGN, if true material is homogeneous')
      
           
      write(OUTFIL,250) SYM
250   format(l1,18x,':SYM; if true laminate is symmetric')

      write(OUTFIL,260) NSTACK
260   format(i2,17x,':Number of stacks')
            
      do 20 I=1,NSTACK
          write(OUTFIL,270) I,MATNO(I),NPLY(I),PTHK(I),THETA(I)
270       format(3(x,i2),x,e12.5,x,f6.1,2x,':Stack No.,Material No.,',
     &           'NPLY,PTH,Theta')
20    continue

      
      write(OUTFIL,280) FCODE
280   format(l1,18x,':FCODE; if .TRUE. mechanical load data follows')
      if (FCODE) then
         write(OUTFIL,290) PZTOP,PZBOT
290      format(2(x,e12.5),2x,':Internal Pressure at edges 1 & 2')
         write(OUTFIL,300) N11,N22,M11,M22
300      format(2(x,e12.5),2x,':n11,n22,m11,m22')
      end if

      write(OUTFIL,310) TCODE
310   format(l1,18x,':TCODE; if .TRUE. thermal data follows')
      if (TCODE) then
         do 30 I=1,NTMPI
            write(OUTFIL,320) TNI(I),TII(I),TMI(I),TOI(I)
320         format(i2,3(x,e12.5),2x,':Node,Temp in.,',
     &             'Temp mid.,Temp out.')         
30       continue
      end if

      write(OUTFIL,330) MOIST
330   format(l1,18x,':MOIST; if .TRUE. moisture data follows')
      if (MOIST) then
         do 40 I=1,NMSTI
            write(OUTFIL,340) MNI(I),MII(I),MMI(I),MOI(I)
340         format(i2,3(x,e12.5),2x,':Node,Moist in.,',
     &             'Moist mid.,Moist out.')         
40       continue
      end if


      do 60 EDGENO=1,2
         write(OUTFIL,350)  BCTYPE(EDGENO),EDGENO
350      format(i1,18x,':BCTYPE(',i1,') 0:general 1:s.s 2:clamp',
     &                  ' 3:free 4:sym')
         if (BCTYPE(EDGENO) .eq. 0) then
            do 50 I=1,MXNBC
               if (BCCODE(EDGENO,I) .eq. 1) then
                  write(OUTFIL,360) I,BCCODE(EDGENO,I),UBC(EDGENO,I),
     &                              EDGENO,I,EDGENO,I,DISPBC(I)
360               format(i2,x,i1,x,e12.5,2x,':BCCODE(',i1,',',i2,
     &                   '), BC(',i1,',',i2,'):  ',a)
                 else if (BCCODE(EDGENO,I) .eq. 2) then
                  write(OUTFIL,360) I,BCCODE(EDGENO,I),TBC(EDGENO,I),
     &                              EDGENO,I,EDGENO,I,TRACBC(I)
               end if
50          continue
         end if
60    continue

      write(OUTFIL,370) BACK
370   format(l1,18x,':BACK; if true solve for displacements')

      write(OUTFIL,380) RSLT
380   format(l1,18x,':ReSuLT; if true solve for resultants')

      write(OUTFIL,390) PLTSWC
390   format(l1,18x,':PLoTSWitCh; if true plotting data files')

      write(OUTFIL,400) OTFILE
400   format(a,9x,':Output file name')
           
      close(OUTFIL)
      
      SAVED = .true.
      
      return
      end
      
      
          subroutine XEDGES(EXIST)
      
      
c     Name:      X EDGE Subroutine
c     Purpose:   To input the positions of the edges of the finite elments
c     Input:     EXIST, if true, position information already exists
c                XEDGE, element edge postions from user
c     Output:    The above information into ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      
      integer      MXNVAR,MXNRBM,MXNBC,MAXNOD,MAXELM,MAXNEN,
     *             MXNEPS,MAXINT,
     *             MAXNEQ,MAXLKS,MXSOPT,MAXMAT,MAXSTK
      parameter   (MXNVAR=13,MXNRBM=2,MXNBC=12,
     *             MAXNOD=81,MAXELM=40,MAXNEN=3,
     *             MXNEPS=27,MAXINT=3,MAXNEQ=573,MAXLKS=11000,
     *             MXSOPT=40,MAXMAT=15,MAXSTK=30)
     
      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
      integer        NXEDI,XNODEI(MAXELM+1)
      real*8         XEDGEI(MAXELM+1)
      common /XEDIN/ NXEDI,XNODEI,XEDGEI
      save   /XEDIN/

      
      logical      YES,EXIST,OK
      integer      I,N
      real*8       X
      
1     continue     
      if (EXIST) then
         print 100, X1,X1US
100      format(///' The positions of the',
     &          ' edges of the elements are:'//
     &          ' Element Edge',5x,'Position'/21x,a/18x,'(',a,')')
     
         do 10 I=1,NXEDI
            print 110, XNODEI(I),XEDGEI(I)
110         format(5x,i2,8x,e12.5)
10        continue
     
         print 112
112      format(/' Do you want to see the interpolated positions',
     &           ' of all of the element edges?')
         call YESNO(YES)
         if (YES) then
            call XEDWRT(TERM)
         end if

         print 115
115      format(/' Do you want to change these?')
         call YESNO(YES)
         
         if (.not. YES) return
      end if
      
      print 118
118   format(' Would you like to read some information concerning',
     &       ' entering element'/' edge location data?')
      call YESNO(YES)
      if (YES) then
         print 120, NUMEL+1,XS
120      format(//' In this section, the positions of the edges of the',
     &         ' shell elements are entered.'/' First the edge',
     &         ' element node number and then the position of',
     &         ' that'/' node is entered.  Node numbers and',
     &         ' positions must be given in increasing'/' order.',
     &         ' Any edge element node',
     &         ' position that is not explicitly entered will be'/
     &         ' determined by linear interpolation.',
     &         '  To finish entering',
     &         ' nodes, enter the last'/' node of the last',
     &         ' element (edge node number ',i2,').  The position of',
     &         ' the'/' last node is ',e12.5,' and cannot be changed',
     &         ' here.')
     
         if (X1 .eq. 's') then
            print 122, X1UL
122         format(/' The positions of the edge nodes are to be given',
     &              ' in arc-length (s) along'/
     &              ' the generator in ',a,'.')
          else
            print 123, X1UL
123         format(/' The positions of the edge nodes are to be given',
     &              ' as the angle between a'/
     &              ' line segment normal the midsurface at that',
     &              ' node and a line segment'/
     &              ' parallel to the axis of the shell',
     &              ' (phi) in ',a,'.')
         end if
         
      end if
     
15    print 125, X1,XF,X1US
125   format(/' The position (',a,') of the first node is',
     &       e12.5,x,a'.')
      XNODEI(1) = 1
      XEDGEI(1) = XF
      NXEDI     = 1
      
      print 126, X1,XS,X1US
126   format(' The position (',a,') of the last node is',
     &        e12.5,x,a'.')
      
      
20    print 130, X1,X1UL,NUMEL+1,XS
130   format(/' Enter an edge node number and the',
     &       ' position (',a,') of',
     &       ' that node in ',a,'.'/' The edge node number must be',
     &       ' greater than 1 and less than or',
     &       ' equal to ',i2'.'/' The edge node position must be',
     &       ' less than ',e12.5,'.')
25    read(*,*) N,X


      if (N .eq. NUMEL+1) then
        I=2
        go to 50
      end if
      if (N .le. XNODEI(1)) then
         print 150, XNODEI(1)
150      format(' The edge node number must be greater than ',i2,'.')
         print 145
145      format(' Please reenter the node number and its position.')
         go to 25
      end if
      if (N .gt. NUMEL+1) then
         print 155, NUMEL+1
155      format(' The edge node number must be less than or equal',
     &          ' to ',i2,'.')
         print 145
         go to 25
      end if
      if (X .le. XEDGEI(1)) then
         print 170, XEDGEI(1)
170      format(' The position must be greater than ',e12.5,'.')
         print 145
         go to 25
      end if
      
      if (X .ge. XS) then
         print 171, XS
171      format(' The position must be less that ',e12.5,' .')
         print 145
         go to 25
      end if
      
      call CHECKX(X,OK)
      if (.not. OK) then
           go to 20
         else
           XNODEI(2) = N
           XEDGEI(2) = X
           NXEDI     = NXEDI + 1
      end if
      
     
     
      do 35 I=3,10000
         print 140
140      format(/' Enter an edge node number and its position.')
30       read(*,*) N,X

         if (N .eq. NUMEL+1) go to 50
         if (N .le. XNODEI(I-1)) then
            print 150, XNODEI(I-1)
            print 145
            go to 30
         end if
         if (N .gt. NUMEL+1) then
            print 155, NUMEL+1
            print 145
            go to 30
         end if
         if (X .le. XEDGEI(I-1)) then
            print 170, XEDGEI(I-1)
            print 145
            go to 30
         end if
         if (X .ge. XS) then
            print 141, XS
141         format(' The position must be less that ',e12.5,' .')
            print 145
            go to 30
      end if

         call CHECKX(X,OK)
         if (.not. OK) then
            print 145
            go to 30
          else
           XNODEI(I) = N
           XEDGEI(I) = X
           NXEDI = NXEDI+1
         end if
         

35    continue

50    continue
      
      XNODEI(I) = NUMEL+1
      XEDGEI(I) = XS
      NXEDI     = NXEDI+1
   

      call XEINTP
      EXIST = .true.
      go to 1
         
      end
      
      
      subroutine YESNO(TRU)
      
c     Name:      YESNO
c     Purpose:   To get a yes or answer from the user and set TRU accordingly
c     Input:     A yes or no answer from the user
c     Output:    TRU = .true. if yes and TRU = .false. if no
c     Commons:   None
c     Called by: 
c                                                                      |
c*******************************************************************************


      
      logical      TRU
      character*1  ANS
      
1     read(*,'(a)')    ANS
      
      if ((ANS .eq. 'y') .or. (ANS .eq. 'Y')) then 
           TRU = .true.
           return
        else if ((ANS .eq. 'n') .or. (ANS .eq. 'N')) then 
           TRU = .false.
           return 
        else
           print 100
100        format(' Please answer either "y" or "n".')
           go to 1
      end if
      
      end
      
      
