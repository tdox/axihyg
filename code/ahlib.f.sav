      subroutine ALTOBT(BETA11,BETA22,BETA33,
     &                  ALPH11,ALPH22,ALPH33,
     &                  C1111,C2222,C3333,C1122,C1133,C2233)
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      ALpha TO Beta                                                *
c     Purpose:   To calculate BETA (the expansion stress coefficients) from   *
c                the given ALPHA's (the expansion strain coefficients) and    *
c                stiffness coefficients, C                                    *
c     Input:     ALPHA's and C's                                              *
c     Output:    BETA'S                                                       *
c     Called by: LAMCOF                                                       *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************
 
  
      
      real*8     BETA11,BETA22,BETA33,ALPH11,ALPH22,ALPH33,
     &           C1111,C2222,C3333,C1122,C1133,C2233
     
     
      BETA11 = - (C1111*ALPH11 + C1122*ALPH22 + C1133*ALPH33)
      BETA22 = - (C1122*ALPH11 + C2222*ALPH22 + C2233*ALPH33)
      BETA33 = - (C1133*ALPH11 + C2233*ALPH22 + C3333*ALPH33)
 
      return
      end
      
      
      
      
      
      
      
       subroutine BCWRT(UNITNO)
      
c     Name:      Boundary Condition WRiTe
c     Purpose:   To write the boundary condition data to the
c                unit (file, screen) UNITNO.
c     Input:     UNITNO
c                Boundary Condition data from the LAYUP common
c     Output:    The layup data to the UNITNO unit
c     Called by: BCS,INWRT
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
      
      
     
      character*6  DSPBCU(MXNVAR)
      character*7  DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*8  TRCBCU(MXNVAR)
      
      integer      UNITNO,I,EDGN
      
      data DISPBC/'u1','u2','w','1 Beta1','1 Beta2','1 Eta','2 Eta',
     &            '3 Beta1','3 Beta2',
     &            '2 Beta1','2 Beta2','3 Eta',' '/,
     &     TRACBC/'N11','N12','Q1','1 M11','2 M12','1 S1','2 S1',
     &            '3 M11','3 M12','2 M11','2 M12', '3 S1',' '/
     
      
      call BCUNIT(DISPBC,TRACBC,DSPBCU,TRCBCU)

      write(UNITNO,100)
100   format(////'  Boundary Conditions',//)
      
      do 10 EDGN=1,2
         write(UNITNO,110) EDGN
110      format(' Prescribed at edge number ',i2/)

         if (BCTYPE(EDGN) .eq. 0) then
            write(UNITNO,111)
111         format(' This edge has general supports.')
          else if (BCTYPE(EDGN) .eq. 1) then
            write(UNITNO,112)
112         format(' This edge is simply supported.')
          else if (BCTYPE(EDGN) .eq. 2) then
            write(UNITNO,113)
113         format(' This edge is clamped.')
          else if (BCTYPE(EDGN) .eq. 3) then
            write(UNITNO,114)
114         format(' This edge is free.')
          else if (BCTYPE(EDGN) .eq. 4) then
            write(UNITNO,115)
115         format(' This edge is symmetrically supported.')
         end if
         
         write(UNITNO,117)
117      format(/' Variable',6x,'Prescribed Value'/)
         do 1 I=1,NBC
            if (BCCODE(EDGN,I) .eq. 1) then
               write(UNITNO,120) DISPBC(I),UBC(EDGN,I),DSPBCU(I)
120            format(x,a7,5x,e12.5,x,a)
             else
               write(UNITNO,120) TRACBC(I),TBC(EDGN,I),TRCBCU(I)
            end if
1        continue
         if (UNITNO .eq. TERM) call WAIT
         write(UNITNO,125)
125      format(//)
10    continue
          

      return
      end
      
      




           subroutine BCSET(BC)
      
      
c     Name:      Boundary Condition SET
c     Purpose:   To set the boundary conditions from the input data
c     Input:     BCTYPE(2), I'th element is type of BC at edge I.
c                   if BCTYPE = 0 then BC is general
c                   if BCTYPE = 1 then BC is simply supported
c                   if BCTYPE = 2 then BC is clamped
c                   if BCTYPE = 3 then BC is free
c                   if BCTYPE = 4 then BC is symmetric BC
c                BCCODE(2,NVAR),BC(2,NVAR)
c                if BC(I,J)=1 then BC(I,J) is the prescribed displacement
c                of dof J at edge I
c                if BC(I,J)=2 then BC(I,J) is the prescribed traction
c                of dof J at edge I
c     Output:    UBC(2,MXNVAR),TBC(2,MXNVAR) to BC common
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
      
      
      

       
      integer     EDGN,DOF
       
      real*8       BC(2,MXNBC)
      
                    
      do 10 EDGN=1,2
       
          if (BCTYPE(EDGN) .ne. 0) then
          
             do 20 DOF=1,NBC
                BC(EDGN,DOF) = 0.
20           continue

             if (BCTYPE(EDGN) .eq. 1) then
c              simply supported
                BCCODE(EDGN,1)  = 1
                BCCODE(EDGN,2)  = 1
                BCCODE(EDGN,3)  = 1
                BCCODE(EDGN,4)  = 2
                BCCODE(EDGN,5)  = 2
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 2
                BCCODE(EDGN,9)  = 2
                BCCODE(EDGN,10) = 2
                BCCODE(EDGN,11) = 2
                BCCODE(EDGN,12) = 2
              else if (BCTYPE(EDGN) .eq. 2) then
c              clamped
                BCCODE(EDGN,1)  = 1
                BCCODE(EDGN,2)  = 1
                BCCODE(EDGN,3)  = 1
                BCCODE(EDGN,4)  = 1
                BCCODE(EDGN,5)  = 1
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 1
                BCCODE(EDGN,9)  = 1
                BCCODE(EDGN,10) = 1
                BCCODE(EDGN,11) = 1
                BCCODE(EDGN,12) = 2
              else if (BCTYPE(EDGN) .eq. 3) then
c              free
                BCCODE(EDGN,1)  = 2
                BCCODE(EDGN,2)  = 2
                BCCODE(EDGN,3)  = 2
                BCCODE(EDGN,4)  = 2
                BCCODE(EDGN,5)  = 2
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 2
                BCCODE(EDGN,9)  = 2
                BCCODE(EDGN,10) = 2
                BCCODE(EDGN,11) = 2
                BCCODE(EDGN,12) = 2
              else if (BCTYPE(EDGN) .eq. 4) then
c              symmetric
                BCCODE(EDGN,1)  = 1
                BCCODE(EDGN,2)  = 1
                BCCODE(EDGN,3)  = 2
                BCCODE(EDGN,4)  = 1
                BCCODE(EDGN,5)  = 1
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 1
                BCCODE(EDGN,9)  = 1
                BCCODE(EDGN,10) = 1
                BCCODE(EDGN,11) = 1
                BCCODE(EDGN,12) = 2
            end if
         end if
 

         do 30 DOF = 1,MXNBC
            UBC(EDGN,DOF) = 0.
            TBC(EDGN,DOF) = 0.
30       continue
      
         do 40 DOF=1, NBC      
              if (BCCODE(EDGN,DOF) .eq. 1) then
                    UBC(EDGN,DOF) = BC(EDGN,DOF)
                    TBC(EDGN,DOF) = 0.
                 else if (BCCODE(EDGN,DOF) .eq. 2) then
                    UBC(EDGN,DOF) = 0.
                    TBC(EDGN,DOF) = BC(EDGN,DOF)
                 else 
                    call ERROR('BCSET     ',
     *                        'BCCODE must be either 1 or 2   ')
              end if
40       continue
                

10    continue

      return
      end
      





       subroutine BCUNIT(DISPBC,TRACBC,DSPBCU,TRCBCU)
      
c     Name:      Boundary Condition UNIT
c     Purpose:   To set the boundary condition units
c     Input:     DISPUS, RSLTUS from UNITS common
c     Output:    Physical units in DSPBCU, TRACBCU
c     Called by: BCS,INWRT
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
      
      

      character*6  DSPBCU(MXNVAR)
      character*7  DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*8  TRCBCU(MXNVAR)
      
     
      DSPBCU(1)  = DISPUS(0)
      DSPBCU(2)  = DISPUS(0)
      DSPBCU(3)  = DISPUS(0)
      DSPBCU(4)  = DISPUS(1)
      DSPBCU(5)  = DISPUS(1)
      DSPBCU(6)  = DISPUS(1)
      DSPBCU(7)  = DISPUS(2)
      DSPBCU(8)  = DISPUS(3)
      DSPBCU(9)  = DISPUS(3)
      DSPBCU(10) = DISPUS(2)
      DSPBCU(11) = DISPUS(2)
      DSPBCU(12) = DISPUS(3)
      
      TRCBCU(1)  = RSLTUS(0)
      TRCBCU(2)  = RSLTUS(0)
      TRCBCU(3)  = RSLTUS(0)
      TRCBCU(4)  = RSLTUS(1)
      TRCBCU(5)  = RSLTUS(1)
      TRCBCU(6)  = RSLTUS(1)
      TRCBCU(7)  = RSLTUS(2)
      TRCBCU(8)  = RSLTUS(3)
      TRCBCU(9)  = RSLTUS(3)
      TRCBCU(10) = RSLTUS(2)
      TRCBCU(11) = RSLTUS(2)
      TRCBCU(12) = RSLTUS(3)

      return
      end
      
      




          subroutine CHECKX(X,OK)
      
      
c     Name:      CHECK X position
c     Purpose:   To check whether X is a valid nodal position for the given
c                geometry
c     Input:     X, the nodal position
c     Output:    Message to user about validity of nodal position
c     Commons:   ELMDAT
c     Called by: CREATE
c                                                                      |
c*******************************************************************************



      real*8     PI,PIODEG
      parameter (PI=3.14159265, PIODEG = 0.0174532952)
      
      
      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
      
      logical       OK
      real*8       X,R
      
      OK = .true.
      
      if (SHAPE .eq. 'cone') then
          R = X*sin(ALPHAD*PI/180.) + R0
          if (R .lt. 0.) then
             print 180
180          format(' R is less than 0.  Try a larger s.')
             OK = .false.
          end if
        else if (SHAPE .eq. 'torp') then
          R =  RC + RPHI*sin(X*PI/180.)
          if (R .lt. 0.) then
             print 190
190          format(' R is less than 0.  Try a larger phi.')
             OK = .false.
          end if
        else if (SHAPE .eq. 'para') then
          if ((X .lt. 0.) .or. (X .ge. 90.)) then
             print 200
200          format(' Phi must ge greater than 0 and less than 90.')
             OK = .false.
          end if
         
      end if
      
      return
      end
      
      
      subroutine ERROR(LOC,MESSAGE)

c     Name:      ERROR
c     Purpose:   To print an error message and stop execution.
c     Input:     LOCation, MESSAGE
c     Output:    An error message to OUTPUT.DAT
c     Called by: 

      
      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
      
      character*10 LOC
      character*30 MESSAGE
      
      write (ERRFIL,100) LOC,MESSAGE
100   format(' Fatal Error. Location: ',a,'Reason: ',a)
      if (ERRFIL .eq. TERM) call WAIT
      stop
      end
      
      
      
  
      subroutine GEOWRT(UNITNO)
      
c     Name:      GEOmetry WRiTe
c     Purpose:   To write the geometry data  to the unit(file, screen) UNITNO.
c     Input:     UNITNO
c                Geometry data from the Geometry common
c     Output:    The geometry  data to the UNITNO unit
c     Called by: INWRT,LAYUPS
c     Calls    : 
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
      
      
      
      character*20 SHAPEL
      
      integer      UNITNO
      
      
      if (SHAPE .eq. 'cyln') then
          SHAPEL = 'cylinder'
        else if (SHAPE .eq. 'cone') then
          SHAPEL = 'cone'
        else if (SHAPE .eq. 'torp') then
          SHAPEL = 'toroidal section'
        else if (SHAPE .eq. 'para') then
          SHAPEL = 'paraboloid'
        else if (SHAPE .eq. 'torr') then
          SHAPEL = 'toorroidal section'
        else if (SHAPE .eq. 'cyls') then
          SHAPEL = 'cylindrical section'
      end if
      
      write(UNITNO,100) SHAPEL
100   format(///' Shell Geometry'//' Shape of the shell:  ',a,/)

      if (SHAPE .eq. 'cyln') then
           write(UNITNO,110) RAD,LENUS,CYLEN,LENUS
110        format(' Radius = ',e12.5,x,a/
     &            ' Length = ',e12.5,x,a)
         else if (SHAPE .eq. 'cone') then
           write(UNITNO,120) R0,LENUS,ALPHAD,CONEHT,LENUS
120        format(' Radius (@ s=0) = ',e12.5,x,a/
     *            ' Alpha          = ',e12.5,' degrees'/
     &            ' Height         = ',e12.5,x,a)
         else if (SHAPE .eq. 'torp') then
           write(UNITNO,130) RC,LENUS,RPHI,LENUS,PHI1,PHI2
130        format(' Radius of center = ',e12.5,x,a/
     *            ' R phi            = ',e12.5,x,a/
     &            ' Phi_top          = ',e12.5,' degrees'/
     &            ' Phi_bot          = ',e12.5,' degrees')
         else if (SHAPE .eq. 'torr') then
           write(UNITNO,135) RC,LENUS,RPHI,LENUS,PHI0D
135        format(' Radius of center = ',e12.5,x,a/
     *            ' R phi            = ',e12.5,x,a/
     *            ' Phi (@ s=0)      = ',e12.5,' degrees')
         else if (SHAPE .eq. 'para') then
           write(UNITNO,140) RAT0,LENUS,OFFSET,LENUS,PHI1,PHI2
140        format(' Radius 1 @ phi=0. = ',e12.5,x,a/
     *            ' Offset            = ',e12.5,x,a/
     &            ' Phi_top           = ',e12.5,' degrees'/
     &            ' Phi_bot           = ',e12.5,' degrees')
         else if (SHAPE .eq. 'cyls') then
           write(UNITNO,150) RAD,LENUS,PHI1,PHI2
150        format(' Radius  = ',e12.5,x,a/
     &            ' Phi_top = ',e12.5,' degrees'/
     &            ' Phi_bot = ',e12.5,' degrees')
      end if

      return
      end
      
      
       subroutine INRED(INFILE)
      
      

c     Name:      INput REaD
c     Purpose:   To read input data from a file and make preliminary cal-
c                culations.
c     Common:    TITLE,ELMDAT1,ELMDAT2,ISOMAT,LAMMAT,GEOM,MATCOD,LOAD,BC,CONTRL
c     Input:     Data from the file INRED
c                
c     Output:    Date to the commons
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
      
      real*8            E,NU,ALPHA,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323,
     &                  ALPH11,ALPH22,ALPH33,
     &                  BETA11,BETA22,BETA33,THCKNS,
     &                  CI1111,CI2222,CI1122,BITA11,BITA22
      common /HOMMAT/   E,NU,ALPHA,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323,
     &                  ALPH11,ALPH22,ALPH33,
     &                  BETA11,BETA22,BETA33,THCKNS,
     &                  CI1111,CI2222,CI1122,BITA11,BITA22
      save   /HOMMAT/
      
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

      
      logical      OK,BETLOG
      character*10 INFILE
      
      integer      I,DUMMY,INFIL,N,EDGENO,DOF
   
      real*8       BC(2,MXNBC),X
      
      parameter(INFIL=2)
      
      open (unit=INFIL, file= INFILE, status= 'old')
      
      read(INFIL,'(A)')     TITLE
      
      read(INFIL,'(A)')     UNIT
      
      call UNTSET

      read(INFIL,*)         THEORY
      call THRSET
      
      read(INFIL,*)         RIGID
      if (RIGID) then
         NRBM = 1
        else
         NRBM = 0
      end if
      
      read(INFIL,*)         TORORD
      if ((TORORD .ne. 0) .and. (TORORD .ne. 1)
     *                    .and. (TORORD .ne. 2) )  then
          call ERROR('INPUT ','TORORD must be equal to 0,or 1,2')
      end if
      
      read(INFIL,*)       OSHPU,OSHPW,OSHPD
      read(INFIL,*)       NINT
      read(INFIL,*)       KSHORT
      
      read(INFIL,'(A)')     SHAPE
      FLAT = .false.
      
      if ((SHAPE .eq. 'CYLN') .or. (SHAPE .eq. 'cyln')) then
           SHAPE = 'cyln'
           X1    = 's'
           X1US  = LENUS
           X1UL  = LENUL
           read(INFIL,*) RAD,CYLEN
           if (RIGID) call ERROR('INPUT','RIGID should be F for cyln')
         else if ((SHAPE .eq. 'CONE') .or. (SHAPE .eq. 'cone')) then
           SHAPE = 'cone'
           read(INFIL,*) R0,ALPHAD,CONEHT
           if (RIGID) call ERROR('INPUT','RIGID should be F for cone')
           X1    = 's'           
           X1US  = LENUS
           X1UL  = LENUL
         else if ((SHAPE .eq. 'TORR') .or. (SHAPE .eq. 'torr')) then
           SHAPE = 'torr'
           X1    = 's'           
           X1US  = LENUS
           X1UL  = LENUL
           read(INFIL,*) RC,RPHI,PHI0D
         else if ((SHAPE .eq. 'TORP') .or. (SHAPE .eq. 'torp')) then
           SHAPE = 'torp'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
           read(INFIL,*) RC,RPHI,PHI1,PHI2
           XF = PHI1
           XS = PHI2
         else if ((SHAPE .eq. 'PARA') .or. (SHAPE .eq. 'para')) then
           SHAPE = 'para'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
           read(INFIL,*) RAT0,OFFSET,PHI1,PHI2
           XF = PHI1
           XS = PHI2
        else if ((SHAPE .eq. 'CYLS') .or. (SHAPE .eq. 'cyls')) then
           SHAPE = 'cyls'
           X1    = 'phi'
           X1US  = 'degrees'
           X1UL  = X1US
           read(INFIL,*) RAD,CYLEN
           FLAT = .true.
         else
           call ERROR('INPUT     ','Improper SHAPE                ')
      end if
         
      read(INFIL,*)         NUMEL
      
      if (NUMEL .gt. MAXELM) then
         call ERROR('INPUT     ','NUMEL>MAXELM.Increase MAXELM.  ')
      end if

      I = 1
      read(INFIL,*) N,X
      call CHECKX(X,OK)
      if (.not. OK) stop
      XNODEI(1) = 1
      XEDGEI(1) = X
      XF        = X
          
          
          
comment: read in the nodal coordinates
10    continue
          I = I + 1
          read(INFIL,*) N,X
          if (N .le. XNODEI(I-1)) then
             write(ERRFIL,100) XNODEI(I-1)
100          format(' The edge node number must be',
     &              ' greater than ',i2,'.')
             call WAIT
             stop
          end if
          if (N .gt. NUMEL+1) then
              write(ERRFIL,120) NUMEL+1
120           format(' The edge node number must be',
     &                  ' less than or equal to ',i2,'.')
              call WAIT
              stop
          end if
          if (X .le. XEDGEI(I-1)) then
             write(ERRFIL,130) XEDGEI(I-1)
130          format(' The position must be',
     &                 ' greater than ',e12.5,'.')
             call WAIT
             stop
          end if
          call CHECKX(X,OK)
          if (.not. OK) stop
          XNODEI(I) = N
          XEDGEI(I) = X
                
       if (N .lt. NUMEL+1) go to 10


       NXEDI = I
       XS    = X
       call XEINTP
                   
      
      read(INFIL,*)  HOMOGN
      
      if (HOMOGN) then
         read(INFIL,'(A)')  MATCODE
            if ((MATCODE .eq. 'ISO') .or. (MATCODE .eq. 'iso')) then
               MATCODE = 'iso'
               read(INFIL,*)   E,NU,ALPHA,THCKNS
               C1111  = E*(1.-NU)/((1.+NU)*(1.-2.*NU))
               C1122  = E*NU/((1.+NU)*(1.-2.*NU))
               C2323  = E/(2*(1 + NU))
               C2222  = C1111
               C3333  = C1111
               C1133  = C1122
               C2233  = C1122
               C1212  = C2323
               C1313  = C2323
               ALPH11 = ALPHA
               ALPH22 = ALPH11
               ALPH33 = ALPH11
               call ALTOBT(BETA11,BETA22,BETA33,
     &                     ALPH11,ALPH22,ALPH33,
     &                     C1111,C2222,C3333,C1122,C1133,C2233)
     
         else if ((MATCODE .eq. 'RHO') .or. (MATCODE .eq. 'rho')) then
               MATCODE = 'rho'
               read(INFIL,*)   C1111,C2222,C3333
               read(INFIL,*)   C1122,C1133,C2233
               read(INFIL,*)   C1212,C1313,C2323
               read(INFIL,*)   BETLOG
               if (BETLOG) then 
                  read(INFIL,*) BETA11,BETA22,BETA33
                 else
                  read(INFIL,*)   ALPH11,ALPH22,ALPH33
                  call ALTOBT(BETA11,BETA22,BETA33,
     &                        ALPH11,ALPH22,ALPH33,
     &                        C1111,C2222,C3333,C1122,C1133,C2233)
               end if
               read(INFIL,*)   THCKNS
     
          else if ((MATCODE .eq. 'CUB') .or. (MATCODE .eq. 'cub')) then
               MATCODE = 'cub'
               read(INFIL,*)   C1111
               read(INFIL,*)   C1122
               read(INFIL,*)   C2323
               C2222 = C1111
               C3333 = C1111
               C1133 = C1122
               C2233 = C1122
               C1212 = C2323
               C1313 = C2323
               read(INFIL,*) BETLOG
               if (BETLOG) then 
                  read(INFIL,*) BETA11
                  BETA22 = BETA11
                  BETA33 = BETA11
                 else
                  read(INFIL,*) ALPH11
                  ALPH22 = ALPH11
                  ALPH33 = ALPH11
                  call ALTOBT(BETA11,BETA22,BETA33,
     &                        ALPH11,ALPH22,ALPH33,
     &                        C1111,C2222,C3333,C1122,C1133,C2233)
               end if
               read(INFIL,*)   THCKNS
            
          else if ((MATCODE .eq. 'TIS') .or. (MATCODE .eq. 'tis')) then
               MATCODE = 'tis'
               read(INFIL,*)   C1111,C3333
               read(INFIL,*)   C1122,C1133
               read(INFIL,*)   C2323
               C2222 = C1111
               C2233 = C1133
               C1313 = C2323
               C1212 = (C1111 - C1122)/2.
               read(INFIL,*) BETLOG
               if (BETLOG) then 
                  read(INFIL,*) BETA11,BETA33
                  BETA22 = BETA11
                 else
                  read(INFIL,*)   ALPH11,ALPH33
                  ALPH22 = ALPH11
                  call ALTOBT(BETA11,BETA22,BETA33,
     &                        ALPH11,ALPH22,ALPH33,
     &                        C1111,C2222,C3333,C1122,C1133,C2233)
               end if
               read(INFIL,*)   THCKNS
               
           
           
comment: use reduced stiffness if plane stress is assumed

              if (.not. V3CODE) then
      
                 CI1111 = C1111
                 CI2222 = C2222
                 CI1122 = C1122
                 BITA11 = BETA11
                 BITA22 = BETA22
           
                 C1111  = C1111 - C1133*C1133/C3333
                 C2222  = C2222 - C2233*C2233/C3333
                 C1122  = C1122 - C1133*C2233/C3333
           
                 BETA11 = BETA11 - C1133*BETA33/C3333
                 BETA22 = BETA22 - C2233*BETA33/C3333
               end if

              else 
                call ERROR('INRED', 'Not valid MATCODE')
              end if
    
         THICK = THCKNS
         
        else 
           
            MATCODE = 'lam'
            call MATRED
            read(INFIL,*)   SYM
            read(INFIL,*)   NSTACK

            
            do 3 I=1,NSTACK
               read(INFIL,*) DUMMY,MATNO(I),NPLY(I),PTHK(I),THETA(I)
3           continue

            call LAMTM
            THICK = LTHKNS

      end if
      
      
      read (INFIL,*)      FCODE
      if (FCODE) then
         read(INFIL,*)    PZTOP,PZBOT
         read(INFIL,*)    N11,N22,M11,M22
      end if

      call THETIN(INFIL,THICK,TCODE,NTMPI,TNI,TXI,TII,TMI,TOI,
     &            TEMP0,TEMP1,TEMP2)
     
      call THETIN(INFIL,THICK,MOIST,NMSTI,MNI,MXI,MII,MMI,MOI,
     &            MOIST0,MOIST1,MOIST2)

      do 60 EDGENO=1,2
         read(INFIL,*)  BCTYPE(EDGENO)
         if (BCTYPE(EDGENO) .eq. 0) then
            do 70 DOF=1,MXNBC
               read(INFIL,*) DUMMY,BCCODE(EDGENO,DOF), BC(EDGENO,DOF)
70          continue
         end if
60    continue

         
      call BCSET(BC)


      read(INFIL,*)       BACK
      read(INFIL,*)       RSLT
      read(INFIL,*)       PLTSWC
      read(INFIL,'(a)')   OTFILE
      
      
           
      close(INFIL)
      
      return
      end
      
      
      subroutine INWRT(UNITNO)
      
c     Name:      INput WRiTe
c     Purpose:   To write the input data that currently exists to the unit
c                (file, screen) UNITNO.
c     Input:     UNITNO
c                Input data from the commons
c                
c     Output:    The input data to the screen.
c     Called by: MODIFY
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
      
      
      

      real*8            E,NU,ALPHA,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323,
     &                  ALPH11,ALPH22,ALPH33,
     &                  BETA11,BETA22,BETA33,THCKNS,
     &                  CI1111,CI2222,CI1122,BITA11,BITA22
      common /HOMMAT/   E,NU,ALPHA,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323,
     &                  ALPH11,ALPH22,ALPH33,
     &                  BETA11,BETA22,BETA33,THCKNS,
     &                  CI1111,CI2222,CI1122,BITA11,BITA22
      save   /HOMMAT/
      
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
      
      


      integer      I,MAT,UNITNO
      character*21 MATER


      
      write(UNITNO,1000) TITLE
1000  format(x,a/)

      call SLTWRT(UNITNO)
      
      call GEOWRT(UNITNO)

      
      if (UNITNO .eq. TERM) call WAIT

      write(UNITNO,1060) NUMEL
1060  format(//' Number of elements:',I5/)
     
      call XEDWRT(UNITNO)
      if (UNITNO .eq. TERM) call WAIT


      if (MATCODE .eq. 'iso') then
           MATER = 'isotropic            '
         else if (MATCODE .eq. 'rho') then
           MATER = 'rhombic              '
         else if (MATCODE .eq. 'cub') then
           MATER = 'cubic                '
         else if (MATCODE .eq. 'tis') then
           MATER = 'transversly isotropic'
         else if (MATCODE .eq. 'lam') then
           MATER = 'laminated            '
      end if

      write(UNITNO,1090) MATER
1090  format(///' The shell material is ',a/)

      if (HOMOGN) then
         if (MATCODE .eq. 'iso') then
            write(UNITNO,1100) E,NU,ALPHA
1100        format(' E= ',e12.5,2x,'NU = ',e12.5,2x,'ALPHA = ',e12.5,/)
         end if
      
         if (.not. V3CODE) then
             write(UNITNO,1120) CI1111,CI2222,C3333,CI1122,C1133,C2233,
     *                         C1212,C1313,C2323,ALPH11,ALPH22,ALPH33,
     *                         BITA11,BITA22,BETA33,
     *                         THCKNS
1120         format(/  ' C1111   = ',e12.5,3x,'C2222   = ',e12.5,3x,
     *                  'C3333   = ',e12.5,/
     *                 ' C1122   = ',e12.5,3x,'C1133   = ',e12.5,3x,
     *                  'C2233   = ',e12.5,/
     *                 ' C1212   = ',e12.5,3x,'C1313   = ',e12.5,3x,
     *                  'C2323   = ',e12.5,/
     *                 ' Alpha11 = ',e12.5,3x,'Alpha22 = ',e12.5,3x,
     *                  'Alpha33 = ',e12.5/
     *                 ' Beta11  = ',e12.5,3x,'Beta22  = ',e12.5,3x,
     *                  'Beta33  = ',e12.5,//
     *                 ' Thickness  =',e12.5,//)
     
             write(UNITNO,1130) C1111,C2222,C1122,C1212,BETA11,BETA22
1130         format(' The reduced stiffnesses and thermal stress ',
     *              'coefficients are:'//,
     *              ' C1111  = ',e12.5,3x,'C2222  = ',e12.5,3x,/
     *              ' C1122  = ',e12.5,3x,'C1212  = ',e12.5,/
     *              ' Beta11 = ',e12.5,3x,'Beta22 = ',e12.5//)
     
          else 
                           
             write(UNITNO,1120) C1111,C2222,C3333,C1122,C1133,C2233,
     *                          C1212,C1313,C2323,ALPH11,ALPH22,ALPH33,
     *                          BETA11,BETA22,BETA33,THCKNS
         end if
      
       else
       
         if (SYM) then
            write(UNITNO,1131)
1131        format(' The laminate is symmetric.')
           else
            write(UNITNO,1132)
1132        format(' The laminate is not symmetric.')
         end if
         
         call LAYWRT(UNITNO)
         
         write(UNITNO,1702) LTHKNS,LENUS
1702     format(' Laminate thickness = ',e12.5,x,a)
         if (UNITNO .eq. TERM) call WAIT
     
         write(UNITNO,1137)
1137     format(////,' Material Properties',//)


         do 3 I=1,NMAT
            if (MATLOG(I)) then
               MAT = I
               call WRTMAT(UNITNO,MAT)
               if (UNITNO .eq. TERM) call WAIT
             end if
3         continue

      end if
      
      if (FCODE) then
      
         write(UNITNO,1135) FCODE,PZTOP,PZBOT
1135     format(////' Prescribed Pressure Loading'//
     *          ' FCODE=',L2,/
     *          ' Pz (top) =',e12.5,5X,'Pz (bottom) =',e12.5,//)
     
         write(UNITNO,1701) M11,M22
1701     format(////' Prescribed Pressure Loading'//
     *          ' M11  =',e12.5,5X,'M22  =',e12.5,//)
      end if
     
     
      if (TCODE) then
         call TMPWRT(UNITNO)
         if (UNITNO .eq. TERM) call WAIT
      end if
     
      if (MOIST) then
         call MSTWRT(UNITNO)
         if (UNITNO .eq. TERM) call WAIT
      end if

      call BCWRT(UNITNO)
       
      call OTQWRT(UNITNO)
       

      return
      end
      
      
       subroutine LAMTM
      
      

c     Name:      LAMinate Thickness, Material
c     Purpose:   To calculate the laminate thickness and enter determine
c                whitch materials are in the laminate.
c     Common:    LAYUP, MATPRP
c     Input:     Data from these commons                
c     Output:    LTHKNS and MATLOG data.
c                MATLOG(i) is true if the i'th material in MATPRP common is
c                used in the laminate.
c     Called by:
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

      
      integer MAT,STK

      do 1 MAT=1,NMAT
         MATLOG(MAT) = .false.
1     continue

      LTHKNS = 0.
      
      do 2 STK=1,NSTACK
         STHKNS(STK) = PTHK(STK) * NPLY(STK)
         LTHKNS = LTHKNS + STHKNS(STK)
         MATLOG(MATNO(STK)) = .true.
2     continue

      if (SYM) LTHKNS =  LTHKNS * 2.
      THICK = LTHKNS
      
      return
      end
      subroutine LAYWRT(UNITNO)
      
c     Name:      LAYup WRiTe
c     Purpose:   To write the layup data  to the unit(file, screen) UNITNO.
c     Input:     UNITNO
c                Layup data from the LAYUP common
c     Output:    The layup data to the UNITNO unit
c     Called by: INWRT,LAYUPS
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
      
      
      
      integer      UNITNO,I
      
      write(UNITNO,100) LENUS
100   format(//' Laminate Layup'//
     &       ' Stack',3x,'Material No.',3x,
     &       'No. of Plies',3x,'Ply thickness',3x,'Orientation'/
     &       43x,'(',a,')',10x,'(degrees)')
      do 2 I=1,NSTACK
         write(UNITNO,110) I,MATNO(I),NPLY(I),PTHK(I),THETA(I)
2     continue
110   format(x,i3,6x,i3,13x,i3,9x,e12.5,4x,e12.5)
      if (SYM) then
         write(UNITNO,120)
120      format(x,20('-'),' midsurface (surface of symmetry) ',
     &          15('-'),//)
      end if

      return
      end
      
      
      subroutine MATRED
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      MATerial REaD                                                *
c     Purpose:   To read in the material properties from the file MAT.DAT     *  
c     Input:     Material Properties from the file MAT.DAT                    *
c     Output:    The material properties into MATPRP common                   *
c     Called by:                                                              *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************
 
      
      
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
      
          
      open(unit=1,file='MAT.DAT',status ='OLD')
      
      read(1,*) NMAT
      
      do 1 I=1,NMAT
         read(1,'(a)') MATNAM(I)
         read(1,*)   MATTYP(I)
         
comment: MATTYP = 1, isotropic; MATTYP = 2, transverse isotropic;
c        MATTYP = 3, rhombic

         
         if (MATTYP(I) .eq. 1) then
            read(1,*) E1(I)
            read(1,*) NU12(I)
            read(1,*) ALPHT1(I)
            read(1,*) ALPHM1(I)
            E2(I)     = E1(I)
            E3(I)     = E1(I)
            NU13(I)    = NU12(I)
            NU23(I)    = NU12(I)
            G12(I)    = E1(I)/(2.*(1. + NU12(I)))
            G13(I)    = G12(I)
            G23(I)    = G12(I)
            ALPHT2(I) = ALPHT1(I)
            ALPHT3(I) = ALPHT1(I)
            ALPHM2(I) = ALPHM1(I)
            ALPHM3(I) = ALPHM1(I)
          else if (MATTYP(I) .eq. 2) then
            read(1,*) E1(I),E2(I)
            read(1,*) NU12(I),NU23(I)
            read(1,*) G12(I)
            read(1,*) ALPHT1(I),ALPHT2(I)
            read(1,*) ALPHM1(I),ALPHM2(I)
            E3(I)     = E2(I)
            NU13(I)   = NU12(I)
            G13(I)    = G12(I)
            G23(I)    = E2(I)/(2.*(1. + NU23(I)))
            ALPHT3(I)  = ALPHT2(I)
            ALPHM3(I)  = ALPHM2(I)
          else if (MATTYP(I) .eq. 3) then
            read(1,*) E1(I),E2(I),E3(I)
            read(1,*) NU12(I),NU13(I),NU23(I)
            read(1,*) G12(I),G13(I),G23(I)
            read(1,*) ALPHT1(I),ALPHT2(I),ALPHT3(I)
            read(1,*) ALPHM1(I),ALPHM2(I),ALPHM3(I)
          else 
            call ERROR('MATRED     ','MATTYP must be 1,2, or 3')
         end if
            
1     continue

      close(1)

      return
      end
      
      
         
         
         subroutine MSTWRT(UNITNO)
      
      
c     Name:      MoiSTure WRiTe 
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
      
      
      
      integer     UNITNO,NODE
      
      real*8      MI,MM,MO

      write(UNITNO,100) X1,X1US
100   format(//' Element Edge',5x,'Position',6x,
     &       'Inner Surface',3x,'Mid Surface',3x,
     &       'Outer Surface'/
     &       20x,a,9x,'Moist. Change',3x,'Moist. Change',
     &       3x,'Moist. Change'/
     &       18x,'(',a,')')
      do 1 NODE=1,NUMEL+1
            MI = MOIST0(NODE) - (THICK/2.)*MOIST1(NODE)
     *                 + (THICK*THICK/4.)*MOIST2(NODE)
            MM = MOIST0(NODE)
            MO = MOIST0(NODE) + (THICK/2.)*MOIST1(NODE)
     *                 + (THICK*THICK/4.)*MOIST2(NODE)
          write(UNITNO,110) NODE,XEDGE(NODE),MI,MM,MO
110      format(5x,i2,8x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      
       subroutine OTQWRT(UNITNO)
      
c     Name:      OuTput Quantity WRiTe
c     Purpose:   To write the output quatities to solve for
c                unit (file, screen) UNITNO.
c     Input:     UNITNO
c                RSLT,PLTSWC from CONTRL common
c     Output:    The above data to the UNITNO unit
c     Called by: OUTS,INWRT
c     Calls    : 
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
      
      
      integer      UNITNO
      
      if (RSLT) then
         write(UNITNO,100)
100      format(//' Output Specification'//
     &           ' Currently, the program will solve for strain',
     &           ' measures and stress resultants.')
       else
         write (UNITNO,110)
110      format(//' Output Specification'//
     &            ' Currently, the program will not solve for',
     &            ' stress resultants.')
      end if

      if (PLTSWC) then
         write(UNITNO,120)
120      format(' It will create data files for plotting.')
       else
         write (UNITNO,130)
130      format(' It will not create data files for plotting.')
      end if
      
      write(UNITNO,140) OTFILE
140   format(' The finite element output file is called ',a,'.')
      
      if (UNITNO .eq. TERM) call WAIT
      
      return
      end
      
        
      subroutine SLTWRT(UNITNO)
      
c     Name:      SoLution Technique WRiTe
c     Purpose:   To write the solution technique information to the
c                unit (file, screen) UNITNO.
c     Input:     UNITNO
c                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,B3CODE,KSHORT,RIGID
c     Output:    The above data to the UNITNO unit
c     Called by: OUTS,INWRT
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
      
      
      

      
      integer      UNITNO
      
      write(UNITNO,100) THEORY,NVAR,NEPS,NBC,TORORD
100   format(///' Solution Technique'///' Shell Theory Information'//
     &   ' Theory no.                                         = 'i5,/
     &   ' No. of displacement measures                       = 'i5,/
     &   ' Number of stress resultants and strain measures    = 'i5,/
     &   ' Number of boundary conditions                      = 'i5,/
     &   ' Order of t/R kept in stiffness (N_C)               = 'i5/)
     
      if (.not. V3CODE) then
         write(UNITNO,110)
110      format(/' Plane stress is assumed (Sigma 33 = 0).')
        else
         write(UNITNO,120)
120      format(' Plane stress is not assumed.'//)
      end if
      
      if (UNITNO .eq. TERM) call WAIT
      
      write(UNITNO,130) OSHPU,OSHPW,OSHPD,NINT
130   format(//' Axisymmetric Finite Element Information'//
     &  ' Order of shape function for in-plane displacements = ',i5,/
     &  ' Order of shape function for w displacement         = ',i5,/
     &  ' Order of shape function for rotations, etc.        = ',i5,/
     &  ' Number of integration points per element           = ',i5,/)

      if (RIGID) then
         write(UNITNO,140)
140      format(/' Rigid body motion is included in the element',
     &           ' shape functions.')
       else
         write(UNITNO,150)
150      format(/' Rigid body motion is not included in the element',
     &           ' shape functions.')
      end if
      
      
      if (KSHORT) then
         write(UNITNO,160)
160      format(' The global stiffness matrix is calculated using a',
     &           ' quicker algorithm.')
       else
         write(UNITNO,170)
170      format(' The global stiffness matrix is calculated using a',
     &           ' slower algorithm.')
      end if

      return
      end
       subroutine THETIN(INFIL,THCKNS,DATAEX,NTMPI,TNI,TXI,TII,TMI,TOI,
     &                   THETA0,THETA1,THETA2)
      
c     Name:      THETa INput
c     Purpose:   To read in either the given nodal
c                temperatures (if this is the first call) or the given moisture
c                values (if this is the second call) and calculate
c                the rest.
c     Common:    ELMDAT1, ELMDAT2
c     Input:     THCKNS,NODE, and either 
c                TII,TMI,TOI from INPUT.DAT if this is the first call or
c                MII,MMI,MOI from INPUT.DAT if this is the second call.
c     Output:    TII,TMI,TOI,THETA0,THETA1,THETA2, DATA EXistence
c     Called by: INPUT
c     Calls    : ERROR
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
      
      
      

      
      logical DATAEX
      
      integer INFIL,NTMPI,TN,TNI(MAXELM+1)
      
      real*8  THCKNS,TI,TM,TO,
     &        TXI(MAXELM+1),TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),
     &        THETA0(MAXELM+1),THETA1(MAXELM+1),THETA2(MAXELM+1)

      
      read(INFIL,*)  DATAEX
      if (.not. DATAEX) return
      
      read(INFIL,*) TNI(1),TII(1),TMI(1),TOI(1)
      TXI(1) = XEDGE(1)
      
      if (TNI(1) .ne. 1) then
         write(ERRFIL,100)
100      format(' The first node must equal 1.')
         call WAIT
         stop
      end if
      
      NTMPI = 1
      
1     continue

         NTMPI = NTMPI+1
        
     	   read(INFIL,*) TN,TI,TM,TO
     	   if (TN .le. TNI(NTMPI-1)) then
     	        write(ERRFIL,150) TNI(NTMPI-1)
150           format(' The element edge number must be greater',
     &               ' than ',i2,'.')
              call WAIT
              stop
           else if (TN .gt. NUMEL+1) then
              write(ERRFIL,160) NUMEL+1
160           format(' The element edge number must be less',
     &               ' than or equal to ',i2,'.')
              call WAIT
              stop
         end if
            
         TNI(NTMPI) = TN
         TXI(NTMPI) = XEDGE(TN)
         TII(NTMPI) = TI
         TMI(NTMPI) = TM
         TOI(NTMPI) = TO
            
      if (TN .lt. NUMEL+1) go to 1
      
      call THTINT(THCKNS,TNI,TII,TMI,TOI,THETA0,THETA1,THETA2)
         
      return
      end         

           subroutine THRSET
      
      
c     Name:      THeory SET
c     Purpose:   To set certain theory data based on the theory number
c     Input:     Theory number, from CONTRL common
c     Output:    NVAR,V3CODE,NEPS,SHEARC,NBC to ELMDAT common
c     Commons:   CONTRL,ELMDAT
c     Called by: INRED,SLTCHS
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
      
      
      

      
      if (THEORY.eq. 1) then
         NVAR   = 5
         V3CODE = .false.
         NEPS   = 10
         SHEARC = .true.
         NBC    = 5
         W1CHI  = .false.
       else if (THEORY .eq. 2) then
         NVAR   = 6
         V3CODE = .false.
         SHEARC = .true.
         NEPS   = 10
         NBC    = 5
         W1CHI  = .false.
       else if (THEORY .eq. 3) then
         NVAR   = 6
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 11
         NBC    = 6
         W1CHI  = .false.
       else if (THEORY .eq. 4) then
         NVAR   = 6
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 13
         NBC    = 6
         W1CHI  = .true.
       else if (THEORY .eq. 5) then
         NVAR   = 7
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 14
         NBC    = 7
         W1CHI  = .true.
       else if (THEORY .eq. 6) then
         NVAR   = 9
         V3CODE = .true.
         SHEARC = .false.
         NEPS   = 26
         NBC    = 9
         W1CHI  = .true.
       else if (THEORY .eq. 7) then
         NVAR   = 11
         V3CODE = .true.
         SHEARC = .false.
         NEPS   = 26
         NBC    = 11        
         W1CHI  = .true.
       else if (THEORY .eq. 8) then
         NVAR   = 12
         V3CODE = .true.
         SHEARC = .false.
         NEPS   = 27
         NBC    = 12        
         W1CHI  = .true.
       else if (THEORY .eq. 9) then
c        same as 4 except W1CHI = .false.
         NVAR   = 6
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 13
         NBC    = 6
         W1CHI  = .false.
       else if (THEORY .eq. 10) then
c        same as 5 except W1CHI = .false.
         NVAR   = 7
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 14
         NBC    = 7
         W1CHI  = .false.
       else
         call ERROR('INPUT     ','THEORY must be integer between 1-10')
      end if
            
      return
      end
      
        subroutine THTINT(THCKNS,TNI,TII,TMI,TOI,
     &                    THETA0,THETA1,THETA2)
      
c     Name:      THeTa INTerpolate
c     Purpose:   To interpolate the 
c                temperatures (if this is the first call) or the given moisture
c                values (if this is the second call) and calculate
c                the rest.
c     Common:    ELMDAT1, ELMDAT2
c     Input:     THCKNS,TNI,TII,TMI,TOI
c     Output:    THETA0,THETA1,THETA2
c     Called by: INPUT
c     Calls    : ERROR

      
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
      
      
      

      
      
      integer NODE,I,J,LASTNODE,TNI(MAXELM+1)
      
      real*8  TII(MAXELM+1),TMI(MAXELM+1),TOI(MAXELM+1),THCKNS,
     &        THETA0(MAXELM+1),THETA1(MAXELM+1),THETA2(MAXELM+1),
     &        T0INCR,T1INCR,T2INCR,LASTT0,LASTT1,LASTT2,XDIFF,
     &        LASTX

      I            = 1
      
      NODE         = TNI(I)
      THETA0(NODE) = TMI(I)
      THETA1(NODE) = (TOI(I) - TII(I))/THCKNS
      THETA2(NODE) = (2./(THCKNS*THCKNS))*(TII(I) - 2.*TMI(I) + TOI(I))
         

1      continue 
         LASTNODE = NODE
         LASTT0   = THETA0(LASTNODE)
         LASTT1   = THETA1(LASTNODE)
         LASTT2   = THETA2(LASTNODE)
         LASTX    = XEDGE(LASTNODE)
                  
         I            = I+1
      
         NODE         = TNI(I)
         THETA0(NODE) = TMI(I)
         THETA1(NODE) = (TOI(I) - TII(I))/THCKNS
         THETA2(NODE) = (2./(THCKNS*THCKNS))
     &                   *(TII(I) - 2.*TMI(I) + TOI(I))
        
         XDIFF  = XEDGE(NODE) - LASTX
         T0INCR = (THETA0(NODE) - LASTT0) / XDIFF
         T1INCR = (THETA1(NODE) - LASTT1) / XDIFF
         T2INCR = (THETA2(NODE) - LASTT2) / XDIFF
         
         do 2, J=LASTNODE+1,NODE-1
            THETA0(J) = LASTT0 + T0INCR * (XEDGE(J) - LASTX)
            THETA1(J) = LASTT1 + T1INCR * (XEDGE(J) - LASTX)
            THETA2(J) = LASTT2 + T2INCR * (XEDGE(J) - LASTX)
 2      continue
         
        if (NODE .eq. NUMEL+1) return
         
      go to 1
               
      end
        
         subroutine TMPWRT(UNITNO)
      
      
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
      
      
      real*8           THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0(MAXELM+1), TEMP1(MAXELM+1),
     *                 TEMP2(MAXELM+1),
     *                 MOIST0(MAXELM+1),MOIST1(MAXELM+1),
     *                 MOIST2(MAXELM+1)
     
      common /LOAD/    THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0,TEMP1,TEMP2,
     *                 MOIST0,MOIST1,MOIST2
      save   /LOAD/
      
      
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
      
      
      
      integer     UNITNO,NODE
      
      real*8      TI,TM,TO
       
      write(UNITNO,100) X1,X1US,TEMPUS,TEMPUS,TEMPUS
100   format(//' Temperature Distribution'//
     &       ' Element Edge',5x,'Position',5x,
     &       'Inner Surface',4x,'Midsurface',4x,
     &       'Outer Surface'/
     &       20x,a,9x,'Temp. Change',4x'Temp. Change',
     &       4x,'Temp. Change'/
     &       18x,'(',a,')',7x,'(',a,')',9x,
     &       '(',a,')',9x,'(',a,')')
      do 1 NODE=1,NUMEL+1
            TI = TEMP0(NODE) - (THICK/2.)*TEMP1(NODE)
     *                + (THICK*THICK/4.)*TEMP2(NODE)
            TM = TEMP0(NODE)
            TO = TEMP0(NODE) + (THICK/2.)*TEMP1(NODE)
     *                + (THICK*THICK/4.)*TEMP2(NODE)
         write(UNITNO,110) NODE,XEDGE(NODE),TI,TM,TO
110      format(2x,i5,8x,e12.5,3(4x,e12.5))
1     continue

      return
      end
      
      
      subroutine UNTSET
      
c     Name:      UNiT SET
c     Purpose:   To set the phyical unit variables
c     Input:     UNIT, from UNIT common
c                
c     Output:    Variables of UNITS common
c     Called by: INRED
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
      
      

      if ((UNIT .eq. 's') .or. (UNIT .eq. 'S')) then
           LENUS  = 'm'
           LENUL  = 'meters'
           STRSUS = 'Pa'
           STRSUL = 'pascals'
           TEMPUS = 'C'
           TEMPUL = 'Celsius'
           ALPTUS = '/ C'
           ALPTUL = 'per Celsius'
           ALPMUS = '(m/m)/(kg/kg)'
           ALPMUL = '(m/m)/(kg/kg)'
           DISPUS(0) = 'm'
           DISPUS(1) = 'm/m'
           DISPUS(2) = '1/m'
           DISPUS(3) = '1/m^2'
           DISPUS(4) = '1/m^3'
           DISPUL(0) = 'meters'
           DISPUL(1) = 'm/m'
           DISPUL(2) = 'inverse meters'
           DISPUL(3) = 'inverse meters squared'
           RSLTUS(0) = 'N/m'
           RSLTUS(1) = 'N'
           RSLTUS(2) = 'N-m'
           RSLTUS(3) = 'N-m^2'
           RSLTUL(0) = 'Newtons per meter'
           RSLTUL(1) = 'Newtons'
           RSLTUL(2) = 'Newton-meters'
           RSLTUL(3) = 'Newton - square meters'
          
        else if ((UNIT .eq. 'e') .or. (UNIT .eq. 'E')) then
           UNIT   = 'e'
           LENUS  = 'in'
           LENUL  = 'inches'
           STRSUS = 'psi'
           STRSUL = 'psi'
           TEMPUS = 'deg F'
           TEMPUL = 'deg F'
           ALPTUS = '/ deg F'
           ALPTUL = 'per degree F'
           ALPMUS = '(in/in)/(lbm/lbm)'
           ALPMUL = '(in/in)/(lbm/lbm)'
           DISPUS(0) = 'in'
           DISPUS(1) = 'in/in'
           DISPUS(2) = '1/in'
           DISPUS(3) = '1/in^2'
           DISPUS(4) = '1/in^3'
           DISPUL(0) = 'inches'
           DISPUL(1) = 'in/in'
           DISPUL(2) = 'inverse inches'
           DISPUL(3) = 'inverse inches squared'
           RSLTUS(0) = 'lbf/in'
           RSLTUS(1) = 'lbf'
           RSLTUS(2) = 'lbf-in'
           RSLTUS(3) = 'lbf-in^2'
           RSLTUL(0) = 'pound forces per inch'
           RSLTUL(1) = 'pound forces '
           RSLTUL(2) = 'pound force - inches'
           RSLTUL(3) = 'pound force - square inches'
      end if
      
      return
      end
      
      
           subroutine WAIT
      
      
c     Name:      WAIT
c     Purpose:   To pause printing to the terminal until the user presses
c                return
c                                                                      |
c*******************************************************************************


      print 100
100   format(/15x,' Press "RETURN" to continue')
      read(*,*)
      return
      end
      
      
         subroutine WRTMAT(UNITNO,MAT)
      
      
c     Name:      WRiTe MATerial data
c     Purpose:   To write the material properties of the material MATNO
c                to the unit (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c                MAT,Material no. of the material to write.
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
      
      
      
      integer  UNITNO,MAT

      if (MATTYP(MAT) .eq. 1) then
           write(UNITNO,100) MAT,MATNAM(MAT),E1(MAT),STRSUS,NU12(MAT),
     &                       ALPHT1(MAT),ALPTUS,ALPHM1(MAT),ALPMUS
100       format(//' Material no. ',i3,5x,a,3x,'isotropic'//
     &             ' E = ',e12.5,x,a,5x,'Nu = ',e12.5/
     &             ' Alpha T = ',e12.5,x,a,x,
     &             ' Alpha M = ',e12.5,x,a)
     
        else if (MATTYP(MAT) .eq. 2) then
           write(UNITNO,110) MAT,MATNAM(MAT),E1(MAT),STRSUS,
     &                       E2(MAT),STRSUS,
     &                       NU12(MAT),NU23(MAT),G12(MAT),STRSUS,
     &                       ALPHT1(MAT),ALPTUS,ALPHM1(MAT),ALPMUS,
     &                       ALPHT2(MAT),ALPTUS,ALPHM2(MAT),ALPMUS
110        format(//' Material no. ',i3,5x,a,3x,
     &             'transverse isotropic'//
     &             ' E1    = ',e12.5,x,a,3x,' E2    = ',e12.5,x,a/
     &             ' Nu 12 = ',e12.5,7X,' Nu 23 = ',e12.5,/
     &             ' G 12  = ',e12.5,x,a/
     &             ' Alpha T 1 = ',e12.5,x,a,x,
     &             ' Alpha M 1 = ',e12.5,x,a,/
     &             ' Alpha T 2 = ',e12.5,x,a,x,
     &             ' Alpha M 2 = ',e12.5,x,a)
   
        else if (MATTYP(MAT) .eq. 3) then
           write(UNITNO,120) MAT,MATNAM(MAT),
     &                      E1(MAT),STRSUS,E2(MAT),STRSUS,
     &                      E3(MAT),STRSUS,
     &                      NU12(MAT),NU13(MAT),NU23(MAT),
     &                      G12(MAT),STRSUS,G13(MAT),STRSUS,
     &                      G23(MAT),STRSUS,
     &                      ALPHT1(MAT),ALPTUS,ALPHM1(MAT),ALPMUS,
     &                      ALPHT2(MAT),ALPTUS,ALPHM2(MAT),ALPMUS,
     &                      ALPHT3(MAT),ALPTUS,ALPHM3(MAT),ALPMUS
120       format(//' Material no. ',i3,5x,a,3x,
     &              'rhombic'//
     &              ' E1    = ',e12.5,x,a,3x,' E2    = ',e12.5,x,a,3x,
     &              ' E3    = ',e12.5,x,a/
     &              ' Nu 12 = ',e12.5,7x,' Nu 13 = ',e12.5,7x,
     &              ' Nu 23 = ',e12.5/
     &              ' G 12  = ',e12.5,x,a,3x,' G 13  = ',e12.5,x,a,3x,
     &              ' G 23  = ',e12.5,x,a/
     &              ' Alpha T 1 = ',e12.5,x,a,x,
     &              ' Alpha M 1 = ',e12.5,x,a/
     &              ' Alpha T 2 = ',e12.5,x,a,x
     &              ' Alpha M 2 = ',e12.5,x,a/
     &              ' Alpha T 3 = ',e12.5,x,a,x
     &              ' Alpha M 3 = ',e12.5,x,a)
       end if

       return
       end
       
         subroutine XEDWRT(UNITNO)
      
      
c     Name:      X EDge WRiTe
c     Purpose:   To write the positions of the element edges.
c     Input:     UNITNO, unit number to print to
c     Output:    Material property list to the unit no
c     Commons:   MATPRP
c     Called by: MATERS,XEDGES
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
      
      
      
      integer     NODE,NOENOD,UNITNO

      write(UNITNO,100) X1,X1US
100   format(//' Element Edge Positions'//
     &        ' Element Edge',5x,'Position'/21x,a/18x,'(',a,')')
      NOENOD = NUMEL + 1
      do 1 NODE=1,NOENOD
         write(UNITNO, 110) NODE,XEDGE(NODE)
1     continue
110   format(2x,i5,8x,e12.5)

      return
      end
     
     
          subroutine XEINTP
      
      
c     Name:      X Edge INTerPolate
c     Purpose:   To interpolate the positions of the edges of the finite elments
c     Input:     Prescribed element edge position data from XEDIN common
c     Output:    All interpolated element edge position to ELMDAT common
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
      
      
      

      integer        NXEDI,XNODEI(MAXELM+1)
      real*8         XEDGEI(MAXELM+1)
      common /XEDIN/ NXEDI,XNODEI,XEDGEI
      save   /XEDIN/

      
      integer      I,J,DIFF,NF,NL
      real*8       XEINCR

      XEDGE(1) = XEDGEI(1)
      
      do 10 I=2,NXEDI
         NF = XNODEI(I-1)
         NL = XNODEI(I)
         XEDGE(NL) = XEDGEI(I)
         DIFF   = NL - NF
         XEINCR = (XEDGEI(I) - XEDGEI(I-1)) / DIFF
         
         do 15 J=1,DIFF-1
            XEDGE(NF+J) = XEDGE(NF) + XEINCR*J
15       continue

10    continue

      return
      end
      
      
