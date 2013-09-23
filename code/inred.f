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
      implicit     none
      
      include      'n.par'
      include      'bc.com'
      include      'contrl.com'
      include      'elmdat.com'
      include      'geom.com'
      include      'hommat.com'
      include      'io.com'
      include      'layup.com'
      include      'load.com'
      include      'loadi.com'
      include      'matcod.com'
      include      'matprp.com'
      include      'title.com'
      include      'units.com'
      include      'xedin.com'
      
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
      
      
