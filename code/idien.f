      subroutine IDIEN
      
c     Name:      ID and IEN
c     Purpose:   To calculate the ID, IEN, and JDIAG matrices.
c     Common:    BC,CONTRL,ELDAT1,ELDAT2,
c     Input:     BCCDT,BCCDB,OSHPU,OSHPW,OSHPD,NUMEL,XEDGE(position of edge nodes),
c                from the commons.
c     Output:    XNODE:  position of all nodes
c                ID(MXNVAR,MAXNOD)    : ID array.  The I,J'th element is the
c                    global equation number corrosponding to the I'th variable
c                    for the J'th global node
c                IEN(MAXNEN,MAXELM)  : IEN array.  The I,J'th element is the
c                    global node number of the I'th local node of the J'th
c                    element.
c                NEN : Number of nodes/element
c                NUMNOD: NUMber of NODes
c                JDIAG(MAXNEQ): Pointer array to determine the location in the
c                    skyline profile global K matrix of diagonal pivots.  The
c                    I'th element of JDIAG(I)  is the location in skyline K of 
c                    the diagonal element K(I,I).
c     Called by:
c     Calls    : L121,ERROR

      implicit     none

      include      'n.par'
      include      'elmdat.com'
      include      'bc.com'
      include      'contrl.com'
      include      'io.com'
      
      
      integer I,J,LOCNOD,DOF,EL,EQNUM,MINEQ,GEQNO,I2
    
      
comment: Find the number of nodes per element (NEN) and the FDOF array.
c        The element FDOF(I,J) is the number of the J'th degree of freedom
c        of the I'th local node.
         
      if (OSHPU .eq. 1) then
           if (OSHPW .eq. 2) then
                if (OSHPD .eq. 1) then
                     call L121(NVAR,NRBM,NEN,NDOFPN,FDOF)
                   else
                     call ERROR('IDIEN     ',
     *                          'Improper OHSP''s               ')
                end if
              else 
                call ERROR('IDIEN     ',
     *                     'Improper OHSP''s               ')
           end if           
        else
           call ERROR('IDIEN     ',
     *                'Improper OHSP''s               ')
      end if
      
comment: Determine the IEN array.
      
      do 1 J=1,NUMEL
           do 1 I=1,NEN
                IEN(I,J) = I + (J-1)*(NEN-1)
1     continue

      NUMNOD = NUMEL*(NEN-1) + 1
      
      if (NUMNOD .gt. MAXNOD) then
         call ERROR('IDIEN     ','NUMNOD>MAXNOD.Increase MAXNOD. ')
      end if


      
comment:  Determine the ID array in four steps.
c         1) First initialize ID.
c         2) Then, enter ones for spaces in the ID array that are true
c         degrees of freedom.  Information determining whether or not a space
c         in the ID array corrosponds to a true degree of freedom comes from
c         the FDOF array.
c         3) Next, enter zeros for the degrees of freedom that are prescribed
c         by the boundary conditions.
c         4) Then fill the remaining blanks with incrementing integers.

comment: step 1)

      do 2 I=1,MXNVAR
           do 3 J=1,NUMNOD
                ID(I,J) = 0
3          continue
2     continue


comment: step 2)

      do 4 LOCNOD=1,NEN
         do 5 I=1,NDOFPN(LOCNOD)
            do 6 EL=1,NUMEL
               ID(FDOF(LOCNOD,I),IEN(LOCNOD,EL)) = 1
6           continue
5        continue
4     continue
            
          
comment: step 3) 
          
      do 7 DOF=1,NBC
           if (BCCODE(1,DOF) .eq. 1) ID(DOF,1)      = 0
           if (BCCODE(2,DOF) .eq. 1) ID(DOF,NUMNOD) = 0
7     continue

     
comment: step 4)

      EQNUM = 1
      
      do 8 J=1,NUMNOD
           do 8 I=1,NVAR+NRBM
                if (ID(I,J) .eq. 1) then
                     ID(I,J) = EQNUM
                     EQNUM = EQNUM +1
                end if
8     continue

      NUMEQ = EQNUM-1
      
      if (NUMEQ .gt. MAXNEQ) then
         call ERROR('IDIEN     ','NUMEQ>MAXNEQ.Increase MAXNEQ.  ')                ')
      end if

      
comment: Interpolate to find the position of all nodes.

      do 17 I=1,NUMEL+1
         XNODE(2*I-1) = XEDGE(I)
17    continue

      do 18 I=1,NUMEL
          I2 = I*2
          XNODE(I2) = (XNODE(I2-1) + XNODE(I2+1)) * 0.5
18    continue
         
      
      
comment: Calculate the JDIAG array in two steps: 1) Find the row number of the
c        first non-zero element in each column of the global K matrix and
c        temporarily store this number in JDIAG.  This requires two steps: 1a)
c        For each element, find the minimum global equation number associated
c        with that element. 1b) Store this minimum in the appropriate element
c        of JDIAG if it is less than the value already there.  2)  Use this
c        information to put the proper values into JDIAG.

comment: initialize JDIAG

      do 10 I=1,NUMEQ
         JDIAG(I) = MAXNEQ
10    continue

comment: step 1a)

      do 11 EL=1,NUMEL
      
         MINEQ = MAXNEQ
         do 12 LOCNOD=1,NEN
            do 13 I=1,NDOFPN(LOCNOD)
               DOF = FDOF(LOCNOD,I)
               GEQNO = ID(DOF,IEN(LOCNOD,EL))
               if ((GEQNO .ne. 0) .and. (GEQNO .lt. MINEQ)) then
                  MINEQ = GEQNO
               end if
13          continue
12       continue

comment: step 1b)

         do 14 LOCNOD=1,NEN
            do 15 I=1,NDOFPN(LOCNOD)
               DOF = FDOF(LOCNOD,I)
               GEQNO = ID(DOF,IEN(LOCNOD,EL))
               if (GEQNO .ne. 0) JDIAG(GEQNO) = min0(MINEQ,JDIAG(GEQNO))
15          continue
14       continue

11     continue


comment: step 2)

      do 16 I=2,NUMEQ
         JDIAG(I) = JDIAG(I-1) + (I-JDIAG(I)) + 1
16    continue

      LNKSKY = JDIAG(NUMEQ)
      
      if (LNKSKY .gt. MAXLKS) then
         write (ERRFIL,100) LNKSKY
100      format(' LNKSKY = ',i8)         
         call ERROR('IDIEN     ','LNKSKY>MAXLKS.Increase MAXLKS  ')
      end if

   
      return
      end
   
