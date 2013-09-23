      subroutine SOLVE
      
c     Name:      SOLVE
c     Purpose:   To solve the equation Kd=F, put the results into the DISP,
c                and interpolate to get the other displacements
c     Input:     KSKY(MAXLKS): The K matrix in skyline profile storage.
c                F(NEQ):       The Force vector.
c     Output:    D(NEQ):       The Displacement vector.
c     Called by:
c     Calls    : NODEIN,NODECALC,ERROR
c     Common:    KF,ELMDAT

      implicit     none

      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'kfsky.com'
      include      'out.com'
      
      logical      SKFAC
      
      integer      NODE,VAR,GEQNO,I,DOF,
     *             UNINCR,WNINCR,DNINCR,FNODE,LNODE,MNODE
     
      real*8       U1INCR,U2INCR,WINCR,DINCR(MXNVAR)
      
100   format(/' in solve')
      
      SKFAC = .true.
      
      call SKYLIN(KSKY,F,D,JDIAG,NUMEQ,MAXLKS,SKFAC,BACK)
      if (.not. BACK) return
      

comment: move stored bc to proper place

      do 1 VAR=1,NVAR
         DISP(NUMNOD,VAR) = DISP(2,VAR)
         DISP(2,VAR) = 0.
1     continue

comment: store displacements into the array DISP

      do 2 NODE=1,NUMNOD
         do 3 VAR=1,NVAR+NRBM
            GEQNO = ID(VAR,NODE)
            if (GEQNO .ne. 0) then
               DISP(NODE,VAR) = D(GEQNO)
            end if
3        continue
2     continue

comment: interpolate for the other displacements

      UNINCR = NEN - OSHPU
      WNINCR = NEN - OSHPW
      DNINCR = NEN - OSHPD

      if (UNINCR .gt. 1) then
         FNODE = 1
         LNODE = FNODE + UNINCR
         do 4 I=1,NUMNOD
            if (LNODE .gt. NUMNOD) go to 4
            U1INCR = (DISP(LNODE,1) - DISP(FNODE,1)) / UNINCR
            U2INCR = (DISP(LNODE,2) - DISP(FNODE,2)) / UNINCR
            do 5 MNODE=FNODE+1,LNODE-1
               DISP(MNODE,1) = DISP(MNODE-1,1) + U1INCR
               DISP(MNODE,2) = DISP(MNODE-1,2) + U2INCR
5           continue
            FNODE = LNODE
            LNODE = LNODE + UNINCR
4        continue
      end if

      if (WNINCR .gt. 1) then
         FNODE = 1
         LNODE = FNODE + UNINCR
         do 6 I=1,NUMNOD
            if (LNODE .gt. NUMNOD) go to 6
            WINCR = (DISP(LNODE,3) - DISP(FNODE,3)) / WNINCR
            do 7 MNODE=FNODE+1,LNODE-1
               DISP(MNODE,3) = DISP(MNODE-1,3) + WINCR
7           continue
            FNODE = LNODE
            LNODE = LNODE + WNINCR
6        continue
      end if
      
      if (DNINCR .gt. 1) then
         FNODE = 1
         LNODE = FNODE + DNINCR
         do 8 I=1,NUMNOD
            if (LNODE .gt. NUMNOD) go to 8
            do 10 DOF=4,NVAR
               DINCR(DOF) = (DISP(LNODE,DOF) - DISP(FNODE,DOF)) / DNINCR
10          continue
            do 9 MNODE=FNODE+1,LNODE-1
               do 11 DOF=4,NVAR
                  DISP(MNODE,DOF) = DISP(MNODE-1,DOF) + DINCR(DOF)
11             continue
9           continue
            FNODE = LNODE
            LNODE = LNODE + DNINCR
8        continue
      end if
      
            
      return
      end
      
      
