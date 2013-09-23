c- DSPPLT -********************************************************************
c                                                                             *
c                                                                             *
      subroutine DSPPLT                                                       *
c                                                                             *
c     Name:       DiSPlacement PLoT                                           *
c     Purpose:   To create data files for plotting displacemts.               *  
c     Input:     DISP from OUT common                                         *
c     Output:    The displacements to the data files UOUT,WOUT,DOUT           *
c     Called by: PLTOUT                                                       *
c     Calls    :                                                              *
c     Common:    CONTRL,ELMDAT,OUT                                            *
c                                                                             *
c******************************************************************************

      implicit     undefined(a-z)

      include      'io.par'
      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'out.com'
      
      character*1  TAB
      integer      UINCR,WINCR,DINCR,NODE,I,J
      
      open (unit= UFIL, file= 'UOUT.DAT', status= 'new')
      open (unit= WFIL, file= 'WOUT.DAT', status= 'new')
      open (unit= BFIL, file= 'BOUT.DAT', status= 'new')
      
      TAB = char(9)
      
      UINCR = NEN - OSHPU
      WINCR = NEN - OSHPW
      DINCR = NEN - OSHPD
      
      NODE = 1
      do 1 I=1,NUMNOD
         if (NODE .gt. NUMNOD) go to 1
         write(UFIL,100) NODE,TAB,XNODE(NODE),TAB,DISP(NODE,1),
     *                   TAB,DISP(NODE,2),TAB,DISP(NODE,3),
     *                   TAB,DISP(NODE,6),TAB,DISP(NODE,7),
     *                   TAB,DISP(NODE,12)
         NODE = NODE + UINCR
1     continue

100   format(x,i5,a1,10(x,e12.5,a1))

      NODE = 1
      do 2 I=1,NUMNOD
         if (NODE .gt. NUMNOD) go to 2
         write(WFIL,100) NODE,TAB,XNODE(NODE),TAB,DISP(NODE,3)
         NODE = NODE + WINCR
2     continue

      NODE = 1
      do 3 I=1,NUMNOD
         if (NODE .gt. NUMNOD) go to 3
         write(BFIL,100) NODE,TAB,XNODE(NODE), TAB,DISP(NODE,4),
     *                        TAB,DISP(NODE,5),TAB,DISP(NODE,8),
     *                        TAB,DISP(NODE,9),TAB,DISP(NODE,10),
     *                        TAB,DISP(NODE,11)
         NODE = NODE + DINCR
3     continue
      
      close(UFIL)
      close(WFIL)
      close(BFIL)
      
      return
      end
      
