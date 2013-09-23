c- SHPPLT -********************************************************************
c                                                                             *
c                                                                             *
      subroutine SHPPLT                                                       *
c                                                                             *
c     Name:      SHaP PLoT                                                    *
c     Purpose:   To create data files for plotting the shapes                 *  
c     Input:                                                                  *
c     Output:    Position, (R,Z) of some points along the generator of the    *
c                shell.                                                       *
c     Called by: PLTOUT                                                       *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                             *
c******************************************************************************

      implicit     none
      
      include      'io.par'
      include      'n.par'
      include      'elmdat.com'
      
      character*1  TAB
      integer      NPT,I
      real*8       SSTOP,SSBOT,SINCR,KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,S
      
      open (unit= SHPFIL, file= 'SHPOUT.DAT', status= 'new')
      
      TAB = char(9)

      NPT   = 100
      SSTOP = XEDGE(1)
      SSBOT = XEDGE(NUMEL+1)
      SINCR = (SSBOT - SSTOP) / (NPT - 1)
      
      S = SSTOP
      do 1 I=1,NPT
         call DIMENS( KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,S,.false.)
         write(SHPFIL,100) I,TAB,S,TAB,R,TAB,Z
100      format(x,i5,a1,3(x,e12.5,a1))
         S = S + SINCR
1     continue

       return
       end
       
       
         
