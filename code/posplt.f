c- POSPLT -********************************************************************
c                                                                             *
c                                                                             *
      subroutine POSPLT                                                       *
c                                                                             *
c     Name:      POSition PLot                                                *
c     Purpose:   To create data files for plotting the position of the nodes  * 
c                before and after the deformation.                            *
c     Input:                                                                  *
c     Output:                                                                 *
c     Called by:                                                              *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                             *
c******************************************************************************

      implicit     none

      include      'io.par'
      include      'n.par'
      include      'elmdat.com'
      include      'out.com'
      
      character*1  TAB
      integer      NODE
      
      real*8       GPOSUD(MAXNOD,2),GPOSDF(MAXNOD,2),
     *             BIGNUM,MAXDPM,MINR,MAXR,MINZ,MAXZ,
     *             S,KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,CPHI,SPHI,UPHI,W,
     *             SPAN,SCLFAC
     
      open (unit= POSFIL, file= 'POSOUT.DAT', status= 'new')

      TAB = char(9)
      
      BIGNUM = 9999999.
      MAXDPM = 0.
      MINR   = BIGNUM
      MAXR   = 0.
      MINZ   = BIGNUM
      MAXZ   = -BIGNUM
      
      do 1 NODE=1,NUMNOD
      
         S = XNODE(NODE)
         call DIMENS( KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,S,.false.)
         
Comment:  calculate the Global POSitions of the UnDeformed nodes and store
c         maximum and minimum values of R and Z
         GPOSUD(NODE,1) = R
         GPOSUD(NODE,2) = Z
         MINR = dmin1(MINR,R)
         MAXR = dmax1(MAXR,R)
         MINZ = dmin1(MINZ,Z)
         MAXZ = dmax1(MAXZ,Z)
         
Comment:  temporarily store the R,Z displacements in GPOSDF.
c         (Global POSitions of the DeFormed nodes)
         CPHI = cos(PHI)
         SPHI = sin(PHI)
         UPHI = DISP(NODE,1)
         W    = DISP(NODE,3)
         GPOSDF(NODE,1) = UPHI*CPHI + W*SPHI
         GPOSDF(NODE,2) = UPHI*SPHI - W*CPHI
         
Comment:  store the square of the maximum of the magnitude of the 
c         displacement vector ( Uphi**2 + W**2) in MAXDPM
         MAXDPM = dmax1(MAXDPM,UPHI*UPHI + W*W)
         
1     continue

Comment:  calculate the displacement plotting scaling factor and store the
c         scaled deformed nodal positions in GPOSDF.
          
      SPAN   = sqrt((MAXR-MINR)*(MAXR-MINR) + (MAXZ-MINZ)*(MAXZ-MINZ))
      
      if (MAXDPM .eq. 0.) then
         write(POSFIL,90)
90       format(' U1 and W equal 0.')
         return
      end if
         
      MAXDPM = sqrt(MAXDPM)
      
      SCLFAC = SPAN * 0.1 / MAXDPM
      
      do 2 NODE=1,NUMNOD
         GPOSDF(NODE,1) = GPOSUD(NODE,1) + SCLFAC*GPOSDF(NODE,1)
         GPOSDF(NODE,2) = GPOSUD(NODE,2) + SCLFAC*GPOSDF(NODE,2)
         
         write(POSFIL,100) NODE,TAB,GPOSUD(NODE,1),TAB,GPOSUD(NODE,2),
     *                     TAB,GPOSDF(NODE,1),TAB,GPOSDF(NODE,2)
2     continue
100   format(x,i5,a1,4(x,e13.5,a1))

      return
      end
      
      
