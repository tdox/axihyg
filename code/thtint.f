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

      implicit    none
      
      include 'n.par'
      include 'elmdat.com'
      
      
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
        
