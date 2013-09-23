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

      implicit    undefined(a-z)
      
      include 'n.par'
      include 'io.com'
      include 'elmdat.com'
      
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

