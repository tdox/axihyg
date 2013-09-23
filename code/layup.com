      logical           SYM
      integer           NSTACK,MATNO(MAXSTK),NPLY(MAXSTK)
      real*8            LTHKNS,PTHK(MAXSTK),THETA(MAXSTK),STHKNS(MAXSTK)
     
      common  /LAYUP/   SYM,
     &                  NSTACK,MATNO,NPLY,
     &                  LTHKNS,PTHK,THETA,STHKNS
 
      save    /LAYUP/
      
      
