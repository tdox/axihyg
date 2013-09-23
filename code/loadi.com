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
      
      
