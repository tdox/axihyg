      logical         MATLOG(MAXMAT)
      
      character*30    MATNAM(MAXMAT)
      
      integer         NMAT,MATTYP(MAXMAT)
      
      real*8          E1(MAXMAT),E2(MAXMAT),E3(MAXMAT),
     &                NU12(MAXMAT),NU13(MAXMAT),NU23(MAXMAT),
     &                G12(MAXMAT),G13(MAXMAT),G23(MAXMAT),
     &                ALPHT1(MAXMAT),ALPHT2(MAXMAT),ALPHT3(MAXMAT),
     &                ALPHM1(MAXMAT),ALPHM2(MAXMAT),ALPHM3(MAXMAT)
 
      common /MATPRP/
     &                E1,E2,E3,
     &                NU12,NU13,NU23,
     &                G12,G13,G23,
     &                ALPHT1,ALPHT2,ALPHT3,
     &                ALPHM1,ALPHM2,ALPHM3,
     &                NMAT,MATTYP,
     &                MATLOG, MATNAM

 
      save  /MATPRP/

