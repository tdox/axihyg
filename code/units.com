      character*1    UNIT
      character*2    LENUS
      character*3    X1,STRSUS
      character*5    TEMPUS
      character*6    LENUL,DISPUS(0:4)
      character*7    X1US,X1UL,STRSUL,ALPTUS,TEMPUL
      character*8    RSLTUS(0:3)
      character*10   ALPTUL
      character*17   ALPMUL,ALPMUS
      character*22   DISPUL(0:3)
      character*27   RSLTUL(0:3)
      common /UNITS/ UNIT,LENUS,LENUL,STRSUS,STRSUL,X1,X1US,X1UL,
     &               ALPTUS,ALPTUL,ALPMUS,ALPMUL,TEMPUS,TEMPUL,
     &               DISPUS,DISPUL,RSLTUS,RSLTUL
      save   /UNITS/
      
      
