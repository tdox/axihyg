      character*10    OTFILE
      logical         BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI
      integer         THEORY,TORORD,OSHPU,OSHPW,OSHPD, NINT,NINTW,NINTD
      common /CONTRL/ BACK,FCODE,TCODE,MOIST,PLTSWC,V3CODE,SHEARC,RSLT,
     *                KSHORT,FLAT,RIGID,W1CHI,
     *                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,NINTW,NINTD,
     *                OTFILE
      save   /CONTRL/
      
      
