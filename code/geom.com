      character*4     SHAPE
      real*8          RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS
      common /GEOM/   RAD,R0,ALPHAD,RC,RPHI,PHI0D,RAT0,OFFSET,
     &                CYLEN,CONEHT,PHI1,PHI2,XF,XS,SHAPE
      save   /GEOM/
      
