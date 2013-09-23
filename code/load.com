      real*8           THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0(MAXELM+1), TEMP1(MAXELM+1),
     *                 TEMP2(MAXELM+1),
     *                 MOIST0(MAXELM+1),MOIST1(MAXELM+1),
     *                 MOIST2(MAXELM+1)
     
      common /LOAD/    THICK,PZTOP,PZBOT,N11,N22,M11,M22,
     *                 TEMP0,TEMP1,TEMP2,
     *                 MOIST0,MOIST1,MOIST2
      save   /LOAD/
      
      
