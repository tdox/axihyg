      real*8          DISP(MAXNOD,MXNVAR)
      
      common /OUT/    DISP
      save   /OUT/


