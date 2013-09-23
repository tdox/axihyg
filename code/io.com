      integer      TERM,ERRFIL
      common /IO/  TERM,ERRFIL
      save   /IO/
      
