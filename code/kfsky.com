      real*8           KSKY(MAXLKS), F(MAXNEQ), D(MAXNEQ)
      common /KF/      KSKY,F,D
      save   /KF/
      
