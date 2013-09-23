      real*8   XIINT(MAXINT,MAXINT), WEIGHTS(MAXINT,MAXINT)
      common /INT/ XIINT,WEIGHTS
      save   /INT/

