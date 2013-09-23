      integer           NOSPT
      real*8            SOPT(MXSOPT), STRESR(MXSOPT,MXNEPS),
     *                  STRAIN(MXSOPT,MXNEPS)
      
      common /STRSES/   SOPT,STRESR,STRAIN,NOSPT
      save   /STRSES/

