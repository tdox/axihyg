      real*8           K(MAXNEQ,MAXNEQ)
      common /KBIG/    K
      save   /KBIG/
 
 
