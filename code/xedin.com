      integer        NXEDI,XNODEI(MAXELM+1)
      real*8         XEDGEI(MAXELM+1)
      common /XEDIN/ NXEDI,XNODEI,XEDGEI
      save   /XEDIN/

