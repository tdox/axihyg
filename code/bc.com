      integer     BCTYPE(2),BCCODE(2,MXNBC)
      real*8      UBC(2,MXNVAR),TBC(2,MXNVAR)
      common /BC/ BCTYPE,BCCODE,UBC,TBC
      save /BC/
      
      
