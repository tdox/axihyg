      subroutine FEMECH(FE,N,FMECH,WTJR)
      
c     Name:      Force (Element) array due to MECHanical loads
c     Purpose:   To calculate the portion of the element force vector
c                due to prescribed mechanical loading.  This subroutine
c                multiplies N*FMECH*WTJR
c     Input:                 
c     Output:    FE, portion of the element force vector due to prescribed
c                    mechanical loads.
c     Called by: FORMKF
c     Calls    : 

      implicit      none
      
      include       'n.par'
      include       'elmdat.com'
      
      integer       INODE,I,IDOF
      real*8        N(MAXNEN,MXNVAR),FE(MAXNEN,MXNVAR),WTJR,
     *              FMECH(MXNVAR)
           
100   format(' in femech')
          
      do 1 INODE=1,NEN
        do 2 I=1,NDOFPN(INODE)
          IDOF = FDOF(INODE,I)
          FE(INODE,I) = FE(INODE,I) + N(INODE,I)*FMECH(IDOF)*WTJR
2       continue
1     continue
      
      return
      end
      
