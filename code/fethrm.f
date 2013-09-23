      subroutine FETHRM(FE,B,FTHRM,WTJR)
      
c     Name:      Force (Element) due to THerMal loads
c     Purpose:   To calculate the portion of the force vector due to prescribed
C                thermal loads loading.
c     Input:     B(MAXNEN,MXNEPS,MXNVAR),FTHRM(MXNEPS),WTJR
c     Output:    FE, portion of the element force vector due to prescribed
c                    thermal loads.
c     Called by: FORMKF
c     Calls    : 

      implicit       undefined(a-z)
     
      include        'n.par'
      include        'elmdat.com'
      
      integer        INODE,I,J
      real*8         FE(MAXNEN,MXNVAR),FTHRM(MXNEPS),
     *               B(MAXNEN,MXNEPS,MXNVAR),WTJR

100   format(//' in fethrm')
      
      do 1 INODE=1,NEN
        do 2 I=1,NDOFPN(INODE)
          do 3 J=1,NEPS
            FE(INODE,I) = FE(INODE,I) -
     *                    B(INODE,J,I)*FTHRM(J)*WTJR
3         continue
2       continue
1     continue
      
      return
      end
      

