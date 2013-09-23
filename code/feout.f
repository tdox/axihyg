      subroutine FEOUT(EL,INT,FE)
      
C     Purpose: To print out the element load vector.

      include    'io.par'
      include    'n.par'
      include    'elmdat.com'
      
      integer    INODE,EL,INT,I
      real*8     FE(MAXNEN,MXNVAR)
      
      write(DOCFIL,100) EL,INT
100   format(//' FE for EL=',I4,' and INT=',I4)
      do 1 INODE=1,NEN
         write(DOCFIL,110) INODE
110      format(//' NI=',I5,/)
         do 2 I=1,NDOFPN(INODE)
            write(DOCFIL,120) FE(INODE,I)
120         format(x,e11.4)
2        continue
1     continue

      return
      end
      
