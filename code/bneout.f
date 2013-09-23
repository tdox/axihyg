      subroutine BNEOUT(EL,INT,B,N,EMAT) 
      
c     Purpose: To print out the above variables.

      implicit    none
      
      include     'io.par'
      include     'n.par'
      include     'elmdat.com'
      
      integer     EL,INT,I,J,INODE
      real*8      B(MAXNEN,MXNEPS,MXNVAR),N(MAXNEN,MXNVAR),
     *            EMAT(MXNEPS,MXNEPS)
      
      
      write(4,100)
100   format(//' E matrix'/)

      call BIGOUT(EMAT,MXNEPS,NEPS,4)

      write(4,115) 
115   format(//' N'//)

      do 2 INODE=1,NEN
         write(DOCFIL,120) (N(INODE,I), I=1,NDOFPN(INODE))
120      format(x,8F10.5)
2     continue
      

      write(4,130) 
130   format(//' B matrix'/)
      
      do 3 INODE=1,NEN
         write(DOCFIL,140) INODE
140      format(/' INDOE=',I5)
         do 4 I=1,NEPS
            write(DOCFIL,150) (B(INODE,I,J), J=1,NDOFPN(INODE))
150         format(10(x,f8.5))
4        continue
3     continue

      return
      end
      
