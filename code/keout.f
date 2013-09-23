      subroutine KEOUT(EL,INT,KE)
      
c     Purpose: To print KE

      implicit     undefined(a-z)
      
      include      'io.par'
      include      'n.par'
      include      'elmdat.com'
      
      integer      I,J,NI,NJ,EL,INT
      real*8       KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR)
      
      write(DOCFIL,101) EL,INT
101   format(//' Ke    EL:',I3,'  INT:',I3/)
      
        do 1 NI=1,NEN
          do 1 NJ=NI,NEN
            write(DOCFIL,102) NI,NJ
102         format(//' NI=',I5,'   NJ=',I5/)
            do 1 I=1,NDOFPN(NI)
                write(DOCFIL,100) (KE(NI,NJ,I,J), J=1,NDOFPN(NJ))
100             format(10(X,e11.4))
1       continue

      return
      end

      
