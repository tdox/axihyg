      subroutine KESUB(KE,B,EMAT,WTJR)
      
c     Name:      K (Element stiffness matrix) SUBroutine
c     Purpose:   To calculate Ke by multiplying Bt*EMAT*B * Weight*J*R
c     Common:    
c     Input:     B(NEN,MXNEPS,MXNVAR), B matrix
c                EMAT(MXNEPS,MXNEPS), stiffness matrix
c                WTJ, weight*j*R
c     Output:    KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR), K Element 
c     Called by: FORMKF
c     Calls    : 

      implicit     undefined(a-z)
      
      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      
      integer      NI,NJ,I,J,K,L
      real*8       KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR), WTJR,
     *             B(MAXNEN,MXNEPS,MXNVAR),EMAT(MXNEPS,MXNEPS)

      if (KSHORT) then
         call KESHRT(KE,B,EMAT,WTJR)
       else

         do 1 NI=1,NEN
           do 2 NJ=NI,NEN
             do 3 I=1,NDOFPN(NI)
               do 4 J=1,NDOFPN(NJ)
                 do 5 K=1,NEPS
                   do 6 L=1,NEPS
                     KE(NI,NJ,I,J) = KE(NI,NJ,I,J) + 
     *                            B(NI,K,I)*EMAT(K,L)*B(NJ,L,J)*WTJR
6                  continue
5                continue
4              continue
3            continue
2          continue
1        continue

      end if
      
      return
      end
      
