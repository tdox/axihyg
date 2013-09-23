c- RESULT -********************************************************************
c                                                                             *
c                                                                             *
      subroutine RESULT                                                       *
c                                                                             *
c     Name:      RESULT                                                       *
c     Purpose:   To calculate the stress resultants at the integration points.*  
c     Input:     The displacements of the nodes                               *
c     Output:    The stresses at the integration points.                      *
c     Called by:                                                              *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                             *
c******************************************************************************

      implicit     none

      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'int.com'
      include      'out.com'
      include      'strses.com'
      
      integer      SPTNO,SPTN,SPTPEL,EL,I,J,K,GNODE,LNODE,DOF
      
      real*8       LENGTH,JACOB,XI,X,X1,X2,WEIGHT,XPOS,KAPA1,KAPA2,
     *             OA1,OA2,A2D1,R,Z,PHI,
     *             B(MAXNEN,MXNEPS,MXNVAR),N(MAXNEN,MXNVAR),
     *             EMAT(MXNEPS,MXNEPS),
     *             EPS(MXNEPS),FTHRM(MXNEPS),FMOIST(MXNEPS)
      
      call ZEROR2(STRESR,MXSOPT,MXNEPS)
      call ZEROR2(STRAIN,MXSOPT,MXNEPS)
      
      SPTPEL = min0(OSHPU,OSHPW,OSHPD)
      NOSPT  = NUMEL * SPTPEL

      SPTNO = 1
      
      do 1 EL=1,NUMEL
      
         X2 = XEDGE(EL + 1)
         X1 = XEDGE(EL)
         LENGTH = X2 - X1
         JACOB =  LENGTH * 0.5
                  
         do 2 SPTN=1,SPTPEL

            XI = XIINT(SPTPEL,SPTN)
            WEIGHT = WEIGHTS(SPTPEL,SPTN)
            X = XPOS(EL,XI)
            SOPT(SPTNO) = X
            call DIMENS(KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,X,.true.)
            
            call BN(B,N,XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB)
            call ESUB(EMAT,KAPA1,KAPA2)
            if (TCODE) call FTHERM(FTHRM,KAPA1,KAPA2,EL,XI,.true.)
            if (MOIST) call FTHERM(FMOIST,KAPA1,KAPA2,EL,XI,.false.)
                        
            do 3 I=1,NEPS
             do 7 J=1,NEPS
                EPS(J) = 0.
7            continue
             do 4 J=1,NEPS
              do 5 LNODE=1,NEN
               GNODE = IEN(LNODE,EL)
               do 6 K=1,NDOFPN(LNODE)
                  DOF = FDOF(LNODE,K)
                  EPS(J) = EPS(J) + B(LNODE,J,K)*DISP(GNODE,DOF)
6              continue
5             continue
             STRAIN(SPTNO,J) = EPS(J)
4            continue

             do 8 J=1,NEPS
                STRESR(SPTNO,I) = STRESR(SPTNO,I) + EMAT(I,J)*EPS(J)
c                STRESR(SPTNO,I) = STRESR(SPTNO,I)
c    *                               + EMAT(I,J)*STRAIN(SPTNO,J)
8            continue

             if (TCODE) then
                STRESR(SPTNO,I) = STRESR(SPTNO,I) + FTHRM(I)
             end if

             if (MOIST) then
                STRESR(SPTNO,I) = STRESR(SPTNO,I) + FMOIST(I)
             end if

3           continue
            
            
            SPTNO = SPTNO + 1
            
2        continue
1     continue           
      
      return 
      end
      
       
