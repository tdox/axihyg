      subroutine FORMKF

c     Name:      FORM the K and F matrices.
c     Purpose:   To form the global K and F matrices.
c     Input:                     
c     Output: 
c     Common:       
c     Called by:
c     Calls    : 

      implicit     none

      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'geom.com'
      include      'int.com'
      
      integer      EL,INT
      
      real*8       LENGTH,JACOB,XI,X,X1,X2,WEIGHT,XPOS,KAPA1,KAPA2,
     *             OA1,OA2,A2D1,R,Z,PHI,
     *             WTJRA1,B(MAXNEN,MXNEPS,MXNVAR),
     *             N(MAXNEN,MXNVAR),
     *             EMAT(MXNEPS,MXNEPS),
     *             KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR),
     *             FMECH(MXNVAR),FTHRM(MXNEPS),
     *             FE(MAXNEN,MXNVAR)
      
      call CLRKF
      
      do 1 EL=1,NUMEL
      
         call CLRKE(KE)
         call CLRFE(FE)
         X2 = XEDGE(EL + 1)
         X1 = XEDGE(EL)
         LENGTH = X2 - X1
         JACOB =  LENGTH * 0.5
         
         
         do 2 INT=1,NINT

            XI = XIINT(NINT,INT)
            WEIGHT = WEIGHTS(NINT,INT)
            X = XPOS(EL,XI)
            call DIMENS(KAPA1,KAPA2,OA1,OA2,A2D1,
     *                  R,Z,PHI,X,.true.)
     
            WTJRA1 = WEIGHT*JACOB*R/OA1
            
            
            call BN(B,N,XI,X,X1,X2,KAPA1,KAPA2,OA1,OA2,A2D1,JACOB)
            call ESUB(EMAT,KAPA1,KAPA2)
        
            call KESUB(KE,B,EMAT,WTJRA1)

            
            if (FCODE) then
               call FMECHS(FMECH,X)            
               call FEMECH(FE,N,FMECH,WTJRA1)
            end if
            
            if (TCODE) then
               call FTHERM(FTHRM,KAPA1,KAPA2,EL,XI,.true.)
               call FETHRM(FE,B,FTHRM,WTJRA1)
            end if
            
            if (MOIST) then
               call FTHERM(FTHRM,KAPA1,KAPA2,EL,XI,.false.)
               call FETHRM(FE,B,FTHRM,WTJRA1)
            end if
            
            
2        continue

        if ((EL .eq. 1) .or. (EL .eq. NUMEL)) then
           call FEDISP(EL,KE,FE)
           call FEEDGT(EL,FE)
        end if     
   

        call KFASSM(EL,KE,FE)
        
        
1     continue           
      
      return 
      end
      
       
            
