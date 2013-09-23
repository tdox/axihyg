      subroutine FTHERM(F,KAPA1,KAPA2,EL,XI,THERM)
      
c     Name:      Force  vector due to THeRMal loads
c     Purpose:   To calculate the thermal load vector at point S.  Actually this
c                subroutine just calls the appropriate subroutine depending on
c                the type of material.
c     Input:     S, the meridional arclength coordinate.
c     Output:    FTHRM, the mechanical load vector.
c     Called by: 
c     Calls    : FTHMIS

      implicit     none
 
      include      'n.par'
      include      'elmdat.com'
      include      'load.com'
      include      'matcod.com'
      
      logical       THERM
      integer       I,EL
      real*8        F(MXNEPS),T0,T1,T2,
     *              KAPA1,KAPA2,XI
      
100   format(' in ftherm, THERM = ',l6)
      
      
      if (THERM) then
         T0=   (TEMP0(EL+1) + TEMP0(EL))/2.
     *        + (TEMP0(EL+1) - TEMP0(EL))*XI/2.
     
         T1=   (TEMP1(EL+1) + TEMP1(EL))/2.
     *       + (TEMP1(EL+1) - TEMP1(EL))*XI/2.
     
         T2=   (TEMP2(EL+1) + TEMP2(EL))/2.
     *       + (TEMP2(EL+1) - TEMP2(EL))*XI/2.
       else
         T0=   (MOIST0(EL+1) + MOIST0(EL))/2.
     *        + (MOIST0(EL+1) - MOIST0(EL))*XI/2.
     
         T1=   (MOIST1(EL+1) + MOIST1(EL))/2.
     *       + (MOIST1(EL+1) - MOIST1(EL))*XI/2.
     
         T2=   (MOIST2(EL+1) + MOIST2(EL))/2.
     *       + (MOIST2(EL+1) - MOIST2(EL))*XI/2.
      end if
        
      
      if (HOMOGN) then
           call FTHHOM(THERM,KAPA1,KAPA2,T0,T1,T2,F) 
         else 
           call FTHLAM(THERM,KAPA1,KAPA2,T0,T1,T2,F) 
      end if
      
110   format(/,' F',/,2(5e12.4/))
            
      return
      end

