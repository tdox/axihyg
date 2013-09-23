      subroutine ESUB(EMAT,KAPA1,KAPA2)
      
c     Name:      E SUBroutine
c     Purpose:   To calculate the E (stiffness) matrix.  Actually this subroutine
c                just calls the appropriate subroutine depending on the type
c                of material.
c     Common:    MATCOD
c     Input:     KAPA1,KAPA2
c     Output:    EMAT
c     Called by: FORMKF
c     Calls    : 

      implicit     none
 
      include      'n.par'
      include      'matcod.com'
      
      real*8       KAPA1,KAPA2,EMAT(MXNEPS,MXNEPS)
      
      
      if (HOMOGN) then
           call EHOMO(EMAT,KAPA1,KAPA2)
          
         else 
           call ELAM(EMAT,KAPA1,KAPA2)
      end if
      
      return
      end
      
      
