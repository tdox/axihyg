           subroutine THRSET
      
      
c     Name:      THeory SET
c     Purpose:   To set certain theory data based on the theory number
c     Input:     Theory number, from CONTRL common
c     Output:    NVAR,V3CODE,NEPS,SHEARC,NBC to ELMDAT common
c     Commons:   CONTRL,ELMDAT
c     Called by: INRED,SLTCHS
c     Calls : 
c                                                                      |
c*******************************************************************************

 
 
      implicit    undefined(a-z)
      
      include     'n.par'
      include     'contrl.com'
      include     'elmdat.com'
      
      if (THEORY.eq. 1) then
         NVAR   = 5
         V3CODE = .false.
         NEPS   = 10
         SHEARC = .true.
         NBC    = 5
         W1CHI  = .false.
       else if (THEORY .eq. 2) then
         NVAR   = 6
         V3CODE = .false.
         SHEARC = .true.
         NEPS   = 10
         NBC    = 5
         W1CHI  = .false.
       else if (THEORY .eq. 3) then
         NVAR   = 6
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 11
         NBC    = 6
         W1CHI  = .false.
       else if (THEORY .eq. 4) then
         NVAR   = 6
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 13
         NBC    = 6
         W1CHI  = .true.
       else if (THEORY .eq. 5) then
         NVAR   = 7
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 14
         NBC    = 7
         W1CHI  = .true.
       else if (THEORY .eq. 6) then
         NVAR   = 9
         V3CODE = .true.
         SHEARC = .false.
         NEPS   = 26
         NBC    = 9
         W1CHI  = .true.
       else if (THEORY .eq. 7) then
         NVAR   = 11
         V3CODE = .true.
         SHEARC = .false.
         NEPS   = 26
         NBC    = 11        
         W1CHI  = .true.
       else if (THEORY .eq. 8) then
         NVAR   = 12
         V3CODE = .true.
         SHEARC = .false.
         NEPS   = 27
         NBC    = 12        
         W1CHI  = .true.
       else if (THEORY .eq. 9) then
c        same as 4 except W1CHI = .false.
         NVAR   = 6
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 13
         NBC    = 6
         W1CHI  = .false.
       else if (THEORY .eq. 10) then
c        same as 5 except W1CHI = .false.
         NVAR   = 7
         V3CODE = .true.
         SHEARC = .true.
         NEPS   = 14
         NBC    = 7
         W1CHI  = .false.
       else
         call ERROR('INPUT     ','THEORY must be integer between 1-10')
      end if
            
      return
      end
      
