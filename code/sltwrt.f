      subroutine SLTWRT(UNITNO)
      
c     Name:      SoLution Technique WRiTe
c     Purpose:   To write the solution technique information to the
c                unit (file, screen) UNITNO.
c     Input:     UNITNO
c                THEORY,TORORD,OSHPU,OSHPW,OSHPD,NINT,B3CODE,KSHORT,RIGID
c     Output:    The above data to the UNITNO unit
c     Called by: OUTS,INWRT
c     Calls    : 
c                                                                      |
c*******************************************************************************



      implicit     undefined(a-z)
     
      include      'n.par'
      include      'contrl.com'
      include      'io.com'
      include      'elmdat.com'
      
      integer      UNITNO
      
      write(UNITNO,100) THEORY,NVAR,NEPS,NBC,TORORD
100   format(///' Solution Technique'///' Shell Theory Information'//
     &   ' Theory no.                                         = 'i5,/
     &   ' No. of displacement measures                       = 'i5,/
     &   ' Number of stress resultants and strain measures    = 'i5,/
     &   ' Number of boundary conditions                      = 'i5,/
     &   ' Order of t/R kept in stiffness (N_C)               = 'i5/)
     
      if (.not. V3CODE) then
         write(UNITNO,110)
110      format(/' Plane stress is assumed (Sigma 33 = 0).')
        else
         write(UNITNO,120)
120      format(' Plane stress is not assumed.'//)
      end if
      
      if (UNITNO .eq. TERM) call WAIT
      
      write(UNITNO,130) OSHPU,OSHPW,OSHPD,NINT
130   format(//' Axisymmetric Finite Element Information'//
     &  ' Order of shape function for in-plane displacements = ',i5,/
     &  ' Order of shape function for w displacement         = ',i5,/
     &  ' Order of shape function for rotations, etc.        = ',i5,/
     &  ' Number of integration points per element           = ',i5,/)

      if (RIGID) then
         write(UNITNO,140)
140      format(/' Rigid body motion is included in the element',
     &           ' shape functions.')
       else
         write(UNITNO,150)
150      format(/' Rigid body motion is not included in the element',
     &           ' shape functions.')
      end if
      
      
      if (KSHORT) then
         write(UNITNO,160)
160      format(' The global stiffness matrix is calculated using a',
     &           ' quicker algorithm.')
       else
         write(UNITNO,170)
170      format(' The global stiffness matrix is calculated using a',
     &           ' slower algorithm.')
      end if

      return
      end
