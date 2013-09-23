           subroutine BCSET(BC)
      
      
c     Name:      Boundary Condition SET
c     Purpose:   To set the boundary conditions from the input data
c     Input:     BCTYPE(2), I'th element is type of BC at edge I.
c                   if BCTYPE = 0 then BC is general
c                   if BCTYPE = 1 then BC is simply supported
c                   if BCTYPE = 2 then BC is clamped
c                   if BCTYPE = 3 then BC is free
c                   if BCTYPE = 4 then BC is symmetric BC
c                BCCODE(2,NVAR),BC(2,NVAR)
c                if BC(I,J)=1 then BC(I,J) is the prescribed displacement
c                of dof J at edge I
c                if BC(I,J)=2 then BC(I,J) is the prescribed traction
c                of dof J at edge I
c     Output:    UBC(2,MXNVAR),TBC(2,MXNVAR) to BC common
c     Commons:   BC
c     Called by: CREATE,MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************

 
 
      implicit    undefined(a-z)
       
      include     'n.par'
      include     'bc.com'
      include     'elmdat.com'
       
      integer     EDGN,DOF
       
      real*8       BC(2,MXNBC)
      
                    
      do 10 EDGN=1,2
       
          if (BCTYPE(EDGN) .ne. 0) then
          
             do 20 DOF=1,NBC
                BC(EDGN,DOF) = 0.
20           continue

             if (BCTYPE(EDGN) .eq. 1) then
c              simply supported
                BCCODE(EDGN,1)  = 1
                BCCODE(EDGN,2)  = 1
                BCCODE(EDGN,3)  = 1
                BCCODE(EDGN,4)  = 2
                BCCODE(EDGN,5)  = 2
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 2
                BCCODE(EDGN,9)  = 2
                BCCODE(EDGN,10) = 2
                BCCODE(EDGN,11) = 2
                BCCODE(EDGN,12) = 2
              else if (BCTYPE(EDGN) .eq. 2) then
c              clamped
                BCCODE(EDGN,1)  = 1
                BCCODE(EDGN,2)  = 1
                BCCODE(EDGN,3)  = 1
                BCCODE(EDGN,4)  = 1
                BCCODE(EDGN,5)  = 1
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 1
                BCCODE(EDGN,9)  = 1
                BCCODE(EDGN,10) = 1
                BCCODE(EDGN,11) = 1
                BCCODE(EDGN,12) = 2
              else if (BCTYPE(EDGN) .eq. 3) then
c              free
                BCCODE(EDGN,1)  = 2
                BCCODE(EDGN,2)  = 2
                BCCODE(EDGN,3)  = 2
                BCCODE(EDGN,4)  = 2
                BCCODE(EDGN,5)  = 2
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 2
                BCCODE(EDGN,9)  = 2
                BCCODE(EDGN,10) = 2
                BCCODE(EDGN,11) = 2
                BCCODE(EDGN,12) = 2
              else if (BCTYPE(EDGN) .eq. 4) then
c              symmetric
                BCCODE(EDGN,1)  = 1
                BCCODE(EDGN,2)  = 1
                BCCODE(EDGN,3)  = 2
                BCCODE(EDGN,4)  = 1
                BCCODE(EDGN,5)  = 1
                BCCODE(EDGN,6)  = 2
                BCCODE(EDGN,7)  = 2
                BCCODE(EDGN,8)  = 1
                BCCODE(EDGN,9)  = 1
                BCCODE(EDGN,10) = 1
                BCCODE(EDGN,11) = 1
                BCCODE(EDGN,12) = 2
            end if
         end if
 

         do 30 DOF = 1,MXNBC
            UBC(EDGN,DOF) = 0.
            TBC(EDGN,DOF) = 0.
30       continue
      
         do 40 DOF=1, NBC      
              if (BCCODE(EDGN,DOF) .eq. 1) then
                    UBC(EDGN,DOF) = BC(EDGN,DOF)
                    TBC(EDGN,DOF) = 0.
                 else if (BCCODE(EDGN,DOF) .eq. 2) then
                    UBC(EDGN,DOF) = 0.
                    TBC(EDGN,DOF) = BC(EDGN,DOF)
                 else 
                    call ERROR('BCSET     ',
     *                        'BCCODE must be either 1 or 2   ')
              end if
40       continue
                

10    continue

      return
      end
      





