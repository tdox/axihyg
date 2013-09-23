      subroutine FEEDGT(EL,FE)
      
c     Name:      Force (Element) due to prescribed EDGe Tractions
c     Purpose:   To calculate the portion of the element force vector
c                due to prescribed edge tractions.  
c     Common:    BC,ELMDAT
c     Input:     EL, element no.; 
c                FE, element force vector.
c                TBCTOP,TBCBOT; the prescribed tractions at the top and bottom,
c                   respectively. (From BC common)
c     Output:    FE, portion of the element force vector due to prescribed
c                   tractions.
c     Called by: KFCYLN
c     Calls    :
      implicit     none
      
      include      'n.par'
      include      'bc.com'
      include      'elmdat.com'
      
      integer      EL,INODE,I,EDGE
      real*8       FE(MAXNEN,MXNVAR),
     *             S,KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI
      
100   format(//' in feedgt')
      
      if (EL .eq. 1) then
         
         INODE = 1
          S    = XEDGE(1)
          EDGE = 1
      
        else if (EL .eq. NUMEL) then
          
          INODE = NEN
          S     = XEDGE(NUMEL+1)
          EDGE  = 2
      
        else
          call ERROR('FEEDGT    ',' EL not equal to 1 or NUMEL   ')
          
      end if
      
      call DIMENS(KAPA1,KAPA2,OA1,OA2,A2D1,R,Z,PHI,S,.false.)
      
      do 3 I=1,NDOFPN(INODE)
         FE(INODE,I) = FE(INODE,I) + TBC(EDGE,I)*R
3     continue
     
      return
      end
      
      
