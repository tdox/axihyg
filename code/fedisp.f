      subroutine FEDISP(EL,KE,FE)
      
c     Name:      Force (Element) due to prescribed DISPlacements
c     Purpose:   To calculate the portion of the element force vector
c                due to prescribed displacements.
c     Common:    
c     Input:     EL, element no.; KE, element stiffness matrix; 
c                FE, element force vector with zero valued elements.
c                UBCTOP, UBCBOT; the prescribed displacements at the top
c                    and bottom, respectively. (From BC common)            
c     Output:    FE, portion of the element force vector due to prescribed
c                    displacements
c     Called by:
c     Calls    : 

      implicit     none
      
      include      'n.par'
      include      'bc.com'
      include      'elmdat.com'
      
      integer      EL,INODE,JNODE,I,J,EDGE
      real*8       KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR), FE(MAXNEN,MXNVAR)
     
100   format(/' in fedisp')

      if (EL .eq. 1) then
          JNODE = 1
          EDGE  = 1
        else if (EL .eq. NUMEL) then
          JNODE=NEN
          EDGE  = 2
        else
          call ERROR('FEDISP    ',' EL not equal to 1 or NUMEL   ')          
      end if

      do 3 INODE=1,NEN
        do 4 I=1,NDOFPN(INODE)
          do 5 J=1,NDOFPN(JNODE)
            if (JNODE .ge. INODE) then
              FE(INODE,I) = FE(INODE,I)
     &                      - KE(INODE,JNODE,I,J)*UBC(EDGE,J)
             else
              FE(INODE,I) = FE(INODE,I)
     &                      - KE(JNODE,INODE,J,I)*UBC(EDGE,J)
             end if      
5         continue
4       continue
3     continue
            
      return
      end

