      subroutine KFASSM(EL,KE,FE)
      
c     Name:      K and F ASSeMble
c     Purpose:   To assemble the element K matrices into the KSKYline array, and
c                FE into the global F matrix.
c     Common:    ELMDAT, KF
c     Input:     EL, the ELement number;  KE, the element stiffness matrix.
c                FE, the element load vector.
c     Output:    The KSKY array with the element matrix added to it.
c                F, the load vector with FE added to it.
c     Called by: KFCYLN  
c     Calls    : 

      implicit     undefined(a-z)
      
      include      'n.par'
      include      'elmdat.com'
      include      'kfsky.com'
      
      integer  INODE,JNODE,I,J,IDOF,JDOF,IGEQNO,JGEQNO,POINT,EL
      real*8   KE(MAXNEN,MAXNEN,MXNVAR,MXNVAR), FE(MAXNEN,MXNVAR)
      
      
      do 1 INODE=1,NEN
        do 2 I=1,NDOFPN(INODE)
          IDOF = FDOF(INODE,I)
          IGEQNO = ID(IDOF,IEN(INODE,EL))
          if (IGEQNO .ne. 0) then
            F(IGEQNO) = F(IGEQNO) + FE(INODE,I)
            do 3 JNODE =INODE,NEN
              do 4 J=1,NDOFPN(JNODE)
                JDOF = FDOF(JNODE,J)
                JGEQNO = ID(JDOF,IEN(JNODE,EL))
                if ((JGEQNO .ne. 0) .and. (JGEQNO .ge. IGEQNO)) then
                  POINT  = JDIAG(JGEQNO) - (JGEQNO - IGEQNO)
                  KSKY(POINT) = KSKY(POINT) + KE(INODE,JNODE,I,J)
                end if
4             continue
3           continue
          end if
2       continue
1     continue


      return
      end
      
      
