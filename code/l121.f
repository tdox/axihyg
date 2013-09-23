      subroutine L121(NVAR,NRBM,NENDUM,NDOFPN,FDOF)
      
      
c     Name:      Logical Degree Of Freedom map (OSHPU=1,OSHPW=2,OSHPD=1)
c     Purpose:   To calculate NEN (NENDUM is the dummy variable),LDOF,and
c                NDOFPN for the given element
c     Common:    None
c     Input:    
c     Output:    NEN: Number of nodes per element (integer)
c
c                FDOF(MAXNEN,MXNVAR): Free Degree Of Freedom array.  The element
c                  FDOF(I,J) is the number of the J'th degree of freedom
c                  of the I'th local node.

c     Called by: IDIEN
c     Calls    : 

      implicit     none
      
      include      'n.par'
      include      'contrl.com'
      
      integer      J,NDOFPN(MAXNEN),NENDUM,FDOF(MAXNEN,MXNVAR),
     *             NVAR,NRBM,IRBM
      
      NENDUM = 3
                                     
      do 1 J=1,NVAR
         FDOF(1,J) = J
         FDOF(3,J) = J
1     continue

      FDOF(2,1) = 3
      
      if (RIGID) then
         do 3 IRBM = 1,NRBM
            FDOF(2,1 + IRBM) = NVAR +IRBM
3        continue
      end if
      
      do 2 J=2+NRBM,NVAR+NRBM
         FDOF(2,J) = 0
2     continue
         
      NDOFPN(1) = NVAR
      NDOFPN(2) = 1 + NRBM
      NDOFPN(3) = NVAR
            
      return
      end
      
