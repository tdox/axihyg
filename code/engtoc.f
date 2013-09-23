      subroutine ENGTOC(E1,E2,E3,NU12,NU13,NU23,G12,G13,G23,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323)
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      ENGineering TO C                                             *
c     Purpose:   To calculate the stiffness coeficients given the engineering *
c                constants.                                                   *
c     Input:     The engineering constants: E1,E2,...,G23                     *
c     Output:    The stiffness coefficients, C                                *
c     Called by: LAMCOF                                                       *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************


      implicit    undefined(a-z)
      
      
      real*8      E1,E2,E3,NU12,NU13,NU23,G12,G13,G23, C1111,C2222,
     &            C3333,C1122,C1133,C2233,C1212,C1313,C2323,DELTA,
     &            N12N21,N13N31,N23N32,E1E2D,E1E3D,E2E3D
     
      N12N21 = NU12*NU12*E2/E1
      N13N31 = NU13*NU13*E3/E1
      N23N32 = NU23*NU23*E3/E2
      
      DELTA = (1. - N12N21 - N23N32
     $            - N13N31 - 2.*NU12*NU13*NU23*E3/E1) /
     &        (E1*E2*E3)
      
      E1E2D  = E1*E2*DELTA
      E1E3D  = E1*E3*DELTA
      E2E3D  = E2*E3*DELTA
     
                
                
      C1111 = (1. - N23N32) / E2E3D
      C2222 = (1. - N13N31) / E1E3D
      C3333 = (1. - N12N21) / E1E2D
      C1122 = (NU12 + NU13*NU23*E3/E2) / E1E3D
      C1133 = (NU13 + NU12*NU23)       / E1E2D
      C2233 = (NU23 + NU13*NU12*E2/E1) / E1E2D
      C1212 = G12
      C1313 = G13
      C2323 = G23
      
100   format(/' E1   = ',e12.5,3x,'E2   = ',e12.5,3x,'E2   = ',e12.5/
     &        ' Nu12 = ',e12.5,3x,'Nu13 = ',e12.5,3x,'Nu23 = ',e12.5/
     &        ' G12  = ',e12.5,3x,'G13  = ',e12.5,3x,'G23  = ',e12.5/
     &        ' C1111= ',e12.5,3x,'C2222= ',e12.5,3x,'C3333= ',e12.5/
     &        ' C1122= ',e12.5,3x,'C1133= ',e12.5,3x,'C2233= ',e12.5/
     &        ' C1212= ',e12.5,3x,'C1313= ',e12.5,3x,'C2323= ',e12.5/)
      return
      end
      
