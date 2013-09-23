      subroutine ALTOBT(BETA11,BETA22,BETA33,
     &                  ALPH11,ALPH22,ALPH33,
     &                  C1111,C2222,C3333,C1122,C1133,C2233)
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      ALpha TO Beta                                                *
c     Purpose:   To calculate BETA (the expansion stress coefficients) from   *
c                the given ALPHA's (the expansion strain coefficients) and    *
c                stiffness coefficients, C                                    *
c     Input:     ALPHA's and C's                                              *
c     Output:    BETA'S                                                       *
c     Called by: LAMCOF                                                       *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************
 
  
      implicit   none
      
      real*8     BETA11,BETA22,BETA33,ALPH11,ALPH22,ALPH33,
     &           C1111,C2222,C3333,C1122,C1133,C2233
     
     
      BETA11 = - (C1111*ALPH11 + C1122*ALPH22 + C1133*ALPH33)
      BETA22 = - (C1122*ALPH11 + C2222*ALPH22 + C2233*ALPH33)
      BETA33 = - (C1133*ALPH11 + C2233*ALPH22 + C3333*ALPH33)
 
      return
      end
      
      
      
      
      
      
      
