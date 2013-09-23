      subroutine INTDAT
      
c     Name:      INTegration DATa
c     Purpose:   To enter the Gaussian integration points and corrosponding
c                weights into INT common.
c     Common:    INT
c     Input:     none
c     Output:    XIINT(MAXINT,MAXINT), XI INTegration point.  The I,J'th element
c                   is the J'th local integration coordinate for NINT=I.
c                WEIGHTS(MAXINT,MAXINT), Integration WEIGHTS.  The I,J'th element
c                   is the J'th Gaussian weighting factor for NINT=I.
c     Called by:
c     Calls    : 

      implicit     none
      
      include      'n.par'
      include      'int.com'
      
      XIINT(1,1) = 0.            
      WEIGHTS(1,1) = 2.
c      
      
      XIINT(2,1) = -1./sqrt(3.)  
      WEIGHTS(2,1) = 1.
      
      XIINT(2,2) = -XIINT(2,1)  
      WEIGHTS(2,2) = 1.
c      
      
      XIINT(3,1) = -sqrt(3./5.)  
      WEIGHTS(3,1) = 5./9.
      
      XIINT(3,2) = 0.            
      WEIGHTS(3,2) = 8./9.
      
      XIINT(3,3) = -XIINT(3,1)   
      WEIGHTS(3,3) = WEIGHTS(3,1)
      
      return
      end
      
      
      
      
      
