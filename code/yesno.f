      subroutine YESNO(TRU)
      
c     Name:      YESNO
c     Purpose:   To get a yes or answer from the user and set TRU accordingly
c     Input:     A yes or no answer from the user
c     Output:    TRU = .true. if yes and TRU = .false. if no
c     Commons:   None
c     Called by: 
c     Calls    : undefined(a-z)
c                                                                      |
c*******************************************************************************


      implicit     undefined(a-z)
      
      logical      TRU
      character*1  ANS
      
1     read(*,'(a)')    ANS
      
      if ((ANS .eq. 'y') .or. (ANS .eq. 'Y')) then 
           TRU = .true.
           return
        else if ((ANS .eq. 'n') .or. (ANS .eq. 'N')) then 
           TRU = .false.
           return 
        else
           print 100
100        format(' Please answer either "y" or "n".')
           go to 1
      end if
      
      end
      
      
