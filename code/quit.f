      subroutine QUIT
      
c     Name:      QUIT
c     Purpose:   To quit the  user-friendly interface.
c     Input:     
c                
c     Output:    
c     Called by: MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      
      include 'save.com'
      
      logical YES

      if (.not. SAVED) then
         print 100
100      format(/' Quitting does not save',
     &          ' the changes made to the input',
     &          ' data.'/'  Do you want to save the changes?')
         call YESNO(YES)
         if (YES) then
            call WRITE
            stop
          else
            stop
         end if
      end if
      
      stop
      end
      
      
