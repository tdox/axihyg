      subroutine ERROR(LOC,MESSAGE)

c     Name:      ERROR
c     Purpose:   To print an error message and stop execution.
c     Input:     LOCation, MESSAGE
c     Output:    An error message to OUTPUT.DAT
c     Called by: 

      implicit     undefined(a-z)
      
      include      'io.com'
      
      character*10 LOC
      character*30 MESSAGE
      
      write (ERRFIL,100) LOC,MESSAGE
100   format(' Fatal Error. Location: ',a,'Reason: ',a)
      if (ERRFIL .eq. TERM) call WAIT
      stop
      end
      
      
      
  
