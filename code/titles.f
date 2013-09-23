      subroutine TITLES(EXIST)
      
      
c     Name:      TITLE Subroutine
c     Purpose:   To input the TITLE for the current input file
c     Input:     EXIST, if true, title exists already
c                TITLE, from user
c     Output:    The TITLE in the TITLE common
c     Commons:   TITLE
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      
      include      'title.com'
      
      logical   TRU,EXIST
      
      print 50
50    format(//' TITLE')

      if (EXIST) then
         print 100, TITLE
100      format(/' The current title is:',//,a,//
     &          ' Do you want to change it?')
         call YESNO(TRU)
         if ( .not. TRU) return
      end if 
      
      print 110
110   format(/' Enter the title of this case. It must be less than 80',
     &       ' characters.')
      read(*,'(a)') TITLE
      
      return
      end
      
      

       


