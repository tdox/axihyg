       subroutine LODATS
      
c     Name:      LOad DATa Set
c     Purpose:   To load in the input data set.
c     Input:     Input data in file
c                
c     Output:    The input data to the proper common blocks
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      
      include      'save.com'
      
      logical      INEXT      
      character*1  ANS
      character*10 INFILE
      
      
 10   print 120
120   format(' Enter the name of the file which contains the',
     &       ' "input data set" you wish to'/' modify.')
      read(*,'(a)') INFILE
      inquire(file=INFILE,exist=INEXT)
      if (.not. INEXT) then
          print 130, INFILE
130       format(' The file "',a,'" does not exist. ',
     &           ' Do you want to use another file,'/
     &           ' create a new',
     &           ' input file, or quit?  Enter "a" for another file,',
     &           ' "c" for create,'/' or "q" for quit.')
15       read(*,'(a)') ANS
         if ((ANS .eq. 'c') .or. (ANS .eq. 'C')) then
             call CREATE
           else if ((ANS .eq. 'a') .or. (ANS .eq. 'A')) then 
             go to 10
           else if ((ANS .eq. 'q') .or. (ANS .eq. 'Q')) then 
             call QUIT
           else
             print 140
140          format(' Please enter either "c","a" or "q".')
             go to 15
         end if
      end if
      
      call INRED(INFILE)
      SAVED = .false.
      
      return
      end
      
      
