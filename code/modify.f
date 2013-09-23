      subroutine MODIFY
      
c     Name:      MODIFY
c     Purpose:   To modiy the INPUT.DAT file for the AXI program via a user
c                friendly interface.
c     Input:     
c                
c     Output:    The data files INPUT.DAT and MAT.DAT
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      
      include      'io.com'
      include      'save.com'
      
      logical      EXIST
      character*1  ANS
      
5     print 130
130   format(//' Which part of the input data do you want to change?'/
     &       ' Enter:'/
     &       5x,'u: for physical units'/
     &       5x,'t: for title'/
     &       5x,'g: for shell geometry'/
     &       5x,'n: for nodal position'/
     &       5x,'m: for material properties'/
     &       5x,'l: for laminate layup'/
     &       5x,'h: for hygrothermal data (temperature',
     &              ' and moisture distributions)'/
     &       5x,'b: for boundary conditions'/
     &       5x,'o: for output specification'/
     &       5x,'s: for solution techniques'//
     &       2x,'or v: to view the input data'/
     &       5x,'w: to write input data to a named file and quit'/
     &       5x,'r: to read in an existing input data set'/
     &       5x,'c: to create a new input data set'/
     &       5x,'i: to view the introduction'/
     &       5x,'q: to quit the program'/)
      read(*,'(a)') ANS
      if ((ANS .eq. 'u') .or. (ANS .eq. 'U')) then
           print 140
140        format(/' Warning: Changing the physical units',
     &            ' may affect',
     &            ' other input data such as'/
     &            ' geometry, nodal coordinates,',
     &            '  material properties, hygrothermal data, and'/,
     &            ' boundary conditions.  Be sure to change these if',
     &            ' neccesary.')
           EXIST = .true.
           call UNITSS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 't') .or. (ANS .eq. 'T')) then
           EXIST = .true.
           call TITLES(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'g') .or. (ANS .eq. 'G')) then
           print 150
150        format(/' Warning: Changing the geometry may affect',
     &            ' other input data such as'/' nodal coordinates,',
     &            ' hygrothermal data, and',
     &            ' boundary conditions.  Be sure to'/
     &            ' change these if necessary.')
           EXIST = .true.
           call GEOMS(.true.)
           SAVED = .false.
        else if ((ANS .eq. 'n') .or. (ANS .eq. 'N')) then
           print 160
160        format(/' Warning: Changing the nodal coordinates may affect',
     &            ' other input data such as'/' hygrothermal data, and',
     &            ' boundary conditions.  Be sure to change these if'/
     &            ' necessary.')
           EXIST = .true.
           call NODES(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'm') .or. (ANS .eq. 'M')) then
           call MATERS
           SAVED = .false.
        else if ((ANS .eq. 'l') .or. (ANS .eq. 'L')) then
           EXIST = .true.
           call LAYUPS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'h') .or. (ANS .eq. 'H')) then
           EXIST = .true.
           call LOADS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'b') .or. (ANS .eq. 'B')) then
           EXIST = .true.
           call BCS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'o') .or. (ANS .eq. 'O')) then
           EXIST = .true.
           call OUTS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 's') .or. (ANS .eq. 'S')) then
           EXIST = .true.
           call SLTCHS(EXIST)
           SAVED = .false.
        else if ((ANS .eq. 'v') .or. (ANS .eq. 'V')) then
           call INWRT(TERM)
           SAVED = .false.
        else if ((ANS .eq. 'w') .or. (ANS .eq. 'W')) then
           call WRITE
        else if ((ANS .eq. 'r') .or. (ANS .eq. 'R')) then
           call LODATS
        else if ((ANS .eq. 'c') .or. (ANS .eq. 'C')) then
           call CREATE
        else if ((ANS .eq. 'i') .or. (ANS .eq. 'I')) then
           call INTRO
        else if ((ANS .eq. 'q') .or. (ANS .eq. 'Q')) then
           call QUIT
        else
           print 170
170        format(' Please enter either "u", "t", "g", "n", "m", "l",'/
     &            ' "b", "o", "s", "v", "w", "r", "c", "i" or "q".')
           go to 5
      
      end if
      go to 5
      
      end

