      program USRFRD
      
      

c     Name:      USeR FRienDly
c     Purpose:   To create the INPUT.DAT file for the AXI program via a user
c                friendly interface.
c     Common:    NONE
c     Input:     Data from the file 'INPUT.DAT', and the user
c     Output:    The data files INPUT.DAT and MAT.DAT
c     Calls    : 
c                                                                      |
c*******************************************************************************


 
      implicit     undefined(a-z)
      
      include      'io.com'

      logical      YES
      character*1  ANS
      
      print 100
100   format(//,80('*'),//,36x,'AXIHYG',/
     &       34x,'Version 1.1'/
     &       31x,'15 September 1990',//
     &       ' A program to calculate the hygrothermal deformations of',
     &       ' axisymmetric,'/' laminated, composite shells due to',
     &       ' axisymmetric temperature and moisture'/,
     &       ' distributions.',//
     &       22x,' L. E. Doxsee, Jr. and G. S. Springer',//,19x,
     &       ' Department of Aeronautics and Astronautics',/,29x,
     &       ' Stanford University',/,31x,'Stanford CA 94305',//
     &       32x,'(415) 723-4135'//
     &       27x,'c 1987, 1988, 1989, 1990',//
     &       ' This program is a research tool in',
     &       ' the development stage and is supplied "as'/' is" for',
     &       ' the purpose of scientific collaboration.'//,x,
     &       79('*'))
     
      print 101
101   format(//' Do you want to read an introduction to this program?')
      call YESNO(YES)
      if (YES) call INTRO
      
      print 105
105   format(//' Enter the FORTRAN unit number which corresponds to',
     &         ' terminal output for the'/' particular',
     &         ' version of FORTRAN with which',
     &         ' this program was compiled.'/' (Usually 6)')
      read(*,*) TERM
      ERRFIL = TERM
      
      print 110
110   format(//' Do you wish to create a new "input data set"',
     &       ' or to modify an old one?'/' Please type "c" for',
     &       ' create or "m" for modify.')
     
5     read(*,'(a)') ANS
      
      if ((ANS .eq. 'c') .or. (ANS .eq. 'C')) then
           call CREATE
        else if ((ANS .eq. 'm') .or. (ANS .eq. 'M')) then 
           call LODATS
           call MODIFY
        else
           print 150
150        format(' Please enter either "c" or "m".')
           go to 5
      end if
      
      end
      
      
      
      subroutine dummy
      integer*4 array(10000)
      common array
      end
      
      
      
