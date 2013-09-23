      subroutine INIT
      
c     Name:      INITialize
c     Purpose:   To initialize certain data
c     Input:     none
c                
c     Output:    Data to certain commons
c     Called by: CREATE
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
       include 'n.par'
       include 'elmdat.com'
       include 'contrl.com'
       
       NBC = MXNBC
       FCODE = .false.
       
       return
       end
