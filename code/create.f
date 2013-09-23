      subroutine CREATE
      
c     Name:      CREATE
c     Purpose:   To create the INPUT.DAT file for the AXI program via a user
c                friendly interface.
c     Input:     
c                
c     Output:    The data files INPUT.DAT and MAT.DAT
c     Called by: USRFRD
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit     none
      
      logical      EXIST
      
      include      'save.com'
      

      call INIT
      EXIST = .false.
      call TITLES(EXIST)
      EXIST = .false.
      call UNITSS(EXIST)
      EXIST = .false.
      call GEOMS(EXIST)
      EXIST = .false.
      call NODES(EXIST)
      EXIST = .false.
      call MATERS
      EXIST = .false.
      call LAYUPS(EXIST)
      EXIST = .false.
      call LOADS(EXIST)
      EXIST = .false.
      call BCS(EXIST)
      EXIST = .false.
      call OUTS(EXIST)
      EXIST = .false.
      call SLTCHS(EXIST)
      call MODIFY
      
      SAVED = .false.
      
      stop
      end
      
      
