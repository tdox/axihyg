         subroutine NODES(EXIST)
      
      
c     Name:      NODE input Subroutine
c     Purpose:   To input the location of the finite elment nodes
c     Input:     EXIST, if true, nodal location information already exists
c                NUMEL, no. of elements from user
c                XEDGE(i), edge node postions from user
c     Output:    The above information into ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      implicit     none
      
      logical      EXIST,XEDEXT,YES
      
      print 50
50    format(//' FINITE ELEMENT MESH')

      print 100
100   format(' In this section you may either prescribe',
     &       ' the finite element mesh'/' nodal coordinates',
     &       ' yourself or you may let the computer choose a',
     &       ' mesh for you.'/' Do you want to prescribe the',
     &       ' mesh yourself?')
      call YESNO(YES)
      if (YES) then
         call NUMELS(EXIST,XEDEXT)
         call XEDGES(XEDEXT)
       else
         call MESH
      end if
      
      return
      end
      
      
