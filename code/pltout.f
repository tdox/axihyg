c- PLTOUT -********************************************************************
c                                                                             *
c                                                                             *
      subroutine PLTOUT                                                       *
c                                                                             *
c     Name:      PLot OUTput                                                  *
c     Purpose:   To create data files for plotting routines.                  *  
c     Input:                                                                  *
c     Output:                                                                 *
c     Called by:                                                              *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                             *
c******************************************************************************

      implicit     none
      include      'contrl.com'

      call DSPPLT
      call SHPPLT
      call POSPLT
      
      if (RSLT) then
         call RESPLT
      end if
      
      return
      end
      
      
