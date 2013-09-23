       subroutine GEOMS(EXIST)
      
      
c     Name:      GEOMetry Subroutine
c     Purpose:   To input the geometry of the shell 
c     Input:     EXIST, if true, geometry information exists already
c                SHAPE, from user
c                various dimenstions of the shell
c     Output:    SHAPE and various dimensions into GEOM common
c     Commons:   GEOM
c     Called by: CREATE
c     Calls    : undefined(a-z)
c                                                                      |
c*******************************************************************************
 
 
      implicit     undefined(a-z)
      include      'io.com'
      
      logical      EXIST,DIMEXT,YES
      
      print 50
50    format(//' GEOMETRY')
      
1     continue
      
      if (EXIST) then
         call GEOWRT(TERM)
         print 100
100      format(/' Do you want to change the geometry?')
         call YESNO(YES)
         if (.not. YES) return
      end if
      
      call SHAPES(EXIST,DIMEXT)
      call DIMS(DIMEXT)
      
      EXIST = .true.
      go to 1
      
      end
      
      
    
      
