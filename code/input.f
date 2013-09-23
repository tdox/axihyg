       subroutine INPUT
      
      

c     Name:      Input
c     Purpose:   To read input data from a file and make preliminary cal-
c                culations.
c     Common:    TITLE,ELMDAT1,ELMDAT2,ISOMAT,LAMMAT,GEOM,MATCOD,LOAD,BC,CONTRL
c     Input:     Data from the file 'INPUT.DAT'
c                
c     Output:    Date to the commons
c     Called by:
c     Calls    : NODEIN,NODECALC,ERROR
c                                                                      |
c*******************************************************************************
      implicit     none
      
      include      'io.par'
      include      'n.par'
      include      'bc.com'
      include      'elmdat.com'
      include      'io.com'
      include      'matcod.com'
      include      'out.com'
      
      integer      EDGENO,DOF
   
      ERRFIL = ERRRFL
      
      call INRED('INPUT.DAT')
      
      if (.not. HOMOGN) then
         call LAMCOF
      end if
      
      call ZEROR2(DISP, MAXNOD, MXNVAR)
      
      do 3 EDGENO=1,2
         do 2 DOF=1, NBC
           if (BCCODE(EDGENO,DOF) .eq. 1) then
              DISP(EDGENO,DOF) = UBC(EDGENO,DOF)
           end if      
2        continue
3     continue

      
      return
      end
      
      
