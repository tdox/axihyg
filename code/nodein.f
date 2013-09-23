      subroutine NODEIN
      
c     Name:      NODE INput
c     Purpose:   To read in the given nodal coordinates and calculate the rest.
c     Common:    ELMDAT1, ELMDAT2
c     Input:     NODE, X(NODE) from INPUT.DAT
c     Output:    Data in the ELMDAT2 common
c     Called by: INPUT
c     Calls    : ERROR

      implicit    none
      
      include 'io.par'
      include 'n.par'
      include 'elmdat.com'
      
      integer NODE,NUMENOD,I,DIFF,LASTNODE
      real*8  XEINCR,LASTXE,POSITION

c     NUMber of Edge NODes      
      NUMENOD = NUMEL+1
      
      read(INFIL,*)  NODE,POSITION
      XEDGE(NODE) = POSITION      
      
      if (NODE .ne. 1) call ERROR('NODEIN','First node must be 1.')
      
commment: interpolate to calculate the element edge nodal coordinates that are
c         not given.

1      continue 
         LASTNODE = NODE
         LASTXE   = XEDGE(NODE)               
         read(INFIL,*)  NODE,POSITION
         XEDGE(NODE) = POSITION        
         if (NODE .gt. NUMENOD) then
               call ERROR('NODEIN    ',
     *                    'Last node must be NUMEL+1.    ')
            else if (NODE .le. LASTNODE) then
               call ERROR('NODEIN    ',
     *                    'Nodes must increase.          ')
            else if (XEDGE(NODE) .le. LASTXE) then
               call ERROR('NODEIN    ',
     *                    'XEDGE(NODE) must increase.        ')
         end if
        
          DIFF  = NODE - LASTNODE
          XEINCR = (XEDGE(NODE) - LASTXE) / DIFF
         
          do 2, I=1,DIFF-1
             XEDGE(LASTNODE+I) = LASTXE + XEINCR*I
2         continue
         
          if (NODE .eq. NUMENOD) return
         
      go to 1
               
      end
      
