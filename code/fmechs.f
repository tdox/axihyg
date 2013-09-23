      subroutine FMECHS(FMECH,S)
      
c     Name:      Force  vector due to MECHanical loads Subroutine
c     Purpose:   To calculate the load vector.
c     Input:     S, the meridional arclength coordinate.
c     Output:    The mechanical load vector.
c     Called by: FORMKF
c     Calls    : 


      implicit      none
      
      include       'n.par'
      include       'elmdat.com'
      include       'load.com'
      
      integer       I
      real*8        FMECH(MXNVAR),S,LENGTH
      
100   format(' in fmechs')
      
      do 1 I=1,MXNVAR
         FMECH(I) = 0.
1     continue

      LENGTH = XEDGE(NUMEL+1) - XEDGE(1)     

      FMECH(3) = PZTOP + (S-XEDGE(1))/LENGTH * (PZBOT-PZTOP)
      FMECH(4) = M11
      FMECH(5) = M22
     
110   format(/,' FMECH',/,10F9.4)
     
      return
      end
     
     
      
