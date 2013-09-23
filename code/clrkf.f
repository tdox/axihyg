      subroutine CLRKF
      
c     Name:      CLeaR Ksky and F
c     Purpose:   To clear KSKY and F
c     Common:    KF
c     Input:     KSKY and F from KF common.
c     Output:    KSKY and F with zero valued elements to KF common.
c     Called by: FORMKF
c     Calls    : 


      implicit    none
      
      integer I,J
      
      include 'n.par'
      include 'elmdat.com'
      include 'kfsky.com'
      
      do 1 I=1,LNKSKY
        KSKY(I) = 0.
1     continue

      do 2 I=1,NUMEQ
        F(I) = 0.
2     continue

4       continue
3     continue

      return
      end
      
