          subroutine XEDGES(EXIST)
      
      
c     Name:      X EDGE Subroutine
c     Purpose:   To input the positions of the edges of the finite elments
c     Input:     EXIST, if true, position information already exists
c                XEDGE, element edge postions from user
c     Output:    The above information into ELMDAT common
c     Commons:   ELMDAT
c     Called by: CREATE
c     Calls    : None
c                                                                      |
c*******************************************************************************


      implicit     none
      
      include      'n.par'
      include      'elmdat.com'
      include      'geom.com'
      include      'io.com'
      include      'units.com'
      include      'xedin.com'
      
      logical      YES,EXIST,OK
      integer      I,N
      real*8       X
      
1     continue     
      if (EXIST) then
         print 100, X1,X1US
100      format(///' The positions of the',
     &          ' edges of the elements are:'//
     &          ' Element Edge',5x,'Position'/21x,a/18x,'(',a,')')
     
         do 10 I=1,NXEDI
            print 110, XNODEI(I),XEDGEI(I)
110         format(5x,i2,8x,e12.5)
10        continue
     
         print 112
112      format(/' Do you want to see the interpolated positions',
     &           ' of all of the element edges?')
         call YESNO(YES)
         if (YES) then
            call XEDWRT(TERM)
         end if

         print 115
115      format(/' Do you want to change these?')
         call YESNO(YES)
         
         if (.not. YES) return
      end if
      
      print 118
118   format(' Would you like to read some information concerning',
     &       ' entering element'/' edge location data?')
      call YESNO(YES)
      if (YES) then
         print 120, NUMEL+1,XS
120      format(//' In this section, the positions of the edges of the',
     &         ' shell elements are entered.'/' First the edge',
     &         ' element node number and then the position of',
     &         ' that'/' node is entered.  Node numbers and',
     &         ' positions must be given in increasing'/' order.',
     &         ' Any edge element node',
     &         ' position that is not explicitly entered will be'/
     &         ' determined by linear interpolation.',
     &         '  To finish entering',
     &         ' nodes, enter the last'/' node of the last',
     &         ' element (edge node number ',i2,').  The position of',
     &         ' the'/' last node is ',e12.5,' and cannot be changed',
     &         ' here.')
     
         if (X1 .eq. 's') then
            print 122, X1UL
122         format(/' The positions of the edge nodes are to be given',
     &              ' in arc-length (s) along'/
     &              ' the generator in ',a,'.')
          else
            print 123, X1UL
123         format(/' The positions of the edge nodes are to be given',
     &              ' as the angle between a'/
     &              ' line segment normal the midsurface at that',
     &              ' node and a line segment'/
     &              ' parallel to the axis of the shell',
     &              ' (phi) in ',a,'.')
         end if
         
      end if
     
15    print 125, X1,XF,X1US
125   format(/' The position (',a,') of the first node is',
     &       e12.5,x,a'.')
      XNODEI(1) = 1
      XEDGEI(1) = XF
      NXEDI     = 1
      
      print 126, X1,XS,X1US
126   format(' The position (',a,') of the last node is',
     &        e12.5,x,a'.')
      
      
20    print 130, X1,X1UL,NUMEL+1,XS
130   format(/' Enter an edge node number and the',
     &       ' position (',a,') of',
     &       ' that node in ',a,'.'/' The edge node number must be',
     &       ' greater than 1 and less than or',
     &       ' equal to ',i2'.'/' The edge node position must be',
     &       ' less than ',e12.5,'.')
25    read(*,*) N,X


      if (N .eq. NUMEL+1) then
        I=2
        go to 50
      end if
      if (N .le. XNODEI(1)) then
         print 150, XNODEI(1)
150      format(' The edge node number must be greater than ',i2,'.')
         print 145
145      format(' Please reenter the node number and its position.')
         go to 25
      end if
      if (N .gt. NUMEL+1) then
         print 155, NUMEL+1
155      format(' The edge node number must be less than or equal',
     &          ' to ',i2,'.')
         print 145
         go to 25
      end if
      if (X .le. XEDGEI(1)) then
         print 170, XEDGEI(1)
170      format(' The position must be greater than ',e12.5,'.')
         print 145
         go to 25
      end if
      
      if (X .ge. XS) then
         print 171, XS
171      format(' The position must be less that ',e12.5,' .')
         print 145
         go to 25
      end if
      
      call CHECKX(X,OK)
      if (.not. OK) then
           go to 20
         else
           XNODEI(2) = N
           XEDGEI(2) = X
           NXEDI     = NXEDI + 1
      end if
      
     
     
      do 35 I=3,10000
         print 140
140      format(/' Enter an edge node number and its position.')
30       read(*,*) N,X

         if (N .eq. NUMEL+1) go to 50
         if (N .le. XNODEI(I-1)) then
            print 150, XNODEI(I-1)
            print 145
            go to 30
         end if
         if (N .gt. NUMEL+1) then
            print 155, NUMEL+1
            print 145
            go to 30
         end if
         if (X .le. XEDGEI(I-1)) then
            print 170, XEDGEI(I-1)
            print 145
            go to 30
         end if
         if (X .ge. XS) then
            print 141, XS
141         format(' The position must be less that ',e12.5,' .')
            print 145
            go to 30
      end if

         call CHECKX(X,OK)
         if (.not. OK) then
            print 145
            go to 30
          else
           XNODEI(I) = N
           XEDGEI(I) = X
           NXEDI = NXEDI+1
         end if
         

35    continue

50    continue
      
      XNODEI(I) = NUMEL+1
      XEDGEI(I) = XS
      NXEDI     = NXEDI+1
   

      call XEINTP
      EXIST = .true.
      go to 1
         
      end
      
      
