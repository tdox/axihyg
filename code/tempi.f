           subroutine TEMPI
      
      
c     Name:      TEMPerature Input 
c     Purpose:   To input temperture distribution
c     Input:     Temperature information from user
c     Output:    The above information into LOADI common
c     Commons:   LOADI
c     Called by: LOADS
c     Calls    : None
c                                                                      |
c*******************************************************************************

 
       implicit    undefined(a-z)
       
       include     'n.par'
       include     'elmdat.com'
       include     'geom.com'
       include     'io.com'
       include     'load.com'
       include     'loadi.com'
       include     'units.com'
       
       logical     YES
       integer     TN, I
       real*8      TIIS,TMIS,TOIS

      do 1 I=1,MAXELM+1
         TNI(I) = 0
         TII(I) = 0.
         TMI(I) = 0.
         TOI(I) = 0.
1     continue
     
      TNI(1) = 1
      print 100, X1,XF,X1US
100   format(' At the first edge of the shell,',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' temperature constant',
     &       ' through the thickness or does',
     &       ' it vary linearly or'/' quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
      
      
      call GETTMP(TII(1),TMI(1),TOI(1))
      
      print 110, X1,XS,X1US
110   format(' At the second edge of the shell',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' temperature constant',
     &       ' through the thickness or does',
     &       ' it vary linearly'/' or quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
     
      call GETTMP(TIIS,TMIS,TOIS)
      
      if (SIMP) then
         NTMPI = 2
         TNI(NTMPI) = NUMEL+1
         TII(NTMPI) = TIIS
         TMI(NTMPI) = TMIS
         TOI(NTMPI) = TOIS
         call THTINT(THICK,TNI,TII,TMI,TOI,TEMP0,TEMP1,TEMP2)
         return
      end if
         

      print 101
101   format(/' For reference, the nodal positions',
     &       ' previously specified are:')
     
      call XEDWRT(TERM)
      
      
      NTMPI = 1
                  
         
10    continue

         NTMPI = NTMPI+1
        
         print 120
120      format(' Do you want to enter the temperature at another',
     &         ' position of the shell?')
         call YESNO(YES)
         if (YES) then
20          print 130
130         format(' Enter node number at which you want'
     &             ' to specify the',
     &             ' temperature distribution.')
            read(*,*) TN
     	    if (TN .le. TNI(NTMPI-1)) then
     	        print 140, TNI(NTMPI-1)
140             format(' The element edge number must be greater',
     &               ' than ',i2,'.')
                go to 20
             else if (TN .gt. NUMEL+1) then
                print 150, NUMEL+1
150             format(' The element edge number must be less',
     &               ' than or equal to ',i2,'.')
                go to 20
            end if
          
           TNI(NTMPI) = TN
           print 153
153       format(' Is the temperature constant',
     &             ' through the thickness',
     &             ' or does it vary linearly'/' or quadratically',
     &             ' through the thickness at this position?'/
     &             ' Enter "c" for constant, "l" for',
     &             ' linearly,or "q" for quadratically.')

           call GETTMP(TII(NTMPI),TMI(NTMPI),TOI(NTMPI))
           go to 10
          else
            TNI(NTMPI) = NUMEL+1
            TII(NTMPI) = TIIS
            TMI(NTMPI) = TMIS
            TOI(NTMPI) = TOIS
            call THTINT(THICK,TNI,TII,TMI,TOI,TEMP0,TEMP1,TEMP2)
            return
       end if     

      end
      
      
