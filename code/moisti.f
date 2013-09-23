            subroutine MOISTI
      
      
c     Name:      MOISTure concentration Input 
c     Purpose:   To input moisture concentration distribution
c     Input:     Moisture concentration information from user
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
       integer     MN,I
       real*8      MIIS,MMIS,MOIS

      do 1 I=1,MAXELM+1
         MNI(I) = 0
         MII(I) = 0.
         MMI(I) = 0.
         MOI(I) = 0.
1     continue
     
      MNI(1) = 1
      print 100, X1,XF,X1US
100   format(' At the first edge of the shell,',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' moisture constant',
     &       ' through the thickness or does',
     &       ' it vary linearly or'/' quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
      
      
      call GETMST(MII(1),MMI(1),MOI(1))
      
      print 110, X1,XS,X1US
110   format(' At the second edge of the shell',
     &       ' (',a,' = ',e12.5,x,a,')',
     &       ' is the'/' moisture constant',
     &       ' through the thickness or does',
     &       ' it vary linearly'/' or quadratically',
     &       ' through the thickness?'/' Enter "c" for constant,',
     &       ' "l" for linearly,or "q" for quadratically.')
     
      call GETMST(MIIS,MMIS,MOIS)
      
      if (SIMP) then
         NMSTI = 2
         MNI(NMSTI) = NUMEL+1
         MII(NMSTI) = MIIS
         MMI(NMSTI) = MMIS
         MOI(NMSTI) = MOIS
         call THTINT(THICK,MNI,MII,MMI,MOI,MOIST0,MOIST1,MOIST2)
         return
      end if
         


      print 101
101   format(/' For reference, the nodal positions',
     &       ' previously specified are:')
     
      call XEDWRT(TERM)
      
         
      NMSTI = 1
                  
         
10    continue

         NMSTI = NMSTI+1
           
         print 120
120      format(' Do you want to enter the moisture at another',
     &         ' position of the shell?')
         call YESNO(YES)
         if (YES) then
20          print 130
130         format(' Enter node number at which you want'
     &             ' to specify the',
     &             ' moisture distribution.')
            read(*,*) MN
     	   if (MN .le. MNI(NMSTI-1)) then
     	        print 140, MNI(NMSTI-1)
140             format(' The element edge number must be greater',
     &                 ' than ',i2,'.')
                go to 20
             else if (MN .gt. NUMEL+1) then
                print 150, NUMEL+1
150             format(' The element edge number must be less',
     &               ' than or equal to ',i2,'.')
              go to 20
         end if
         
           MNI(NMSTI) = MN
           print 153
153        format(' Is the moisture constant',
     &             ' through the thickness',
     &             ' or does it vary linearly'/' or quadratically',
     &             ' through the thickness at this position?'/
     &             ' Enter "c" for constant, "l" for',
     &             ' linearly,or "q" for quadratically.')

           call GETMST(MII(NMSTI),MMI(NMSTI),MOI(NMSTI))
           go to 10
          else    
           MNI(NMSTI) = NUMEL+1
           MII(NMSTI) = MIIS
           MMI(NMSTI) = MMIS
           MOI(NMSTI) = MOIS
           call THTINT(THICK,MNI,MII,MMI,MOI,MOIST0,MOIST1,MOIST2)
           return
        end if
               
      end
            
      
 
