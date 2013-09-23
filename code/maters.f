        subroutine MATERS
      
      
c     Name:      MATERial data Subroutine
c     Purpose:   To input the material properties in the data base MAT.DAT
c     Input:     Material property data from MAT.DAT and the user.
c     Output:    Material property data to MAT.DAT
c     Commons:   MATPRP
c     Called by: CREATE
c     Calls    : MATRED
c                                                                      |
c*******************************************************************************



      implicit     none
      
      include      'n.par'
      include      'io.com'
      include      'matprp.com'

      logical      YES,MATEXT
 
      print 90
90    format(//' MATERIAL PROPERTIES')
      
      print 100
100   format(///' Material property data is stored in the file',
     &       ' "MAT.DAT". In this section of'/' the program, the data',
     &       ' in this file may be changed and/or new data',
     &       ' added'/' if the file already exists; or if',
     &       ' the file does not yet exist it may be'/' created.')
     
      inquire(file='MAT.DAT',exist=MATEXT)
     
      if (MATEXT) then
         print 120
120      format(/' The file MAT.DAT exists.'/)
         call WAIT
         call MATRED
         call MATLST
         call MATNOT

         print 130
130      format(/' Would you like to view the properties of',
     &           ' any of these materials?')
15       call YESNO(YES)
         if (YES) then
             call VIEWMT(0)            
             print 140
140          format(/' Do you wish to view the properties of another',
     &               ' material?')
             go to 15
         end if


         print 150
150      format(/' Would you like to change any of the existing',
     &           ' material properties?')
25       call YESNO(YES)
         if (YES) then
            call CHGMAT(0)
            print 160
160         format(/' Would you like to change any other material',
     &              ' properties?')
            go to 25          
          end if



40        print 170
170       format(/' Would you like to add additional material',
     &           ' properties to the data base?')
          call YESNO(YES)
          if (YES) then
             call ADDMAT
             go to 40
            else
             call MATWRT
             return
          end if
          
       end if
       
       
       print 180
180    format(/' MAT.DAT does not exist so there are no material',
     &        ' properties defined.'/' You must define some now.')
       NMAT = 0
       call ADDMAT
50     print 170
       call YESNO(YES)
       if (YES) then
          call ADDMAT
          go to 50
         else
          call MATWRT
          return
       end if
          
          
       end
       
         
