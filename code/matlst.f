        subroutine MATLST
      
      
c     Name:      MATERial LiST
c     Purpose:   To list the material properties in the data base MAT.DAT
c     Input:     
c     Output:    Material property list to the screen
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************

      implicit    undefined(a-z)
      
      include      'n.par'
      include      'matprp.com'
      
      integer      I
         
      print 110, NMAT
110   format(//' There are ',i2,' materials whose properties have',
     &         ' already been defined.  They are:',//
     &         ' Material no.',3x,'Material name'/)
      do 10 I=1,NMAT
         print 120, I,MATNAM(I)
120      format(6x,i2,9x,a)
10    continue

      return
      end
      
      
