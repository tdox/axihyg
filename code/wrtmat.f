         subroutine WRTMAT(UNITNO,MAT)
      
      
c     Name:      WRiTe MATerial data
c     Purpose:   To write the material properties of the material MATNO
c                to the unit (terminal or file) UNITNO.
c     Input:     UNITNO, unit no to write to
c                MAT,Material no. of the material to write.
c                If a 0 is passed, get the material no. from the user.
c     Output:    Material property data to the screen
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************


      implicit     none
      
      include      'n.par'
      include      'matprp.com'
      include      'units.com'
      
      integer  UNITNO,MAT

      if (MATTYP(MAT) .eq. 1) then
           write(UNITNO,100) MAT,MATNAM(MAT),E1(MAT),STRSUS,NU12(MAT),
     &                       ALPHT1(MAT),ALPTUS,ALPHM1(MAT),ALPMUS
100       format(//' Material no. ',i3,5x,a,3x,'isotropic'//
     &             ' E = ',e12.5,x,a,5x,'Nu = ',e12.5/
     &             ' Alpha T = ',e12.5,x,a,x,
     &             ' Alpha M = ',e12.5,x,a)
     
        else if (MATTYP(MAT) .eq. 2) then
           write(UNITNO,110) MAT,MATNAM(MAT),E1(MAT),STRSUS,
     &                       E2(MAT),STRSUS,
     &                       NU12(MAT),NU23(MAT),G12(MAT),STRSUS,
     &                       ALPHT1(MAT),ALPTUS,ALPHM1(MAT),ALPMUS,
     &                       ALPHT2(MAT),ALPTUS,ALPHM2(MAT),ALPMUS
110        format(//' Material no. ',i3,5x,a,3x,
     &             'transverse isotropic'//
     &             ' E1    = ',e12.5,x,a,3x,' E2    = ',e12.5,x,a/
     &             ' Nu 12 = ',e12.5,7X,' Nu 23 = ',e12.5,/
     &             ' G 12  = ',e12.5,x,a/
     &             ' Alpha T 1 = ',e12.5,x,a,x,
     &             ' Alpha M 1 = ',e12.5,x,a,/
     &             ' Alpha T 2 = ',e12.5,x,a,x,
     &             ' Alpha M 2 = ',e12.5,x,a)
   
        else if (MATTYP(MAT) .eq. 3) then
           write(UNITNO,120) MAT,MATNAM(MAT),
     &                      E1(MAT),STRSUS,E2(MAT),STRSUS,
     &                      E3(MAT),STRSUS,
     &                      NU12(MAT),NU13(MAT),NU23(MAT),
     &                      G12(MAT),STRSUS,G13(MAT),STRSUS,
     &                      G23(MAT),STRSUS,
     &                      ALPHT1(MAT),ALPTUS,ALPHM1(MAT),ALPMUS,
     &                      ALPHT2(MAT),ALPTUS,ALPHM2(MAT),ALPMUS,
     &                      ALPHT3(MAT),ALPTUS,ALPHM3(MAT),ALPMUS
120       format(//' Material no. ',i3,5x,a,3x,
     &              'rhombic'//
     &              ' E1    = ',e12.5,x,a,3x,' E2    = ',e12.5,x,a,3x,
     &              ' E3    = ',e12.5,x,a/
     &              ' Nu 12 = ',e12.5,7x,' Nu 13 = ',e12.5,7x,
     &              ' Nu 23 = ',e12.5/
     &              ' G 12  = ',e12.5,x,a,3x,' G 13  = ',e12.5,x,a,3x,
     &              ' G 23  = ',e12.5,x,a/
     &              ' Alpha T 1 = ',e12.5,x,a,x,
     &              ' Alpha M 1 = ',e12.5,x,a/
     &              ' Alpha T 2 = ',e12.5,x,a,x
     &              ' Alpha M 2 = ',e12.5,x,a/
     &              ' Alpha T 3 = ',e12.5,x,a,x
     &              ' Alpha M 3 = ',e12.5,x,a)
       end if

       return
       end
       
