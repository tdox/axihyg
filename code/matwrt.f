        subroutine MATWRT
      
      
c     Name:      MATERial WRiTe subroutine
c     Purpose:   To write the material properties to the data base MAT.DAT
c     Input:     Material property data from MATPRP common
c     Output:    Material property data to MAT.DAT
c     Commons:   MATPRP
c     Called by: MATERS
c     Calls    : 
c                                                                      |
c*******************************************************************************



      implicit     undefined(a-z)
      
      include      'n.par'
      include      'matprp.com'

      integer      I

      open(unit=1,file='MAT.DAT',status ='UNKNOWN')

      write(1,100) NMAT
100   format(i2,5x,':NMAT, Number of materials')

      do 1 I=1,NMAT
      
         write(1,110) MATNAM(I)
110      format(a)
         write(1,120) MATTYP(I)
120      format(i1,5x,':MATTYP, (1:iso., 2:trnsv. iso., 3:rhombic)')

         if (MATTYP(I) .eq. 1) then

            write(1,130) E1(I)
130         format(e11.4,5x,':E')
            write(1,135) NU12(I)
135         format(e11.4,5x,':Nu')
            write(1,140) ALPHT1(I)
140         format(e11.4,5x,':Alpha Thermal')
            write(1,145) ALPHM1(I)
145         format(e11.4,5x,':Alpha Moisture ')

          else if (MATTYP(I) .eq. 2) then
            write(1,150) E1(I),E2(I)
150         format(2(e11.4,x),':E1, E2')
            write(1,152) NU12(I),NU23(I)
152         format(2(e11.4,x),':Nu12, Nu23')
            write(1,154) G12(I)
154         format(e11.4,13x,':G12')
            write(1,160) ALPHT1(I),ALPHT2(I)
160         format(2(e11.4,x),':Alpha thermal 11, At22')
            write(1,165) ALPHM1(I),ALPHM2(I)
165         format(2(e11.4,x),':Am11, Am22')
  
          else if (MATTYP(I) .eq. 3) then
            write(1,170) E1(I),E2(I),E3(I)
170         format(3(e11.4,x),5x,':E1, E2, E3')
            write(1,180) NU12(I),NU13(I),NU23(I)
180         format(3(e11.4,x),5x,':Nu12, Nu13, Nu23')
            write(1,190) G12(I),G13(I),G23(I)
190         format(3(e11.4,x),5x,':G12, G13, G23')
            write(1,200) ALPHT1(I),ALPHT2(I),ALPHT3(I)
200         format(3(e11.4,x),5x,':At11, At22, At33')
            write(1,210) ALPHM1(I),ALPHM2(I),ALPHM3(I)
210         format(3(e11.4,x),5x,':Am11, Am22, Am33')

         end if

1     continue

      close(1)

      return
      end
