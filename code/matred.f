      subroutine MATRED
c                                                                             *
c                                                                             *
c                                                                             *
c     Name:      MATerial REaD                                                *
c     Purpose:   To read in the material properties from the file MAT.DAT     *  
c     Input:     Material Properties from the file MAT.DAT                    *
c     Output:    The material properties into MATPRP common                   *
c     Called by:                                                              *
c     Calls    :                                                              *
c     Common:                                                                 *
c                                                                      |      *
c******************************************************************************
 
      
      implicit     none
      
      include      'n.par'
      include      'matprp.com'
      
      integer      I
      
          
      open(unit=1,file='MAT.DAT',status ='OLD')
      
      read(1,*) NMAT
      
      do 1 I=1,NMAT
         read(1,'(a)') MATNAM(I)
         read(1,*)   MATTYP(I)
         
comment: MATTYP = 1, isotropic; MATTYP = 2, transverse isotropic;
c        MATTYP = 3, rhombic

         
         if (MATTYP(I) .eq. 1) then
            read(1,*) E1(I)
            read(1,*) NU12(I)
            read(1,*) ALPHT1(I)
            read(1,*) ALPHM1(I)
            E2(I)     = E1(I)
            E3(I)     = E1(I)
            NU13(I)    = NU12(I)
            NU23(I)    = NU12(I)
            G12(I)    = E1(I)/(2.*(1. + NU12(I)))
            G13(I)    = G12(I)
            G23(I)    = G12(I)
            ALPHT2(I) = ALPHT1(I)
            ALPHT3(I) = ALPHT1(I)
            ALPHM2(I) = ALPHM1(I)
            ALPHM3(I) = ALPHM1(I)
          else if (MATTYP(I) .eq. 2) then
            read(1,*) E1(I),E2(I)
            read(1,*) NU12(I),NU23(I)
            read(1,*) G12(I)
            read(1,*) ALPHT1(I),ALPHT2(I)
            read(1,*) ALPHM1(I),ALPHM2(I)
            E3(I)     = E2(I)
            NU13(I)   = NU12(I)
            G13(I)    = G12(I)
            G23(I)    = E2(I)/(2.*(1. + NU23(I)))
            ALPHT3(I)  = ALPHT2(I)
            ALPHM3(I)  = ALPHM2(I)
          else if (MATTYP(I) .eq. 3) then
            read(1,*) E1(I),E2(I),E3(I)
            read(1,*) NU12(I),NU13(I),NU23(I)
            read(1,*) G12(I),G13(I),G23(I)
            read(1,*) ALPHT1(I),ALPHT2(I),ALPHT3(I)
            read(1,*) ALPHM1(I),ALPHM2(I),ALPHM3(I)
          else 
            call ERROR('MATRED     ','MATTYP must be 1,2, or 3')
         end if
            
1     continue

      close(1)

      return
      end
      
      
         
         
