      subroutine INWRT(UNITNO)
      
c     Name:      INput WRiTe
c     Purpose:   To write the input data that currently exists to the unit
c                (file, screen) UNITNO.
c     Input:     UNITNO
c                Input data from the commons
c                
c     Output:    The input data to the screen.
c     Called by: MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************


      implicit      none
    
      include      'n.par'
      include      'bc.com'
      include      'contrl.com'
      include      'elmdat.com'
      include      'hommat.com'
      include      'io.com'
      include      'layup.com'
      include      'load.com'
      include      'loadi.com'
      include      'matcod.com'
      include      'matprp.com'
      include      'title.com'
      include      'units.com'


      integer      I,MAT,UNITNO
      character*21 MATER


      
      write(UNITNO,1000) TITLE
1000  format(x,a/)

      call SLTWRT(UNITNO)
      
      call GEOWRT(UNITNO)

      
      if (UNITNO .eq. TERM) call WAIT

      write(UNITNO,1060) NUMEL
1060  format(//' Number of elements:',I5/)
     
      call XEDWRT(UNITNO)
      if (UNITNO .eq. TERM) call WAIT


      if (MATCODE .eq. 'iso') then
           MATER = 'isotropic            '
         else if (MATCODE .eq. 'rho') then
           MATER = 'rhombic              '
         else if (MATCODE .eq. 'cub') then
           MATER = 'cubic                '
         else if (MATCODE .eq. 'tis') then
           MATER = 'transversly isotropic'
         else if (MATCODE .eq. 'lam') then
           MATER = 'laminated            '
      end if

      write(UNITNO,1090) MATER
1090  format(///' The shell material is ',a/)

      if (HOMOGN) then
         if (MATCODE .eq. 'iso') then
            write(UNITNO,1100) E,NU,ALPHA
1100        format(' E= ',e12.5,2x,'NU = ',e12.5,2x,'ALPHA = ',e12.5,/)
         end if
      
         if (.not. V3CODE) then
             write(UNITNO,1120) CI1111,CI2222,C3333,CI1122,C1133,C2233,
     *                         C1212,C1313,C2323,ALPH11,ALPH22,ALPH33,
     *                         BITA11,BITA22,BETA33,
     *                         THCKNS
1120         format(/  ' C1111   = ',e12.5,3x,'C2222   = ',e12.5,3x,
     *                  'C3333   = ',e12.5,/
     *                 ' C1122   = ',e12.5,3x,'C1133   = ',e12.5,3x,
     *                  'C2233   = ',e12.5,/
     *                 ' C1212   = ',e12.5,3x,'C1313   = ',e12.5,3x,
     *                  'C2323   = ',e12.5,/
     *                 ' Alpha11 = ',e12.5,3x,'Alpha22 = ',e12.5,3x,
     *                  'Alpha33 = ',e12.5/
     *                 ' Beta11  = ',e12.5,3x,'Beta22  = ',e12.5,3x,
     *                  'Beta33  = ',e12.5,//
     *                 ' Thickness  =',e12.5,//)
     
             write(UNITNO,1130) C1111,C2222,C1122,C1212,BETA11,BETA22
1130         format(' The reduced stiffnesses and thermal stress ',
     *              'coefficients are:'//,
     *              ' C1111  = ',e12.5,3x,'C2222  = ',e12.5,3x,/
     *              ' C1122  = ',e12.5,3x,'C1212  = ',e12.5,/
     *              ' Beta11 = ',e12.5,3x,'Beta22 = ',e12.5//)
     
          else 
                           
             write(UNITNO,1120) C1111,C2222,C3333,C1122,C1133,C2233,
     *                          C1212,C1313,C2323,ALPH11,ALPH22,ALPH33,
     *                          BETA11,BETA22,BETA33,THCKNS
         end if
      
       else
       
         if (SYM) then
            write(UNITNO,1131)
1131        format(' The laminate is symmetric.')
           else
            write(UNITNO,1132)
1132        format(' The laminate is not symmetric.')
         end if
         
         call LAYWRT(UNITNO)
         
         write(UNITNO,1702) LTHKNS,LENUS
1702     format(' Laminate thickness = ',e12.5,x,a)
         if (UNITNO .eq. TERM) call WAIT
     
         write(UNITNO,1137)
1137     format(////,' Material Properties',//)


         do 3 I=1,NMAT
            if (MATLOG(I)) then
               MAT = I
               call WRTMAT(UNITNO,MAT)
               if (UNITNO .eq. TERM) call WAIT
             end if
3         continue

      end if
      
      if (FCODE) then
      
         write(UNITNO,1135) FCODE,PZTOP,PZBOT
1135     format(////' Prescribed Pressure Loading'//
     *          ' FCODE=',L2,/
     *          ' Pz (top) =',e12.5,5X,'Pz (bottom) =',e12.5,//)
     
         write(UNITNO,1701) M11,M22
1701     format(////' Prescribed Pressure Loading'//
     *          ' M11  =',e12.5,5X,'M22  =',e12.5,//)
      end if
     
     
      if (TCODE) then
         call TMPWRT(UNITNO)
         if (UNITNO .eq. TERM) call WAIT
      end if
     
      if (MOIST) then
         call MSTWRT(UNITNO)
         if (UNITNO .eq. TERM) call WAIT
      end if

      call BCWRT(UNITNO)
       
      call OTQWRT(UNITNO)
       

      return
      end
      
      
