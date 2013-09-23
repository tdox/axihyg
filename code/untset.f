      subroutine UNTSET
      
c     Name:      UNiT SET
c     Purpose:   To set the phyical unit variables
c     Input:     UNIT, from UNIT common
c                
c     Output:    Variables of UNITS common
c     Called by: INRED
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit    none
      
      include     'units.com'

      if ((UNIT .eq. 's') .or. (UNIT .eq. 'S')) then
           LENUS  = 'm'
           LENUL  = 'meters'
           STRSUS = 'Pa'
           STRSUL = 'pascals'
           TEMPUS = 'C'
           TEMPUL = 'Celsius'
           ALPTUS = '/ C'
           ALPTUL = 'per Celsius'
           ALPMUS = '(m/m)/(kg/kg)'
           ALPMUL = '(m/m)/(kg/kg)'
           DISPUS(0) = 'm'
           DISPUS(1) = 'm/m'
           DISPUS(2) = '1/m'
           DISPUS(3) = '1/m^2'
           DISPUS(4) = '1/m^3'
           DISPUL(0) = 'meters'
           DISPUL(1) = 'm/m'
           DISPUL(2) = 'inverse meters'
           DISPUL(3) = 'inverse meters squared'
           RSLTUS(0) = 'N/m'
           RSLTUS(1) = 'N'
           RSLTUS(2) = 'N-m'
           RSLTUS(3) = 'N-m^2'
           RSLTUL(0) = 'Newtons per meter'
           RSLTUL(1) = 'Newtons'
           RSLTUL(2) = 'Newton-meters'
           RSLTUL(3) = 'Newton - square meters'
          
        else if ((UNIT .eq. 'e') .or. (UNIT .eq. 'E')) then
           UNIT   = 'e'
           LENUS  = 'in'
           LENUL  = 'inches'
           STRSUS = 'psi'
           STRSUL = 'psi'
           TEMPUS = 'deg F'
           TEMPUL = 'deg F'
           ALPTUS = '/ deg F'
           ALPTUL = 'per degree F'
           ALPMUS = '(in/in)/(lbm/lbm)'
           ALPMUL = '(in/in)/(lbm/lbm)'
           DISPUS(0) = 'in'
           DISPUS(1) = 'in/in'
           DISPUS(2) = '1/in'
           DISPUS(3) = '1/in^2'
           DISPUS(4) = '1/in^3'
           DISPUL(0) = 'inches'
           DISPUL(1) = 'in/in'
           DISPUL(2) = 'inverse inches'
           DISPUL(3) = 'inverse inches squared'
           RSLTUS(0) = 'lbf/in'
           RSLTUS(1) = 'lbf'
           RSLTUS(2) = 'lbf-in'
           RSLTUS(3) = 'lbf-in^2'
           RSLTUL(0) = 'pound forces per inch'
           RSLTUL(1) = 'pound forces '
           RSLTUL(2) = 'pound force - inches'
           RSLTUL(3) = 'pound force - square inches'
      end if
      
      return
      end
      
      
