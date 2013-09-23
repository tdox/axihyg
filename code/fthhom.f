      subroutine FTHHOM(THERM,KAPA1,KAPA2,THET0S,THET1S,THET2S,FTHRM)
      
c     Name:      Force  vector due to THerMal loads for HOmogenious (through
c                the thickness) materials
c     Purpose:   To calculate the thermal load vector for homgeneous materials
c     Input:     KAPA1,KAPA2: the curvatures at the point.
c                THET0S,THET1S: THETa 0 at S and THETa 1 at S.
c                BETA: The coefficient of thermal expansion for isotropic 
c                 materials.
c     Output:    FTHRM, the mechanical load vector.
c     Called by: FTHERM
c     Calls    : 

      implicit      none
      
      include       'io.par'
      include       'n.par'
      include       'contrl.com'
      include       'elmdat.com'
      include       'hommat.com'
      
      logical       THERM
      integer       I

      real*8        FTHRM(MXNEPS),THET0S,THET1S,THET2S,H,H2,H3,H5,
     *              KAPA1,KAPA2,H3O12,H5O80,
     *              H3K1,H3K2,H5K1,H5K2,H3K1K2,H5K1K2,H3K1PK2,H5K1PK2,
     *              K1K2,K1PK2,TN11,TN22,TM111,TM122,
     *              T0T,T1T,T2T,TM211,TM222,TM311,TM322
      
100   format(' in fthhom')
      
      do 1 I=1,MXNEPS
         FTHRM(I) = 0.
1     continue

     
      H     = THCKNS
      H2    = H*H
      H3    = H2*H
      H5    = H2*H3
      
      H3O12  = H3/12.
      H5O80  = H5/80.
      
      K1K2   = KAPA1*KAPA2
      K1PK2  = KAPA1 + KAPA2
      
      if (TORORD .eq. 2) then
      
         H3K1 = H3O12*KAPA1
         H3K2 = H3O12*KAPA2
         H5K1 = H5O80*KAPA1
         H5K2 = H5O80*KAPA2
         H3K1K2  = H3O12*KAPA1*KAPA2
         H5K1K2  = H5O80*KAPA1*KAPA2
         H3K1PK2 = H3O12*(KAPA1 + KAPA2)
         H5K1PK2 = H5O80*(KAPA1 + KAPA2)
         
       else if (TORORD .eq. 1) then
       
         H3K1 = H3O12*KAPA1
         H3K2 = H3O12*KAPA2
         H5K1 = H5O80*KAPA1
         H5K2 = H5O80*KAPA2
         H3K1K2  = 0.
         H5K1K2  = 0.
         H3K1PK2 = H3O12*(KAPA1 + KAPA2)
         H5K1PK2 = H5O80*(KAPA1 + KAPA2)

       else if (TORORD .eq. 0) then
       
         H3K1 = 0.
         H3K2 = 0.
         H5K1 = 0.
         H5K2 = 0.
         H3K1K2  = 0.
         H5K1K2  = 0.
         H3K1PK2 = 0.
         H5K1PK2 = 0.
         
       else
         call ERROR('FTHHOM',' TORORD must be 0,1,2')
      
      end if
      
      
      TN11 = BETA11*(H*THET0S + H3K2*THET1S)
      TN22 = BETA22*(H*THET0S + H3K1*THET1S)
      
      TM111 = BETA11*(H3O12*THET1S + H3K2*THET0S)
      TM122 = BETA22*(H3O12*THET1S + H3K1*THET0S)
      
      FTHRM(1) = TN11
      FTHRM(2) = TN22
      FTHRM(5) = TM111
      FTHRM(6) = TM122
      
      if (NEPS .ge. 11) then
      
         T0T = BETA33*(H*THET0S + (H3K1PK2*THET1S + H3K1K2*THET0S))
         T1T = BETA33 * ((H3O12*THET1S + H3K1PK2*THET0S)
     *                         + H5K1K2*THET1S)
         
         FTHRM(11) = T0T
         FTHRM(14) = T1T
         
         if (NEPS .ge. 15) then
      
            TM211 = BETA11*(H3O12*THET0S + H5K2*THET1S)
            TM222 = BETA22*(H3O12*THET0S + H5K1*THET1S)
            
            TM311 = BETA11*(H5O80*THET1S + H5K2*THET0S)
            TM322 = BETA22*(H5O80*THET1S + H5K1*THET0S)
            
            T2T = BETA33*(H3O12*THET0S
     *                + (H5K1PK2*THET1S + H5K1K2*THET0S))            
            
            FTHRM(15) = TM211
            FTHRM(16) = TM222
            FTHRM(19) = TM311
            FTHRM(20) = TM322
            FTHRM(27) = T2T
            
         end if
            
      end if
           
      return
      end
     

