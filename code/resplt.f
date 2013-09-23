c- RESPLT -********************************************************************
c                                                                             *
c                                                                             *
      subroutine RESPLT                                                       *
c                                                                             *
c     Name:      stress RESultant PLoT                                        *
c     Purpose:   To create data files for plotting stress resutants.          *  
c     Input:     STRESR from STRSES common                                    *
c     Output:    The resultants to the data files NROUT.DAT, MROUT.DAT,       *
c                VROUT.DAT                                                    *
c     Called by: PLTOUT                                                       *
c     Calls    :                                                              *
c     Common:    CONTRL                                                       *
c                                                                             *
c******************************************************************************

      implicit     none

      include      'io.par'
      include      'n.par'
      include      'contrl.com'
      include      'elmdat.com'
      include      'strses.com'
      
      integer      SPTNO,I
      
      open (unit= NRFIL,  file= 'NROUT.DAT',  status= 'new')
      do 1 SPTNO = 1,NOSPT
         write(NRFIL,100) SOPT(SPTNO), (STRESR(SPTNO,I), I=1,4)
1     continue
100   format(x,5(x,e13.5))
      close(NRFIL)
      
      open (unit= M1RFIL,  file= 'M1ROUT.DAT', status= 'new')
      do 2 SPTNO = 1,NOSPT
         write(M1RFIL,100) SOPT(SPTNO), (STRESR(SPTNO,I), I=5,8)
2     continue
      close(M1RFIL)

      open (unit= M2RFIL, file= 'M2ROUT.DAT', status= 'new')
      do 3 SPTNO = 1,NOSPT
         write(M2RFIL,100) SOPT(SPTNO), (STRESR(SPTNO,I), I=15,18)
3     continue
      close(M2RFIL)

      open (unit= M3RFIL, file= 'M3ROUT.DAT', status= 'new')
      do 4 SPTNO = 1,NOSPT
         write(M3RFIL,100) SOPT(SPTNO), (STRESR(SPTNO,I), I=19,22)
4     continue
      close(M3RFIL)

      open (unit= QRFIL,  file= 'QROUT.DAT',  status= 'new')
      do 5 SPTNO = 1,NOSPT
         write(QRFIL,100) SOPT(SPTNO), (STRESR(SPTNO,I), I=9,11),
     *                    (STRESR(SPTNO,I), I=12,13),
     *                    (STRESR(SPTNO,I), I=23,26)
5     continue
      close(QRFIL)

      open (unit= TRFIL,  file= 'TROUT.DAT',  status= 'new')
      do 6 SPTNO = 1,NOSPT
         write(TRFIL,100) SOPT(SPTNO),STRESR(SPTNO,11),
     *                    STRESR(SPTNO,14),STRESR(SPTNO,27)
6     continue
      close(TRFIL)

      
      return
      end
      
