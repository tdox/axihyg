      real*8          BTTN11(0:6),BTTN22(0:6),BTTN12(0:6),
     &                BTTN33(0:6),
     &                BTMN11(0:6),BTMN22(0:6),BTMN12(0:6),
     &                BTMN33(0:6)
     
      common /LAMECF/ BTTN11,BTTN22,BTTN12,BTTN33,
     &                BTMN11,BTMN22,BTMN12,BTMN33

      save   /LAMECF/

