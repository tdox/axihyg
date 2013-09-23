      real*8            E,NU,ALPHA,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323,
     &                  ALPH11,ALPH22,ALPH33,
     &                  BETA11,BETA22,BETA33,THCKNS,
     &                  CI1111,CI2222,CI1122,BITA11,BITA22
      common /HOMMAT/   E,NU,ALPHA,
     &                  C1111,C2222,C3333,C1122,C1133,C2233,
     &                  C1212,C1313,C2323,
     &                  ALPH11,ALPH22,ALPH33,
     &                  BETA11,BETA22,BETA33,THCKNS,
     &                  CI1111,CI2222,CI1122,BITA11,BITA22
      save   /HOMMAT/
      
