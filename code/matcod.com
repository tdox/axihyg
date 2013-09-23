      logical          HOMOGN
      character*3      MATCODE      
      common /MATCOD/  HOMOGN,MATCODE
      save   /MATCOD/


