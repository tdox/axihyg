      character*80     TITLE
      common /TITLE/   TITLE
      save   /TITLE/

