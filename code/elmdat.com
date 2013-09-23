      integer          NUMEL,NUMNOD,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF(MAXNEN,MXNVAR),
     *                 ID(MXNVAR,MAXNOD),IEN(MAXNEN,MAXNOD),
     *                 JDIAG(MAXNEQ),NUMEQ,NDOFPN(MAXNEN),LNKSKY
      real*8           XEDGE(MAXELM + 1),XNODE(MAXNOD)
      common /ELDATI/  NUMNOD, NUMEL,NEN,NVAR,NRBM,NBC,NEPS,
     *                 FDOF,NUMEQ,ID,
     *                 IEN,JDIAG,NDOFPN,LNKSKY
      common /ELDATR/  XEDGE,XNODE
      save   /ELDATI/
      save   /ELDATR/
      
      
      

