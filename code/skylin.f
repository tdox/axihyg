C***************************************************************

	     subroutine SKYLIN(SK,F,D,JDIAG,NEQ,NSK,SKFAC,BACK)

c     Name:      SKYLINe
c     Purpose:   To solve Kd=F for d where K is stored is skyline profile.
c     Input:     SK(NSK): K stored is skyline profile.
c                F(NEQ):  Right hand side vector.
c                JDIAG(MAXNEQ): Pointer array to determine the location in the
c                    skyline profile global K matrix of diagonal pivots.  The
c                    I'th element of JDIAG(I)  is the location in skyline K of 
c                    the diagonal element K(I,I).
c                NEQ: Number of EQuations.
c                NSK: Number of elements of SK.
c                SKFAC: TRUE is SK is to be factored.
c                BACK: TRUE if D is to be found
c                
c     Output:    D(NEQ)
c     Called by:
c     Calls    : 
c     Note:     This subroutine is taken from The Finite Element Method, Third
c               Edition by O. C. Zienkiewicz (p. 740).

      implicit     none
      
      logical    SKFAC,BACK
      integer    NEQ,JDIAG(NEQ),I,J,NSK,JR,JD,JH,IS,IE,ID,IH,IR,K
      real*8     F(NEQ),D(NEQ),SK(NSK),DD,DOT
      

        do 10 I=1,NEQ
10      D(I)=F(I)
                                                                               
        JR=0
        do 600 J=1,NEQ
        JD=JDIAG(J)
        JH=JD-JR
        IS=J-JH+2
        if(JH-2) 600,300,100
100     if(.not. SKFAC) go to 500
        IE=J-1
        K=JR+2
        ID=JDIAG(IS-1)
                      
C-------(REDUCE ALL EQUATIONS EXCEPT DIAGNAL )
                                                
        do 200 I=IS,IE
        IR=ID
        ID=JDIAG(I)
        if (ID-IR-1 .lt. I-IS+1) then
            IH = ID-IR-1
          else
            IH = I-IS+1
        end if
        if(IH .gt. 0) SK(K)=SK(K)-DOT(SK(K-IH),SK(ID-IH),IH)
200     K=K+1
                                      
C-------(REDUCE  THE DIAGNAL )        
                                      
300     if(.not. SKFAC) go to 500
        IR=JR+1
        IE=JD-1
        K=J-JD
        do 400 I=IR,IE
        ID=JDIAG(K+I)
        if(SK(ID) .eq. 0.) go to 400
        DD=SK(I)
        SK(I)=SK(I)/SK(ID)
        SK(JD)=SK(JD)-DD*SK(I)
400     continue
                                      
C-------( REDUCE THE  LOAD VECTOR )
                                      
500     if (BACK) D(J)=D(J)-DOT(SK(JR+1),D(IS-1),JH-1)
600     JR=JD
        if(.not. BACK) return
                                               
C-------(DIVIDED BY THE DIAGNAL PIVOTS )
                                               
        do 700 I=1,NEQ
        ID=JDIAG(I)
        if(SK(ID) .ne. 0.) D(I)=D(I)/SK(ID)
700     continue
                                               
C-------BACK SUBSTITUTION                      
                                               
1100    J=NEQ
        JD=JDIAG(J)
800     DD=D(J)
        J=J-1
        if(J .le. 0) return
        JR=JDIAG(J)
        if(JD-JR .le. 1) go to 1000
        IS=J-JD+JR+2
        K=JR-IS+1
        do 900 I=IS,J
900     D(I)=D(I)-SK(I+K)*DD
1000    JD=JR
        go to 800
   
   
        end

C--------------------------------------------------------
