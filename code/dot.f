
        function DOT(A,B,N)
        
        implicit      none             

        integer      I,N  
        real*8       A(N),B(N),DOT     
           
        DOT=0.0
        do 1 I=1,N 
          DOT=DOT + A(I)*B(I)
1       continue
       
        return
        end
        
