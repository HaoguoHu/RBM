!!====Code provided by Haoguo Hu====Jun. 2016====
subroutine randn( M, N, F ) 
	implicit none
                                         
	double precision:: pi, temp, mean, sd
	integer::  i, M, N
	
	double precision,dimension(M, N):: F
	double precision,dimension(M*N):: array , u1	 
	
	 pi = 4.0*ATAN(1.0)
	 mean = 0.
	 sd = 1.
	 
	   call RANDOM_NUMBER(u1) 	
	      	   
	   DO i = 1, m*n, 2
     	 array(i)   = sd * SQRT(-2.0*LOG(u1(i))) * COS(2*pi*u1(i+1)) + mean
	     array(i+1) = sd * SQRT(-2.0*LOG(u1(i))) * SIN(2*pi*u1(i+1)) + mean
	   END DO	
	   
	    F =  reshape(array, (/m, n/))
		  
end subroutine randn
