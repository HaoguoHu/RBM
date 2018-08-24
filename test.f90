	PROGRAM main
	 IMPLICIT NONE

	 integer,parameter::M=784, N=1000
	 integer A(5)
	 integer B(2, 3)
	 integer::C(3, 2)
	 INTEGER i, j

	 DATA A / 1, 2, 3, 4, 5 /
	 DATA B / 1, 2, 3, 4, 5, 6 /
	 DATA C / 1, 2, 3, 4, 5, 6 /
	 
	 real, DIMENSION (6) :: E = [1, 2, 3, 4, 5, 6], F
	 real, DIMENSION (2,3) :: D, G
	 real,dimension(M,N)::P
	 

	  D = reshape( E, [2,3] )
      write(*,*) 'default ordering'
	  CALL WRITEM(D)
      CALL WRITEM(D*D)
	  
!	  E = reshape(D, (/6/))
	  
!	  write(*,*) E
	 
!      D = reshape( E, [2,3], order=[2,1])
!      write(*,*) 'row-wise ordering'
!      CALL WRITEM(D)
	

!	 call randn(M, N, P)
!	 CALL WRITEM(P)
	
	contains
	    !--- internal functions below ------
       SUBROUTINE writeM(A)
       implicit none
       real, DIMENSION(:,:) :: A
       integer :: i,j

       WRITE(*,*)
       DO i = lbound(A,1), ubound(A,1)
          WRITE(*,*) (A(i,j), j = lbound(A,2), ubound(A,2))
       END DO
       
	END SUBROUTINE writeM
	
	end
	
