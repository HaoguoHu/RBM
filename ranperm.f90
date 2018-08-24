! From Knuth's book
!ranperm(N,p) returns a row vector containing a random permutation of the integers from 1 to n inclusive

subroutine ranperm(N, p)

integer, intent(in) :: N
integer, dimension(N), intent(out) :: p

integer :: i, j, k
integer :: temp
double precision :: u

	p = (/ (i, i=1,N) /)
	
	do j = N,2,-1

	call random_number(u)
	k = floor(j*u) + 1

	! exchange p(k) and p(j)
	temp = p(k)
	p(k) = p(j)
	p(j) = temp

	enddo

end subroutine ranperm

