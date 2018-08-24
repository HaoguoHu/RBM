!% Version 1.000
!% Code provided by Ruslan Salakhutdinov and Geoff Hinton
!%!! Fortran Code provided by Haoguo Hu
!% Permission is granted for anyone to copy, use, modify, or distribute this
!% program and accompanying programs and documents for any purpose, provided
!% this copyright notice is retained and prominently displayed, along with
!% a note saying that the original programs are available from our
!% web page.
!% The programs and documents are distributed without any warranty, express or
!% implied.  As the programs were written for research purposes only, they have
!% not been tested to the degree that would be advisable in any important
!% application.  All use of these programs is entirely at the user's own risk.

!function [err] = mnistdisp(digits);
!% display a group of MNIST images 
subroutine mnistdisp(digits, dd, N)

implicit none

integer::dd, N, nn, ii, jj, k, i, j, nf=20
integer::col=28;
integer::row=28*2;
!integer,dimension(56,integer(ceiling(N/2.))*28)::imdisp
double precision,dimension(dd, N)::digits
double precision,dimension(28*2, 28, N)::img1, img2


write(*,*) dd, N, 'mnistdisp.f90 784*2=1568'

!imdisp=zeros(2*28,ceil(N/2)*28);

do nn = 1, N
! ii = mod(nn, 2)  !rem(nn,2) 
! if(ii == 0) ii = 2
! jj = int(ceiling(nn/2.))

	k = 1
	do i=1, row
	do j=1, col 
		img1(i,j,nn) = digits(k, nn) 
		k = k + 1
	enddo
	enddo	
enddo

!	img1 = img1 * 255
	write(*,*)'gnuplot draw now'
	
	open(nf, file='outCompare.txt',status='unknown')
	do i=row,1,-1
	write(nf,*)img1(i,:,:)
	enddo
	close(nf)
	 
	 open(nf,file='commanddata.txt', status='replace')
		write(nf,'(a)')' set size ratio -1'
		write(nf,'(a)')' set palette grey  '
		write(nf,'(a)')' plot "outCompare.txt" matrix with image   '
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commanddata.txt')
	
	
!err=0


end subroutine mnistdisp
