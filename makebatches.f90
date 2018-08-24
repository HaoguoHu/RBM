subroutine makebatches  
	use DataC
implicit none
! Version 1.000
! Code provided by Ruslan Salakhutdinov and Geoff Hinton
!!! Fortran Code provided by Haoguo Hu
! Permission is granted for anyone to copy, use, modify, or distribute this
! program and accompanying programs and documents for any purpose, provided
! this copyright notice is retained and prominently displayed, along with
! a note saying that the original programs are available from our
! web page.
! The programs and documents are distributed without any warranty, express or
! implied.  As the programs were written for research purposes only, they have
! not been tested to the degree that would be advisable in any important
! application.  All use of these programs is entirely at the user's own risk.

integer,parameter::nf=20, IM=60000, numdims1=784, IMM=10000
integer,parameter::numbatchesT = 100

integer,parameter::IM0=5923,IM1=6742,IM2=5958,IM3=6131,IM4=5842
integer,parameter::IM5=5421,IM6=5918,IM7=6265,IM8=5851,IM9=5949

integer,parameter::JM0=980,JM1=1135,JM2=1032,JM3=1010,JM4=982
integer,parameter::JM5=892,JM6=958,JM7=1028,JM8=974,JM9=1009

integer,allocatable::randomorder(:)
double precision,allocatable::digitdata(:,:),targets(:,:)

integer::totnum, i, b, k, j, ii

!double precision::testbatchdata(batchsize, numdims1,  numbatchesT) 
double precision::testbatchtargets(batchsize, 10,  numbatchesT) 

double precision::batchtargets(batchsize, 10, numbatches) 

 character*1::D
 
	allocate(randomorder(IM), digitdata(IM, numdims1), targets(IM, 10))
	allocate(batchdata(batchsize, numdims1, numbatches))

	targets = 0
	totnum = 0

	do i=0,9
	write(D,'(i1)')i 
!	write(*,*)'D=',D
 	open(nf,file='data/digit'//D//'.ascii',status='old')
		select case(i)
		case(0)
			totnum=totnum+IM0
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM0+1,totnum)
			targets(totnum-IM0+1:totnum, 1) = 1
		case(1)
			totnum=totnum+IM1
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM1+1,totnum)
			targets(totnum-IM1+1:totnum, 2) = 1
		case(2)
			totnum=totnum+IM2
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM2+1,totnum)
			targets(totnum-IM2+1:totnum, 3) = 1
		case(3)
			totnum=totnum+IM3
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM3+1,totnum)
			targets(totnum-IM3+1:totnum, 4) = 1
		case(4)
			totnum=totnum+IM4
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM4+1,totnum)
			targets(totnum-IM4+1:totnum, 5) = 1
		case(5)
			totnum=totnum+IM5
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM5+1,totnum)
			targets(totnum-IM5+1:totnum, 6) = 1
		case(6)
			totnum=totnum+IM6
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM6+1,totnum)
			targets(totnum-IM6+1:totnum, 7) = 1
		case(7)
			totnum=totnum+IM7
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM7+1,totnum)
			targets(totnum-IM7+1:totnum, 8) = 1
		case(8)
			totnum=totnum+IM8
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM8+1,totnum)
			targets(totnum-IM8+1:totnum, 9) = 1
		case(9)
			totnum=totnum+IM9
 			read(nf,*)(digitdata(ii,:),ii=totnum-IM9+1,totnum)
			targets(totnum-IM9+1:totnum, 10) = 1
		case default
		    
		end select
			
 	close(nf)
	enddo
	
		digitdata = digitdata/255;


	write(*,*) 'Size of the training dataset= ', totnum
		
!	rand('state',0); !so we know the permutation of the training data
	call ranperm(totnum,randomorder)
!	write(*,*)randomorder(1:10)

	batchdata = 0      !zeros(batchsize, numdims1, numbatches);
	batchtargets = 0  !zeros(batchsize, 10, numbatches);

	forall( b=1: numbatches)
	  batchdata(:,:,b) = digitdata(randomorder(1+(b-1)*batchsize:b*batchsize), :);
	  batchtargets(:,:,b) = targets(randomorder(1+(b-1)*batchsize:b*batchsize), :);
	end forall
	
!	k = 1
!	do i=1, 28
!	do j=1, 28 
!		img1(i, j) = batchdata(100,k,10)  !digitdata(1000, k)  !batchdata(1,k,1) 
!		k = k + 1
!	enddo
!	enddo
!!!	img1 = img1*255	
!	open(nf, file='outCompare2.txt',status='unknown')
!	do i=1,28
!	write(nf,*) img1(i, :) 
!	enddo
!	close(nf)
	
!	stop
100 continue

deallocate(randomorder, digitdata, targets)
allocate(randomorder(IMM), digitdata(IMM, numdims1), targets(IMM, 10))

!return

!load test data--
	totnum = 0
	digitdata = 0
	targets = 0
	do i=0,9
	write(D,'(i1)')i 
!	write(*,*)'D=',D
 	open(nf,file='data/test'//D//'.ascii',status='old')
		select case(i)
		case(0)
			totnum=totnum+JM0
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM0+1,totnum)
			targets(totnum-JM0+1:totnum,1) = 1
		case(1)
			totnum=totnum+JM1
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM1+1,totnum)
			targets(totnum-JM1+1:totnum,2) = 1
		case(2)
			totnum=totnum+JM2
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM2+1,totnum)
			targets(totnum-JM2+1:totnum,3) = 1
		case(3)
			totnum=totnum+JM3
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM3+1,totnum)
			targets(totnum-JM3+1:totnum,4) = 1
		case(4)
			totnum=totnum+JM4
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM4+1,totnum)
			targets(totnum-JM4+1:totnum,5) = 1
		case(5)
			totnum=totnum+JM5
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM5+1,totnum)
			targets(totnum-JM5+1:totnum,6) = 1
		case(6)
			totnum=totnum+JM6
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM6+1,totnum)
			targets(totnum-JM6+1:totnum,7) = 1
		case(7)
			totnum=totnum+JM7
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM7+1,totnum)
			targets(totnum-JM7+1:totnum,8) = 1
		case(8)
			totnum=totnum+JM8
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM8+1,totnum)
			targets(totnum-JM8+1:totnum,9) = 1
		case(9)
			totnum=totnum+JM9
 			read(nf,*)(digitdata(ii,:),ii=totnum-JM9+1,totnum)
			targets(totnum-JM9+1:totnum,10) = 1
		case default
		    
		end select
			
 	close(nf)
	enddo
	
		digitdata = digitdata/255;


	write(*,*) 'Size of the test dataset= ', totnum

!	rand('state',0); !so we know the permutation of the training data
	call ranperm(totnum,randomorder)
!	write(*,*)randomorder(1:10)


	testbatchdata = 0      !zeros(batchsize, numdims1, numbatches);
	testbatchtargets = 0.  !zeros(batchsize, 10, numbatches);
	
	do b=1, numbatchesT
	  testbatchdata(:,:,b) = digitdata(randomorder(1+(b-1)*batchsize:b*batchsize), :);
	  testbatchtargets(:,:,b) = targets(randomorder(1+(b-1)*batchsize:b*batchsize), :);
	enddo

deallocate(randomorder, digitdata, targets)

end subroutine makebatches
