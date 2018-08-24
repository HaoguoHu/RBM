! Version 1.000
!
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

! This program fine-tunes an autoencoder with backpropagation.
! Weights of the autoencoder are going to be saved in mnist_weights.mat
! and trainig and test reconstruction errors in mnist_error.mat
! You can also set maxepoch, default value is 200 as in our paper.  

subroutine backprop()
	use dataC
implicit none 

integer:: N, l1,l2,l3,l4,l5,l6,l7,l8,l9, epoch, batch, nf=20
integer::i1,j1,i2,j2,i3,j3,i4,j4, ii, i, j, kk, xxx
integer::k1,k2,k3,k4,k5,k6,k7,k8,k12,k13,k14,k15,k16,k17,k18

double precision,dimension(:,:),allocatable::w1,w2,w3,w4, w5,w6,w7,w8
double precision,dimension(:,:),allocatable:: datab, output, dataout
double precision,dimension(:,:),allocatable:: w1probs,w2probs,w3probs,w4probs,w5probs,w6probs,w7probs,w8probs

double precision,dimension(:,:),allocatable::vishid, hidpen, hidpen2, hidtop


double precision,dimension(:),allocatable:: penrecbiases, hidgenbiases, hidrecbiases, visbiases
double precision,dimension(:),allocatable:: penrecbiases2, hidgenbiases2
double precision,dimension(:),allocatable:: toprecbiases, topgenbiases
double precision,dimension(:),allocatable:: w1u,w2u,w3u,w4u,w5u,w6u,w7u,w8u

double precision::err, train_err(maxepoch), test_err(maxepoch)
 character*80::atmp

integer::testnumcases, testnumdims, testnumbatches, tt, Dim(9), max_iter
double precision::X(2837314)


write(*,*)'Fine-tuning deep autoencoder by minimizing cross entropy error.'
write(*,*)'60 batches of 1000 cases each.'

	open(nf, file='callrbm01.dat',status='old')
	read(nf, *) i1, j1  !1000 784, 
	write(*,*)  i1, j1
	allocate( vishid(j1,i1), hidrecbiases(i1), visbiases(j1) )
!	read(nf, *) vishid, hidrecbiases, visbiases
	do j=1, j1; read(nf,*)vishid(j,:); enddo  
		read(nf,*) hidrecbiases
		read(nf,*) visbiases
	close(nf)
	
	open(nf, file='callrbm02.dat',status='old')
	read(nf, *) i2, j2  !500 1000,
	write(*,*)  i2, j2
	allocate( hidpen(j2, i2), penrecbiases(i2), hidgenbiases(j2)  )
!	read(nf, *) hidpen, penrecbiases, hidgenbiases
	do j=1, j2; read(nf,*)hidpen(j,:); enddo  
		read(nf,*) penrecbiases
		read(nf,*) hidgenbiases
	close(nf)
	
	open(nf, file='callrbm03.dat',status='old')
	read(nf, *) i3, j3  ! 250 500,
	write(*,*)  i3, j3
	allocate( hidpen2(j3, i3), penrecbiases2(i3), hidgenbiases2(j3) )
!	read(nf, *) hidpen2, penrecbiases2, hidgenbiases2
	do j=1, j3; read(nf,*)hidpen2(j,:); enddo  
		read(nf,*) penrecbiases2
		read(nf,*) hidgenbiases2
	close(nf)
	
	open(nf, file='callrbm04.dat',status='old')
	read(nf, *) i4, j4  !30 250, 
	write(*,*)  i4, j4
	allocate( hidtop(j4, i4), toprecbiases(i4), topgenbiases(j4) )
!	read(nf, *) hidtop, toprecbiases, topgenbiases
	do j=1, j4; read(nf,*)hidtop(j,:); enddo  
		read(nf,*) toprecbiases
		read(nf,*) topgenbiases
	close(nf)

!goto 139	
!	open(nf, file='../../Geoffrey_Hinton/mnistvh',status='old')
!		do ii=1,5; read(nf,'(a)')atmp ; enddo
!		do i=1,j1; read(nf,*)vishid(i,:); enddo
!		do ii=1,6; read(nf,'(a)')atmp;  enddo
!		 read(nf,*)hidrecbiases 
!		do ii=1,6; read(nf,'(a)')atmp;  enddo
!		 read(nf,*)visbiases 
!	close(nf)
!	
!	open(nf, file='../../Geoffrey_Hinton/mnisthp',status='old')
!		do ii=1,5; read(nf,'(a)')atmp ;  enddo
!		do i=1,j2; read(nf,*)hidpen(i,:); enddo
!		do ii=1,6; read(nf,'(a)')atmp ;  enddo
!		read(nf,*)penrecbiases
!		do ii=1,6; read(nf,*); enddo
!		read(nf,*)hidgenbiases
!	close(nf)
!	
!	open(nf, file='../../Geoffrey_Hinton/mnisthp2',status='old')
!		do ii=1,5; read(nf,'(a)')atmp ; enddo
!		do i=1,j3; read(nf,*)hidpen2(i,:); enddo
!		do ii=1,6;read(nf,'(a)')atmp ; enddo
!		read(nf,*)penrecbiases2
!		do ii=1,6; read(nf,*); enddo
!		read(nf,*)hidgenbiases2
!	close(nf)
!	
!	open(nf, file='../../Geoffrey_Hinton/mnistpo',status='old')
!		do ii=1,5; read(nf,'(a)')atmp ;enddo
!		do i=1,j4; read(nf,*)hidtop(i,:); enddo
!		do ii=1,6; read(nf,'(a)')atmp ;enddo
!		read(nf,*)toprecbiases
!		do ii=1,6; read(nf,*); enddo
!		read(nf,*)topgenbiases
!	close(nf)
!139 continue	
	
	call makebatches()
	
	numcases =   size(batchdata, 1)
	numdims =    size(batchdata, 2)	
	numbatches = size(batchdata, 3)		
	N = numcases 
	
	allocate(w1(j1+1, i1),w2(j2+1, i2),w3(j3+1, i3),w4(j4+1, i4))
	allocate(w5(i4+1, j4),w6(i3+1, j3),w7(i2+1, j2),w8(i1+1, j1) )	
	
!!!!! PREINITIALIZE WEIGHTS OF THE AUTOENCODER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
w1(1:j1,:)= vishid(:,:); w1(j1+1, :)= hidrecbiases
w2(1:j2,:)= hidpen(:,:); w2(j2+1, :)= penrecbiases
w3(1:j3,:)=hidpen2(:,:); w3(j3+1, :)= penrecbiases2
w4(1:j4,:)= hidtop(:,:); w4(j4+1, :)= toprecbiases
w5(1:i4,:)= transpose(hidtop);  w5(i4+1, :)= topgenbiases
w6(1:i3,:)= transpose(hidpen2); w6(i3+1, :)= hidgenbiases2
w7(1:i2,:)= transpose(hidpen);  w7(i2+1, :)= hidgenbiases
w8(1:i1,:)= transpose(vishid);  w8(i1+1, :)= visbiases 
!!!!!!!!!!! END OF PREINITIALIZATIO OF WEIGHTS  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
l1=size(w1,1)-1
l2=size(w2,1)-1
l3=size(w3,1)-1
l4=size(w4,1)-1
l5=size(w5,1)-1
l6=size(w6,1)-1
l7=size(w7,1)-1
l8=size(w8,1)-1
l9=l1 

test_err=0
train_err=0
	
	allocate(  datab(numcases, numdims+1))
	allocate(w1probs(numcases, i1+1))
	allocate(w2probs(numcases, i2+1))
	allocate(w3probs(numcases, i3+1))
	allocate(w4probs(numcases, i4+1))
	allocate(w5probs(numcases, j4+1))
	allocate(w6probs(numcases, j3+1))
	allocate(w7probs(numcases, j2+1))
	allocate(dataout(numcases, j1))
	
	
do epoch = 1, maxepoch
!!!!!!!!!!!!!!!!!!!!! COMPUTE TRAINING RECONSTRUCTION ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	err=0 
	numcases =   size(batchdata, 1)
	numdims =    size(batchdata, 2)	
	numbatches = size(batchdata, 3)
	
	N = numcases
	do batch = 1, numbatches	
	if(mod(batch,100).eq.0)write(*,*)'batch =', batch
	
	datab(:, 1:numdims) = batchdata(:,:,1)	!datab(100,784) here
!	datab(:, 1:numdims) = batchdata(:,:,batch)	!datab(100,784) here
	datab(:, numdims+1) = 1 

	w1probs(:, 1:i1) = 1./(1.+exp(matmul(-datab, w1))); w1probs(:, i1+1) = 1.;!100x1001	
	w2probs(:, 1:i2)=1./(1.+exp(matmul(-w1probs, w2))); w2probs(:, i2+1) = 1  !100x501
	w3probs(:, 1:i3)=1./(1.+exp(matmul(-w2probs, w3))); w3probs(:, i3+1) = 1  !100x251
	w4probs(:, 1:i4)= matmul(w3probs, w4);   w4probs(:, i4+1) = 1             !100x31
	w5probs(:, 1:j4)=1./(1.+exp(matmul(-w4probs, w5))); w5probs(:, j4+1) = 1  !100x251
	w6probs(:, 1:j3)=1./(1.+exp(matmul(-w5probs, w6))); w6probs(:, j3+1) = 1  !100x501
	w7probs(:, 1:j2)=1./(1.+exp(matmul(-w6probs, w7))); w7probs(:, j2+1) = 1  !100x1001
	dataout =        1./(1.+exp(matmul(-w7probs, w8)))               !dataout (100x784)
	
	err = err +  1./N*sum((datab(:,1:numdims)-dataout)**2) ;	
	enddo
	
   train_err(epoch)=err/numbatches

!!!!!!!!!!!!!!! END OF COMPUTING TRAINING RECONSTRUCTION ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! DISPLAY FIGURE TOP ROW double precision DATA BOTTOM ROW RECONSTRUCTIONS !!!!!!!!!!!!!!!!!!!!!!!!!
write(*,*)'Displaying in figure 1: Top row - double precision data, Bottom row -- reconstructions '
	
	if(epoch == 1)then
		allocate(output(2*numdims, 15))
	 do ii=1,15
	  output(1:numdims,ii) =  datab(ii,1:numdims)
	  output(numdims+1:2*numdims,ii) = dataout(ii,:)
	 enddo	 
	 call mnistdisp(output, 2*numdims, 15)
	 	deallocate(output)
	endif

!!!!!!!!!!!!!!!!!!!!! COMPUTE TEST RECONSTRUCTION ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	testnumcases = size(testbatchdata, 1)
	testnumdims =  size(testbatchdata, 2)
	testnumbatches = size(testbatchdata, 3)
	
	write(*,*)testnumcases, testnumdims, testnumbatches
	
	N=testnumcases
	err=0
	
	do batch = 1, testnumbatches	
	if(mod(batch,100).eq.0)write(*,*)'batch =', batch
	
	datab(:, 1:testnumdims) = testbatchdata(:,:,batch)	!datat(100,784) here
	datab(:, testnumdims+1) = 1 

	w1probs(:, 1:i1) = 1./(1.+exp(matmul(-datab, w1))); w1probs(:, i1+1) = 1.;!100x1001	
	w2probs(:, 1:i2)=1./(1.+exp(matmul(-w1probs, w2))); w2probs(:, i2+1) = 1 !100x501
	w3probs(:, 1:i3)=1./(1.+exp(matmul(-w2probs, w3))); w3probs(:, i3+1) = 1 !100x251
	w4probs(:, 1:i4)= matmul(w3probs, w4);   w4probs(:, i4+1) = 1            !100x31
	w5probs(:, 1:j4)=1./(1.+exp(matmul(-w4probs, w5))); w5probs(:, j4+1) = 1 !100x251
	w6probs(:, 1:j3)=1./(1.+exp(matmul(-w5probs, w6))); w6probs(:, j3+1) = 1 !100x501
	w7probs(:, 1:j2)=1./(1.+exp(matmul(-w6probs, w7))); w7probs(:, j2+1) = 1 !100x1001
	dataout =        1./(1.+exp(matmul(-w7probs, w8)))               !dataout(100x784)
	
	err = err +  1./N*sum((datab(:,1:testnumdims)-dataout)**2) ;	
	enddo
	
   test_err(epoch)=err/testnumbatches

![testnumcases testnumdims testnumbatches]=size(testbatchdata)
!N=testnumcases
!err=0
!for batch = 1:testnumbatches
!  data = [testbatchdata(:,:,batch)]
!  data = [data ones(N,1)]
!  w1probs = 1./(1 + exp(-data*w1)) w1probs = [w1probs  ones(N,1)]
!  w2probs = 1./(1 + exp(-w1probs*w2)) w2probs = [w2probs ones(N,1)]
!  w3probs = 1./(1 + exp(-w2probs*w3)) w3probs = [w3probs  ones(N,1)]
!  w4probs = w3probs*w4 w4probs = [w4probs  ones(N,1)]
!  w5probs = 1./(1 + exp(-w4probs*w5)) w5probs = [w5probs  ones(N,1)]
!  w6probs = 1./(1 + exp(-w5probs*w6)) w6probs = [w6probs  ones(N,1)]
!  w7probs = 1./(1 + exp(-w6probs*w7)) w7probs = [w7probs  ones(N,1)]
!  dataout = 1./(1 + exp(-w7probs*w8))
!  err = err +  1/N*sum(sum( (data(:,1:end-1)-dataout).^2 ))
!  end
! test_err(epoch)=err/testnumbatches
 write(*,*)epoch,'Before epoch Train, squared error:', train_err(epoch), 'Test squared error:',test_err(epoch)

!!!!!!!!!!!!!!! END OF COMPUTING TEST RECONSTRUCTION ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  deallocate(datab)
  allocate(datab(10*numcases, 784))
  
 tt=0
 do batch=1, numbatches/10
 write(*,*)'epoch=',epoch, ' batch=',batch


!!!!!!!!!!!! COMBINE 10 MINIBATCHES INTO 1 LARGER MINIBATCH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 tt=tt+1  
 do kk =1, 10
 	datab((kk-1)*numcases+1:kk*numcases, :) = batchdata(:,:,(tt-1)*10+kk)
 enddo


!!!!!!!!!!!!!!!! PERFORM CONJUGATE GRADIENT WITH 3 LINESEARCHES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !X(2837314,1)
     k1 = size(w1, 1) * size(w1, 2)
	 k2 = size(w2, 1) * size(w2, 2)
	 k3 = size(w3, 1) * size(w3, 2)
	 k4 = size(w4, 1) * size(w4, 2)
	 k5 = size(w5, 1) * size(w5, 2)
	 k6 = size(w6, 1) * size(w6, 2)
	 k7 = size(w7, 1) * size(w7, 2)
	 k8 = size(w8, 1) * size(w8, 2)
	 k12 = k1 +  k2
	 k13 = k12 + k3
	 k14 = k13 + k4
	 k15 = k14 + k5
	 k16 = k15 + k6
	 k17 = k16 + k7
	 k18 = k17 + k8	
	  
	 allocate(w1u(k1),w2u(k2),w3u(k3),w4u(k4),w5u(k5),w6u(k6),w7u(k7),w8u(k8))
	 
    w1u = reshape(w1, (/k1/))  !(/785*1000/))
	w2u = reshape(w2, (/k2/))  !(/1001*500/))
	w3u = reshape(w3, (/k3/))  !(/501*250/))
	w4u = reshape(w4, (/k4/))  !(/251*30/))
	w5u = reshape(w5, (/k5/))  !(/31*250/))
	w6u = reshape(w6, (/k6/))  !(/251*500/))
	w7u = reshape(w7, (/k7/))  !(/501*1000/))
	w8u = reshape(w8, (/k8/))  !(/1001*784/))
	
	X(1:k1     ) = w1u
	X(k1+1:k12 ) = w2u
	X(k12+1:k13) = w3u
	X(k13+1:k14) = w4u
	X(k14+1:k15) = w5u
	X(k15+1:k16) = w6u
	X(k16+1:k17) = w7u
	X(k17+1:k18) = w8u
	
	deallocate(w1u,w2u,w3u,w4u,w5u,w6u,w7u,w8u)
	
	Dim(1)=l1  !784
	Dim(2)=l2  !1000
	Dim(3)=l3  !500
	Dim(4)=l4  !250
	Dim(5)=l5  !30
	Dim(6)=l6  !250
	Dim(7)=l7  !500
	Dim(8)=l8  !1000
	Dim(9)=l9  !784
	

!	write(*,*)'before minimize'	
    max_iter=3
	call minimize(X, max_iter, Dim, datab)
!	write(*,*)'after minimize'
	
!  [X, fX] = minimize(X,'CG_MNIST',max_iter,Dim,data)
  w1 = reshape(X(1:(l1+1)*l2),(/l1+1,l2/))
  xxx = (l1+1)*l2
  w2 = reshape(X(xxx+1:xxx+(l2+1)*l3),(/l2+1,l3/))
  xxx = xxx+(l2+1)*l3
  w3 = reshape(X(xxx+1:xxx+(l3+1)*l4),(/l3+1,l4/))
  xxx = xxx+(l3+1)*l4
  w4 = reshape(X(xxx+1:xxx+(l4+1)*l5),(/l4+1,l5/))
  xxx = xxx+(l4+1)*l5
  w5 = reshape(X(xxx+1:xxx+(l5+1)*l6),(/l5+1,l6/))
  xxx = xxx+(l5+1)*l6
  w6 = reshape(X(xxx+1:xxx+(l6+1)*l7),(/l6+1,l7/))
  xxx = xxx+(l6+1)*l7
  w7 = reshape(X(xxx+1:xxx+(l7+1)*l8),(/l7+1,l8/))
  xxx = xxx+(l7+1)*l8
  w8 = reshape(X(xxx+1:xxx+(l8+1)*l9),(/l8+1,l9/))

!!!!!!!!!!!!!!!! END OF CONJUGATE GRADIENT WITH 3 LINESEARCHES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 enddo

!save mnist_weights w1 w2 w3 w4 w5 w6 w7 w8 
!save mnist_error test_err train_err;
	open(nf, file='mnist_weights.dat',status='unknown')
	write(nf,*)w1,w2,w3,w4,w5,w6,w7,w8
	close(nf)
	
	open(nf, file='mnist_error.dat',status='unknown')
	write(nf,*)test_err, train_err
	close(nf)


enddo


deallocate(vishid, hidrecbiases, visbiases)
deallocate(hidpen, penrecbiases, hidgenbiases)
deallocate(hidpen2, penrecbiases2, hidgenbiases2)
deallocate(hidtop, toprecbiases, topgenbiases)

deallocate(datab)
deallocate(w1,w2,w3,w4,w5,w6,w7,w8)
deallocate(w1probs,w2probs,w3probs,w4probs,w5probs,w6probs,w7probs, dataout)


end subroutine backprop

