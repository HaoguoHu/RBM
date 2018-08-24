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

!call minimize(VV, 'CG_MNIST',max_iter,Dim)
!subroutine minimize(X, f, length, varargin)
![f0 df0] = feval(f, X, varargin{:});    

!function [f, df] = CG_MNIST(VV,Dim,XX);
subroutine CG_MNIST(VV, Dims, XX0, f,  df)
use dataC
implicit none 


integer,parameter::tnumcases = 10*100
double precision::f, XX0(tnumcases, 784), XX(tnumcases, 784+1)  !datab(numcases, numdims+1)
double precision,dimension(2837314)::df, VV 
integer:: xxx
double precision,dimension(:,:),allocatable:: w1,w2,w3,w4, w5,w6,w7,w8
double precision,dimension(:,:),allocatable:: w1probs,w2probs,w3probs,w4probs,w5probs,w6probs,w7probs,w8probs
double precision,dimension(:),allocatable:: w1u,w2u,w3u,w4u,w5u,w6u,w7u,w8u

double precision,dimension(:,:),allocatable:: dw1,dw2,dw3,dw4,dw5,dw6,dw7,dw8 
double precision,dimension(:,:),allocatable:: XXout
double precision,dimension(:,:),allocatable::Ix1,Ix2,Ix3,Ix4,Ix5,Ix6,Ix7,Ix8
double precision,dimension(:,:),allocatable::Ix11,Ix21,Ix31,Ix41,Ix51,Ix61,Ix71
integer:: N, l1,l2,l3,l4,l5,l6,l7,l8,l9, Dims(9)
integer::k1,k2,k3,k4,k5,k6,k7,k8,k12,k13,k14,k15,k16,k17,k18

l1 = Dims(1);
l2 = Dims(2);
l3 = Dims(3);
l4 = Dims(4);
l5 = Dims(5);
l6 = Dims(6);
l7 = Dims(7);
l8 = Dims(8);
l9 = Dims(9);
N  = size(XX,1);
   
   
	allocate(w1(l1+1, l2),w2(l2+1, l3),w3(l3+1, l4),w4(l4+1, l5))
	allocate(w5(l4+1, l6),w6(l6+1, l7),w7(l7+1, l8),w8(l8+1, l9) )	
	
! Do decomversion.
 w1 = reshape(VV(1:(l1+1)*l2),(/l1+1,l2/));
 xxx = (l1+1)*l2;
 w2 = reshape(VV(xxx+1:xxx+(l2+1)*l3),(/l2+1,l3/));
 xxx = xxx+(l2+1)*l3;
 w3 = reshape(VV(xxx+1:xxx+(l3+1)*l4),(/l3+1,l4/));
 xxx = xxx+(l3+1)*l4;
 w4 = reshape(VV(xxx+1:xxx+(l4+1)*l5),(/l4+1,l5/));
 xxx = xxx+(l4+1)*l5;
 w5 = reshape(VV(xxx+1:xxx+(l5+1)*l6),(/l5+1,l6/));
 xxx = xxx+(l5+1)*l6;
 w6 = reshape(VV(xxx+1:xxx+(l6+1)*l7),(/l6+1,l7/));
 xxx = xxx+(l6+1)*l7;
 w7 = reshape(VV(xxx+1:xxx+(l7+1)*l8),(/l7+1,l8/));
 xxx = xxx+(l7+1)*l8;
 w8 = reshape(VV(xxx+1:xxx+(l8+1)*l9),(/l8+1,l9/));

	
	XX(:, 1:784) = XX0
    XX(:,784+1) = 1
	allocate(w1probs(tnumcases, l2+1))
	allocate(w2probs(tnumcases, l3+1))
	allocate(w3probs(tnumcases, l4+1))
	allocate(w4probs(tnumcases, l5+1))
	allocate(w5probs(tnumcases, l6+1))
	allocate(w6probs(tnumcases, l7+1))
	allocate(w7probs(tnumcases, l8+1))
	allocate(XXout(tnumcases, l9))
	
	w1probs(:, 1:l2) = 1./(1.+exp(matmul(-XX, w1))); w1probs(:, l2+1) = 1.;  !100x1001	
	w2probs(:, 1:l3)=1./(1.+exp(matmul(-w1probs, w2))); w2probs(:, l3+1) = 1 !100x501
	w3probs(:, 1:l4)=1./(1.+exp(matmul(-w2probs, w3))); w3probs(:, l4+1) = 1 !100x251
	w4probs(:, 1:l5)= matmul(w3probs, w4);   w4probs(:, l5+1) = 1            !100x31
	w5probs(:, 1:l6)=1./(1.+exp(matmul(-w4probs, w5))); w5probs(:, l6+1) = 1 !100x251
	w6probs(:, 1:l7)=1./(1.+exp(matmul(-w5probs, w6))); w6probs(:, l7+1) = 1 !100x501
	w7probs(:, 1:l8)=1./(1.+exp(matmul(-w6probs, w7))); w7probs(:, l8+1) = 1 !100x1001
	xxout = 1./(1.+exp(matmul(-w7probs, w8)))               !dataout(100x784)
	
	allocate(Ix8(tnumcases, l9));allocate(dw8(l8+1, l9))
	
	allocate(Ix7(tnumcases, l8+1));
	allocate(Ix6(tnumcases, l7+1));
	allocate(Ix5(tnumcases, l6+1));
	allocate(Ix4(tnumcases, l5+1));
	allocate(Ix3(tnumcases, l4+1));
	allocate(Ix2(tnumcases, l3+1));
	allocate(Ix1(tnumcases, l2+1));
	
	allocate(Ix71(tnumcases, l8));allocate(dw7(l7+1, l8))
	allocate(Ix61(tnumcases, l7));allocate(dw6(l6+1, l7))
	allocate(Ix51(tnumcases, l6));allocate(dw5(l5+1, l6))
	allocate(Ix41(tnumcases, l5));allocate(dw4(l4+1, l5))
	allocate(Ix31(tnumcases, l4));allocate(dw3(l3+1, l4))
	allocate(Ix21(tnumcases, l3));allocate(dw2(l2+1, l3))
	allocate(Ix11(tnumcases, l2));allocate(dw1(l1+1, l2))
	
!	write(*,*)'CG line 109', sum(xx0),sum(xxout), N
!	write(*,*)sum( XX0*log(XXout) +(1-XX0)*log(1 - XXout) )
	f = -1.*sum( XX0*log(XXout) +(1-XX0)*log(1 - XXout) )/N;
!	write(*,*)'CG 112 f=',f
!	pause
	
	Ix8 = 1.*(XXout - XX(:,1:l9))/N;
	dw8 =  matmul(transpose(w7probs), Ix8);
	Ix7 = matmul(Ix8, transpose(w8)) *  w7probs*(1-w7probs);
	Ix71 = Ix7(:,1:l8)
	dw7 =  matmul(transpose(w6probs), Ix71);
	Ix6 =matmul(Ix71, transpose(w7))* (w6probs *(1-w6probs));
	Ix61 = Ix6(:,1:l7) 
	dw6 =  matmul(transpose(w5probs), Ix61);
	Ix5 = matmul(Ix61, transpose(w6)) * (w5probs *(1-w5probs));
	Ix51 = Ix5(:,1:l6)
	dw5 =  matmul(transpose(w4probs), Ix51);
	Ix4 = matmul(Ix51, transpose(w5))
	Ix41 = Ix4(:,1:l5)
	dw4 = matmul(transpose(w3probs), Ix41);
	Ix3 = matmul(Ix41, transpose(w4))*  (w3probs*(1-w3probs)); 
	Ix31 = Ix3(:, 1:l4)
	dw3 =  matmul(transpose(w2probs), Ix31);
	Ix2 = matmul(Ix31, transpose(w3))*  (w2probs* (1-w2probs)); 
	Ix21 = Ix2(:, 1:l3)
	dw2 =  matmul(transpose(w1probs), Ix21);
	Ix1 =  matmul(Ix21, transpose(w2))*  (w1probs* (1-w1probs)); 
	Ix11 = Ix1(:, 1:l2)
	dw1 =  matmul(transpose(XX), Ix11);
	
!	write(*,*)sum(dw1),sum(dw2),sum(dw3),sum(dw4)
!	write(*,*)sum(dw5),sum(dw6),sum(dw7),sum(dw8)
	 k1 = size(dw1, 1) * size(dw1, 2)
	 k2 = size(dw2, 1) * size(dw2, 2)
	 k3 = size(dw3, 1) * size(dw3, 2)
	 k4 = size(dw4, 1) * size(dw4, 2)
	 k5 = size(dw5, 1) * size(dw5, 2)
	 k6 = size(dw6, 1) * size(dw6, 2)
	 k7 = size(dw7, 1) * size(dw7, 2)
	 k8 = size(dw8, 1) * size(dw8, 2)
	 k12 = k1 +  k2
	 k13 = k12 + k3
	 k14 = k13 + k4
	 k15 = k14 + k5
	 k16 = k15 + k6
	 k17 = k16 + k7
	 k18 = k17 + k8	
	  
	 allocate(w1u(k1),w2u(k2),w3u(k3),w4u(k4),w5u(k5),w6u(k6),w7u(k7),w8u(k8))
	 
    w1u = reshape(dw1, (/k1/))  !(/785*1000/))
	w2u = reshape(dw2, (/k2/))  !(/1001*500/))
	w3u = reshape(dw3, (/k3/))  !(/501*250/))
	w4u = reshape(dw4, (/k4/))  !(/251*30/))
	w5u = reshape(dw5, (/k5/))  !(/31*250/))
	w6u = reshape(dw6, (/k6/))  !(/251*500/))
	w7u = reshape(dw7, (/k7/))  !(/501*1000/))
	w8u = reshape(dw8, (/k8/))  !(/1001*784/))
	
	df(1:k1     ) = w1u
	df(k1+1:k12 ) = w2u
	df(k12+1:k13) = w3u
	df(k13+1:k14) = w4u
	df(k14+1:k15) = w5u
	df(k15+1:k16) = w6u
	df(k16+1:k17) = w7u
	df(k17+1:k18) = w8u
	
deallocate(w1,w2,w3,w4,w5,w6,w7,w8)
deallocate(w1probs,w2probs,w3probs,w4probs,w5probs,w6probs,w7probs, XXout)
deallocate(w1u,w2u,w3u,w4u,w5u,w6u,w7u,w8u)	
deallocate(dw1,dw2,dw3,dw4,dw5,dw6,dw7,dw8)
deallocate(Ix1,Ix2,Ix3,Ix4,Ix5,Ix6,Ix7,Ix8)
deallocate(Ix11,Ix21,Ix31,Ix41,Ix51,Ix61,Ix71)
	 
end subroutine CG_MNIST
