program main
	USE DataC
implicit none
! Version 1.000
! Code provided by Ruslan Salakhutdinov and Geoff Hinton  
!!! Fortran Code provided by Haoguo Hu
!!
! Permission is granted for anyone to copy, use, modify, or distribute this
! program and accompanying programs and documents for any purpose, provided
! this copyright notice is retained and prominently displayed, along with
! a note saying that the original programs are available from our 
! web page. 
! The programs and documents are distributed without any warranty, express or
! implied.  As the programs were written for research purposes only, they have
! not been tested to the degree that would be advisable in any important
! application.  All use of these programs is entirely at the user's own risk.


! This program pretrains a deep autoencoder for MNIST dataset
! You can set the maximum number of epochs for pretraining each layer
! and you can set the architecture of the multilayer net.

!clear all
!close all

	
!integer::maxepoch=1; !10;!In the Science paper we use maxepoch=50, but it works just fine. 

integer:: numpen=500, numpen2=250, numopen=30

 numhid=1000
 callrbm = 1

write(*,*)'Converting Raw files into Matlab format ';
!call converter; 

write(*,*)'Pretraining a deep autoencoder.';
write(*,'(a,i3,a)')'The Science paper used 50 epochs. This uses ', maxepoch,' epoches';
goto 1000	

  call makebatches 
  
  numdims = size(batchdata,2)
  write(*,*) numcases,  numdims,  numbatches 

  write(*,*)'Pretraining Layer 1 with RBM:  ',numdims,numhid;
  restart=1;
  
  call rbm()
	
!    hidrecbiases=hidbiases; 
!	save mnistvh vishid hidrecbiases visbiases;

	write(*,*)'Pretraining Layer 2 with RBM: ',numhid, numpen;		
	numdims = size(batchdata,2)	
	numhid = numpen
	restart=1;
	
   call rbm()
!hidpen=vishid; penrecbiases=hidbiases; hidgenbiases=visbiases;	
!save mnisthp hidpen penrecbiases hidgenbiases;

   write(*,*)'Pretraining Layer 3 with RBM:  ',numpen,numpen2;  
    numdims = size(batchdata,2)   
	numhid=numpen2;
    restart=1;
	
 	call rbm()

!hidpen2=vishid; penrecbiases2=hidbiases; hidgenbiases2=visbiases;
!save mnisthp2 hidpen2 penrecbiases2 hidgenbiases2;

	write(*,*)'Pretraining Layer 4 with RBM: ',numpen2,numopen;
	numdims = size(batchdata,2)		
	numhid=numopen; 
	restart=1;
	call rbmhidlinear()
  
!hidtop=vishid; toprecbiases=hidbiases; topgenbiases=visbiases;
!save mnistpo hidtop toprecbiases topgenbiases;
1000 continue
   
	call backprop; 

write(*,*)' Successed! ';

end
