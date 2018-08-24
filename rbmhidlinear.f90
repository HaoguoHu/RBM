! Version 1.000
! Code provided by Ruslan Salakhutdinov and Geoff Hinton
! Fortran Code provided by Haoguo Hu
!!
! Permission is granted for anyone to copy, use, modify, or distribute this
! program and accompanying programs and documents for any purpose, provided
! this copyright notice is retained and prominently displayed, along with
! a note saying that the original programs are available from our
! web page.
! The programs and documents are distributed without any warranty, express or
! implied.  As the programs were written for research purposes only, they have
! not been tested to the degree that would be advisable in any important
! application.  All use of these programs is entirely at the user's own risk.
!
! This program trains Restricted Boltzmann Machine in which
! visible, binary, stochastic pixels are connected to
! hidden, to chastic double precision-valued feature detectors drawn from a unit
! variance Gaussian whose mean is determined by the input from 
! the logistic visible units. Learning is done with 1-step Contrastive Divergence.
! The program assumes that the following variables are set externally:
! maxepoch  -- maximum number of epochs
! numhid    -- number of hidden units
! batchdata -- the data that is divided into batches (numcases numdims numbatches)
! restart   -- set to 1 if learning starts from beginning

subroutine rbmhidlinear()
  USE DataC
  implicit none
  
double precision::epsilonw      = 0.001 ! Learning rate for weights 
double precision::epsilonvb     = 0.001 ! Learning rate for biases of visible units
double precision::epsilonhb     = 0.001 ! Learning rate for biases of hidden units 
double precision::weightcost  = 0.0002  
double precision::initialmomentum  = 0.5
double precision::finalmomentum    = 0.9

double precision,dimension(numdims, numhid)::vishid,posprods,negprods,vishidinc
double precision,dimension(numcases, numhid)::poshidprobs,neghidprobs
double precision,dimension(numcases, numhid)::poshidstates 
double precision,dimension(numcases, numhid)::rep1, randtmp
double precision,dimension(numcases,numdims)::datab, rep2, negdata

double precision,dimension(numdims)::visbiases,visbiasinc,hidgenbiases,hidgenbiases2,topgenbiases
double precision,dimension(numdims)::posvisact, negvisact

double precision,dimension(numhid):: hidrecbiases,penrecbiases,penrecbiases2, toprecbiases
double precision,dimension(numhid)::hidbiases,hidbiasinc, poshidact, neghidact

double precision::batchposhidprobs( numcases, numhid, numbatches)
double precision::errsum,  err,momentum 


integer::batch, epoch, i, nf=20, j !, maxepoch=1
  character(len=2) :: callrbma

if (restart ==1) then
  restart=0
  epoch=1

!!After a long searching on internet, can't find any simmilar subroutine in Fortran
	call randn(numdims, numhid,  vishid )  !!Finally, I wrote my own subroutine
	
    vishid = 0.1 * vishid    
	
	hidbiases = 0
	visbiases = 0
	
	poshidprobs = 0
	neghidprobs = 0
	posprods = 0
	negprods = 0
	vishidinc = 0
	hidbiasinc = 0
	visbiasinc = 0
!	sigmainc = zeros(1,numhid);
	batchposhidprobs = 0
endif

 
do epoch = 1, maxepoch
	errsum=0
	
 do batch = 1, numbatches
	if(mod(batch,100).eq.0)write(*,'(a,i3,6x,a,i5)')'epoch=',epoch, 'batch=',batch 
	
!!!!!!!!! START POSITIVE PHASE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	datab = batchdata(:,:,batch)	
	forall(i=1:numcases)
		rep1(i, :)=hidbiases(:)
		rep2(i, :)=visbiases(:)
	end forall
	
	poshidprobs =  matmul(datab, vishid) + rep1   
    batchposhidprobs(:,:,batch)=poshidprobs
    posprods    = matmul(transpose(datab), poshidprobs)
    poshidact   = sum(poshidprobs, 1)
    posvisact = sum(datab, 1)
!!!!!!!!! END OF POSITIVE PHASE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	call randn( numcases, numhid,  randtmp )
	
	poshidstates = poshidprobs + randtmp
	
!!!!!!!!! START NEGATIVE PHASE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 	  negdata = 1./(1 + exp(matmul(-1*poshidstates, transpose(vishid)) - rep2))
      neghidprobs =  matmul(negdata, vishid) + rep1  
      negprods  = matmul( transpose(negdata), neghidprobs)
      neghidact = sum(neghidprobs, 1)
      negvisact = sum(negdata, 1) 
!!!!!!!!! END OF NEGATIVE PHASE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   err= sum( (datab - negdata)**2 )
	  errsum = err + errsum

   if (epoch > 5) then
      momentum = finalmomentum
   else
      momentum = initialmomentum
   endif
   
!!!!!!!!! UPDATE WEIGHTS AND BIASES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  	
	vishidinc = momentum*vishidinc + &
                epsilonw*( (posprods-negprods)/numcases - weightcost*vishid)
    visbiasinc = momentum*visbiasinc + (epsilonvb/numcases)*(posvisact-negvisact)
    hidbiasinc = momentum*hidbiasinc + (epsilonhb/numcases)*(poshidact-neghidact)

    vishid = vishid + vishidinc
    visbiases = visbiases + visbiasinc
    hidbiases = hidbiases + hidbiasinc
!!!!!!!!!!!!! END OF UPDATES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 enddo

	write(*,*)'epoch', epoch, 'error', errsum
enddo

    deallocate(batchdata)	

	write(callrbma,'(i2.2)')callrbm
	open(nf, file='callrbm'//callrbma//'.dat',status='unknown')
		write(nf,*)numhid, numdims
!		write(nf,*)vishid,  hidbiases, visbiases
		do i=1, numdims; write(nf,*)vishid(i,:); enddo  
		write(nf,*) hidbiases
		write(nf,*) visbiases
	close(nf)	
	callrbm = callrbm + 1	

end subroutine rbmhidlinear
