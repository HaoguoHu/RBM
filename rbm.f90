
! Version 1.000 
! Original Matlab/Octave Code provided by Geoff Hinton and Ruslan Salakhutdinov 
! Fortran Code provided by Haoguo Hu==
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
!
! This program trains Restricted Boltzmann Machine in which
! visible, binary, stochastic pixels are connected to
! hidden, binary, stochastic feature detectors using symmetrically
! weighted connections. Learning is done with 1-step Contrastive Divergence.   
! The program assumes that the following variables are set externally:
! maxepoch  -- maximum number of epochs
! numhid    -- number of hidden units 
! batchdata -- the data that is divided into batches (numcases numdims numbatches)
! restart   -- set to 1 if learning starts from beginning 

subroutine rbm()
  USE DataC
  implicit none
  
double precision::epsilonw      = 0.1   ! Learning rate for weights 
double precision::epsilonvb     = 0.1   ! Learning rate for biases of visible units 
double precision::epsilonhb     = 0.1   ! Learning rate for biases of hidden units 
double precision::weightcost  = 0.0002   
double precision::initialmomentum  = 0.5  
double precision::finalmomentum    = 0.9

integer::batch, epoch, i, nf = 20  !, maxepoch=1
double precision,dimension(numdims, numhid)::vishid,posprods,negprods,vishidinc

double precision,dimension(numcases, numhid)::poshidprobs,neghidprobs
double precision,dimension(numcases, numhid)::poshidstates 

double precision,dimension(numcases, numhid)::rep1, randtmp

double precision,dimension(numcases,numdims)::rep2, negdata
double precision,dimension(numcases,numdims)::datab

double precision,dimension(numdims)::hidgenbiases,hidgenbiases2,topgenbiases
double precision,dimension(numdims)::posvisact, negvisact,visbiasinc,visbiases 

double precision,dimension(numhid)::poshidact, neghidact ,hidbiasinc, hidbiases
double precision,dimension(numhid):: hidrecbiases,penrecbiases,penrecbiases2, toprecbiases


double precision::batchposhidprobs( numcases, numhid, numbatches)
double precision::errsum,err,momentum  

 character(len=2) :: callrbma

	
if (restart ==1) then
    restart=0		

!!After a long searching on internet, I didn't find any randn subroutine in Fortran.
!!Finally, I wrote my own randn subroutine in Fortran.

	call randn(numdims, numhid,  vishid )  !W(i,j), normal distribution 
	
    vishid = 0.1* vishid    !W(i,j), normal distribution 
	hidbiases = 0           !hide biases
	visbiases = 0           !visual biases
	
	poshidprobs = 0         !positive hide probabilities 
	neghidprobs = 0         !negtive hide probabilities 
	posprods = 0            !positive products
	negprods = 0            !negtive  products
	vishidinc = 0           !visual  hide increase
	hidbiasinc = 0          !hide   biase increase
	visbiasinc = 0          !visual biase increase
	batchposhidprobs = 0    !batch positive hide probabilities
	epoch = 1
endif

do epoch = epoch, maxepoch
	errsum=0
	do batch = 1, numbatches
	if(mod(batch,100).eq.0)write(*,'(a,i3,6x,a,i5)')'epoch=',epoch, 'batch=',batch 

!!!!!!!!!! START POSITIVE PHASE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   	datab = batchdata(:,:,batch)

	forall(i=1:numcases)
	  rep1(i, :)=hidbiases
	  rep2(i, :)=visbiases
	end forall
	
	poshidprobs = 1./(1 + exp(- matmul(datab, vishid)- rep1))   !numcases*numhid 
    batchposhidprobs(:,:,batch)=poshidprobs                     !numcases*numhid*batches
    posprods    = matmul( transpose(datab), poshidprobs)        !numdims*numhid  (v,h)
    poshidact   = sum(poshidprobs, 1)                           !numhid  !positive hide activer
    posvisact   = sum(datab, 1)                                 !numdims !positive visual activer
!!!!!!!!!! END OF POSITIVE PHASE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	call random_number( randtmp  ) !randtmp(numcases, numhid)
	
	where(poshidprobs > randtmp)
		poshidstates =  1
	else where
		poshidstates = 0
	end where

!!!!!!!!!! START NEGATIVE PHASE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      negdata =     1./(1 + exp( - matmul( poshidstates, transpose(vishid)) - rep2)) !numcases*numdims
      neghidprobs = 1./(1 + exp( - matmul( negdata, vishid) - rep1))                 !numcases*numhid 
      negprods  = matmul(transpose(negdata),  neghidprobs)                           !numdims*numhid
      neghidact = sum(neghidprobs, 1)                                                !1*numhis !negtive hide activer
      negvisact = sum(negdata, 1)                                                    !1*numdims !negtive visual activer
!!!!!!!!!! END OF NEGATIVE PHASE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	  err= sum( (datab - negdata)**2 )
	  errsum = err + errsum

   if (epoch > 5) then
      momentum = finalmomentum
   else
      momentum = initialmomentum
   endif

!!!!!!!!!! UPDATE WEIGHTS AND BIASES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    vishidinc = momentum*vishidinc + &
                epsilonw*( (posprods-negprods)/numcases - weightcost*vishid)
    visbiasinc = momentum*visbiasinc + (epsilonvb/numcases)*(posvisact-negvisact)
    hidbiasinc = momentum*hidbiasinc + (epsilonhb/numcases)*(poshidact-neghidact)

    vishid = vishid + vishidinc
    visbiases = visbiases + visbiasinc
    hidbiases = hidbiases + hidbiasinc
!!!!!!!!!!!!!!!!! END OF UPDATES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  enddo !do batch = 1, numbatches
  
    write(*,*)'epoch', epoch, 'error', errsum
enddo   !do epoch = epoch, maxepoch

    deallocate(batchdata)	
	allocate(batchdata(numcases, numhid, numbatches))	
	batchdata=batchposhidprobs
	
	write(callrbma,'(i2.2)')callrbm
	open(nf, file='callrbm'//callrbma//'.dat',status='unknown')
		write(nf,*) numhid, numdims
!		write(nf,*)vishid,  hidbiases, visbiases
		do i=1, numdims; write(nf,*)vishid(i,:); enddo  
		write(nf,*) hidbiases
		write(nf,*) visbiases
	close(nf)	
	callrbm = callrbm + 1			

end subroutine rbm
