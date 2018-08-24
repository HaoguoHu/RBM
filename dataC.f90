MODULE DataC

	integer,parameter::maxepoch=1
	integer::numbatches=600, batchsize=100, numcases=100
		
	integer::numhid, numdims, seed
	double precision,dimension(:,:,:),allocatable::batchdata  
!	double precision,dimension(:,:,:),allocatable::testbatchdata 	
	double precision::testbatchdata(100, 784,  100) 	
	integer::restart, callrbm	
	  
END MODULE DataC


