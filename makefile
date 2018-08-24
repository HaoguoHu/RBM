#!! Haoguo Hu, Dec. 07, 2009. University of Michigan, Ann Arbor.

F90 = gfortran 
EXE = 0hinton 
SWITCH = -O3   

SRC =module.f90 mnistdeepauto.f90 CG_MNIST.f90 backprop.f90 makebatches.f90 \
matrix.f90 minimize.f90 mnistdisp.f90 normal.f90  \
randn.f90 ranperm.f90 rbm.f90 rbmhidlinear.f90 \
statistics.f90 unroll.f90

OBJS = ${SRC:.f90=.o}

%.o: %.f90
	$(F90) $(SWITCH) -c $<
	
$(EXE):$(OBJS)
	$(F90) $(SWITCH)  -o $(EXE) $(OBJS)

clean:
	rm -f  $(EXE) $(OBJS) dataC.mod  
