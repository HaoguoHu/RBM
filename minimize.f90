subroutine minimize(X, length, Dims, datab)
implicit none

double precision, dimension(2837314)::X,df0,s, X0, DF0U, df3, itmp
double precision:: f0, fx, f2, f3, F0U, d3, f1, A, B, f4
integer:: length, Dims(9)
double precision::datab(10*100, 784)

!!===Fortran Code provided by Haoguo Hu====Jun. 2016===
!!
!function [X, fX, i] = minimize(X, f, length, varargin)
! Minimize a differentiable multivariate function. 
! Usage: [X, fX, i] = minimize(X, f, length, P1, P2, P3, ... )
!
! where the starting point is given by "X" (D by 1), and the function named in
! the string "f", must return a function value and a vector of partial
! derivatives of f wrt X, the "length" gives the length of the run: if it is
! positive, it gives the maximum number of line searches, if negative its
! absolute gives the maximum allowed number of function evaluations. You can
! (optionally) give "length" a second component, which will indicate the
! reduction in function value to be expected in the first line-search (defaults
! to 1.0). The parameters P1, P2, P3, ... are passed on to the function f.
!
! The function returns when either its length is up, or if no further progress
! can be made (ie, we are at a (local) minimum, or so close that due to
! numerical problems, we cannot get any closer). NOTE: If the function
! terminates within a few iterations, it could be an indication that the
! function values and derivatives are not consistent (ie, there may be a bug in
! the implementation of your "f" function). The function returns the found
! solution "X", a vector of function values "fX" indicating the progress made
! and "i" the number of iterations (line searches or function evaluations,
! depending on the sign of "length") used.
!
! The Polack-Ribiere flavour of conjugate gradients is used to compute search
! directions, and a line search using quadratic and cubic polynomial
! approximations and the Wolfe-Powell stopping criteria is used together with
! the slope ratio method for guessing initial step sizes. Additionally a bunch
! of checks are made to make sure that exploration is taking place and that
! extrapolation will not be unboundedly large.
! See also: checkgrad 
! Copyright (C) 2001 - 2006 by Carl Edward Rasmussen (2006-09-08).

double precision::INT = 0.1    ! don't reevaluate within 0.1 of the limit of the current bracket
double precision::EXT = 3.0                  ! extrapolate maximum 3 times the current step-size
integer::MAXx = 20                         ! max 20 function evaluations per line search
double precision::RATIO = 10                                       ! maximum allowed slope ratio
double precision::SIG = 0.1
double precision::RHO = 0.1/2.  !SIG/2 ! SIG and RHO are the constants controlling the Wolfe-
double precision::realmin = 0.  !2.2251e-38
double precision::inf = 1.0e+38

integer:: i, ls_failed, M, success, kk
	character::SA*20
double precision::red = 1., d0, x3, x2, d2, x1, d1, x4, d4

! Powell conditions. SIG is the maximum allowed absolute ratio between
! previous and new slopes (derivatives in the search direction), thus setting
! SIG to low (positive) values forces higher precision in the line-searches.
! RHO is the minimum allowed fraction of the expected (from the slope at the
! initial point in the linesearch). Constants must satisfy 0 < RHO < SIG < 1.
! Tuning of SIG (depending on the nature of the function to be optimized) may
! speed up the minimization; it is probably not worth playing much with RHO.

! The code falls naturally into 3 parts, after the initial line search is
! started in the direction of steepest descent. 1) we first enter a while loop
! which uses point 1 (p1) and (p2) to compute an extrapolation (p3), until we
! have extrapolated far enough (Wolfe-Powell conditions). 2) if necessary, we
! enter the second loop which takes p2, p3 and p4 chooses the subinterval
! containing a (local) minimum, and interpolates it, unil an acceptable point
! is found (Wolfe-Powell conditions). Note, that points are always maintained
! in order p0 <= p1 <= p2 < p3 < p4. 3) compute a new search direction using
! conjugate gradients (Polack-Ribiere flavour), or revert to steepest if there
! was a problem in the previous line-search. Return the best value so far, if
! two consecutive line-searches fail, or whenever we run out of function
! evaluations or line-searches. During extrapolation, the "f" function may fail
! either with an error or returning Nan or Inf, and minimize should handle this
! gracefully.



!if (max(size(length)) == 2) then; red=length(2);length=length(1); else red=1;endif
if (length>0)then; SA='Linesearch'; else; SA='Function evaluation'; endif

i = 0;                                            ! zero the run length counter
ls_failed = 0;                             ! no previous line search has failed
![f0 df0] = feval(f, X, varargin{:});          ! get function value and gradient
 call CG_MNIST(X, Dims, datab, f0, df0)
!   write(*,*)'line 85',  f0, sum(df0)
   
fX = f0;
if(length < 0) i = i + 1                                            ! count epochs?!
s = -df0; d0 = - sum(s*s);           ! initial search direction (steepest) and slope
x3 = red/(1-d0);                                  ! initial step is red/(|s|+1)

!write(*,*)'ok 91 minimize'
do while (i < abs(length) )                          ! while not finished
   if(length > 0)  i = i +  1 ;                                   ! count iterations?!
!   write(*,*)'i=',i,'length=',length
   
   X0 = X; F0U = f0; dF0U = df0;                   ! make a copy of current values
   if (length>0) then; M = MAXx ; else; M = min(MAXx, -length-i); endif;
!write(*,*)'ok 97 minimize'	
!kk = 0
    do !while ( 1 > 0 )                            ! keep extrapolating as long as necessary
    x2 = 0; f2 = f0; d2 = d0; f3 = f0; df3 = df0;
    success = 0;
!     kk=kk+1   
!     write(*,*)'kk=', kk
    do while (success < 1 .and. M > 0 )
    	
        M = M - 1; if(length < 0) i = i + 1;                        ! count epochs?!
!        [f3 df3] = feval(f, X+x3*s, varargin{:});
		 call CG_MNIST(X+x3*s, Dims, datab, f3, df3)
!		 write(*,*)'line 110', f3, sum(df3)
!	     pause;
		 itmp = 0		
		 where(isnan(df3)) itmp = 1
		 where(df3>inf)  itmp = 1
		 		 
         if (isnan(f3) .or. f3>inf .or. sum(itmp) > 1  ) then
		 	 x3 = (x2+x3)/2.   ! bisect and try again
		 else
         	success = 1;
		 endif
    enddo
	
    if (f3 < F0U)then; X0 = X+x3*s; F0U = f3; dF0U = df3; endif;         ! keep best values
      d3 = sum(df3*s);                                                    ! new slope
      if(d3 > SIG*d0 .or. f3 > f0+x3*RHO*d0 .or. M == 0) then ! are we done extrapolating?
!        write(*,*)'exit do !while ( 1 > 0 )   '
        exit
      endif
	
    x1 = x2; f1 = f2; d1 = d2;                        ! move point 2 to point 1
    x2 = x3; f2 = f3; d2 = d3;                        ! move point 3 to point 2
    A = 6*(f1-f2)+3*(d2+d1)*(x2-x1);                 ! make cubic extrapolation
    B = 3*(f2-f1)-(2*d1+d2)*(x2-x1);
    x3 = x1-d1*(x2-x1)**2/(B+sqrt(B*B-A*d1*(x2-x1))); ! num. error possible, ok!
!    if (~isreal(x3) .or. isnan(x3) .or. x3>inf .or. x3 < 0 ) then ! num prob | wrong sign?
	 if ( isnan(x3) .or. x3>inf .or. x3 < 0 ) then ! num prob | wrong sign?
      x3 = x2*EXT;                                 ! extrapolate maximum amount
    elseif (x3 > x2*EXT) then                  ! new point beyond extrapolation limit?
      x3 = x2*EXT;                                 ! extrapolate maximum amount
    elseif (x3 < x2+INT*(x2-x1))then         ! new point too close to previous point?
      x3 = x2+INT*(x2-x1);
    endif
   enddo   !do while ( 1 > 0 )                                  ! end extrapolation

!1000 continue

!write(*,*)'ok 137 minimize'	


   do while ((abs(d3) > -SIG*d0 .or. f3 > f0+x3*RHO*d0) .and. M > 0)  ! keep interpolating
    if (d3 > 0 .or. f3 > f0+x3*RHO*d0 )then                        ! choose subinterval
      x4 = x3; f4 = f3; d4 = d3;                      ! move point 3 to point 4
    else
      x2 = x3; f2 = f3; d2 = d3;                      ! move point 3 to point 2
    endif
	
    if (f4 > f0) then         
      x3 = x2-(0.5*d2*(x4-x2)**2)/(f4-f2-d2*(x4-x2));  ! quadratic interpolation
    else
      A = 6*(f2-f4)/(x4-x2)+3*(d4+d2);                    ! cubic interpolation
      B = 3*(f4-f2)-(2*d2+d4)*(x4-x2);
      x3 = x2+(sqrt(B*B-A*d2*(x4-x2)**2)-B)/A;        ! num. error possible, ok!
    endif
	
    if (isnan(x3) .or. x3>inf ) then
      x3 = (x2+x4)/2;               ! if we had a numerical problem then bisect
    endif
	
    x3 = max(min(x3, x4-INT*(x4-x2)),x2+INT*(x4-x2))  ! don't accept too close
	call CG_MNIST(X+x3*s, Dims, datab, f3, df3)
    if (f3 < F0U)then; X0 = X+x3*s; F0U = f3; dF0U = df3; endif         ! keep best values
    M = M - 1; if(length<0) i = i + 1                             ! count epochs?!
    d3 = sum(df3*s)                                                    ! new slope
   enddo   !do while ((abs(d3)                              ! end interpolation

!write(*,*)'ok 166 minimize'	
!write(*,*)abs(d3),-sig*d0, f3, f0+x3*RHO*d0
  if (abs(d3) < -SIG*d0 .and. f3 < f0+x3*RHO*d0)then         ! if line search succeeded
    X = X+x3*s; f0 = f3; !fX = [fX' f0]';                     ! update variables
    
     write(*,*) SA, i, f0
     
    s = sum(df3*df3-df0*df3)/sum(df0*df0)*s - df3;   ! Polack-Ribiere CG direction
    df0 = df3;                                               ! swap derivatives
    d3 = d0; d0 = sum(df0*s);
    if (d0 > 0 )then                                     ! new slope must be negative
      s = -df0; d0 = -sum(s*s);                  ! otherwise use steepest direction
    endif
    x3 = x3 * min(RATIO, d3/(d0-realmin));          ! slope ratio but max RATIO
    ls_failed = 0;                              ! this line search did not fail
  else
    X = X0; f0 = F0U; df0 = dF0U;                     ! restore best point so far
    if (ls_failed>0 .or. i > abs(length) ) then        ! line search failed twice in a row
!      write(*,*)'exit do while (i < abs(length) ) '    ! or we ran out of time, so we give up
	  exit                          
    endif
    s = -df0; d0 = -sum(s*s);                                        ! try steepest
    x3 = 1/(1-d0);                     
    ls_failed = 1;                                    ! this line search failed
  endif
!write(*,*)'ok 188 minimize'	

enddo !do while (i < abs(length) )   
!1001 continue

end subroutine minimize
