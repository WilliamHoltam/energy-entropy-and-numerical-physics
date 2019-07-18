!--------------------------------------------------
!PHY2063 Task 1
!URN 6309823 - Penguin Lab Group 1B
!October 29th 2015
!--------------------------------------------------
!
PROGRAM Task1
! this program is just to illustrate how the pseudo-random number function is called
  IMPLICIT NONE
!
  real    ::  pi_exact,x,y,pi_est
  integer ::  i,iseed,first,n,n_circ
!
  real, external :: rand
!
! initialise by calling with first = 0 and iseed = some big integer
! only do this when you want to start a sequence, e.g., only at the
! start of your program. Once out have done do not change iseed and first.
!
  first=0
  iseed=971739
!
  write(6,'(a,i9)')'seed value that generates this sequence is ',iseed
!
  n=1000
  n_circ=0
  pi_exact=4.0*atan(1.0)
  do i=1,n
    x=2.0*(rand(iseed,first)-0.5)
    y=2.0*(rand(iseed,first)-0.5)
    if(x**2+y**2<1.0)n_circ=n_circ+1.0
  enddo
  pi_est=4.0*real(n_circ)/real(n)
  write(6,*)'estimate for pi',pi_est
  write(6,*)'exact pi       ',pi_exact
  write(6,*)'error in pi=   ',pi_exact-pi_est
121 format('pseudo-random number',i5,' in sequence = ',f13.10)
end program Task1
!
!
real function rand(iseed,first)
!
!  This function returns a pseudo-random number for each invocation.
!  It is an f90 adaptation of an
!  FORTRAN 77 adaptation 
!  by Dick Valent and Fred Clare
!  http://www.cisl.ucar.edu/zine/96/spring/articles/3.random-6.html
!  of the "Integer Version 2" minimal 
!  standard number generator whose Pascal code appears in the article:
!
!     Park, Steven K. and Miller, Keith W., "Random Number Generators: 
!     Good Ones are Hard to Find", Communications of the ACM, 
!     October, 1988.
!
  implicit none
  integer, parameter :: MPLIER=16807
  integer, parameter :: MODLUS=2147483647
  integer, parameter :: MOBYMP=127773
  integer, parameter :: MOMDMP=2836
  integer :: hvlue,lvlue,testv,nextn,first,iseed
  save nextn
!
  if(first == 0) THEN
    nextn=iseed
    first=1
  endif
!
  hvlue=nextn/mobymp
  lvlue=mod(nextn,mobymp)
  testv=mplier*lvlue-momdmp*hvlue
  if(testv > 0)then
    nextn=testv
  else
    nextn=testv+modlus
  endif
  rand = real(nextn)/real(modlus)
!
end function rand
