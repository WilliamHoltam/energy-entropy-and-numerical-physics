!--------------------------------------------------
!PHY2063 Task 3
!URN 6309823 - Penguin Lab Group 1B
!November 5th 2015
!--------------------------------------------------
!
Program Task3
  IMPLICIT NONE
!
  REAL          ::  ran,lambda,distance,sum_distance,ave_distance
  INTEGER       ::  i,iseed,first,n
  real,external ::  rand,ran_exp
!
  first=0
  iseed=971739
  ran=rand(iseed,first)
!
  write(6,*)'How many scattering events are there?'
  read(5,*) n
  write(6,'(a,i9)')'seed value that generates this sequence is ',iseed
! now the exponential distribution has one parameter
! this is its mean, given by the variable: lambda
! distribution for is then p(x)=exp(-x/lambda) / lambda
! set lambda
  write(6,*)'What is your value of lambda?'
  read (5,*)lambda
! call lambda
  ran=ran_exp(iseed,first,lambda)
! this gives a ran number distributed according
! to an exponential distribution with mean = lambda
  dist=0
!
  do i=1,n
    do i=1,n
      distance=distance+ran.exp(iseed,first,lambda)
    end do
    sum_distance=sum_distance+distance
    ave_distance=sum_distance/real(n)
  end do
End Program Task3
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
