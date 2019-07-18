!--------------------------------------------------
!PHY2063 A1
!URN 6309823 - Penguin Lab Group 1B
!November 19th 2015
!--------------------------------------------------

Program A1
  IMPLICIT NONE

  REAL          ::  ran,lambda,energy,distance,thick
  REAL          ::  sum_distance,ave_distance,ave_distance_sqr,sum_distance_sqr
  REAL          ::  sum_energy, ave_energy,sum_energy_sqr,ave_energy_sqr
  REAL          ::  SD_distance,SD_energy
  INTEGER       ::  i,iseed,first,n
  real,external ::  rand,ran_exp

  first=0
  iseed=971739
  ran=rand(iseed,first)

  write(6,*)' '
  write(6,*)'This program estimates the average fractional energy lost from'
  write(6,*)'particles to a slab of material of thickness, t, due to inelastic'
  write(6,*)'scattering of the particles from the atoms of the material.'
  write(6,*)' '
  write(6,*)'This program assumes, that in each inelastic scattering event,'
  write(6,*)'the particle loses a random fraction, uniformly distributed'
  write(6,*)"between 0 and 1 of it's energy."
  write(6,*)' '

  write(6,*)'The number the pseudo-random number generator'
  write(6,*)'gives me is:',ran
  write(6,*)' '
  write(6,'(a,i9)')'seed value that generates this sequence is ',iseed
  write(6,*)' '

! now the exponential distribution has one parameter
! this is its mean, given by the variable: lambda
! distribution for is then p(x)=exp(-x/lambda) / lambda
! set lambda

! Attempt lambda with values of 1 and 100

  write(6,*)'What is your value of lambda?'
  read (5,*)lambda
  write(6,*)' '

! call lambda

! Attempt thickness with values of 100 and 1

  write(6,*)'What is the thickness of the detector?'
  read (5,*)thick
  write(6,*)' '
  write(6,*)'The number of particles is 10000'
  write(6,*)' '
  ran=ran_exp(iseed,first,lambda)

! this gives a ran number distributed according
! to an exponential distribution with mean = lambda

  n=10000

  do i=1,n
    distance=0
    energy=1
    do

!     This section calculates the distance travelled by the particle between
!     each scattering event until the distance is greater than the thickness
!     of the detector.

      distance=distance+ran_exp(iseed,first,lambda)
      if(distance>thick)exit

!     This section calculates the fractional energy of the particle after each
!     scattering event until the fractional energy is less than 10^-7.

      energy=energy*rand(iseed,first)
      if(energy<1E-7)exit
    end do

!   This section calculates the sum of the distances travelled by the particle 
!   between scattering events and the sum of the distances squared.

    sum_distance=sum_distance+distance
    sum_distance_sqr=sum_distance_sqr+distance**2

!   This section calculates the sum of the fractional energy lost by the
!   particle during each scattering event and the sum of the fractional energy
!   squared.

    sum_energy=sum_energy+energy
    sum_energy_sqr=sum_energy_sqr+energy**2
  end do

! This section calculates the mean distance, the mean energy, the mean of the
! distances squared and the mean of the averages squared.

  ave_distance=sum_distance/real(n)
  ave_energy=sum_energy/real(n)
  ave_distance_sqr=sum_distance_sqr/real(n)
  ave_energy_sqr=sum_energy_sqr/real(n)

! SD=Standard deviation

  SD_distance=sqrt(ave_distance_sqr-ave_distance**2)
  write(6,*)'This is the average distance of a particle that'
  write(6,*)'scatters elastically until distance>thickness of the detector'
  write(6,*)ave_distance
  write(6,*)' '
  write(6,*)'This is the standard deviation of the distance of a particle'
  write(6,*)'that scatters elastically until distance>thickness of the detector'
  write(6,*)SD_distance
  write(6,*)' '
  write(6,*)' '

  SD_energy=sqrt(ave_energy_sqr-ave_energy**2)
  write(6,*)'This is the average fractional energy lost by a particle that'
  write(6,*)'scatters elastically until distance>thickness of the detector'
  write(6,*)ave_energy
  write(6,*)' '
  write(6,*)'This is the standard deviation of the average fractional energy'
  write(6,*)'lost by a particle that scatters elastically until' 
  write(6,*)'distance>thickness of the detector'
  write(6,*)SD_energy
End Program A1


real function ran_exp(iseed,first,lambda)
  implicit none
  integer :: iseed,first
  real :: dum,lambda
  real, external :: rand
1 continue
  dum=rand(iseed,first)
  if(dum == 0.0d00)goto 1
  ran_exp=-log(dum)*lambda
end function ran_exp


real function rand(iseed,first)

!  This function returns a pseudo-random number for each invocation.
!  It is an f90 adaptation of an
!  FORTRAN 77 adaptation 
!  by Dick Valent and Fred Clare
!  http://www.cisl.ucar.edu/zine/96/spring/articles/3.random-6.html
!  of the "Integer Version 2" minimal 
!  standard number generator whose Pascal code appears in the article:

!     Park, Steven K. and Miller, Keith W., "Random Number Generators: 
!     Good Ones are Hard to Find", Communications of the ACM, 
!     October, 1988.

  implicit none
  integer, parameter :: MPLIER=16807
  integer, parameter :: MODLUS=2147483647
  integer, parameter :: MOBYMP=127773
  integer, parameter :: MOMDMP=2836
  integer :: hvlue,lvlue,testv,nextn,first,iseed
  save nextn

  if(first == 0) THEN
    nextn=iseed
    first=1
  endif

  hvlue=nextn/mobymp
  lvlue=mod(nextn,mobymp)
  testv=mplier*lvlue-momdmp*hvlue
  if(testv > 0)then
    nextn=testv
  else
    nextn=testv+modlus
  endif
  rand = real(nextn)/real(modlus)

end function rand
