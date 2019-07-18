!--------------------------------------------------
!PHY2063 Task 2
!URN 6309823 - Penguin Lab Group 1B
!October 29th 2015
!--------------------------------------------------
!
PROGRAM Task2
  IMPLICIT NONE
!
  REAL          ::  ran,r,l,x,y,z,ratio,error
  REAL          ::  v_box_exact,n_torus,v_torus_exact,v_torus_est
  INTEGER       ::  i,iseed,first,n
  real,external ::  rand
!
  first=0
  iseed=971739
  ran=rand(iseed,first)
  write(6,*)'This is the random number the pseudo-random number generator'
  write(6,*)'gives me',ran
  write(6,*)' '
  write(6,*)'The radius of the ring is "r"'
  write(6,*)' '
  write(6,*)'The radius of the circle that runs'
  write(6,*)'along the middle of ring "r" is "l"'
  write(6,*)' '
  write(6,*)'What is the value of "l"?'
  read (5,*)l
  write(6,*)' '
  write(6,*)'What is the value of "r"?'
  read (5,*)r
  write(6,*)' '
  write(6,*)'How many points "n" should be calculated?'
  read (5,*)n
  write(6,*)' '
  write(6,'(a,i9)')'seed value that generates this sequence is ',iseed
!
! v_torus is the volume of the torus
!
  v_torus_exact=((4.0*atan(1.0))*r**2)*(2*(4.0*atan(1.0))*l)
  v_box_exact=2*r*(2*(r+l))**2
  n_torus=0
!
  do i=1,n
    x=2.0*(r+l)*(rand(iseed,first)-(0.5))
    y=2.0*(r+l)*(rand(iseed,first)-(0.5))
    z=2.0*r*(rand(iseed,first)-(0.5))
    if((l-(x**2+y**2)**0.5)**2+z**2-r**2<0.0)n_torus=n_torus+1.0
  end do
!
  ratio=n_torus/n
  v_torus_est=v_box_exact*ratio
  error=v_torus_exact-v_torus_est
  write(6,*)'This is the estimate for the volume of the torus',v_torus_est
  write(6,*)'This is the actual volume of the torus          ',v_torus_exact
  write(6,*)'This is the error in the volume of the torus    ',error
end program Task2
!
!
real function rand(iseed,first)
!
! This function returns a pseudo-random number for each invocation.
! It is an f90 adaptation of an
! FORTRAN 77 adaptation 
! by Dick Valent and Fred Clare
! http://www.cisl.ucar.edu/zine/96/spring/articles/3.random-6.html
! of the "Integer Version 2" minimal 
! standard number generator whose Pascal code appears in the article:
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
