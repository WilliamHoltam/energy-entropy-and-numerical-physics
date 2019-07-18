!--------------------------------------------------
!PHY2063 Assignment 0
!URN 6309823 - Penguin Lab Group 1B
!October 26th 2015
!--------------------------------------------------

!Please note Unable to compile file and test program on Monday
!26th October due to nagfor error "licence not found"
! - tried multiple computers
!Therefore unable to complete assignment in time.


Program A0
  IMPLICIT NONE

  REAL    ::  h,xdash_s,xdash_mid,x_mid,vdash_s,vdash_mid,v_mid,t_mid,k
  INTEGER ::  i

  REAL, DIMENSION(1:1001) :: t
  REAL, DIMENSION(1:1001) :: x
  REAL, DIMENSION(1:1001) :: v
  REAL, DIMENSION(1:1001) :: error_x
  REAL, DIMENSION(1:1001) :: error_v

  !Open Files to store data

  OPEN(unit=12,file='A0.dat')
  
  WRITE(6,*) "The values of t,x,v,error_x and error_v are"
  WRITE(6,*) "written to the file A0.dat"
  
  !k=spring_constant h=step v=velocity x=position

  h=5E-3

  !Define initial BCs

  t(1)=0
  v(1)=2
  x(1)=0
  k=5
  error_x(1)=x(1)-0.1*cos((5**0.5)*t(1))
  error_v(1)=v(1)+0.1*(5**0.5)*sin((5**0.5)*t(1))

  WRITE(12,'(5f9.5)') t(1),x(1),v(1),error_x(1),error_v(1)

  !Euler method for integrating two coupled ODEs simultaneously
  
  DO i=1,10000

  !Use these values at the midpoint

    vdash_s=-k*x(i)+0.5*cos(2*t(i))
    xdash_s=v(i)

    x_mid=x(i)+0.5*h*xdash_s
    v_mid=v(i)+0.5*h*vdash_s
    t_mid=t(i)+0.5*h

    xdash_mid=v_mid
    vdash_mid=-k*x_mid+0.5*cos(2*t_mid)

    !Input these values into the formulae below

    t(i+1)=t(i)+h
    x(i+1)=x(i)+h*xdash_mid
    v(i+1)=v(i)+h*vdash_mid

    error_x(i+1)=x(i+1)-0.1*cos((5**0.5)*t(i+1))
    error_v(i+1)=v(i+1)+0.1*(5**0.5)*sin((5**0.5)*t(i+1))

    !Write the values in the array to A0.dat

    WRITE(12,'(5f9.5)') t(i+1),x(i+1),v(i+1),error_x(i+1),error_v(i+1)
  END DO

  !Run Program
  !Run gnuplot
  !Write "plot "A0.dat" using 1:2 with lines
  !,-5x+0.5cos(2t)" in the terminal 
  !to compare the calculated values with the actual graph

END PROGRAM A0
