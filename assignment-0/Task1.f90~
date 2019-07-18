!--------------------------------------------------
!PHY2063 Task 1
!URN 6309823 - Penguin Lab Group 1B
!October 8th 2015
!--------------------------------------------------

PROGRAM Task1
  IMPLICIT NONE

  REAL    ::  dmdt,h,r
  INTEGER ::  i

  REAL, DIMENSION(1:301) :: m
  REAL, DIMENSION(1:301) :: t
  REAL, DIMENSION(1:301) :: Error


!Open Files to store data

  OPEN(unit=12,file='Task1.dat')

!Assign Values  (r=rate, h=step, t=time, M=mass)
!Error is error in Euler estimate

  r=0.3
  h=0.1
  t(1)=0
  M(1)=0.1
  Error(1)=m(1)-0.1*exp(-0.3*t(1))

  WRITE(12,*) t(1),M(1),Error(1)

!Euler method

  DO i=1,301
    dmdt=-r*M(i)
    t(i+1)=t(i)+h
    M(i+1)=M(i)+h*dmdt
    Error(i+1)=m(i+1)-0.1*exp(-0.3*t(i+1))
!Write the values in the array to Task1.dat

    WRITE(12,*) t(i+1),M(i+1),Error(i+1)
  END DO

  Close (12)

!Run Program
!Run gnuplot
!Write "plot "Task1.dat" using 1:2 with lines" in the terminal

!Find error: Run gnuplot
!Write "plot "Task1.dat" using 1:3 with lines" in the terminal

END PROGRAM Task1
