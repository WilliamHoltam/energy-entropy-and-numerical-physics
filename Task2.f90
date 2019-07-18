!--------------------------------------------------
!PHY2063 Task 2
!URN 6309823 - Penguin Lab Group 1B
!October 15th 2015
!--------------------------------------------------

PROGRAM Task2
  IMPLICIT NONE
   
  REAL    :: r,massdash_S,h,mass_mid,massdash_mid
  INTEGER :: i

  REAL, DIMENSION(1:301) :: mass
  REAL, DIMENSION(1:301) :: t
  REAL, DIMENSION(1:301) :: Error


  !Open Files to store data

  OPEN(unit=12,file='Task2.dat')

  !Assign Values  (r=rate, h=step, t=time, M=mass)
  !Error is error in Euler estimate

  r=0.3
  h=0.1
  t(1)=0
  mass(1)=0.1
  Error(1)=mass(1)-0.1*exp(-0.3*t(1))

  WRITE(12,'e5.8') t(1),mass(1),error(1)

  !Euler method

  DO i=1,300

  !Use these values at the midpoint

    !t_mid=t(i)+0.5*h   not used therefore not needed.
    massdash_s=-r*mass(i)
    mass_mid=mass(i)+0.5*h*massdash_s
    massdash_mid=-r*mass_mid

    !Input these values into the formulae below

    t(i+1)=t(i)+h
    mass(i+1)=mass(i)+h*massdash_mid

    error(i+1)=mass(i+1)-0.1*exp(-0.3*t(i+1))

    !Write the values in the array to Task2.dat

    WRITE(12,'e5.8') t(i+1),mass(i+1),error(i+1)
  END DO

  CLOSE(12)

!Run Program
!Run gnuplot
!Write "plot "Task2.dat" using 1:2 with lines" in the terminal

!Compare to Task1: Run gnuplot
!Write "plot "Task1.dat" using 1:2 with lines" in the terminal

!Find error: Run gnuplot
!Write "plot "Task2.dat" using 1:3 with lines" in the terminal

END PROGRAM Task2

