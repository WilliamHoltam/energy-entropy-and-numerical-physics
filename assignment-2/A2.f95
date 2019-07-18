!--------------------------------------------------
!PHY2063 Assignment 2
!URN 6309823 - Penguin Lab Group 1B
!January 11th 2016
!--------------------------------------------------

program A2
  implicit none

  integer,parameter ::  n=300
  real              ::  temp(-n:n),dtempdt(-n:n),temp_xx(-n:n)
  real,parameter    ::  h=0.1, diffc=1.1, dt=1E-4
  integer           ::  i,j,k
  real              ::  x
  
! temp(-n:n) = Temperature array
! dtempdt(-n:n) = differential of temperature with respect to time array
! temp_xx(-n:n) = laplacian opperator times
! h = grid spacing, diffc= diffusion constant, dt = time step
! [i,j,k] = iteration steps, x = region in the x axis

  open(unit=12,file='A2.dat')

  write(6,*)"This program uses an array of real numbers to calculate the time"
  write(6,*)"evolution, by diffusion, of a temperature distribution along the"
  write(6,*)"x-axis."

  temp=0.0

  do i=-10,10
    temp(i)=20.0
  enddo

  write(6,*)" "
  write(6,*)" "
  write(6,*)"The initial boundary conditions are:"
  write(6,*)"T=0 degrees C when x<-1 cm"
  write(6,*)"T=20 degrees C when -1<=x<=1 cm"
  write(6,*)"T=0 degrees C when x>1 cm"
  write(6,*)" "
  write(6,*)" "



  do i=0,100000

!   This integrates forward in time from t=0s to t=10s in increments of 10E-4.
    do j=-n+1,n-1

!     This calculates the values of dT/dt at each position.

!     dtempdt is the time derivative of the temperature.
!     diffc is the diffusion constant.
 
      temp_xx(j)=(temp(j+1)-2.0*temp(j)+temp(j-1))/h**2
      dtempdt(j)=diffc*temp_xx(j)

     end do
    do k=-n+1,n-1

!       This uses these values in the Euler method to calculate the temperature
!       at all x values.

        temp(k)=temp(k)+dt*dtempdt(k)

!       Find the error in result here.
        
      end do
  end do

  do i=-n,n

!   This do loop writes the values of distance x and temperature to the file
!   A0.dat

    x=real(i)*h
    write(12,*)x,Temp(i)
  end do
  close (12)

  write(6,*)"The distance, x, and the temperature, Temp(i), is found"
  write(6,*)" "
  write(6,*)"The data 'x,Temp(i)' is written to the file A2.dat"
  write(6,*)" "
  write(6,*)"Plot the data using gnuplot"
  write(6,*)"In gnuplot write: plot 'A2.dat' using 1:2 with lines"
end program A2
