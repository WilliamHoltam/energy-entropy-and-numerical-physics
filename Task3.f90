Program Task3
  IMPLICIT NONE

  REAL    ::  h,dxdt,dvdt
  INTEGER ::  i

  REAL, DIMENSION(1:1001) :: t
  REAL, DIMENSION(1:1001) :: x
  REAL, DIMENSION(1:1001) :: v
  REAL, DIMENSION(1:1001) :: error_x
  REAL, DIMENSION(1:1001) :: error_v

  !Open Files to store data

  OPEN(unit=12,file='Task3.dat')

  !k=spring_constant h=step v=velocity x=position

  h=0.01  

  !Define initial BCs

  t(1)=0
  v(1)=0
  x(1)=0.1
  error_x(1)=x(1)-0.1*cos((5**0.5)*t(1))
  error_v(1)=v(1)+0.1*(5**0.5)*sin((5**0.5)*t(1))

  WRITE(12,'(f6.4,f6.4,f6.4,f6.4,f6.4)') t(1),x(1),v(1),error_x(1),error_v(1)

  !Euler method for integrating two coupled ODEs simultaneously
  
  DO i=1,1000
    dxdt=v(i)
    dvdt=-5*x(i)
    t(i+1)=t(i)+h
    x(i+1)=x(i)+h*dxdt
    v(i+1)=v(i)+h*dvdt

    error_x(i+1)=x(i+1)-0.1*cos((5**0.5)*t(i+1))
    error_v(i+1)=v(i+1)+0.1*(5**0.5)*sin((5**0.5)*t(i+1))

    !Write the values in the array to Task3.dat

    WRITE(12,'(f6.4,f6.4,f6.4,f6.4,f6.4)') t(i+1),x(i+1),v(i+1),error_x(i+1),error_v(i+1)
  END DO

END PROGRAM Task3
