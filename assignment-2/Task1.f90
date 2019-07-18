!--------------------------------------------------
!PHY2063 Task1
!URN 6309823 - Penguin Lab Group 1B
!November 19th 2015
!--------------------------------------------------

program Task1
  implicit none

  integer,parameter ::  n=80
  real              ::  phi(-n:n,-n:n)
  real,parameter    ::  h=0.05,e0=8.854E-12,e_elec=1.602E-19,c=3E26,T=310
  real,parameter    ::  k=1.381E-23,ep_water=80.0*e0,r_dna=1
  integer           ::  i,j,flag,iter
  real              ::  x,y,phi_s,accuracy,oldval,a,b,kappa

! ep=effective permittivity, c=concentration, e0=permittivity of free space
! T=temperature, e_elec=charge on an electron, iter=# iterations
! k=Boltzman constant

! A NUMBER OF N ITTERATIONS HAS TO BE GIVEN AS DO THE BOUNDARY CONDITIONS PHI_S

! The second derivatives of the Poisson-Boltzmann equation in both x and y

! where h is the grid spacing of the array
! r_dna is the radius of each double helix

  OPEN(unit=12,file='Task1.dat')

  write(6,*)"The role of this program is to determine the electrostatic"
  write(6,*)"potential in the xy plane in the persence of a DNA double helix"
  write(6,*)"aligned with its axis along the z axis."
  write(6,*)" "
  write(6,*)"In this program the double helix is modelled as a very long rigid"
  write(6,*)"charged cylinder of radius r_DNA = 1nm, parllel to the z axis and"
  write(6,*)"at the origin in the xy plane."
  write(6,*)" "
  write(6,*)" "

! phi=0.0 is the initial guess for the Gauss-Seidel iteration

  phi=0.0
  phi_s=100
  do i=-n,n

!   This do loop conducts the Gaus-Seidel iteration
!   The Gauss-Seidel iteration starts with a guess at the solution
!   which should include the imposed BCs

    do j=-n,n

!     phi_s is a boundary condition value

      x=real(i)*h
      y=real(j)*h
      if(x**2+y**2<r_dna**2)phi(i,j)=phi_s
    enddo
  enddo

  kappa=sqrt((2*c*e_elec**2)/(ep_water*k*T))/1E9

  write(6,*)"Our calcualted value for the constant kappa =",kappa

! Starting from the initial guess, we now iteratively improve on our initial
! guess, using the equation we derived for phi(i;j) in terms of phi on the four
! neighbouring grid points.
!
! In each iteration, we need to loop over all interior points of the grid, 
! calculating at each point the next estimate of the solution at point (i;j)
! from the current values at the four adjacent grid points.
  
 
  accuracy=1.0E-3
  do iter=1,10000
    flag=0
    do i=-n+1,n-1
      do j=-n+1,n-1
        x=real(i)*h
        y=real(j)*h

!       Each pre-iteration value phi(i,j) is stored in oldval

        oldval=phi(i,j)
        if(x**2+y**2<r_dna**2)then
        else
          a=phi(i+1,j)+phi(i-1,j)+phi(i,j+1)+phi(i,j-1)
          b=4.0+h**2.0*kappa**2
          phi(i,j)=a/b

!         After phi has been updated we check to see if it has changed by more
!         than accuracy.

!         If oldvalue has changed more than accuracy then we set flag=1
!         which means that we need annother iteration.
      
          if(abs(oldval-phi(i,j))>accuracy)flag=1
        endif
      enddo
    enddo
    if(flag==0)exit
  enddo

  write(6,*)"The number of iterations is",iter
  write(6,*)" "
  write(6,*)" "
  write(6,*)"The values of x,y and phi(i,j) are written to the file Task1.dat"
  write(6,*)" "
  write(6,*)" "
  write(6,*)"In order to view a 3D representation of the data open gnuplot in"
  write(6,*)"the terminal, type (splot 'Task1.dat') in the terminal."

! Note that when we loop over the array we need to avoid changing the values
! of phi elements at the boundaries because these need to remain at the values
! already set by the BC's. Thus the loops are from -n+1 to n-1.

! This do loop writes the values of x,y and phi to the file Task1.dat

  do i=-n+1,n-1
    do j=-n+1,n-1
      x=real(i)*h
      y=real(j)*h
      write(12,*)x,y,phi(i,j)
    end do
  end do
end program Task1
