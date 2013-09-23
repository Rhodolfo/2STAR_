  function medium_func(x,t)
  use ph_vars, only: ph_pi
  use  medium, only: A,B,ism_dens,medium_Lfunc
  implicit none
! Input and output
  real                     :: t
  real, dimension(:)       :: x
  real, dimension(size(x)) :: medium_func
! Local vars
  real :: r,u,f1,f2,f3,L
! Phase space point
  r = x(1)
  u = x(2)
! Computing
  L  = medium_Lfunc(t)
  f1 = 1./(B*(r**3)*u)
  f2 = 3.*L/(4.*ph_pi*ism_dens)
  f3 = A*(r**2)*(u**3)
! Wait, they don't love you like I love you
  medium_func(1) = u
  medium_func(2) = f1*(f2-f3)
  write(*,*) "func_in  ",t,x
  write(*,*) "func_out ",t,medium_func
  write(*,*) "func_part",t,f1,f2,f3,L
  write(*,*) "func_prod",t,f1*f2,f1*f3
  end function medium_func
