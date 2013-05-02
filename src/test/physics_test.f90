program physics_test
  use physics
  implicit none
  interface 
    function f(x) 
    implicit none
    real :: x,f
    end function
  end interface
  real :: r,xu,xl
  xl = 0.5
  xu = 4.0
  r = ph_midpoint_root(f,xl,xu,0.001,100)
  write(*,*) r,f(r)
  write(*,*) c
end program physics_test

function f(x) 
implicit none
real :: x,f
f = sin(x)
end function f
