! Basic mathematical functions

  function ph_norm(vec)
  implicit none
  real :: ph_norm
  real, dimension(:) :: vec
  ph_norm = sqrt(dot_product(vec,vec))
  end function ph_norm



  function ph_cross(vec1,vec2)
  implicit none
  real, dimension(3) :: vec1,vec2,ph_cross
  ph_cross(1) = vec1(2)*vec2(3)-vec1(3)*vec2(2)
  ph_cross(2) = vec1(3)*vec2(1)-vec1(1)*vec2(3)
  ph_cross(3) = vec1(1)*vec2(2)-vec1(2)*vec2(1)
  end function ph_cross



  function ph_midpoint_root(func,lowbound,higbound,rtol,ntol)
  implicit none
  interface 
    function func(arg)
    implicit none
    real :: func,arg
    end function func
  end interface
  real     :: ph_midpoint_root
  integer  :: ntol
  real     :: rtol
  real     :: lowbound,higbound
  real     :: aux1,aux2,aux3,midpoint
  integer  :: n
  logical  :: done
  n    = 0
  done = .false.
  do while (.not.done)
    aux1     = func(lowbound)
    aux2     = func(higbound)
    midpoint = (lowbound+higbound)/2.
    aux3     = func(midpoint)
    if (aux3/aux1.gt.0.) then
        lowbound = midpoint
    else 
        higbound = midpoint
    end if  
    n = n + 1
    if (abs(aux3).le.rtol.or.n.gt.ntol) done = .true.
  end do
  ph_midpoint_root = midpoint
  end function ph_midpoint_root





  subroutine ph_newton_raphson(iguess,fguess,dx,f,ntol,rtol,relax)
  use ph_interface, only: ph_newton_raphson_step
  implicit none
! Interface variables
  interface
    function f(x)
    implicit none
    real, dimension(:)       :: x
    real, dimension(size(x)) :: f
    end function f
  end interface
  real, dimension(:) :: iguess,fguess,dx
  integer, optional :: ntol
  real   , optional :: rtol
  real   , optional :: relax
! Local variables
  integer :: n,i,j
  real    :: r,s,rlx
  real, dimension(size(iguess)) :: gc,fc,gn,fn,dg,df,g0
  n   = 1e1
  r   = 1e-1
  rlx = 1e-1
  if (present(ntol))  n   = ntol
  if (present(rtol))  r   = rtol
  if (present(relax)) rlx = relax
  i  = 0
  s  = 10*r
  gc = iguess
  g0 = gc
  do while (i.lt.n.and.s.gt.r)
    call ph_newton_raphson_step(gc,dg,dx,f,fc)
    i     = i + 1
    gn    = gc + relax*dg
    gc    = gn
    dx    = 0.1*gc
  end do
  fguess = gn
  end subroutine ph_newton_raphson





  subroutine ph_newton_raphson_step(gc,gn,dg,f,ce)
  use io_interface, only: IO_2string
  use dr_interface, only: dr_abort
  use     lu, only: linear_solver
  implicit none
! Iinterface variables
  interface
    function f(x)
    implicit none
    real, dimension(:)       :: x
    real, dimension(size(x)) :: f
    end function f
  end interface
  real, dimension(:) :: gc,gn,dg
  real, dimension(:), optional :: ce
! Local variables
  real, dimension(size(dg))          :: gt,ft,dx,df,fc,b
  real, dimension(size(dg),size(dg)) :: J
  real    :: d
  integer :: i,n,jj
  logical :: abort
! Sanity checks
  abort = .false.
  if (size(gc).ne.size(gn)) abort = .true.
  if (size(gc).ne.size(dg)) abort = .true.
  if (abort) then 
    call dr_abort("ph_nr_step","Interface arrays are of incompatible size "//&
         new_line("a")//IO_2string(size(gc))//IO_2string(size(gn))//IO_2string(size(dg)))
  end if
  n  = size(gc)
! Getting the derivative (Jacobian)
  fc    = f(gc)
  if (present(ce)) ce = fc
  do i = 1,n
    dx       = 0.
    dx(i)    = dg(i) 
    gt       = gc + dx
    ft       = f(gt)
    df       = ft - fc
    J(1:n,i) = df(1:n) / dx(i)
  end do
! Solve for delta given by J*delta = -f
  b = -fc
  call linear_solver(n,J,b,gn)
  end subroutine ph_newton_raphson_step
