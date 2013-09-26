! Gets a first guess for the density and temperature
  function eos_dens_temp_best_guess(input)
  use eos_root_table, only: eos_set_root_file, eos_root_file
  use io_vars, only: io_unit
  implicit none
  real, dimension(2) :: input,eos_dens_temp_best_guess
  real :: ip,ie,od,ot
  real :: fd,ft,fp,fe,diffp,diffe,diffo,diff
  integer :: eof,ii,jj,un
  un = io_unit + 10
  ip = input(1)
  ie = input(2)
! Locating the right data file for the given pressure
  call eos_set_root_file(ip)
! Parsing data file
  eof   = 0
  diffo = huge(ip)
  open(unit=un,file="src/eos/rtable/rtable."//eos_root_file,status="old")
  do while (eof.eq.0) 
    read(un,*,iostat=eof) fd,ft,fp,fe
    if (eof.eq.-1) exit
    diffp = (ip - fp)/ip
    diffe = (ie - fe)/ie
    diff  = sqrt(diffp**2+diffe**2)
    if (diff.lt.diffo) then 
      od    = fd
      ot    = ft   
      diffo = diff
    end if
  end do
  eos_dens_temp_best_guess(1) = od
  eos_dens_temp_best_guess(2) = ot
  end function eos_dens_temp_best_guess




  subroutine eos_newton_raphson(iguess,fguess,dx,f,ntol,rtol,relax)
  use eos_interface, only: eos_newton_raphson_step
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
  real    :: r,s,rlx,np,nm
  real, dimension(size(iguess)) :: gc,fc,gn,fn,dg,df,g0,gp,gm,fgp,fgm
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
    call eos_newton_raphson_step(gc,dg,dx,f,fc)
    i   = i + 1
! Now that we have a direction, we choose a sign
  ! gp  = gc + rlx*dg
  ! gm  = gc - rlx*dg 
  ! fgp = f(gp)
  ! fgm = f(gm)
  ! np  = sqrt(dot_product(fgp,fgp))
  ! nm  = sqrt(dot_product(fgm,fgm))
  ! if (np.ge.nm) then 
  !   gn = gm
  ! else
  !   gn = gp 
  ! end if
    gn = gc + rlx*dg
    gc = gn
    dx = 0.01*gc
  end do
  fguess = gn
  end subroutine eos_newton_raphson





  subroutine eos_newton_raphson_step(gc,gn,dg,f,ce)
  use io_interface, only: IO_2string
  use dr_interface, only: dr_abort
  use           lu, only: linear_solver
  use   eos_vector, only: dpd_row,dpt_row,dsd_row,dst_row
  use   eos_vars
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
    call dr_abort("eos_nr_step","Interface arrays are of incompatible size "//&
         new_line("a")//IO_2string(size(gc))//IO_2string(size(gn))//IO_2string(size(dg)))
  end if
  n  = size(gc)
  if (n.ne.2) stop "LOL"
! Getting the derivative (Jacobian)
  fc = f(gc)
  if (present(ce)) ce = fc
! Building the Jacobian
  J(1,1) = dpd_row(1)*(1.0/eos_target_pres)
  J(1,2) = dpt_row(1)*(1.0/eos_target_pres)
  J(2,1) = dsd_row(1)*(1.0/eos_target_entr)
  J(2,2) = dst_row(1)*(1.0/eos_target_entr)
! Solve for delta given by J*delta = -f
  b = - fc
  call linear_solver(n,J,b,gn)
  end subroutine eos_newton_raphson_step
