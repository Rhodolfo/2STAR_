  subroutine md_lightcurve(fullpath)
  implicit none
! Input
  character(len=100) :: fullpath
! Local 
  real, parameter :: pi  = 3.141592
  real, parameter :: k_B = 8.61733238e-5
  real, parameter :: h   = 4.13566751e-15
  real, dimension(26) :: tem 
  real :: Ledd,Lx,xi,xf,t,teff,ebase,Lo
  integer :: ii,jj,kk
  interface 
  function bbody(x)
  implicit none
  real :: x,bbody
  end function bbody
  end interface
  interface
  function bbody_log(x)
  implicit none
  real :: x
  real :: bbody_log
  end function bbody_log
  end interface
  interface
  function bbody_integral(a,b)
  implicit none
  real :: a,b
  real :: bbody_integral
  end function bbody_integral
  end interface
  open(unit=67,file=trim(adjustl(fullpath))//"/rad.dat"       ,status="old"    )
  open(unit=68,file=trim(adjustl(fullpath))//"/lightcurve.dat",status="unknown")
  kk    = 0
  ebase = exp(1.d0)
  do while (kk.eq.0) 
    read(67,*,iostat=kk) tem
    t    = tem(1) 
    Ledd = tem(11)
    teff = tem(24)
    xi   = (1e3)/(k_B*teff)
    xf   = (1e4)/(k_B*teff)
    Lx   = - bbody_log(xi) + log(Ledd) - log(pi**5/15.0)
    Lx   = log10(ebase)*Lx
    xi   = (2.d0)/(k_B*teff)
    xf   = (3.d0)/(k_B*teff)
    Lo   = (Ledd/(((pi**5.0)/15.0)))*bbody_integral(xi,xf)
    write(*,*)  t,Lo,Lx,Ledd
    write(68,*) t,Lo,Lx,Ledd
  end do
  close(67)
  close(68)
  return
  end subroutine md_lightcurve

  function bbody(x) 
  implicit none
  real :: x
  real :: bbody
  bbody = - ((x**3)+3.0*(x**2)+6.0*x + 6.0)*exp(-x)
  end function bbody

  function bbody_log(x)
  implicit none
  real :: x
  real :: bbody_log
  bbody_log = x - 3.0*log(x)
  end function bbody_log

  function bbody_integral(a,b)
  implicit none
  real :: bbody_integral,a,b
  real :: h,x,s,m
  integer :: ii,nn
  nn = 1000
  x  = a
  h  = (b-a)/real(nn)
  s  = 0
  do ii = 1,nn+1
    if (ii.eq.1) then
      s = exp(-x)*(x**3)
    else if (ii.eq.(nn+1)) then 
      s = s + exp(-x)*(x**3)
    else if (ii.gt.1.and.ii.lt.(nn+1)) then
      s = s + 2.0*exp(-x)*(x**3)
    end if
    x = x + h 
  end do
  bbody_integral = s*h/2.0
  end function bbody_integral

