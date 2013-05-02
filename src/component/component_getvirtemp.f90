  subroutine cp_getvirtemp(E,T,N,tol)
  use physics, only: a_rad,k_B
  implicit none
! Inputs and outputs
  real    :: E,T,N
  integer :: tol 
! Local variables
  real    :: Tmax,Tmin,Eg,Tc
  integer :: ntol,ncou
  logical :: switch
! Initialazing
  Tmin   = 0.
  Tmax   = E/k_B
  ncou   = 0
  switch = .false.
  ntol   = tol
! Looping
  do while (ncou.lt.ntol)
    Tc = (Tmin+Tmax)/2.
    Eg = (3./2.)*k_B*Tc + (1./3.)*(a_rad/N)*(Tc**4)
    if (Eg.gt.E) then
      Tmax = Tc
    else 
      Tmin = Tc
    end if
    ncou = ncou + 1
  end do
  T = Tc
  end subroutine cp_getvirtemp
