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
