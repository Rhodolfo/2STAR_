  subroutine cp_get_L1
  use    driver, only: dr_res_factor
  use component, only: cp_donor_mass,cp_accretor_mass,cp_binary_separation,&
                       cp_donor_position,cp_accretor_position,cp_roche_1d,&
                       cp_L1_position,cp_mass_ratio
  use   physics, only: ph_eggleton_L1,ph_midpoint_root
  implicit none
  real :: x_L1
  real :: x_lo,x_hi
  x_L1 = cp_binary_separation*ph_eggleton_L1(cp_mass_ratio)
  x_L1 = x_L1 + cp_donor_position(1)
  x_lo = (x_L1 + cp_donor_position(1))/2.
  x_hi = (x_L1 + cp_accretor_position(1))/2.
  x_L1 = ph_midpoint_root(cp_roche_1d,x_lo,x_hi,dr_res_factor,ceiling(1./dr_res_factor))
  cp_L1_position = (/ x_L1, 0. /)
  end subroutine cp_get_L1



  function cp_roche_1d(pos)
  use component, only: cp_donor_mass,cp_accretor_mass,cp_binary_frequency,&
                       cp_donor_position,cp_accretor_position
  use physics,   only: ph_roche_acc
  implicit none
! This simply evaluates the Roche field on one axis, in order to find the
! Lagrange point by the secant method.
! This routine assumes components lie on the x axis (1)
  real :: pos,cp_roche_1d
  real, dimension(1) :: v,v1,v2
  v  = (/pos/)
  v1 = (/cp_donor_position(1)/)
  v2 = (/cp_accretor_position(1)/)
  v  = ph_roche_acc(v,v1,v2,cp_donor_mass,cp_accretor_mass,cp_binary_frequency)
  cp_roche_1d = v(1)
  end function cp_roche_1d
