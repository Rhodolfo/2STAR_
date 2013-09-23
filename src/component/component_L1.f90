  subroutine cp_get_L1
  use dr_vars, only: dr_res_factor
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_bin_sepa,&
                     cp_don_pos,cp_acc_pos,cp_L1_pos,cp_mass_ratio
  use ph_interface, only: ph_eggleton_L1,ph_midpoint_root
  use cp_interface, only: cp_roche_1d
  implicit none
  real :: x_L1
  real :: x_lo,x_hi
  x_L1 = cp_bin_sepa*ph_eggleton_L1(cp_mass_ratio)
  x_L1 = x_L1 + cp_don_pos(1)
  x_lo = (x_L1 + cp_don_pos(1))/2.
  x_hi = (x_L1 + cp_acc_pos(1))/2.
  x_L1 = ph_midpoint_root(cp_roche_1d,x_lo,x_hi,dr_res_factor,ceiling(1./dr_res_factor))
  cp_L1_pos = (/ x_L1, 0. /)
  end subroutine cp_get_L1



  function cp_roche_1d(pos)
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_bin_freq,&
                     cp_don_pos,cp_acc_pos
  use ph_interface, only: ph_roche_acc
  implicit none
! This simply evaluates the Roche field on one axis, in order to find the
! Lagrange point by the secant method.
! This routine assumes components lie on the x axis (1)
  real :: pos,cp_roche_1d
  real, dimension(1) :: v,v1,v2
  v  = (/pos/)
  v1 = (/cp_don_pos(1)/)
  v2 = (/cp_acc_pos(1)/)
  v  = ph_roche_acc(v,v1,v2,cp_don_mass,cp_acc_mass,cp_bin_freq)
  cp_roche_1d = v(1)
  end function cp_roche_1d
