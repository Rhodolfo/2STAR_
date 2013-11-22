! Setting up the peri and sepa for the modes that do not need an
! iteration scheme

  subroutine cp_binary_parameters_period
  use dr_interface, only: dr_abort
  use ph_interface, only: ph_kepler_separation 
  use io_interface, only: io_log
  use cp_vars, only: cp_setup_var,cp_bin_peri,cp_bin_sepa,&
                     cp_don_mass,cp_acc_mass,cp_bin_peri,cp_bin_sepa
  implicit none
  cp_bin_peri = cp_setup_var
  if (cp_bin_peri.le.0.0) call io_log("[component] Warning: Input period is negative")
  cp_bin_sepa = ph_kepler_separation(cp_don_mass+cp_acc_mass,cp_bin_peri)
  end subroutine cp_binary_parameters_period

  subroutine cp_binary_parameters_separation
  use dr_interface, only: dr_abort 
  use ph_interface, only: ph_kepler_period
  use io_interface, only: io_log
  use cp_vars, only: cp_setup_var,cp_bin_peri,cp_bin_sepa,&
                     cp_don_mass,cp_acc_mass,cp_bin_peri,cp_bin_sepa 
  implicit none
  cp_bin_sepa = cp_setup_var
  if (cp_bin_sepa.le.0.0) call io_log("[component] Warning: Input separation is negative") 
  cp_bin_peri = ph_kepler_period(cp_don_mass+cp_acc_mass,cp_bin_sepa)
  end subroutine cp_binary_parameters_separation

  subroutine cp_binary_parameters_contact
  use ph_interface, only: ph_kepler_period
  use cp_vars, only: cp_contact_limit,cp_bin_sepa,cp_bin_peri,&
                     cp_mass_ratio,cp_overflow_par,cp_don_mass,cp_acc_mass
  implicit none
  cp_bin_sepa = cp_contact_limit
  cp_bin_peri = ph_kepler_period(cp_don_mass+cp_acc_mass,cp_bin_sepa)
  end subroutine cp_binary_parameters_contact

  subroutine cp_binary_parameters_roche_limit
  use ph_interface, only: ph_kepler_period
  use cp_vars, only: cp_setup_var,cp_bin_peri,cp_bin_sepa,cp_roche_limit,&
                     cp_don_mass,cp_acc_mass,cp_bin_peri,cp_bin_sepa  
  implicit none
  cp_bin_sepa = cp_roche_limit
  cp_bin_peri = ph_kepler_period(cp_don_mass+cp_acc_mass,cp_bin_sepa)
  end subroutine cp_binary_parameters_roche_limit
 
! This routine takes care of all setup_modes that require a cretain mdot
  subroutine cp_binary_parameters_mdot(mdot_in,mode)
  use dr_vars, only: dr_mode_mdot,dr_mode_overflow_eq,dr_mode_separation_eq
  use ph_vars, only: ph_pi
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_radius,cp_acc_radius,&
                     cp_bin_freq,cp_bin_peri,cp_bin_sepa,cp_contact_limit,&
                     cp_roche_limit,cp_don_pos,cp_acc_pos,cp_setup_var,&
                     cp_driver_roche,cp_driver_don_radius,cp_driver_sepa,&
                     cp_zeta_roche,cp_zeta_sepa,cp_don_zeta,&
                     cp_mass_ratio,cp_roche_radius,cp_don_mdot,cp_mdot_tolerance,cp_overflow_par
  use dr_interface, only: dr_abort
  use ph_interface, only: ph_kepler_period,ph_eggleton_formula
  use io_interface, only: io_log
  implicit none  
  real   , intent(in)           :: mdot_in
  integer, intent(in), optional :: mode
  real    :: mdot_candidate,low_lim_a,hig_lim_a
  integer :: loop
  cp_bin_sepa = cp_contact_limit
  cp_bin_peri     = ph_kepler_period(cp_don_mass+cp_acc_mass,cp_bin_sepa)
  cp_bin_freq  = 2.*ph_pi / cp_bin_peri
  cp_don_pos    = (/-(cp_acc_mass/(cp_don_mass+cp_acc_mass))*cp_bin_sepa,0./)
  cp_acc_pos = (/ (cp_don_mass   /(cp_don_mass+cp_acc_mass))*cp_bin_sepa,0./)
  cp_roche_radius      = cp_bin_sepa*ph_eggleton_formula(cp_mass_ratio)
  cp_overflow_par      = cp_don_radius - cp_roche_radius 
  call cp_get_L1
  call cp_get_mdot
  call cp_evolution_coefficients
  if (present(mode)) then
    select case (mode)
    case (dr_mode_overflow_eq)
      mdot_candidate = - abs(cp_don_mass*(cp_driver_roche-cp_driver_don_radius)/(cp_zeta_roche-cp_don_zeta))
    case (dr_mode_separation_eq)
      mdot_candidate = - abs(cp_don_mass*(cp_driver_sepa/cp_zeta_sepa)) 
    case default
      call dr_abort("cp_binary_parameters_mdot","Invalid mode for cp_setup_mdot")
    end select
  else
    mdot_candidate = mdot_in
  end if
  hig_lim_a = cp_contact_limit
  low_lim_a = 0.0
  call io_log("Looking for the sepa that yields the required mdot")
  loop = 0
  do while ( (abs(mdot_candidate-cp_don_mdot)/abs(mdot_candidate).ge.cp_mdot_tolerance).or.(loop.eq.0) ) ! Midpoint scheme to get the right mdot
    cp_bin_peri = ph_kepler_period(cp_don_mass+cp_acc_mass,cp_bin_sepa)
    cp_bin_freq = 2.*ph_pi / cp_bin_peri
    cp_don_pos  = (/-(cp_acc_mass/(cp_don_mass+cp_acc_mass))*cp_bin_sepa,0./)
    cp_acc_pos  = (/ (cp_don_mass   /(cp_don_mass+cp_acc_mass))*cp_bin_sepa,0./)
    cp_roche_radius = cp_bin_sepa*ph_eggleton_formula(cp_mass_ratio)
    cp_overflow_par = cp_don_radius - cp_roche_radius 
    call cp_get_L1
    call cp_get_mdot
    call cp_evolution_coefficients 
    if (present(mode)) then
      select case (mode)
      case (dr_mode_overflow_eq)
        mdot_candidate = -abs(cp_don_mass*(cp_driver_roche-cp_driver_don_radius)/(cp_zeta_roche-cp_don_zeta))
      case (dr_mode_separation_eq)
        mdot_candidate = -abs(cp_don_mass*(cp_driver_sepa/cp_zeta_sepa)) 
      case default
        call dr_abort("cp_binary_parameters_mdot","Invalid mode")
      end select
    else
    end if
    if (cp_don_mdot.le.mdot_candidate)   low_lim_a = cp_bin_sepa
    if (cp_don_mdot.ge.mdot_candidate)   hig_lim_a = cp_bin_sepa
      cp_bin_sepa = (low_lim_a+hig_lim_a)/2.
      loop = 1
  end do 
  cp_bin_peri = ph_kepler_period(cp_don_mass+cp_acc_mass,cp_bin_sepa)
  call io_log("Desired configuration has been found")
  end subroutine cp_binary_parameters_mdot
