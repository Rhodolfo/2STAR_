! Setting up the period and separation for the modes that do not need an
! iteration scheme

  subroutine cp_binary_parameters_period
  use   physics, only: ph_kepler_separation 
  use component, only: cp_setup_var,cp_binary_period,cp_binary_separation,&
                       cp_donor_mass,cp_accretor_mass,cp_binary_period,cp_binary_separation
  implicit none
  cp_binary_period     = cp_setup_var
  cp_binary_separation = ph_kepler_separation(cp_donor_mass+cp_accretor_mass,cp_binary_period)
  end subroutine cp_binary_parameters_period

  subroutine cp_binary_parameters_separation
  use   physics, only: ph_kepler_period
  use component, only: cp_setup_var,cp_binary_period,cp_binary_separation,&
                       cp_donor_mass,cp_accretor_mass,cp_binary_period,cp_binary_separation 
  implicit none
  cp_binary_separation = cp_setup_var
  cp_binary_period     = ph_kepler_period(cp_donor_mass+cp_accretor_mass,cp_binary_separation)
  end subroutine cp_binary_parameters_separation

  subroutine cp_binary_parameters_contact
  use   physics, only: pi,G,ph_kepler_period,ph_eggleton_formula
  use component, only: cp_contact_limit,cp_binary_separation,cp_binary_period,&
                       cp_mass_ratio,cp_overflow_par,cp_donor_mass,cp_accretor_mass
  implicit none
  cp_binary_separation = cp_contact_limit
  cp_binary_period     = ph_kepler_period(cp_donor_mass+cp_accretor_mass,cp_binary_separation)
  end subroutine cp_binary_parameters_contact

  subroutine cp_binary_parameters_roche_limit
  use   physics, only: ph_kepler_period
  use component, only: cp_setup_var,cp_binary_period,cp_binary_separation,cp_roche_limit,&
                       cp_donor_mass,cp_accretor_mass,cp_binary_period,cp_binary_separation  
  implicit none
  cp_binary_separation = cp_roche_limit
  cp_binary_period     = ph_kepler_period(cp_donor_mass+cp_accretor_mass,cp_binary_separation)
  end subroutine cp_binary_parameters_roche_limit
 
! This routine takes care of all setup_modes that require a cretain mdot
  subroutine cp_binary_parameters_mdot(mdot_in,mode)
  use    driver, only: dr_abort,dr_mode_mdot,dr_mode_overflow_eq,dr_mode_separation_eq
  use   physics, only: pi,ph_kepler_period,ph_eggleton_formula
  use component, only: cp_donor_mass,cp_accretor_mass,cp_donor_radius,cp_accretor_radius,&
                       cp_binary_frequency,cp_binary_period,cp_binary_separation,cp_contact_limit,&
                       cp_roche_limit,cp_donor_position,cp_accretor_position,cp_setup_var,&
                       cp_driver_roche,cp_driver_donor_radius,cp_driver_separation,&
                       cp_zeta_roche,cp_zeta_separation,cp_donor_zeta,&
                       cp_mass_ratio,cp_roche_radius,cp_mdot_donor,cp_mdot_tolerance,cp_overflow_par
  use        IO, only: IO_log
  implicit none  
  real   , intent(in)           :: mdot_in
  integer, intent(in), optional :: mode
  real    :: mdot_candidate,low_lim_a,hig_lim_a
  integer :: loop
  cp_binary_separation = cp_contact_limit
  cp_binary_period     = ph_kepler_period(cp_donor_mass+cp_accretor_mass,cp_binary_separation)
  cp_binary_frequency  = 2.*pi / cp_binary_period
  cp_donor_position    = (/-(cp_accretor_mass/(cp_donor_mass+cp_accretor_mass))*cp_binary_separation,0./)
  cp_accretor_position = (/ (cp_donor_mass   /(cp_donor_mass+cp_accretor_mass))*cp_binary_separation,0./)
  cp_roche_radius      = cp_binary_separation*ph_eggleton_formula(cp_mass_ratio)
  cp_overflow_par      = cp_donor_radius - cp_roche_radius 
  call cp_get_L1
  call cp_get_mdot
  call cp_evolution_coefficients
  if (present(mode)) then
    select case (mode)
    case (dr_mode_overflow_eq)
      mdot_candidate = - abs(cp_donor_mass*(cp_driver_roche-cp_driver_donor_radius)/(cp_zeta_roche-cp_donor_zeta))
    case (dr_mode_separation_eq)
      mdot_candidate = - abs(cp_donor_mass*(cp_driver_separation/cp_zeta_separation)) 
    case default
      call dr_abort("cp_binary_parameters_mdot","Invalid mode for cp_setup_mdot")
    end select
  else
    mdot_candidate = mdot_in
  end if
  hig_lim_a = cp_contact_limit
  low_lim_a = 0.0
  call IO_log("Looking for the separation that yields the required mdot")
  loop = 0
  do while ( (abs(mdot_candidate-cp_mdot_donor)/abs(mdot_candidate).ge.cp_mdot_tolerance).or.(loop.eq.0) ) ! Midpoint scheme to get the right mdot
    cp_binary_period     = ph_kepler_period(cp_donor_mass+cp_accretor_mass,cp_binary_separation)
    cp_binary_frequency  = 2.*pi / cp_binary_period
    cp_donor_position    = (/-(cp_accretor_mass/(cp_donor_mass+cp_accretor_mass))*cp_binary_separation,0./)
    cp_accretor_position = (/ (cp_donor_mass   /(cp_donor_mass+cp_accretor_mass))*cp_binary_separation,0./)
    cp_roche_radius      = cp_binary_separation*ph_eggleton_formula(cp_mass_ratio)
    cp_overflow_par      = cp_donor_radius - cp_roche_radius 
    call cp_get_L1
    call cp_get_mdot
    call cp_evolution_coefficients 
    if (present(mode)) then
      select case (mode)
      case (dr_mode_overflow_eq)
        mdot_candidate = -abs(cp_donor_mass*(cp_driver_roche-cp_driver_donor_radius)/(cp_zeta_roche-cp_donor_zeta))
      case (dr_mode_separation_eq)
        mdot_candidate = -abs(cp_donor_mass*(cp_driver_separation/cp_zeta_separation)) 
      case default
        call dr_abort("cp_binary_parameters_mdot","Invalid mode")
      end select
    else
    end if
    if (cp_mdot_donor.le.mdot_candidate)   low_lim_a = cp_binary_separation
    if (cp_mdot_donor.ge.mdot_candidate)   hig_lim_a = cp_binary_separation
      cp_binary_separation = (low_lim_a+hig_lim_a)/2.
      loop = 1
  end do 
  cp_binary_period = ph_kepler_period(cp_donor_mass+cp_accretor_mass,cp_binary_separation)
  call IO_log("Desired configuration has been found")
  end subroutine cp_binary_parameters_mdot
