subroutine cp_binary_parameters
  use   physics, only: ph_eggleton_formula,ph_eggleton_rcirc,ph_eggleton_rmin,pi,G,stefan_boltzmann
  use    driver, only: dr_setup_mode,dr_mode_period,dr_mode_separation,&
                       dr_mode_overflow_eq,dr_mode_separation_eq,&
                       dr_mode_contact,dr_mode_roche_limit,dr_mode_mdot,dr_time,dr_include_tides
  use component, only: cp_donor_mass,cp_accretor_mass,cp_donor_radius,cp_accretor_radius,&
                       cp_donor_position,cp_accretor_position,cp_donor_sync_0,cp_accretor_sync_0,&
                       cp_donor_sync_time,cp_accretor_sync_time,&
                       cp_donor_sync_freq,cp_accretor_sync_freq,&
                       cp_binary_frequency,cp_min_radius,cp_circularization_radius,cp_roche_radius,&
                       cp_mass_ratio,cp_binary_separation,cp_binary_period, &
                       cp_roche_limit,cp_contact_limit,cp_overflow_par,&
                       cp_binary_parameters_period,cp_binary_parameters_separation,&
                       cp_binary_parameters_contact,cp_binary_parameters_roche_limit,&
                       cp_binary_parameters_mdot,cp_get_L1,cp_get_mdot,cp_setup_var,&
                       cp_v_escape,cp_rtau_wind,cp_teff_wind,cp_L_eddington,&
                       cp_mdot_donor,cp_opacity,cp_total_mass,cp_envelope
  implicit none
! Binary separation and period are calculated here
  select case (dr_setup_mode) 
    case (dr_mode_period)
      call cp_binary_parameters_period
    case (dr_mode_separation)
      call cp_binary_parameters_separation
    case (dr_mode_contact)
      call cp_binary_parameters_contact
    case (dr_mode_roche_limit)
      call cp_binary_parameters_roche_limit
    case (dr_mode_mdot)
      call cp_binary_parameters_mdot(cp_setup_var)
    case (dr_mode_overflow_eq)
      call cp_binary_parameters_mdot(cp_setup_var,dr_setup_mode)
    case (dr_mode_separation_eq)
      call cp_binary_parameters_mdot(cp_setup_var,dr_setup_mode)
    case default
      stop "Invalid mode for cp_binary_parameters"
  end select

! Calculating some binary physical parameters
  cp_total_mass             = cp_accretor_mass+cp_donor_mass
  cp_roche_radius           = cp_binary_separation*ph_eggleton_formula(cp_mass_ratio)
  cp_circularization_radius = cp_binary_separation*ph_eggleton_rcirc(cp_mass_ratio)
  cp_min_radius             = cp_binary_separation*ph_eggleton_rmin(cp_mass_ratio)
  cp_binary_frequency       = 2.0*pi / cp_binary_period
  cp_donor_position         = (/-(cp_accretor_mass/cp_total_mass)*cp_binary_separation, 0./)
  cp_accretor_position      = (/ (cp_donor_mass   /cp_total_mass)*cp_binary_separation, 0./)
  cp_overflow_par           = cp_donor_radius - cp_roche_radius 
  cp_v_escape               = sqrt(2.*G*(cp_total_mass)/cp_roche_radius)

! Finding Lagrange point, calculating mass transfer rate and computing envelope properties
  call cp_get_L1
  call cp_get_mdot
  call cp_envelope

! Let's handle some tidal terms
  if (dr_time.le.0.) then 
    call IO_log("[component] Saving initial syncronization timescales and relevant data")
    cp_donor_sync_0    = cp_donor_sync_0/((cp_mass_ratio**2)*((cp_binary_separation/cp_donor_radius)**6))
    cp_accretor_sync_0 = cp_accretor_sync_0/(((1./cp_mass_ratio)**2)*((cp_binary_separation/cp_accretor_radius)**6))
  end if
  cp_donor_sync_time        = cp_donor_sync_0*(cp_mass_ratio**2)*((cp_binary_separation/cp_donor_radius)**6)
  cp_accretor_sync_time     = cp_accretor_sync_0*((1./cp_mass_ratio)**2)*((cp_binary_separation/cp_accretor_radius)**6)
  if (.not.dr_include_tides) then
    cp_donor_sync_time      = 1e7
    cp_accretor_sync_time   = 1e7
  end if
  cp_donor_sync_freq        = 0.
  cp_accretor_sync_freq     = 0.
  if (cp_donor_sync_time.gt.0.) then
    cp_donor_sync_freq    = 1./cp_donor_sync_time
  else
    call dr_abort("cp_binary_parameters","Donor syncronization time is negative")
  end if
  if (cp_accretor_sync_time.gt.0.) then 
    cp_accretor_sync_freq = 1./cp_accretor_sync_time
  else
    call dr_abort("cp_binary_parameters","Accretor syncronization time is negative") 
  end if

! Additional things that depend on mdot
  return
  end subroutine cp_binary_parameters
