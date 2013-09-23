  subroutine cp_binary_parameters
  use ph_vars, only: ph_pi,ph_G,ph_sigma_sb,ph_year,ph_msun
  use dr_vars, only: dr_setup_mode,dr_mode_period,dr_mode_separation,&
                     dr_mode_overflow_eq,dr_mode_separation_eq,&
                     dr_mode_contact,dr_mode_roche_limit,&
                     dr_mode_mdot,dr_time,dr_include_tides,dr_initial_mdot_tstep,&
                     dr_time,dr_time_step
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_radius,cp_acc_radius,&
                     cp_don_pos,cp_acc_pos,cp_don_sync_0,cp_acc_sync_0,&
                     cp_don_sync_time,cp_acc_sync_time,&
                     cp_don_sync_freq,cp_acc_sync_freq,&
                     cp_bin_freq,cp_min_radius,cp_cir_radius,cp_roche_radius,&
                     cp_mass_ratio,cp_bin_sepa,cp_bin_peri, &
                     cp_roche_limit,cp_contact_limit,cp_overflow_par,cp_setup_var,&
                     cp_vesc,cp_L_edd,cp_don_mdot,cp_opacity,cp_tot_mass,cp_don_mdot
  use ph_interface, only: ph_eggleton_formula,ph_eggleton_rcirc,ph_eggleton_rmin 
  use dr_interface, only: dr_abort
  use cp_interface, only: cp_binary_parameters_period,cp_binary_parameters_separation,&
                          cp_binary_parameters_contact,cp_binary_parameters_roche_limit,&
                          cp_binary_parameters_mdot,cp_get_L1,cp_get_mdot,cp_envelope
  use io_interface, only: io_log
  implicit none

! Binary sepa and peri are calculated here
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

! Sanity checks
  if (cp_bin_peri.le.0.0) call io_log("[cp_system] Period is negative")
  if (cp_bin_sepa.le.0.0) call io_log("[cp_system] Separation is negative")

! Calculating some binary physical parameters
  cp_tot_mass     = cp_acc_mass+cp_don_mass
  cp_roche_radius = cp_bin_sepa*ph_eggleton_formula(cp_mass_ratio)
  cp_cir_radius   = cp_bin_sepa*ph_eggleton_rcirc(cp_mass_ratio)
  cp_min_radius   = cp_bin_sepa*ph_eggleton_rmin(cp_mass_ratio)
  cp_bin_freq     = 2.0*ph_pi / cp_bin_peri
  cp_don_pos      = (/-(cp_acc_mass/cp_tot_mass)*cp_bin_sepa, 0./)
  cp_acc_pos      = (/ (cp_don_mass   /cp_tot_mass)*cp_bin_sepa, 0./)
  cp_overflow_par = cp_don_radius - cp_roche_radius 
  cp_vesc         = sqrt(2.*ph_G*(cp_tot_mass)/cp_roche_radius)

! Finding Lagrange point, calculating mass transfer rate and computing envelope properties
  call cp_get_L1
  call cp_get_mdot
  call cp_envelope

! Let's handle some tidal terms
  if (dr_initial_mdot_tstep) then 
    call IO_log("[component] Saving initial syncronization timescales and relevant data")
    cp_don_sync_0      = cp_don_sync_0/((cp_mass_ratio**2)*((cp_bin_sepa/cp_don_radius)**6))
    cp_acc_sync_0      = cp_acc_sync_0/(((1./cp_mass_ratio)**2)*((cp_bin_sepa/cp_acc_radius)**6))
  end if
  cp_don_sync_time     = cp_don_sync_0*(cp_mass_ratio**2)*((cp_bin_sepa/cp_don_radius)**6)
  cp_acc_sync_time     = cp_acc_sync_0*((1./cp_mass_ratio)**2)*((cp_bin_sepa/cp_acc_radius)**6)
  if (.not.dr_include_tides) then
    cp_don_sync_time   = 1e7
    cp_acc_sync_time   = 1e7
  end if
  cp_don_sync_freq     = 0.
  cp_acc_sync_freq     = 0.
  if (cp_don_sync_time.gt.0.) then
    cp_don_sync_freq   = 1./cp_don_sync_time
  else
    call dr_abort("cp_binary_parameters","Donor syncronization time is negative")
  end if
  if (cp_acc_sync_time.gt.0.) then 
    cp_acc_sync_freq   = 1./cp_acc_sync_time
  else
    call dr_abort("cp_binary_parameters","Accretor syncronization time is negative") 
  end if

! Additional things that depend on mdot
  return
  end subroutine cp_binary_parameters
