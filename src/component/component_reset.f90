subroutine cp_reset
  use component
! This routine resets all data to the default values
  implicit none
  cp_setup_var              = 0.
! Stream parameters
  cp_initial_density        = 0.
  cp_impact_density         = 0. 
  cp_initial_potential      = 0.
  cp_impact_potential       = 0.
  cp_distance               = 0.
  cp_arc_distance           = 0.
  cp_speed                  = 0.
  cp_initial_speed          = 0.
  cp_impact_speed           = 0.
  cp_radial_speed           = 0.
  cp_tangential_speed       = 0.
  cp_circularization_radius = 0.
  cp_min_radius             = 0.
  cp_hit_radius             = 0.
  cp_ang_radius             = 0.
  cp_hit_time               = 0.
  cp_virial_temperature     = 0.
  cp_kinetic_energy         = 0.

! Variables pertaining to the binary star system
  cp_total_mass         = 0.
  cp_total_mass_initial = 0.
  cp_ejected_mass       = 0.
  cp_donor_mass         = 0.
  cp_donor_radius       = 0.
  cp_donor_zeta         = 0.
  cp_donor_freq         = 0.
  cp_donor_k_factor     = 0.
  cp_donor_k_zeta       = 0.
  cp_donor_sync_0       = 0.
  cp_donor_sync_time    = 0.
  cp_donor_sync_freq    = 0.
  cp_donor_density      = 0.
  cp_donor_tau          = 0. 
  cp_accretor_mass      = 0.
  cp_accretor_radius    = 0.
  cp_accretor_zeta      = 0.
  cp_accretor_freq      = 0.
  cp_accretor_k_factor  = 0.
  cp_accretor_k_zeta    = 0.
  cp_accretor_sync_0    = 0.
  cp_accretor_sync_time = 0.
  cp_accretor_sync_freq = 0.
  cp_accretor_density   = 0.
  cp_accretor_tau       = 0.
  cp_binary_period      = 0.
  cp_initial_period     = 0.
  cp_binary_frequency   = 0.
  cp_binary_separation  = 0.
  cp_initial_separation = 0.
  cp_roche_limit        = 0.
  cp_contact_limit      = 0.
  cp_v_escape           = 0.
  cp_cons_wind          = 0.
  cp_rtau_wind          = 0.
  cp_teff_wind          = 0.
  cp_cons_rad           = 0.
  cp_rtau_rad           = 0.
  cp_teff_rad           = 0.
  cp_cons_bubble        = 0.
  cp_rtau_bubble        = 0.
  cp_teff_bubble        = 0.

! Arrays for the Roche potential
  cp_donor_position          = 0.
  cp_accretor_position       = 0.
  cp_L1_position             = 0.
  cp_gravitational_field     = 0.

! Auxuliary variables for the binary
  cp_mdot_0         = 0.
  cp_mdot_donor     = 0.
  cp_mdot_donor_old = 0.
  cp_mdot_donor_max = 0.
  cp_mdot_mir       = 0.
  cp_mdotdot        = 0.
  cp_mdot_accretor  = 0.
  cp_mass_ratio     = 0.
  cp_roche_radius   = 0.

! Parameters proper to the WD
  cp_overflow_par               = 0.
  cp_mean_nucleons_per_electron = 2.

! Variables that characterize the infalling gas
  cp_stream_temperature         = 5000.
  cp_mintemp                    = 0.
  cp_maxtemp                    = 0.
  cp_stream_molecular_mass      = 2.*nucleon_mass
  cp_stream_sound_speed         = 0.
  cp_stream_theta               = 0.

! Driver terms for the mdot evolution
  cp_driver_separation          = 0.
  cp_driver_roche               = 0.
  cp_driver_donor_radius        = 0.
  cp_driver_donor_freq          = 0.
  cp_driver_accretor_freq       = 0.

! Mass transfer terms (effective exponents) for the mdot evolution
  cp_zeta_separation            = 0.
  cp_zeta_roche                 = 0.
  cp_zeta_donor_freq            = 0.
  cp_zeta_accretor_freq         = 0.

! Auxiliary terms for this mode of evolution
  cp_mdot_eq                    = 0.
  cp_q_a                        = 0.
  cp_q_stable                   = 0.

! Arrays for the test particle
  cp_initial_position      = 0.
  cp_impact_position       = 0.
  cp_particle_position     = 0.
  cp_initial_velocity      = 0.
  cp_particle_velocity     = 0.
  cp_impact_velocity       = 0.
  cp_particle_acceleration = 0.

! Timescales
  cp_exponential_timescale               = 0.
  cp_exponential_timescale_old           = 0.
  cp_mdotdot_timescale                   = 0.
  cp_mass_transfer_timescale             = 0.
  cp_mass_transfer_change_timescale      = 0.  
  cp_gravitational_timescale             = 0.
  cp_roche_timescale                     = 0.
  cp_overflow_timescale                  = 0.
  cp_total_timescale                     = 0.
! Timescales related to analytical solutions
  cp_tau                                 = 0.
  cp_tau_chr                             = 0.
  cp_tau_iso                             = 0.
  cp_tau_star                            = 0.
! Minimum of all timescales
  cp_min_timescale                       = 0.

! Iteration scheme tolerances
  cp_mdot_tolerance = 1.0e-6

! Eddington parameters
  cp_L_eddington            = 0.0     ! Eddington luminosity
  cp_accretion_efficiency   = 0.0     ! Beta = - Mass accreted / Mdot
  cp_ejection_efficiency    = 0.0     ! Epsilon = Mass ejected / Mdot
  cp_disk_efficiency        = 0.0     ! Eta = - Mass into disk / Mdot
  cp_opacity                = 0.2     ! Opacity of accreting matter
  cp_accretion_radius       = 0.0     ! Where accretion radiates
  cp_r_isco                 = 0.0     ! Last stable orbit
  cp_binding_to_rad_eff     = 1.0     ! Efficiency of conversion grav->rad
  cp_mdot_eddington         = 3.0e-8  ! Eddington limit
  cp_mdot_eddington_infty   = 3.0e-8  ! Ignpring full Roche potential
  cp_mdot_hyper_eddington   = 3.0e-8  ! For disk accretion
  cp_potential_L1           = 0.0     ! Potential at L1
  cp_potential_Ra           = 0.0     ! Potential at accretion radius
  cp_photon_diffusion_speed = 0.0     ! Photon diffusion speed 
  cp_infalling_speed        = 0.0     ! Infalling speed of the medium
  cp_v_photon_v_infalling   = 0.0     ! Quotient of the two
  cp_initial_mdot_eddington = 3.0e-8  ! Just the initial one

end subroutine cp_reset
