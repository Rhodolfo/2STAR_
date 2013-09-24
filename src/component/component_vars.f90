  module cp_vars
  use ph_vars, only: ph_amu
  implicit none

! A dummy parameter
  real :: cp_setup_var              = 0.

! Stream parameters
  real :: cp_initial_dens           = 0.
  real :: cp_impact_dens            = 0. 
  real :: cp_initial_potential      = 0.
  real :: cp_impact_potential       = 0.
  real :: cp_distance               = 0.
  real :: cp_arc_distance           = 0.
  real :: cp_speed                  = 0.
  real :: cp_initial_speed          = 0.
  real :: cp_impact_speed           = 0.
  real :: cp_radial_speed           = 0.
  real :: cp_tangential_speed       = 0.
  real :: cp_cir_radius             = 0.
  real :: cp_min_radius             = 0.
  real :: cp_hit_radius             = 0.
  real :: cp_ang_radius             = 0.
  real :: cp_hit_time               = 0.
  real :: cp_virtemp                = 0.
  real :: cp_ekin                   = 0.

! Variables pertaining to the binary star system
  real :: cp_tot_mass         = 0.
  real :: cp_tot_mass_initial = 0.
  real :: cp_eje_mass         = 0.
  real :: cp_eje_mass_peak    = 0.
  real :: cp_bin_peri         = 0.
  real :: cp_initial_peri     = 0.
  real :: cp_bin_freq         = 0.
  real :: cp_bin_sepa         = 0.
  real :: cp_initial_sepa     = 0.
  real :: cp_roche_limit      = 0.
  real :: cp_contact_limit    = 0.

! Donor variables
  real :: cp_don_mass         = 0.
  real :: cp_don_radius       = 0.
  real :: cp_don_zeta         = 0.
  real :: cp_don_freq         = 0.
  real :: cp_don_k_factor     = 0.
  real :: cp_don_k_zeta       = 0.
  real :: cp_don_sync_0       = 0.
  real :: cp_don_sync_time    = 0.
  real :: cp_don_sync_freq    = 0.
  real :: cp_don_dens         = 0.
  real :: cp_don_tau          = 0. 
  real :: cp_don_mdot         = 0.

! Accretor variables
  real :: cp_acc_mass         = 0.
  real :: cp_acc_radius       = 0.
  real :: cp_acc_zeta         = 0.
  real :: cp_acc_freq         = 0.
  real :: cp_acc_k_factor     = 0.
  real :: cp_acc_k_zeta       = 0.
  real :: cp_acc_sync_0       = 0.
  real :: cp_acc_sync_time    = 0.
  real :: cp_acc_sync_freq    = 0.
  real :: cp_acc_dens         = 0.
  real :: cp_acc_tau          = 0.
  real :: cp_acc_mdot

! Envelope variables
  real :: cp_vesc             = 0.
  real :: cp_vesc_peak        = 0.
  real :: cp_env_radius       = 0.
  real :: cp_env_mass         = 0.
  real :: cp_env_mdot         = 0. 
  real :: cp_env_mdot_in      = 0.
  real :: cp_env_mdot_out     = 0.
  real :: cp_env_teff         = 0.
  real :: cp_env_vesc         = 0.
  real :: cp_wind_mdot        = 0.
  real :: cp_env_toosmall     = 0.

! Period derivatives
  real :: cp_pdot_total       = 0.
  real :: cp_pdot_grw         = 0.
  real :: cp_pdot_dontid      = 0.
  real :: cp_pdot_acctid      = 0.
  real :: cp_pdot_mdot        = 0.
  real :: cp_pdot_massfl      = 0.

! Arrays for the Roche potential
  real, dimension(2) :: cp_don_pos  = 0.
  real, dimension(2) :: cp_acc_pos  = 0.
  real, dimension(2) :: cp_L1_pos   = 0.
  real, dimension(2) :: cp_geff     = 0.

! Auxuliary variables for the binary
  real :: cp_mdot_0       = 0.
  real :: cp_mdot_old     = 0.
  real :: cp_mdot_max     = 0.
  real :: cp_mdot_mir     = 0.
  real :: cp_mdotdot      = 0.
  real :: cp_mass_ratio   = 0.
  real :: cp_roche_radius = 0.

! Parameters proper to the WD
! Variables that characterize the infalling gas
  real :: cp_stream_temp  = 5000.
  real :: cp_mintemp      = 0.
  real :: cp_maxtemp      = 0.
  real :: cp_stream_mu    = 1.
  real :: cp_stream_mu_e  = 2.
  real :: cp_stream_cs    = 0.
  real :: cp_stream_theta = 0.
  real :: cp_overflow_par = 0.
  real :: cp_mu_e         = 2.

! Driver terms for the mdot evolution
  real :: cp_driver_sepa        = 0.
  real :: cp_driver_roche       = 0.
  real :: cp_driver_don_radius  = 0.
  real :: cp_driver_don_freq    = 0.
  real :: cp_driver_acc_freq    = 0.

! Separating out the driver terms for data output
  real :: cp_driver_sepa_grw    = 0.
  real :: cp_driver_sepa_dontid = 0.
  real :: cp_driver_sepa_acctid = 0.

! Mass transfer terms (effective exponents) for the mdot evolution
  real :: cp_zeta_sepa      = 0.
  real :: cp_zeta_roche     = 0.
  real :: cp_zeta_don_freq  = 0.
  real :: cp_zeta_acc_freq  = 0.
 
! Auxiliary terms for this mode of evolution
  real :: cp_mdot_eq                    = 0.
  real :: cp_q_a                        = 0.
  real :: cp_q_stable                   = 0.

! Arrays for the test particle
  real, dimension(2) :: cp_initial_pos      = 0.
  real, dimension(2) :: cp_impact_pos       = 0.
  real, dimension(2) :: cp_particle_pos     = 0.
  real, dimension(2) :: cp_initial_vel      = 0.
  real, dimension(2) :: cp_impact_vel       = 0.
  real, dimension(2) :: cp_particle_vel     = 0.
  real, dimension(2) :: cp_particle_acc     = 0.

! Timescales
  real :: cp_exponential_tscale           = 0.
  real :: cp_exponential_tscale_old       = 0.
  real :: cp_mdotdot_tscale               = 0.
  real :: cp_mass_transfer_tscale         = 0.
  real :: cp_mass_transfer_change_tscale  = 0.  
  real :: cp_gravitational_tscale         = 0.
  real :: cp_roche_tscale                 = 0.
  real :: cp_overflow_tscale              = 0.
  real :: cp_total_tscale                 = 0.

! Timescales related to analytical solutions
  real :: cp_tau                          = 0.
  real :: cp_tau_chr                      = 0.
  real :: cp_tau_iso                      = 0.
  real :: cp_tau_star                     = 0.
  real :: cp_min_tscale                   = 0.

! Iteration scheme tolerances
  real :: cp_mdot_tolerance = 1.0e-6

! Eddington parameters
  real    :: cp_L_edd              = 0.0     ! Eddington luminosity
  real    :: cp_L_edd_infty        = 0.0     ! Eddington luminosity, from infinity
  real    :: cp_accretion_eff      = 0.0     ! Beta = - Mass accreted / Mdot
  real    :: cp_ejection_eff       = 0.0     ! Epsilon = Mass eje / Mdot
  real    :: cp_disk_eff           = 0.0     ! Eta = - Mass into disk / Mdot
  real    :: cp_opacity            = 0.2     ! Opacity of accreting matter
  real    :: cp_accretion_radius   = 0.0     ! Where accretion radiates
  real    :: cp_r_isco             = 0.0     ! Last stable orbit
  real    :: cp_binding_to_rad_eff = 1.0     ! Efficiency of conversion grav->rad
  real    :: cp_mdot_edd           = 3.0e-8  ! Eddington limit
  real    :: cp_mdot_edd_infty     = 3.0e-8  ! Ignoring full Roche potential
  real    :: cp_mdot_hyper_edd     = 3.0e-8  ! For disk accretion
  real    :: cp_pot_L1             = 0.0     ! Potential at L1
  real    :: cp_pot_Ra             = 0.0     ! Potential at accretion radius
  real    :: cp_photon_diff_speed  = 0.0     ! Photon diffusion speed 
  real    :: cp_infall_speed       = 0.0     ! Infalling speed of the medium
  real    :: cp_v_photon_v_infall  = 0.0     ! Quotient of the two
  real    :: cp_initial_mdot_edd   = 3.0e-8  ! Just the initial one
  
  end module cp_vars
