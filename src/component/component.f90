  module component
  use physics, only: nucleon_mass
  implicit none

! A dummy parameter
  real :: cp_setup_var              = 0.

! Stream parameters
  real :: cp_initial_density        = 0.
  real :: cp_impact_density         = 0. 
  real :: cp_initial_potential      = 0.
  real :: cp_impact_potential       = 0.
  real :: cp_distance               = 0.
  real :: cp_arc_distance           = 0.
  real :: cp_speed                  = 0.
  real :: cp_initial_speed          = 0.
  real :: cp_impact_speed           = 0.
  real :: cp_radial_speed           = 0.
  real :: cp_tangential_speed       = 0.
  real :: cp_circularization_radius = 0.
  real :: cp_min_radius             = 0.
  real :: cp_hit_radius             = 0.
  real :: cp_ang_radius             = 0.
  real :: cp_hit_time               = 0.
  real :: cp_virial_temperature     = 0.
  real :: cp_kinetic_energy         = 0.

! Variables pertaining to the binary star system
  real :: cp_total_mass         = 0.
  real :: cp_total_mass_initial = 0.
  real :: cp_ejected_mass       = 0.
  real :: cp_ejected_mass_peak  = 0.
  real :: cp_donor_mass         = 0.
  real :: cp_donor_radius       = 0.
  real :: cp_donor_zeta         = 0.
  real :: cp_donor_freq         = 0.
  real :: cp_donor_k_factor     = 0.
  real :: cp_donor_k_zeta       = 0.
  real :: cp_donor_sync_0       = 0.
  real :: cp_donor_sync_time    = 0.
  real :: cp_donor_sync_freq    = 0.
  real :: cp_donor_density      = 0.
  real :: cp_donor_tau          = 0. 
  real :: cp_accretor_mass      = 0.
  real :: cp_accretor_radius    = 0.
  real :: cp_accretor_zeta      = 0.
  real :: cp_accretor_freq      = 0.
  real :: cp_accretor_k_factor  = 0.
  real :: cp_accretor_k_zeta    = 0.
  real :: cp_accretor_sync_0    = 0.
  real :: cp_accretor_sync_time = 0.
  real :: cp_accretor_sync_freq = 0.
  real :: cp_accretor_density   = 0.
  real :: cp_accretor_tau       = 0.
  real :: cp_binary_period      = 0.
  real :: cp_initial_period     = 0.
  real :: cp_binary_frequency   = 0.
  real :: cp_binary_separation  = 0.
  real :: cp_initial_separation = 0.
  real :: cp_roche_limit        = 0.
  real :: cp_contact_limit      = 0.
  real :: cp_v_escape           = 0.
  real :: cp_v_escape_peak      = 0.
  real :: cp_cons_wind          = 0.
  real :: cp_rtau_wind          = 0.
  real :: cp_teff_wind          = 0.
  real :: cp_cons_wind_peak     = 0.
  real :: cp_rtau_wind_peak     = 0.
  real :: cp_teff_wind_peak     = 0.
  real :: cp_cons_rad           = 0.
  real :: cp_rtau_rad           = 0.
  real :: cp_teff_rad           = 0.
  real :: cp_cons_rad_peak      = 0.
  real :: cp_rtau_rad_peak      = 0.
  real :: cp_teff_rad_peak      = 0. 
  real :: cp_cons_bubble        = 0.
  real :: cp_rtau_bubble        = 0.
  real :: cp_teff_bubble        = 0.
  real :: cp_cons_bubble_peak   = 0.
  real :: cp_rtau_bubble_peak   = 0.
  real :: cp_teff_bubble_peak   = 0. 

! Arrays for the Roche potential
  real, dimension(2) :: cp_donor_position          = 0.
  real, dimension(2) :: cp_accretor_position       = 0.
  real, dimension(2) :: cp_L1_position             = 0.
  real, dimension(2) :: cp_gravitational_field     = 0.

! Auxuliary variables for the binary
  real :: cp_mdot_0         = 0.
  real :: cp_mdot_donor     = 0.
  real :: cp_mdot_donor_old = 0.
  real :: cp_mdot_donor_max = 0.
  real :: cp_mdot_mir       = 0.
  real :: cp_mdotdot        = 0.
  real :: cp_mdot_accretor  = 0.
  real :: cp_mass_ratio     = 0.
  real :: cp_roche_radius   = 0.

! Parameters proper to the WD
  real :: cp_overflow_par               = 0.
  real :: cp_mean_nucleons_per_electron = 2.

! Variables that characterize the infalling gas
  real    :: cp_stream_temperature         = 5000.
  real    :: cp_mintemp                    = 0.
  real    :: cp_maxtemp                    = 0.
  real    :: cp_stream_molecular_mass      = 2.*nucleon_mass
  real    :: cp_stream_sound_speed         = 0.
  real    :: cp_stream_theta               = 0.

! Driver terms for the mdot evolution
  real :: cp_driver_separation          = 0.
  real :: cp_driver_roche               = 0.
  real :: cp_driver_donor_radius        = 0.
  real :: cp_driver_donor_freq          = 0.
  real :: cp_driver_accretor_freq       = 0.

! Mass transfer terms (effective exponents) for the mdot evolution
  real :: cp_zeta_separation            = 0.
  real :: cp_zeta_roche                 = 0.
  real :: cp_zeta_donor_freq            = 0.
  real :: cp_zeta_accretor_freq         = 0.
 
! Auxiliary terms for this mode of evolution
  real :: cp_mdot_eq                    = 0.
  real :: cp_q_a                        = 0.
  real :: cp_q_stable                   = 0.

! Arrays for the test particle
  real, dimension(2) :: cp_initial_position      = 0.
  real, dimension(2) :: cp_impact_position       = 0.
  real, dimension(2) :: cp_particle_position     = 0.
  real, dimension(2) :: cp_initial_velocity      = 0.
  real, dimension(2) :: cp_particle_velocity     = 0.
  real, dimension(2) :: cp_impact_velocity       = 0.
  real, dimension(2) :: cp_particle_acceleration = 0.

! Timescales
  real :: cp_exponential_timescale               = 0.
  real :: cp_exponential_timescale_old           = 0.
  real :: cp_mdotdot_timescale                   = 0.
  real :: cp_mass_transfer_timescale             = 0.
  real :: cp_mass_transfer_change_timescale      = 0.  
  real :: cp_gravitational_timescale             = 0.
  real :: cp_roche_timescale                     = 0.
  real :: cp_overflow_timescale                  = 0.
  real :: cp_total_timescale                     = 0.
! Timescales related to analytical solutions
  real :: cp_tau                                 = 0.
  real :: cp_tau_chr                             = 0.
  real :: cp_tau_iso                             = 0.
  real :: cp_tau_star                            = 0.
! Minimum of all timescales
  real :: cp_min_timescale                       = 0.

! Iteration scheme tolerances
  real :: cp_mdot_tolerance = 1.0e-6

! Eddington parameters
  real    :: cp_L_eddington            = 0.0     ! Eddington luminosity
  real    :: cp_L_eddington_infty      = 0.0     ! Eddington luminosity, from infinity
  real    :: cp_accretion_efficiency   = 0.0     ! Beta = - Mass accreted / Mdot
  real    :: cp_ejection_efficiency    = 0.0     ! Epsilon = Mass ejected / Mdot
  real    :: cp_disk_efficiency        = 0.0     ! Eta = - Mass into disk / Mdot
  real    :: cp_opacity                = 0.2     ! Opacity of accreting matter
  real    :: cp_accretion_radius       = 0.0     ! Where accretion radiates
  real    :: cp_r_isco                 = 0.0     ! Last stable orbit
  real    :: cp_binding_to_rad_eff     = 1.0     ! Efficiency of conversion grav->rad
  real    :: cp_mdot_eddington         = 3.0e-8  ! Eddington limit
  real    :: cp_mdot_eddington_infty   = 3.0e-8  ! Ignoring full Roche potential
  real    :: cp_mdot_hyper_eddington   = 3.0e-8  ! For disk accretion
  real    :: cp_potential_L1           = 0.0     ! Potential at L1
  real    :: cp_potential_Ra           = 0.0     ! Potential at accretion radius
  real    :: cp_photon_diffusion_speed = 0.0     ! Photon diffusion speed 
  real    :: cp_infalling_speed        = 0.0     ! Infalling speed of the medium
  real    :: cp_v_photon_v_infalling   = 0.0     ! Quotient of the two
  real    :: cp_initial_mdot_eddington = 3.0e-8  ! Just the initial one

  interface

    subroutine cp_reset
    implicit none
    end subroutine cp_reset



  ! Basic Computation of parameters

    subroutine cp_init(mode,m1,m2,invar,f1,f2)
    implicit none
    integer, intent(in) :: mode
    real,    intent(in) :: m1,m2,invar,f1,f2
    end subroutine cp_init

    subroutine cp_auxiliary_parameters
    implicit none
    end subroutine cp_auxiliary_parameters

    subroutine cp_star_parameters
    implicit none
    end subroutine cp_star_parameters
     
    subroutine cp_stream_parameters
    implicit none
    end subroutine cp_stream_parameters

    subroutine cp_getvirtemp(E,T,N,tol)
    implicit none
    real    :: E,T,N
    integer :: tol 
    end subroutine cp_getvirtemp



  ! Parameters of the binary system as a whole

    subroutine cp_binary_parameters
    implicit none
    end subroutine cp_binary_parameters

    subroutine cp_binary_parameters_period
    implicit none
    end subroutine cp_binary_parameters_period

    subroutine cp_binary_parameters_separation
    implicit none
    end subroutine cp_binary_parameters_separation

    subroutine cp_binary_parameters_contact
    implicit none
    end subroutine cp_binary_parameters_contact

    subroutine cp_binary_parameters_roche_limit
    implicit none
    end subroutine cp_binary_parameters_roche_limit

    subroutine cp_binary_parameters_mdot(x,n)
    implicit none
    real, intent(in) :: x
    integer, intent(in), optional :: n
    end subroutine cp_binary_parameters_mdot



  ! Properties of the circumbinary envelope

    subroutine cp_envelope
    implicit none 
    end subroutine cp_envelope



  ! Lagrangian points
    
    subroutine cp_get_L1
    implicit none
    end subroutine cp_get_L1

    function cp_roche_1D(x)
    implicit none
    real :: x,cp_roche_1D
    end function cp_roche_1D



  !  Mass transfer rates

    subroutine cp_get_mdot
    implicit none
    end subroutine cp_get_mdot

    subroutine cp_get_mdot_donor
    implicit none
    end subroutine cp_get_mdot_donor

    subroutine cp_get_mdot_accretor
    implicit none
    end subroutine cp_get_mdot_accretor


 
   ! Super Eddington terms

   subroutine cp_super_Eddington
   implicit none
   end subroutine cp_super_Eddington

   subroutine cp_hyper_Eddington
   implicit none
   end subroutine cp_hyper_Eddington



  ! Parameters relevant to the source terms of mdot

    subroutine cp_evolution_coefficients
    implicit none
    end subroutine cp_evolution_coefficients

    subroutine cp_driver_terms
    implicit none
    end subroutine cp_driver_terms

    subroutine cp_zeta_terms
    implicit none
    end subroutine cp_zeta_terms

    subroutine cp_timescales
    implicit none
    end subroutine cp_timescales



  end interface 

  end module component
