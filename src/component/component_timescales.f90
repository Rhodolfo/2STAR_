  subroutine cp_timescales
  use   physics, only: G,k_B,H_0,ph_eggleton_formula_zeta,&
                       ph_kepler_jorb,ph_grw_jdot,year,solar_mass
  use    driver, only: dr_time,dr_step_counter
  use component, only: cp_mdot_donor,cp_mass_transfer_timescale,&
                       cp_donor_mass,cp_accretor_mass,cp_binary_separation,cp_gravitational_timescale,&
                       cp_roche_timescale,cp_driver_roche,cp_zeta_roche,cp_total_timescale,&
                       cp_driver_separation,cp_zeta_separation,cp_driver_donor_radius,cp_donor_zeta,cp_mass_ratio,&
                       cp_mass_ratio,cp_accretion_efficiency,cp_donor_radius,cp_overflow_timescale,cp_overflow_par,&
                       cp_mdot_eq,cp_stream_temperature,cp_stream_molecular_mass,cp_mdot_0,&
                       cp_mdot_eddington,cp_mdotdot,cp_exponential_timescale,cp_q_stable,&
                       cp_mass_transfer_change_timescale,cp_min_timescale,&
                       cp_tau,cp_tau_chr,cp_tau_iso,cp_tau_star,cp_binary_period
  implicit none
  real :: n_poly
  real :: df_dDR2,y0,yeq,sh,little_g
  real :: overflow_dot
  real :: jorb, jdot_grw

! Pure mass tranfer timescale
  if (cp_mdot_donor.lt.0.0) cp_mass_transfer_timescale = abs(cp_donor_mass/cp_mdot_donor)
  if (cp_mdot_donor.ge.0.0) cp_mass_transfer_timescale = 1.0 / H_0

! Timescale of gravitational wave losses
  jorb     = ph_kepler_jorb(cp_donor_mass,cp_accretor_mass,cp_binary_separation)
  jdot_grw =    ph_grw_jdot(cp_donor_mass,cp_accretor_mass,cp_binary_separation)
  cp_gravitational_timescale = abs(jorb / jdot_grw)

! Mass transfer coupled with gravitational loss timescale of separation
  cp_roche_timescale      = 1.0 / (abs(cp_driver_roche+cp_zeta_roche*cp_mdot_donor/cp_donor_mass))
  cp_total_timescale      = 1.0 / (abs(cp_driver_separation+cp_zeta_separation*cp_mdot_donor/cp_donor_mass))

! Timescale of overflow change
  overflow_dot       = cp_driver_donor_radius - cp_driver_separation &
    + (cp_donor_zeta - cp_zeta_separation - ph_eggleton_formula_zeta(cp_mass_ratio,cp_accretion_efficiency))&
    *cp_mdot_donor/cp_donor_mass
  overflow_dot       = cp_donor_radius*overflow_dot
  cp_overflow_timescale = abs(cp_overflow_par/overflow_dot)
  if (cp_overflow_par.le.0.) then 
    cp_overflow_timescale = cp_total_timescale
  else
  end if
  if (abs(cp_mdot_donor).le.1e-18*solar_mass/year) then 
    cp_overflow_timescale = cp_binary_period*100.
  else
  end if

! Critical timescale of system evolution, obtained through analytical estimates
  cp_mdot_eq   = - ((cp_driver_donor_radius-cp_driver_roche)/(cp_donor_zeta-cp_zeta_roche))*cp_donor_mass
  little_g    = G*cp_donor_mass/(cp_donor_radius**2.)
  sh          = k_B*cp_stream_temperature/(cp_stream_molecular_mass*little_g)
  n_poly      = 1./2.
  y0          = abs((cp_mdot_eq-cp_mdot_donor)/cp_mdot_0)**(2./(2.*n_poly+3.))
  cp_tau      = abs(cp_donor_mass/cp_mdot_0)
  cp_tau_chr  = abs(cp_tau / ((n_poly+1./2.)*(y0**(n_poly+1./2.))*(cp_donor_zeta-cp_zeta_roche)))
  cp_tau_iso  = abs(sh/(cp_donor_radius*(cp_driver_donor_radius-cp_driver_roche))) 
  y0          = abs((cp_mdot_donor)/cp_mdot_0)**(2./(2.*n_poly+3.)) 
  yeq         = abs((cp_mdot_eq   )/cp_mdot_0)**(2./(2.*n_poly+3.)) 
  cp_tau_star = abs((cp_driver_donor_radius-cp_driver_roche) &
           * ((abs(cp_mdot_eddington/cp_mdot_eq))**(2./(2.*n_poly+3.))))
  cp_tau_star = 1.0/cp_tau_star
  cp_tau_star = min(cp_tau_star,cp_tau_chr,cp_tau_iso)


! Timescale of change in mass transfer rate
  df_dDR2 = 3.0*(cp_overflow_par**2)/(cp_donor_radius**3)
  cp_mdotdot = - 2.0*(cp_mdot_0/cp_donor_mass)*df_dDR2*(cp_q_stable-cp_mass_ratio)*(cp_mdot_donor-cp_mdot_eq)
  cp_mass_transfer_change_timescale    = abs(cp_mdot_donor/cp_mdotdot)
  if (cp_mass_transfer_change_timescale.le.0.) cp_mass_transfer_change_timescale = cp_total_timescale
  if (abs(cp_mdotdot).le.0.)                   cp_mass_transfer_change_timescale = cp_total_timescale
  if (dr_time.le.0.)                           cp_exponential_timescale          = cp_mass_transfer_change_timescale

! Minimum timescale
  cp_min_timescale = min(cp_gravitational_timescale,&
                      cp_roche_timescale,&
                      cp_total_timescale,&
                      cp_overflow_timescale,&
                      cp_mass_transfer_timescale,&
                      cp_mass_transfer_change_timescale)
! write(*,*) "FLAG",&
!           &cp_gravitational_timescale,cp_roche_timescale,&
!           cp_total_timescale,cp_overflow_timescale,cp_mass_transfer_timescale,cp_mass_transfer_change_timescale
  return
  end subroutine cp_timescales


