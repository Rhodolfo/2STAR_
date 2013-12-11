  subroutine cp_timescales
  use ph_vars, only: ph_G,ph_k_B,ph_hubble,ph_year,ph_msun
  use dr_vars, only: dr_time,dr_step_counter
  use cp_vars, only: cp_don_mdot,cp_mass_transfer_tscale,&
                     cp_don_mass,cp_acc_mass,cp_bin_sepa,cp_gravitational_tscale,&
                     cp_roche_tscale,cp_driver_roche,cp_zeta_roche,cp_total_tscale,&
                     cp_driver_sepa,cp_zeta_sepa,cp_driver_don_radius,cp_don_zeta,cp_mass_ratio,&
                     cp_mass_ratio,cp_accretion_eff,cp_don_radius,cp_overflow_tscale,cp_overflow_par,&
                     cp_mdot_eq,cp_stream_temp,cp_stream_mu,cp_mdot_0,&
                     cp_mdot_edd,cp_mdotdot,cp_exponential_tscale,cp_q_stable,&
                     cp_mass_transfer_change_tscale,cp_min_tscale,&
                     cp_tau,cp_tau_chr,cp_tau_iso,cp_tau_star,cp_bin_peri
  use ph_interface, only: ph_eggleton_formula_zeta,ph_kepler_jorb,ph_grw_jdot
  implicit none
  real :: n_poly
  real :: df_dDR2,y0,yeq,sh,little_g
  real :: overflow_dot
  real :: jorb, jdot_grw

! Pure mass tranfer tscale
  if (cp_don_mdot.lt.0.0) cp_mass_transfer_tscale = abs(cp_don_mass/cp_don_mdot)
  if (cp_don_mdot.ge.0.0) cp_mass_transfer_tscale = 1.0 / ph_hubble

! Timescale of gravitational wave losses
  jorb     = ph_kepler_jorb(cp_don_mass,cp_acc_mass,cp_bin_sepa)
  jdot_grw =    ph_grw_jdot(cp_don_mass,cp_acc_mass,cp_bin_sepa)
  cp_gravitational_tscale = abs(jorb / jdot_grw)

! Mass transfer coupled with gravitational loss tscale of sepa
  cp_roche_tscale      = 1.0 / (abs(cp_driver_roche+cp_zeta_roche*cp_don_mdot/cp_don_mass))
  cp_total_tscale      = 1.0 / (abs(cp_driver_sepa+cp_zeta_sepa*cp_don_mdot/cp_don_mass))

! Timescale of overflow change
  overflow_dot       = cp_driver_don_radius - cp_driver_sepa &
    + (cp_don_zeta - cp_zeta_sepa - ph_eggleton_formula_zeta(cp_mass_ratio,cp_accretion_eff))&
    *cp_don_mdot/cp_don_mass
  overflow_dot       = cp_don_radius*overflow_dot
  cp_overflow_tscale = abs(cp_overflow_par/overflow_dot)
  if (cp_overflow_par.le.0.) then 
    cp_overflow_tscale = cp_total_tscale
  else
  end if
  if (abs(cp_don_mdot).le.1e-18*ph_msun/ph_year) then 
    cp_overflow_tscale = cp_bin_peri*100.
  else
  end if

! Critical tscale of system evolution, obtained through analytical estimates
  cp_mdot_eq   = - ((cp_driver_don_radius-cp_driver_roche)/(cp_don_zeta-cp_zeta_roche))*cp_don_mass
  little_g    = ph_G*cp_don_mass/(cp_don_radius**2.)
  sh          = ph_k_B*cp_stream_temp/(cp_stream_mu*little_g)
  n_poly      = 1./2.
  y0          = abs((cp_mdot_eq-cp_don_mdot)/cp_mdot_0)**(2./(2.*n_poly+3.))
  cp_tau      = abs(cp_don_mass/cp_mdot_0)
  cp_tau_chr  = abs(cp_tau / ((n_poly+1./2.)*(y0**(n_poly+1./2.))*(cp_don_zeta-cp_zeta_roche)))
  cp_tau_iso  = abs(sh/(cp_don_radius*(cp_driver_don_radius-cp_driver_roche))) 
  y0          = abs((cp_don_mdot)/cp_mdot_0)**(2./(2.*n_poly+3.)) 
  yeq         = abs((cp_mdot_eq   )/cp_mdot_0)**(2./(2.*n_poly+3.)) 
  cp_tau_star = abs((cp_driver_don_radius-cp_driver_roche) &
           * ((abs(cp_mdot_edd/cp_mdot_eq))**(2./(2.*n_poly+3.))))
  cp_tau_star = 1.0/cp_tau_star
  cp_tau_star = min(cp_tau_star,cp_tau_chr,cp_tau_iso)


! Timescale of change in mass transfer rate
  df_dDR2 = 3.0*(cp_overflow_par**2)/(cp_don_radius**3)
  cp_mdotdot = - 2.0*(cp_mdot_0/cp_don_mass)*df_dDR2*(cp_q_stable-cp_mass_ratio)*(cp_don_mdot-cp_mdot_eq)
  cp_mass_transfer_change_tscale = abs(cp_don_mdot/cp_mdotdot)
  if (cp_mass_transfer_change_tscale.le.0.)    cp_mass_transfer_change_tscale = cp_total_tscale
  if (abs(cp_mdotdot).le.0.)                   cp_mass_transfer_change_tscale = cp_total_tscale
  if (dr_time.le.0.)                           cp_exponential_tscale          = cp_mass_transfer_change_tscale

! Minimum tscale
  cp_min_tscale = min(cp_gravitational_tscale,&
                      cp_roche_tscale,&
                      cp_total_tscale,&
                      cp_overflow_tscale,&
                      cp_mass_transfer_tscale,&
                      cp_mass_transfer_change_tscale)

  return
  end subroutine cp_timescales


