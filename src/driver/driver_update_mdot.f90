
subroutine dr_update_mdot
  use rho_ode_solver, only: rho_reject_step,rho_dt_suggested
  use   physics, only: chandra_mass
  use    driver, only: dr_hybrid,dr_include_tides,dr_accretion_flow,&
                       dr_is_sub_eddington,dr_is_super_eddington,&
                       dr_time,dr_time_step,dr_mode_separation,dr_time_peak,&
                       dr_mdot_vector_old,dr_mdot_vector_new,dr_exit_trigger,&
                       dr_mode_separation,dr_set_time_step,dr_time_eddington_elapsed,&
                       dr_accretor_mode,dr_mode_he_white_dwarf,dr_mode_co_white_dwarf
  use component, only: cp_mdot_donor,cp_mdot_donor_old,&
                       cp_exponential_timescale,cp_exponential_timescale_old,&
                       cp_init,cp_mdot_donor_max,cp_virial_temperature,cp_mintemp,cp_maxtemp,&
                       cp_total_mass,cp_total_mass_initial,cp_ejected_mass,cp_ejected_mass_peak,cp_v_escape,cp_v_escape_peak,&
                       cp_cons_wind,cp_cons_wind_peak,cp_rtau_wind,cp_rtau_wind_peak,cp_teff_wind,cp_teff_wind_peak,&
                       cp_cons_rad,cp_cons_rad_peak,cp_rtau_rad,cp_rtau_rad_peak,cp_teff_rad,cp_teff_rad_peak,&
                       cp_cons_bubble,cp_cons_bubble_peak,cp_rtau_bubble,cp_rtau_bubble_peak,cp_teff_bubble,cp_teff_bubble_peak
  implicit none
  real :: donmass,accmass,invar,donfreq,accfreq
  logical :: nullmass,superchandra,accwd
! Advancing time and shit
  if (rho_reject_step) then 
    dr_time_step = min(0.9*rho_dt_suggested,0.5*dr_time_step)
    return
  else 
  end if
! Advancing time and storing data that needs to be saved
  dr_time            = dr_time + dr_time_step
  dr_mdot_vector_old = dr_mdot_vector_new
  cp_ejected_mass    = abs(cp_total_mass - cp_total_mass_initial)
  cp_exponential_timescale_old = cp_exponential_timescale
! Extracting data from the driver data vector
  invar   = dr_mdot_vector_old(1)
  donmass = dr_mdot_vector_old(2)
  accmass = dr_mdot_vector_old(3)
  if (dr_include_tides) donfreq = dr_mdot_vector_old(4)
  if (dr_include_tides) accfreq = dr_mdot_vector_old(5) 
  if (dr_accretion_flow.eq.dr_is_super_eddington) then
    dr_time_eddington_elapsed = dr_time_eddington_elapsed + dr_time_step
  end if
! Dealing with total mass transfer or super chandrashekar things
  accwd = (dr_accretor_mode.eq.dr_mode_he_white_dwarf)&
      .or.(dr_accretor_mode.eq.dr_mode_co_white_dwarf)
  superchandra = (accmass.ge.0.98*chandra_mass).and.accwd
  nullmass     = donmass.le.0.
  if (superchandra.or.nullmass) then 
    dr_exit_trigger = .true.
    return 
  else
    call cp_init(dr_mode_separation,donmass,accmass,invar,donfreq,accfreq)
    call dr_set_time_step
  end if
! Saving some data
  cp_mdot_donor_old = cp_mdot_donor 
  if (abs(cp_mdot_donor).ge.abs(cp_mdot_donor_max)) then 
    dr_time_peak         = dr_time
    cp_v_escape_peak     = cp_v_escape
    cp_cons_wind_peak    = cp_cons_wind
    cp_rtau_wind_peak    = cp_rtau_wind
    cp_teff_wind_peak    = cp_teff_wind
    cp_cons_rad_peak     = cp_cons_rad
    cp_rtau_rad_peak     = cp_rtau_rad
    cp_teff_rad_peak     = cp_teff_rad
    cp_cons_bubble_peak  = cp_cons_bubble
    cp_rtau_bubble_peak  = cp_rtau_bubble
    cp_teff_bubble_peak  = cp_teff_bubble 
    cp_ejected_mass_peak = cp_ejected_mass
    cp_mdot_donor_max    = cp_mdot_donor
  end if
! Hybrid mode things
  if (dr_hybrid) then
    cp_mintemp = min(cp_mintemp,cp_virial_temperature)
    cp_maxtemp = max(cp_maxtemp,cp_virial_temperature)
  end if
return
end subroutine dr_update_mdot
