  subroutine dr_init_mdot(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  use rho_ode_solver, only: rho_ref_scale,rho_error_control
  use   physics, only: ph_norm
  use    driver, only: dr_hybrid,dr_accretion_flow,dr_eddington_switch,dr_eddington_exit_switch,&
                       dr_is_sub_eddington,dr_time_tolerance,dr_time_step_tolerance,dr_time,dr_time_step,&
                       dr_set_time_step,dr_mdot_vector_old,dr_mdot_vector_new,dr_include_tides,dr_res_factor,&
                       dr_time_peak
  use component, only: cp_init,cp_mintemp,cp_maxtemp,cp_total_mass,cp_total_mass_initial,&
                       cp_initial_mdot_eddington,cp_initial_separation,cp_initial_period,&
                       cp_mdot_eddington,cp_binary_separation,cp_binary_period,&
                       cp_mdot_donor_max,cp_mdot_donor,cp_tau_chr,cp_tau_iso,cp_tau_star,cp_tau,&
                       cp_donor_mass,cp_accretor_mass,cp_donor_freq,cp_accretor_freq,cp_virial_temperature
  use      IO,   only: IO_write_header,IO_open,IO_log
  implicit none
  integer, intent(in) :: setup_mode
  real   , intent(in) :: donmass,accmass,invar,donfreq,accfreq
  rho_error_control = .true.
  call cp_init(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  if (dr_hybrid) then 
    cp_mintemp = cp_virial_temperature
    cp_maxtemp = cp_virial_temperature
  end if
  cp_initial_mdot_eddington = cp_mdot_eddington
  cp_initial_separation     = cp_binary_separation
  cp_initial_period         = cp_binary_period
  cp_mdot_donor_max         = cp_mdot_donor
  cp_total_mass_initial     = cp_total_mass
  dr_accretion_flow         = dr_is_sub_eddington
  dr_eddington_switch       = .false.
  dr_eddington_exit_switch  = .false.
  dr_time_peak              = 0.0
  dr_time_tolerance         = 1e4*max(cp_tau_chr,cp_tau_iso,cp_tau_star,cp_tau)
  dr_time_step_tolerance    = dr_res_factor*dr_time_tolerance
  dr_time                   = 0.
  call dr_set_time_step    
  if (allocated(dr_mdot_vector_old)) deallocate(dr_mdot_vector_old)
  if (allocated(dr_mdot_vector_new)) deallocate(dr_mdot_vector_new) 
  if (allocated(rho_ref_scale))      deallocate(rho_ref_scale)
  call IO_log("[driver] Allocating data arrays for mdot evolution, saving data in arrauys")
  if (dr_include_tides) then 
    allocate(dr_mdot_vector_old(5),dr_mdot_vector_new(5),rho_ref_scale(5))
    dr_mdot_vector_old(1) = cp_binary_separation
    dr_mdot_vector_old(2) = cp_donor_mass
    dr_mdot_vector_old(3) = cp_accretor_mass
    dr_mdot_vector_old(4) = cp_donor_freq
    dr_mdot_vector_old(5) = cp_accretor_freq
  else
    allocate(dr_mdot_vector_old(3),dr_mdot_vector_new(3),rho_ref_scale(3))
    dr_mdot_vector_old(1) = cp_binary_separation
    dr_mdot_vector_old(2) = cp_donor_mass
    dr_mdot_vector_old(3) = cp_accretor_mass
  end if  
  rho_ref_scale      = dr_res_factor*dr_mdot_vector_old
  dr_mdot_vector_new = dr_mdot_vector_old
  call IO_log("[driver] Arrays allocated, initial conditions have been set.")
  return 
  end subroutine dr_init_mdot
