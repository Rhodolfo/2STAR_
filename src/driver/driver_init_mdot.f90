  subroutine dr_init_mdot(setup_mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  use     ode, only: ode_ref_scale,ode_error_control
  use ph_vars, only: ph_msun
  use dr_vars, only: dr_hybrid,dr_accretion_flow,dr_eddington_switch,dr_eddington_exit_switch,&
                     dr_is_sub_eddington,dr_time_tolerance,dr_time_step_tolerance,dr_time,dr_time_step,&
                     dr_mdot_vector_old,dr_mdot_vector_new,dr_include_tides,dr_res_factor,&
                     dr_time_peak,dr_mdot_new,dr_mdot_ref,dr_initial_mdot_tstep,&
                     sepa_var,mdon_var,macc_var,menv_var,facc_var,fdon_var,dr_step_counter,&
                     dr_period_new,dr_period_ref
  use cp_vars, only: cp_mintemp,cp_maxtemp,cp_tot_mass,cp_tot_mass_initial,&
                     cp_initial_mdot_edd,cp_initial_sepa,cp_initial_peri,&
                     cp_mdot_edd,cp_bin_sepa,cp_bin_peri,cp_bin_freq,&
                     cp_mdot_max,cp_don_mdot,cp_tau_chr,cp_tau_iso,cp_tau_star,cp_tau,&
                     cp_don_mass,cp_acc_mass,cp_don_freq,cp_acc_freq,cp_virtemp,&
                     cp_don_mdot,cp_don_radius,cp_roche_radius,cp_gravitational_tscale
  use dr_interface, only: dr_set_time_step,dr_abort
  use ph_interface, only: ph_norm
  use cp_interface, only: cp_init
  use io_interface, only: io_write_header,io_open,io_log
  implicit none
  integer, intent(in) :: setup_mode
  real   , intent(in) :: donmass,accmass,envmass,invar,donfreq,accfreq
  ode_error_control     = .true.
  dr_initial_mdot_tstep = .true.
  call cp_init(setup_mode,donmass,accmass,invar,envmass,donfreq,accfreq)
  if (dr_hybrid) then 
    cp_mintemp = cp_virtemp
    cp_maxtemp = cp_virtemp
  end if
  cp_initial_mdot_edd       = cp_mdot_edd
  cp_initial_sepa           = cp_bin_sepa
  cp_initial_peri           = cp_bin_peri
  cp_mdot_max               = cp_don_mdot
  cp_tot_mass_initial       = cp_tot_mass
  dr_accretion_flow         = dr_is_sub_eddington
  dr_eddington_switch       = .false.
  dr_eddington_exit_switch  = .false.
  dr_time_peak              = 0.0
  dr_time_tolerance         = 1e3*max(cp_tau_chr,cp_tau_iso,cp_tau_star,cp_tau)
  if (cp_roche_radius.ge.1.01*cp_don_radius) then 
    call io_log("[driver] The onset of mass transfer is far away, evolving for 10^4 GRW timescales")
    dr_time_tolerance         = 1e4*cp_gravitational_tscale
  end if
  dr_time_step_tolerance    = dr_res_factor*dr_time_tolerance
  dr_time                   = 0.
  dr_step_counter           = 0
  call dr_set_time_step    
  if (allocated(dr_mdot_vector_old)) deallocate(dr_mdot_vector_old)
  if (allocated(dr_mdot_vector_new)) deallocate(dr_mdot_vector_new) 
  if (allocated(ode_ref_scale))      deallocate(ode_ref_scale)
  call IO_log("[driver] Allocating data arrays for mdot evolution, saving data in arrauys")
  if (dr_include_tides) then 
    allocate(dr_mdot_vector_old(6),dr_mdot_vector_new(6),ode_ref_scale(6))
    dr_mdot_vector_old(SEPA_VAR) = cp_bin_sepa
    dr_mdot_vector_old(MDON_VAR) = cp_don_mass
    dr_mdot_vector_old(MACC_VAR) = cp_acc_mass
    dr_mdot_vector_old(MENV_VAR) = 0.0 
    dr_mdot_vector_old(FDON_VAR) = cp_don_freq
    dr_mdot_vector_old(FACC_VAR) = cp_acc_freq
  else
    allocate(dr_mdot_vector_old(4),dr_mdot_vector_new(4),ode_ref_scale(4))
    dr_mdot_vector_old(SEPA_VAR) = cp_bin_sepa
    dr_mdot_vector_old(MDON_VAR) = cp_don_mass
    dr_mdot_vector_old(MACC_VAR) = cp_acc_mass
    dr_mdot_vector_old(MENV_VAR) = 0.0 
  end if  
  ode_ref_scale           = dr_res_factor*dr_mdot_vector_old
  ode_ref_scale(menv_var) = donmass/1000000.0
  if (dr_include_tides) then 
  ode_ref_scale(fdon_var) = cp_bin_freq
  ode_ref_scale(facc_var) = cp_bin_freq
  end if
  dr_mdot_vector_new      = dr_mdot_vector_old
  dr_mdot_new             = cp_don_mdot
  dr_mdot_ref             = cp_don_mdot
  dr_period_new           = cp_bin_peri
  dr_period_ref           = cp_bin_peri
  call IO_log("[driver] Arrays allocated, initial conditions have been set.")
  dr_initial_mdot_tstep = .false.
  if (cp_bin_sepa.le.0.0) then 
    call dr_abort("driver","Separation is null or negative")
  else
  end if
  if (cp_bin_peri.le.0.0) then 
    call dr_abort("driver","Separation is null or negative")
  else
  end if
  return 
  end subroutine dr_init_mdot
